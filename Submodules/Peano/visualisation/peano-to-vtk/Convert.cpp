#include <iostream>
#include <vector>
#include <string>
#include <limits>

#include "PeanoReader.h"
#include "PeanoConverter.h"
#include "PeanoMetaFile.h"
#include "PeanoDataSet.h"

#include <vtkSmartPointer.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkMergeCells.h>


#ifndef noCreateDirectory
#include <experimental/filesystem> // or #include <filesystem>
#endif


void convertFile( std::string filename, const std::string& outputDirectory ) {
  std::string outFile = outputDirectory + "/" + filename.erase(filename.find_last_of(".") );
  std::cout << "writing file " << outFile << std::endl;

  PeanoReader reader( filename );
  PeanoConverter::combineAndWriteToFile( reader.patches, outFile );
}


void convertTimeSeries( std::string filename, std::string outputDirectory ) {
  std::cout << "read file " << filename << std::endl;
  PeanoMetaFile reader( filename );


  std::string outFileNamePrefix  = outputDirectory + "/" + filename.erase(filename.find_last_of(".") );
  std::string outFileName        = outFileNamePrefix + "-full-resolution.pvd";


  #ifdef noCreateDirectory
  std::string dataFileNamePrefix = outFileNamePrefix;
  #else
  std::string dataFileNamePrefix = outFileNamePrefix + "-full-resolution";
  if (
    !std::experimental::filesystem::is_directory(dataFileNamePrefix)
    ||
    !std::experimental::filesystem::exists(dataFileNamePrefix)
  ) {
	try {
      std::experimental::filesystem::create_directory(dataFileNamePrefix);
      std::cout << "created directory " << dataFileNamePrefix << std::endl;
	}
	catch (std::exception exc) {
      std::cerr << "failed to create directory " << dataFileNamePrefix << std::endl;
      std::cerr << "error message: " << exc.what() << std::endl;
	}
  }
  dataFileNamePrefix += "/data";
  #endif

  std::ofstream pvdFile(outFileName);
  pvdFile << "<?xml version=\"1.0\"?>" << std::endl
		  << "<VTKFile type=\"Collection\" version=\"0.1\" >" << std::endl
		  << "<Collection>" << std::endl;
/*
		  ""
		  "byte_order="LittleEndian"
         compressor="vtkZLibDataCompressor">
*/

  int timeStepCounter = 0;
  std::vector<PeanoDataSet*>* dataSets = reader.getDataSets();
  for( auto timeStep: *dataSets ) {
    std::vector<PeanoReader*>* readers = timeStep->createReadersFull();

    #pragma omp parallel for
    for( int i=0; i<readers->size(); i++) {
      auto p = (*readers)[i];
      std::string outFile = dataFileNamePrefix + "-" + std::to_string(i) + "-" + std::to_string(timeStepCounter);

      #pragma omp critical
      std::cout << "writing file " << outFile << std::endl;

      if (!p->patches.empty()) {
        std::string filename =  PeanoConverter::combineAndWriteToFile( p->patches, outFile );

        // Strip output directory
        std::string toRemove = outputDirectory + "/";
        int pos = filename.find(toRemove);
        filename.erase(pos, toRemove.length());

        #pragma omp critical
        {
        pvdFile << "<DataSet timestep=\"" << timeStepCounter << "\" group=\"\" part=\"" << i << "\" "
      	        << " file=\"" << filename << "\" "
		        << "/>" << std::endl;
        }
      }
    }

    timeStepCounter++;
  }

  pvdFile << "</Collection>" << std::endl
		  << "</VTKFile>" << std::endl;
  pvdFile.close();
}


int main(int argc, char* argv[]) {
    std::cout << "Peano block file to vtk converter" << std::endl;
    std::cout << "(C) 2018 Dan Tuthill-Jones, Tobias Weinzierl" << std::endl << std::endl;
    bool validParams = true;

    if(argc < 4) {
      std::cerr << "too few arguments" << std::endl;
      validParams = false;
    }
    else {
      std::string mode = argv[1];
      if (mode.compare("convert-file")==0) {
    	std::string outputDirectory = argv[ argc-1 ];
    	std::cout << "write into directory " << outputDirectory << std::endl;
    	for (int i=2; i<argc-1; i++) {
    	  convertFile( argv[i], outputDirectory );
    	}
      }
      else if (mode.compare("convert-time-series")==0) {
    	std::string outputDirectory = argv[ argc-1 ];
    	std::cout << "write into directory " << outputDirectory << std::endl;
    	for (int i=2; i<argc-1; i++) {
    	  convertTimeSeries( argv[i], outputDirectory );
    	}
      }
      else {
        std::cerr << "unknown command. First argument has to be convert" << std::endl;
        validParams = false;
      }
    }
/*
    if(argc == 6) {
        std::string command = argv[1];
        if(command.compare("subsample") == 0) {
            validParams = true;
            std::string input = argv[2];

        	int xSize = std::stoi(argv[3]);
        	int ySize = std::stoi(argv[4]);
        	int zSize = std::stoi(argv[5]);

            std::cout << "Reading initial file " << input << "...\n";
            PeanoMetaFile metaFile = PeanoMetaFile(input);

            #pragma omp parallel for
            for(int i = 0; i < metaFile.numberOfDataSets(); i++) {
                PeanoDataSet* dataset = metaFile.getDataSet(i);
                PeanoPatch* sample = dataset->createSubSample(xSize, ySize, zSize, true);
                delete sample;
            }

            //save the metadatafile
            metaFile.save();
            return 0;
        }
    } else if(argc == 4) {
        std::string command = argv[1];
        if(command.compare("convert") == 0) {
            validParams = true;
            std::string input = argv[2];
            std::string output = argv[3];

            std::cout << "Reading initial file " << input << "...\n";
            PeanoMetaFile metaFile = PeanoMetaFile(input);

            #pragma omp parallel for
            for(int i = 0; i < metaFile.numberOfDataSets(); i++) {
                PeanoDataSet* dataset = metaFile.getDataSet(i);
                std::vector<PeanoReader*>* readers = dataset->createReadersFull();
                vtkSmartPointer<vtkUnstructuredGrid> vtkGrid = PeanoConverter::combineImageData(readers);

                //save the patch as an XML file
                vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();

                std::string outFile = output + "/" + dataset->getSimpleName() + ".vtk";

                std::cout << "Writing to file " << outFile << "\n";
                writer->SetFileName(outFile.c_str());
                writer->AddInputDataObject(vtkGrid.GetPointer());
                writer->Write();
            }

            return 0;
        }
    }
*/

    if (!validParams) {
      std::cerr << std::endl << std::endl;
      std::cerr << "Usage:";
      std::cerr << "\t./executable convert-file InputFile1 [InputFile2 ...] OutputFolder" << std::endl;
      std::cerr << "\t./executable convert-time-series InputFile1 [InputFile2 ...] OutputFolder" << std::endl;
//      std::cerr << "\t./PeanoStandalone subsample INPUT_FILE X_SIZE Y_SIZE Z_SIZE\n";
 //     std::cerr << "or:\n";
      std::cerr << std::endl << std::endl;
      std::cerr << "Only convert will yield actual VTK files. All other operations create new " << std::endl
      		<< "internal data representations that then can be converted. " << std::endl << std::endl
                << "Please ensure that the OutputFolder exists prior to program invocation." << std::endl << std::endl
                << "convert-file only should be used for individual snapshot files, but not for files referring to/including other files" << std::endl << std::endl
                << "Please invoke the tool from the directory the actual input files are stored in, i.e. do not refer to files stored relative" << std::endl
                << "to the current directory." << std::endl << std::endl;
      return -1;
    }
    else return 0;
}
