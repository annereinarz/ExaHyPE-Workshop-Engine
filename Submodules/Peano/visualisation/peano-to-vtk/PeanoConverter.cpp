/*
 * PeanoPatch.cpp
 *
 *  Created on: 16 Oct 2017
 *      Author: dan
 */

#include "PeanoPatch.h"
#include "PeanoConverter.h"
#include "PeanoPatchData.h"
#include "PeanoVariable.h"

#include <string>
#include <unordered_map>
#include <vector>

#include "vtkImageData.h"
#include "vtkSmartPointer.h"
#include "vtkDoubleArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkAppendFilter.h"
#include "vtkUnstructuredGrid.h"
#include "vtkPoints.h"
#include "vtkIdTypeArray.h"
#include "vtkCellArray.h"
#include "vtkVoxel.h"
#include "vtkQuad.h"

#include <vtkSmartPointer.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkMergeCells.h>


vtkSmartPointer<vtkImageData> PeanoConverter::toImageData(PeanoPatch *patch) {
	vtkSmartPointer<vtkImageData> imageData = vtkSmartPointer<vtkImageData>::New();


	//get the data from the patch in to arrays
	double spacing[3] = {1, 1, 1};
	double offSets[3] = {0, 0, 0};
	int dimensions[3] = {1, 1, 1};

	for(int i = 0; i < patch->dimensions; i++) {
		dimensions[i] = patch->resolution[i] +1;
		offSets[i] = patch->offsets[i];
		spacing[i] = patch->sizes[i]/patch->resolution[i];
	}

	imageData->SetDimensions(dimensions);
	imageData->SetOrigin(offSets);
	imageData->SetSpacing(spacing);


	//allocate any variables
	for(auto kv : patch->patchData) {
		PeanoPatchData* data = kv.second;
		vtkSmartPointer<vtkDoubleArray> variableArray = vtkSmartPointer<vtkDoubleArray>::New();
		variableArray->SetNumberOfComponents(data->structure->unknowns);
		variableArray->SetName(data->structure->name.c_str());
		//std:: << "\n " << data->variableName << ": ";
		for(int i = 0; i < data->structure->totalValues; i += data->structure->unknowns) {
			variableArray->InsertNextTuple(&data->values[i]);
		}

		if(data->structure->type == Cell_Values) {
			imageData->GetCellData()->AddArray(variableArray);
		} else if(data->structure->type == Vertex_Values) {
			imageData->GetPointData()->AddArray(variableArray);
		}
	}
	return imageData;
}


vtkSmartPointer<vtkUnstructuredGrid> PeanoConverter::toUnstructuredGrid(PeanoPatch *patch) {
	vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();

	PeanoVariable* structure = patch->getStructure();

	double* offsets = patch->offsets;
	double* sizes = patch->sizes;
	double* mapping = structure->mapping;
	int mappings = structure->mappings;
	int dimensions = patch->dimensions;
	int numberOfPoints = mappings/patch->dimensions;

	points->SetNumberOfPoints(numberOfPoints);

	for(int i = 0; i < numberOfPoints; i++) {
		int index = i*patch->dimensions;

		double position[3] = {0, 0, 0};
		for(int j = 0; j < patch->dimensions; j++) {
			position[j] = mapping[index + j]*sizes[j] + offsets[j];
		}
		points->SetPoint(i, position);
	}


	int dimensions3D[3] = {1, 1, 1};
	int totalCells = 1;
	for(int i = 0; i < patch->dimensions; i++) {
		totalCells *= patch->resolution[i];
		dimensions3D[i] = patch->resolution[i];
	}

	vtkSmartPointer<vtkCellArray> connectivity = vtkSmartPointer<vtkCellArray>::New();
	connectivity->Allocate(VTK_VOXEL,totalCells);

    vtkSmartPointer<vtkUnstructuredGrid> grid = vtkSmartPointer<vtkUnstructuredGrid>::New();
	if (patch->dimensions==3) {
      for(int x = 0; x < dimensions3D[0]; x++) {
		for(int y = 0; y < dimensions3D[1]; y++) {
			for(int z = 0; z < dimensions3D[2]; z++) {
				vtkSmartPointer<vtkVoxel> voxel = vtkSmartPointer<vtkVoxel>::New();
				vtkIdList* points = voxel->GetPointIds();
				points->SetId(0, xyzToIndex(x,y,z, dimensions3D));
				points->SetId(1, xyzToIndex(x+1,y,z, dimensions3D));
				points->SetId(2, xyzToIndex(x,y+1,z, dimensions3D));
				points->SetId(3, xyzToIndex(x+1,y+1,z, dimensions3D));
				points->SetId(4, xyzToIndex(x,y,z+1, dimensions3D));
				points->SetId(5, xyzToIndex(x+1,y,z+1, dimensions3D));
				points->SetId(6, xyzToIndex(x,y+1,z+1, dimensions3D));
				points->SetId(7, xyzToIndex(x+1,y+1,z+1, dimensions3D));

				connectivity->InsertNextCell(voxel);
			}
		}
	  }
      grid->SetPoints(points);
      grid->SetCells(VTK_VOXEL, connectivity);
	}
	else {
      for(int x = 0; x < dimensions3D[0]; x++) {
		for(int y = 0; y < dimensions3D[1]; y++) {
          vtkSmartPointer<vtkQuad> voxel = vtkSmartPointer<vtkQuad>::New();
          vtkIdList* points = voxel->GetPointIds();
          points->SetId(0, xyzToIndex(x,y,0, dimensions3D));
		  points->SetId(1, xyzToIndex(x+1,y,0, dimensions3D));
		  points->SetId(2, xyzToIndex(x+1,y+1,0, dimensions3D));
		  points->SetId(3, xyzToIndex(x,y+1,0, dimensions3D));

          connectivity->InsertNextCell(voxel);
        }
      }
      grid->SetPoints(points);
      grid->SetCells(VTK_QUAD, connectivity);
	}


	//allocate any variables
	std::vector<vtkSmartPointer<vtkDoubleArray>> variables;
	for(auto kv : patch->patchData) {
		PeanoPatchData* data = kv.second;
		vtkSmartPointer<vtkDoubleArray> variableArray = vtkSmartPointer<vtkDoubleArray>::New();
		variableArray->SetNumberOfComponents(data->structure->unknowns);
		variableArray->SetName(data->structure->name.c_str());
		//std:: << "\n " << data->variableName << ": ";
		for(int i = 0; i < data->structure->totalValues; i += data->structure->unknowns) {
			variableArray->InsertNextTuple(&data->values[i]);
		}

		if(data->structure->type == Cell_Values) {
			grid->GetCellData()->AddArray(variableArray);
		} else if(data->structure->type == Vertex_Values) {
			grid->GetPointData()->AddArray(variableArray);
		}
	}

	return grid;
}


int PeanoConverter::xyzToIndex(int x, int y, int z, int dimensions[3]) {
	return x + y*(dimensions[0]+1) + z*(dimensions[0]+1)*(dimensions[1]+1);
}


std::string PeanoConverter::combineAndWriteToFile(const std::vector<PeanoPatch*>& patches, const std::string& outputFileWithoutExtention) {
  vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();

  std::string outFile = outputFileWithoutExtention + "." + writer->GetDefaultFileExtension();

  vtkSmartPointer<vtkUnstructuredGrid> outputGrid = PeanoConverter::combine( patches );

  writer->SetFileName(outFile.c_str());
  writer->SetCompressorTypeToZLib();
  writer->AddInputDataObject( outputGrid );
  writer->Write();

  return outFile;
}


vtkSmartPointer<vtkUnstructuredGrid> PeanoConverter::combine(const std::vector<PeanoPatch*>& patches){
  vtkSmartPointer<vtkAppendFilter> appendFilter = vtkSmartPointer<vtkAppendFilter>::New();
  for(uint i = 0; i < patches.size(); i++) {
    PeanoPatch* patch = patches.at(i);
   	if(patch->hasMappings()) {
	  appendFilter->AddInputData(toUnstructuredGrid(patches.at(i)));
	} else {
      appendFilter->AddInputData(toImageData(patches.at(i)));
	}
  }

  appendFilter->Update();
  vtkSmartPointer<vtkUnstructuredGrid> combined = vtkSmartPointer<vtkUnstructuredGrid>::New();
  combined->ShallowCopy(appendFilter->GetOutput());

  return combined;
}


vtkSmartPointer<vtkUnstructuredGrid> PeanoConverter::combine(const std::vector<PeanoReader*>& readers) {
	vtkSmartPointer<vtkAppendFilter> appendFilter = vtkSmartPointer<vtkAppendFilter>::New();
	for(uint i = 0; i < readers.size(); i++) {
		std::vector<PeanoPatch*> patches = readers.at(i)->patches;
		for(uint j = 0; j < patches.size(); j++) {
			PeanoPatch* patch = patches[j];
			if(patch->hasMappings()) {
				appendFilter->AddInputData(toUnstructuredGrid(patch));
			} else {
				appendFilter->AddInputData(toImageData(patch));
			}
		}
	}

	appendFilter->Update();
	vtkSmartPointer<vtkUnstructuredGrid> combined = vtkSmartPointer<vtkUnstructuredGrid>::New();
	combined->ShallowCopy(appendFilter->GetOutput());

	return combined;
}

PeanoPatch* PeanoConverter::subSample(std::vector<PeanoReader*> &readers, int x, int y, int z) {
	std::vector<PeanoPatch*> patches;

	double xMax, yMax, zMax, xMin, yMin, zMin;
	xMax = yMax = zMax = std::numeric_limits<double>::min();
	xMin = yMin = zMin = std::numeric_limits<double>::max();

	//calculate the minimum & maximum sizes and add all the patches to the patches vector
	for(uint i = 0; i < readers.size(); i++) {
		//add all the reader's patches to the patch vector
		std::vector<PeanoPatch*> currentPatches = readers[i]->patches;
		for(uint j = 0; j < currentPatches.size(); j++) {
			PeanoPatch* patch = currentPatches[j];
			patches.push_back(patch);
			if(patch->offsets[0] < xMin) xMin = patch->offsets[0];
			if(patch->offsets[1] < yMin) yMin = patch->offsets[1];
			if(patch->offsets[2] < zMin) zMin = patch->offsets[2];
			if(patch->offsets[0] + patch->sizes[0] > xMax) xMax = patch->offsets[0] + patch->sizes[0];
			if(patch->offsets[1] + patch->sizes[1] > yMax) yMax = patch->offsets[1] + patch->sizes[1];
			if(patch->offsets[2] + patch->sizes[2] > zMax) zMax = patch->offsets[2] + patch->sizes[2];
		}
	}

	std::vector<int> patchSize;
	patchSize.push_back(x);
	patchSize.push_back(y);
	patchSize.push_back(z);

	int* resolution = new int[3];
	resolution[0] = x;
	resolution[1] = y;
	resolution[2] = z;

	double* offsets = new double[3];
	offsets[0] = xMin;
	offsets[1] = yMin;
	offsets[2] = zMin;

	double* sizes = new double[3];
	sizes[0] = xMax - xMin;
	sizes[1] = yMax - yMin;
	sizes[2] = zMax - zMin;


	//put the pieces together
	PeanoPatch* outPatch = new PeanoPatch();
	outPatch->dimensions = 3;
	outPatch->offsets = offsets;
	outPatch->resolution = resolution;
	outPatch->sizes = sizes;

	int outputCells = x*y*z;
	int outputVertices = (x+1)*(y+1)*(z+1);


	//std::unordered_map<std::string, PeanoPatchData*> patchData;


	//create a list of patchData for the new patch
	std::vector<PeanoPatchData*> datas;
	PeanoPatch* patch1 = patches[0];
	for (auto it : patch1->patchData) {
		PeanoVariable* var = it.second->structure;
		PeanoVariable* newVar = new PeanoVariable(var->name, var->unknowns, var->type,
				(var->type==Cell_Values?outputCells:outputVertices), nullptr, -1);

		PeanoPatchData* newData = new PeanoPatchData(newVar);
		//set all values to 0
		for(int j = 0; j < newVar->totalValues; j++) {
			newData->values[j] = 0;
		}

		//add the data to the patch and to our list of data objects
		outPatch->patchData[newData->structure->name] = newData;
		datas.push_back(newData);
	}

	for(uint i = 0; i < patches.size(); i++) {
		PeanoPatch* patch = patches[i];
		for (uint dataIndex = 0; dataIndex < datas.size(); dataIndex++) {
			PeanoPatchData* newData = datas[dataIndex];
			PeanoVariable* newVar = newData->structure;
			PeanoPatchData* oldData = patch->patchData[newData->structure->name];
			PeanoVariable* oldVar = oldData->structure;

			int unknowns = newVar->unknowns;
			double* data = oldData->values;

			int x = 0;
			int y = 0;
			int z = 0;

			//std::cout << "total values = " << oldVar->totalValues << "\n";

			if(newData->structure->type == Cell_Values) {
				for(int j = 0; j < oldVar->totalValues; j+= oldVar->unknowns) {
					double* position = patch->getPositionCellCenter(x,y,z);

					//see my notes for this equation
					int xCell = (position[0] - offsets[0])*resolution[0]/sizes[0];
					int yCell = (position[1] - offsets[1])*resolution[1]/sizes[1];
					int zCell = (position[2] - offsets[2])*resolution[2]/sizes[2];

					delete position;

					int index = outPatch->getIndexCellData(xCell, yCell, zCell);
					newData->setData(index, data +j);

					//increment the x,y,z values
					z++;
					if(z == patch->resolution[2]) {
						z = 0;
						y++;
						if(y == patch->resolution[1]) {
							y = 0;
							x++;
						}
					}
				}
				int breakpoint = 0;
			} else {//Vertex_Values
				for(int j = 0; j < oldVar->totalValues; j+= oldVar->unknowns) {
					double* position = patch->getPositionVertex(x,y,z);

					//see my notes for this equation
					int xVert = ((position[0] - offsets[0])*resolution[0]/sizes[0])+0.5;
					int yVert = ((position[1] - offsets[1])*resolution[1]/sizes[1])+0.5;
					int zVert = ((position[2] - offsets[2])*resolution[2]/sizes[2])+0.5;

					delete position;

					int index = outPatch->getIndexVertexData(xVert, yVert, zVert);
					newData->setData(index, data +j);

					//increment the x,y,z values
					z++;
					if(z == patch->resolution[2] +1) {
						z = 0;
						y++;
						if(y == patch->resolution[1] +1) {
							y = 0;
							x++;
						}
					}
				}
				int breakpoint = 0;
			}
		}
	}
	return outPatch;
}
