#include "exahype/plotters/Carpet/CarpetWriter.h"

#include "exahype/plotters/Carpet/CarpetASCIIWriter.h"
#include "exahype/plotters/Carpet/CarpetHDF5Writer.h"

#include <exception>

// my small C++11 to_string-independent workaround.
template <typename T> inline std::string toString( T Number ) {
	std::ostringstream ss; ss << Number; return ss.str();
}

exahype::plotters::CarpetWriter::~CarpetWriter() {}

exahype::plotters::CarpetWriter::CarpetWriter(
	const std::string& _filename,
	int _basisSize,
	int _solverUnknowns,
	int _writtenUnknowns,
	exahype::parser::ParserView  _plotterParameters,
	char** _writtenQuantitiesNames)
	:
	solverUnknowns(_solverUnknowns),
	writtenUnknowns(_writtenUnknowns),
	basisFilename(_filename),
	basisSize(_basisSize),
	plotterParameters(_plotterParameters),
	oneFilePerTimestep(_plotterParameters.getValueAsBoolOrDefault("select/one_file_per_timestep", true)),
	allUnknownsInOneFile(_plotterParameters.getValueAsBoolOrDefault("select/all_unknowns_in_one_file", true)),
	
	// default values for init(...)-level runtime parameters:
	dim(DIMENSIONS),
	slicer(nullptr),
	dimextension(".xyz"),
	component(-100),
	iteration(0),
	writtenQuantitiesNames(_writtenQuantitiesNames)
	{

	// This one is only for the constructor of this abstract class
	static tarch::logging::Log _log("exahype::plotters::CarpetWriter");

	// just for convenience/a service, store an indexer for the actual ExaHyPE cells.
	switch(DIMENSIONS) { // simulation dimensions
		case 3:
		patchCellIdx = new kernels::index(basisSize, basisSize, basisSize, writtenUnknowns);
		break;
		
		case 2:
		patchCellIdx = new kernels::index(basisSize, basisSize, writtenUnknowns);
		break;
		
		default:
			throw std::domain_error("CarpetWriter: I think ExaHyPE only supports 2D and 3D");
	}
	writtenCellIdx = patchCellIdx; // without slicing, this is true.

	slicer = Slicer::bestFromSelectionQuery(plotterParameters);
	if( slicer ) {
	  bool isCartesianSlicer = (slicer->getIdentifier() == "CartesianSlicer");
		logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<basisFilename);
		if(isCartesianSlicer) {
			exahype::plotters::CartesianSlicer* cs = static_cast<CartesianSlicer*>(slicer);
			dim = cs->targetDim;
			dimextension = std::string(".") + cs->planeLabel();
		}
	}

	// Important: The CarpetHDF5Writer assumes that dimensional reduction really happens in the
	//            mapped patches. This is not the case in the VTK plotters which don't make a
	//            difference between CartesianSlicer and RegionSlicer.
	switch(dim) { // written dimensions
		case 3:
		writtenCellIdx = new kernels::index(basisSize, basisSize, basisSize, writtenUnknowns);
		singleFieldIdx = new kernels::index(basisSize, basisSize, basisSize);
		break;
		
		case 2:
		writtenCellIdx = new kernels::index(basisSize, basisSize, writtenUnknowns);
		singleFieldIdx = new kernels::index(basisSize, basisSize);
		break;
		
		case 1:
		writtenCellIdx = new kernels::index(basisSize, writtenUnknowns);
		singleFieldIdx = new kernels::index(basisSize);
		break;
		
		default:
			logError("CarpetWriter", "Error, only dimensions 1, 2, 3 supported. Slicing requested: " << slicer->toString());
			throw std::domain_error("CarpetWriter does not like your domain"); // har har
	}
	
	// just as shorthands
	patchFieldsSize = patchCellIdx->size;
	writtenFieldsSize = writtenCellIdx->size;
	singleFieldSize = singleFieldIdx->size;
	
	// make sure there are reasonable names everywhere
	for(int u=0; u<writtenUnknowns; u++) {
		if(!writtenQuantitiesNames[u]) {
			std::string* replacement_name = new std::string("Q_");
			*replacement_name += toString(u);
			writtenQuantitiesNames[u] = const_cast<char*>(replacement_name->c_str());
		}
		
		// in CarpetHDF5, the field name *must* contain a "::"
		std::string qualifiedName = "ExaHyPE::";
		qualifiedName += writtenQuantitiesNames[u];
		qualifiedWrittenQuantitiesNames.push_back(qualifiedName);
	}
	
	// for Debugging:
	logInfo("CarpetWriter", "Writing in " << dim << " Dimensions, written cell shape " << writtenCellIdx->toString() << ", single field shape " << singleFieldIdx->toString()
		<< ", "
		<< (oneFilePerTimestep ? "One file per timestep" : "All times in one file")
		<< ", "
		<< (allUnknownsInOneFile ? "All unknowns in the same file" : "Each unknown goes in its own file.")
	);
}

exahype::plotters::CarpetWriter* exahype::plotters::CarpetWriter::newCarpetWriterFor(exahype::plotters::CarpetWriter::FileFormat format,
	const std::string& filename, int basisSize, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters, char** writtenQuantitiesNames) {
	
	switch(format) {
		case exahype::plotters::CarpetWriter::FileFormat::FileFormatHDF5:
			return new exahype::plotters::CarpetHDF5Writer (filename, basisSize, solverUnknowns, writtenUnknowns, plotterParameters, writtenQuantitiesNames);
		case exahype::plotters::CarpetWriter::FileFormat::FileFormatASCII:
			return new exahype::plotters::CarpetASCIIWriter(filename, basisSize, solverUnknowns, writtenUnknowns, plotterParameters, writtenQuantitiesNames);
		default:
			throw std::domain_error("Unknown CarpetWriter::FileFormat");
	}
} 
