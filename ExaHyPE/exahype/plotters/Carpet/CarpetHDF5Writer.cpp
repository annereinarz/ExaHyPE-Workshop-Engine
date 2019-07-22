#include "exahype/plotters/Carpet/CarpetHDF5Writer.h"
#include "peano/utils/Loop.h" // dfor
#include "tarch/parallel/Node.h" // for basic MPI rank determination only
#include <sstream>
#include <stdexcept>

typedef tarch::la::Vector<DIMENSIONS, double> dvec;
typedef tarch::la::Vector<DIMENSIONS, bool> boolvec;
typedef tarch::la::Vector<DIMENSIONS, int> ivec;

exahype::plotters::CarpetHDF5Writer::~CarpetHDF5Writer() {}

#ifndef HDF5
  // CarpetHDF5Writer Dummy implementation in case HDF5 libraries are not available.

  exahype::plotters::CarpetHDF5Writer::CarpetHDF5Writer(const std::string& _filename, int _basisSize, int _solverUnknowns, int _writtenUnknowns, exahype::parser::ParserView _plotterParameters, char** _writtenQuantitiesNames)
    : CarpetWriter(_filename, _basisSize, _solverUnknowns, _writtenUnknowns, _plotterParameters, _writtenQuantitiesNames),
      _log("exahype::plotters::CarpetHDF5Writer")
    {
	logError("CarpetHDF5Writer()", "Compile with -DHDF5, otherwise you cannot use the CarpetHDF5Writer. There will be no output going to " << _filename << " today.");
      	if(std::getenv("EXAHYPE_STRICT")) {
		logError("CarpetHDF5Writer()", "Aborting since EXAHYPE_STRICT is given.");
		abort();
	} else {
		logError("CarpetHDF5Writer()", "Will fail gracefully. If you want to stop the program in such a case, please set the environment variable EXAHYPE_STRICT=\"Yes\".");
	}
  }
  void exahype::plotters::CarpetHDF5Writer::writeBasicGroup(H5::H5File* file, int writtenUnknown) {}
  void exahype::plotters::CarpetHDF5Writer::openFile() {}
  void exahype::plotters::CarpetHDF5Writer::flushFile() {}
  void exahype::plotters::CarpetHDF5Writer::closeFile() {}
  void exahype::plotters::CarpetHDF5Writer::startPlotting(double time) {
	logError("startPlotting()", "Skipping HDF5 output due to missing support.");
  }
  void exahype::plotters::CarpetHDF5Writer::finishPlotting() {}
  void exahype::plotters::CarpetHDF5Writer::plotPatch(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus) {}
  void exahype::plotters::CarpetHDF5Writer::plotPatchForSingleUnknown(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus,
      int writtenUnknown, H5::H5File* target) {}

#else /* HDF5 support in ExaHyPE is active */

// HDF5 library, only available if HDF5 is on the path
#include "H5Cpp.h"
using namespace H5;

// my small C++11 to_string-independent workaround.
template <typename T> inline std::string toString( T Number ) {
	std::ostringstream ss; ss << Number; return ss.str();
}

exahype::plotters::CarpetHDF5Writer::CarpetHDF5Writer(
	const std::string& _filename,
	int _basisSize,
	int _solverUnknowns,
	int _writtenUnknowns,
	exahype::parser::ParserView  _plotterParameters,
	char** _writtenQuantitiesNames)
	:
	CarpetWriter(_filename, _basisSize, _solverUnknowns, _writtenUnknowns, _plotterParameters, _writtenQuantitiesNames),
	_log("exahype::plotters::CarpetHDF5Writer"),
	files(allUnknownsInOneFile ? 1 : writtenUnknowns)
	{

	// this is the dataspace describing how to write a patch/cell/component
	hsize_t *dims = new hsize_t[dim];
	std::fill_n(dims, dim, basisSize);
	patch_space = new DataSpace(dim, dims);
	
	// this is just a vector of rank 1 with dim entries
	const int tupleDim_rank = 1;
	hsize_t tupleDim_len[tupleDim_rank];
	std::fill_n(tupleDim_len, tupleDim_rank, dim);
	dtuple = new DataSpace(tupleDim_rank, tupleDim_len);
	
	// open file(s) initially, if neccessary
	if(!oneFilePerTimestep) openFile();
}

void exahype::plotters::CarpetHDF5Writer::writeBasicGroup(H5::H5File* file, int writtenUnknown) {
	Group* parameters = new Group(file->createGroup( "/Parameters and Global Attributes" ));
	
	// nioprocs is required
	int ranks = tarch::parallel::Node::getInstance().getNumberOfNodes();
	Attribute nioprocs = parameters->createAttribute("nioprocs", PredType::NATIVE_INT, H5S_SCALAR);
	nioprocs.write(PredType::NATIVE_INT, &ranks);
	
	// authoringCode is just for inforamative purpose
	std::string authoringCode = "ExaHyPE (CarpetHDF5Writer)";
	StrType t_str = H5::StrType(H5::PredType::C_S1, authoringCode.size()+1); // probably +1 for \0
	Attribute authoringCode_attr = parameters->createAttribute("authoringCode", t_str, H5S_SCALAR);
	authoringCode_attr.write(t_str, authoringCode.c_str());
	
	/* other attributes which are typically present, with values:
	        Cactus version = 4.3.0
		GH$iteration = 0
		build id = build-somerville9-supermuc-di25cux3-2017.04.08-12.51.39-16854
		carpet_delta_time = 0.96
		carpet_global_time = 0.0
		carpet_reflevels = 6
		config id = config-somerville9-supermuc-home-hpc-pr62do-di25cux3-ET-somerville-thc-Cactus
		main loop index = 0
		nioprocs = 1
		run id = run-bns_sfho_em_1-i12r01c07-di25cux3-2017.05.23-19.48.04-21052
		simulation id = run-bns_sfho_em_1-i12r01c07-di25cux3-2017.05.23-19.48.04-21052
	
	  Note that these are not required, except nioprocs. */
	
	// Todo: add information how much files are printed (oneFilePerTimestep)
	
	
	// Dataset to write out
	// hsize_t *str_dims = new hsize_t[writtenUnknowns];
	// std::fill_n(str_dims, writtenUnknowns, /* ranks */ 1);
	// DataSpace datasets_string_space(writtenUnknowns, str_dims);

	// List all fields which go into this file (required by rugutils, not by visit),
	// goes to "/Parameters and Global Attributes/Datasets" and contains the qualified variable names
	
	hsize_t str_dimsf[1] {(hsize_t) (allUnknownsInOneFile ? writtenUnknowns : 1)};
        DataSpace str_dataspace(1, str_dimsf);
	StrType str_datatype(H5::PredType::C_S1, H5T_VARIABLE); 
	DataSet list_of_datasets = parameters->createDataSet("Datasets", str_datatype, str_dataspace);
	
	std::vector<const char*> qualifiedWrittenQuantitiesNames_c;
	if(allUnknownsInOneFile) {
		for (int ii = 0; ii < writtenUnknowns; ++ii) 
			qualifiedWrittenQuantitiesNames_c.push_back(qualifiedWrittenQuantitiesNames[ii].c_str());
	} else {
		qualifiedWrittenQuantitiesNames_c.push_back(qualifiedWrittenQuantitiesNames[writtenUnknown].c_str());
	}
		
	list_of_datasets.write(qualifiedWrittenQuantitiesNames_c.data(), str_datatype);
}

/**
 * Opens or switchs the currently active H5 file or the list of H5 files.
 * 
 * ATTENTION: The composal of the CarpetHDF5 filename has to be chosen carefully. Some (!) readers except
 *    for 1D files a pattern *.{x,y,z}.h5
 *    for 2D files a pattern *.{xy,yz,xz}.h5
 *    for 3D files a pattern *.xyz.h5
 *    (all patterns here are bash linux command line globbing patterns)
 * This is especially for the readers from David Radice (https://bitbucket.org/dradice/scidata)
 * and Wolfgang Kastaun (https://bitbucket.org/DrWhat/pycactuset). In contrast, the Visit plugin does not
 * care about the names of the files.
 **/
void exahype::plotters::CarpetHDF5Writer::openFile() {
	std::string local_filename, suffix, prefix, sep("-");
	prefix = basisFilename;
	suffix = (oneFilePerTimestep?(sep + "it" + toString(iteration)):"") + dimextension + ".h5";
	
	closeFile(); // just to be sure
	int writtenUnknown=0;
	for(auto& file : files) {
		local_filename = prefix + (allUnknownsInOneFile ? "" : (sep + writtenQuantitiesNames[writtenUnknown]));
		#ifdef Parallel
		local_filename += sep + "rank" + toString(tarch::parallel::Node::getInstance().getRank());
		#endif
		local_filename += suffix;

		logInfo("open", "Opening File '"<< local_filename << "'");
		file = new H5File(local_filename, H5F_ACC_TRUNC);
		file->setComment("Created by ExaHyPE/CarpetHDF5Writer");
		writeBasicGroup(file, writtenUnknown);
		writtenUnknown++;
	}
}

void exahype::plotters::CarpetHDF5Writer::closeFile() {
	// flush in any case, even when closing the file. Cf. http://stackoverflow.com/a/31301117
	flushFile();
	for(auto& file : files) {
		if(file) {
			file->close();
			delete file;
			file = nullptr;
		}
	}
}

void exahype::plotters::CarpetHDF5Writer::flushFile() {
	for(auto& file : files) {
		if(file) {
			file->flush(H5F_SCOPE_GLOBAL);
		}
	}
}

void exahype::plotters::CarpetHDF5Writer::startPlotting(double time) {
	component = 0; // CarpetHDF5 wants the components start with 0.

	if(oneFilePerTimestep) openFile();
}
  
void exahype::plotters::CarpetHDF5Writer::finishPlotting() {
	if(oneFilePerTimestep) closeFile();
	else flushFile();

	iteration++;
}

/**
 * This is 2D and 3D, allows several unknowns, named fields and all that.
 **/
void exahype::plotters::CarpetHDF5Writer::plotPatch(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus) {
	for(int writtenUnknown=0; writtenUnknown < writtenUnknowns; writtenUnknown++) {
		H5::H5File* target = files[allUnknownsInOneFile ? 0 : writtenUnknown];
		plotPatchForSingleUnknown(offsetOfPatch, sizeOfPatch, dx, mappedCell, timeStamp, limiterStatus, writtenUnknown, target);
	} // for writtenUnknown
	component++;
}

/**
 * ATTENTION at  composing the HDF5 data field names for the Carpet file format!
 * Many (actually all I know) codes parse these field names with regexps which are sometimes not too flexible.
 * For instance, Visit requries the presence of "::" in the field name.
 * Scidata parses with this python regexp: r"(\w+:?:?\w*\[?\d*\]?) it=(\d+) tl=(\d+) rl=(\d+) c=(\d+)$"
 * That means the whitespace is crucial and has to be exactly like this.
 **/
void exahype::plotters::CarpetHDF5Writer::plotPatchForSingleUnknown(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus_data,
      int writtenUnknown, H5::H5File* target) {
	assertion(target != nullptr);
	
	const std::string name = qualifiedWrittenQuantitiesNames[writtenUnknown];
	// Attention: I removed "tl=0 m=0 rl=0" => "tl=0 rl=0" for scidata.
	std::stringstream component_name;
	component_name << name << " it=" << iteration << " tl=0 rl=0 c=" << component;
	
	double debugCheckForNans = false;
	
	// 1) Compose a continous storage which is suitable.
	// TODO: I'm sure HDF5 provides a more performant way to interpret the different data layout.
	double *componentPatch = new double[singleFieldIdx->size];
	ivec i(0);
	// dfor(i,basisSize)  // replacing with proper loops
	switch(dim) {
		case 3:
		for(i(0)=0; i(0)<basisSize; i(0)++)
		for(i(1)=0; i(1)<basisSize; i(1)++)
		for(i(2)=0; i(2)<basisSize; i(2)++) {
			componentPatch[singleFieldIdx->get(i(2),i(1),i(0))] = mappedCell[writtenCellIdx->get(i(2),i(1),i(0),writtenUnknown)];
			if(debugCheckForNans && componentPatch[singleFieldIdx->get(i(2),i(1),i(0))] != componentPatch[singleFieldIdx->get(i(2),i(1),i(0))] ) {
				//logError("plotPatchForSingleUnknown()", "At dim=3, i[]="<< i(0) <<","<< i(1) <<","<< i(2) << "; value is NaN");
				//throw std::domain_error("NaN found");
			}
		}
		break;

		case 2:
		for(i(0)=0; i(0)<basisSize; i(0)++)
		for(i(1)=0; i(1)<basisSize; i(1)++) {
			componentPatch[singleFieldIdx->get(i(1),i(0))] = mappedCell[writtenCellIdx->get(i(1),i(0),writtenUnknown)];
			if(debugCheckForNans && componentPatch[singleFieldIdx->get(i(1),i(0))] != componentPatch[singleFieldIdx->get(i(1),i(0))]) {
				logError("plotPatchForSingleUnknown()", "At dim=2, i[]="<< i(0) <<","<< i(1) << "; value is NaN");
				throw std::domain_error("NaN found");
			}

		}
		break;
		
		case 1:
		for(i(0)=0; i(0)<basisSize; i(0)++) {
			componentPatch[singleFieldIdx->get(i(0))] = mappedCell[writtenCellIdx->get(i(0),writtenUnknown)];
			if(debugCheckForNans && componentPatch[singleFieldIdx->get(i(0))] != componentPatch[singleFieldIdx->get(i(0))]) {
				logError("plotPatchForSingleUnknown()", "At dim=1, i[]="<< i(0) << "; value is NaN");
				throw std::domain_error("NaN found");
			}
		}
		break;
	}
	
	DataSet table = target->createDataSet(component_name.str(), PredType::NATIVE_FLOAT, *patch_space);
	table.write(componentPatch, PredType::NATIVE_DOUBLE);
	
	delete[] componentPatch;
	
	// write all meta information about the table

	Attribute origin = table.createAttribute("origin", PredType::NATIVE_DOUBLE, *dtuple);
	origin.write(PredType::NATIVE_DOUBLE, offsetOfPatch.data());
	
	dvec iorigin_data = 0.0;
	Attribute iorigin = table.createAttribute("iorigin", PredType::NATIVE_INT, *dtuple);
	iorigin.write(PredType::NATIVE_INT, iorigin_data.data());
	
	int level_data = 0; // TODO: Read out real (AMR) cell level (needs to be passed from ADERDG2CarpetHDF5 class, for instance)
	Attribute level = table.createAttribute("level", PredType::NATIVE_INT, H5S_SCALAR);
	level.write(PredType::NATIVE_INT, &level_data);

	// This is not a Carpet metadata but something we add from the ExaHyPE side.
	// It tells about the limiter status and typically reads like 0-O,1..2-DG,3..4-FV,5-T
	Attribute limiterStatus = table.createAttribute("limiterStatus", PredType::NATIVE_INT, H5S_SCALAR);
	limiterStatus.write(PredType::NATIVE_INT, &limiterStatus_data);
	
	Attribute timestep = table.createAttribute("timestep", PredType::NATIVE_INT, H5S_SCALAR);
	timestep.write(PredType::NATIVE_INT, &iteration);

	Attribute time = table.createAttribute("time", PredType::NATIVE_DOUBLE, H5S_SCALAR);
	time.write(PredType::NATIVE_DOUBLE, &timeStamp);

	// dx in terms of Cactus: Real seperation from each value
	Attribute delta = table.createAttribute("delta", PredType::NATIVE_FLOAT, *dtuple);
	delta.write(PredType::NATIVE_DOUBLE, dx.data()); // issue: conversion from double to float
	
	StrType t_str = H5::StrType(H5::PredType::C_S1, name.size()+1); // Todo: not sure about +1 for \0
	Attribute aname = table.createAttribute("name", t_str, H5S_SCALAR);
	aname.write(t_str, name.c_str());
	
	int cctk_nghostzones_data = 0;  // no ghostzones for ADERDG; we won't save ghostzones for FV solvers.
	Attribute nghostzones = table.createAttribute("cctk_nghostzones", PredType::NATIVE_INT, H5S_SCALAR);
	nghostzones.write(PredType::NATIVE_INT, &cctk_nghostzones_data);
}


#endif /* HDF5 */
