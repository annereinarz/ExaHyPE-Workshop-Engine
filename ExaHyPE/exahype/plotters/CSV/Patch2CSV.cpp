/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon 
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/
 
#include "tarch/parallel/Node.h"
#include "tarch/parallel/NodePool.h"

#include "exahype/solvers/FiniteVolumesSolver.h"
#include "exahype/plotters/slicing/Slicer.h"
#include "Patch2CSV.h"

#include "exahype/plotters/ascii/CSVWriter.h"

#include "kernels/DGMatrices.h"
#include "kernels/KernelUtils.h" // index functions
#include "peano/utils/Loop.h"

#include <cstring> // memcpy

struct exahype::plotters::Patch2CSV_Dataset {
	// Geometry
	double timeStamp;
	double offsetOfPatch[DIMENSIONS];
	double sizeOfPatch[DIMENSIONS];

	int cellDescriptionIndex; //< Peano cellInfo._cellDescriptionIndexWriter
	int element;   //< element counter
	int level;     //< AMR level
	
	// Only interesting if it is a limiting application
	int refinementStatus;
	int previousRefinementStatus;
	
	// Only interesting if MPI is enabled
	int rank;
	
	void geometryFromTarch(
	  tarch::la::Vector<DIMENSIONS, double> offsetOfPatchVector,
	  tarch::la::Vector<DIMENSIONS, double> sizeOfPatchVector) {
		std::memcpy(offsetOfPatch, offsetOfPatchVector.data(), DIMENSIONS*sizeof(double));
		std::memcpy(sizeOfPatch,     sizeOfPatchVector.data(), DIMENSIONS*sizeof(double));
	}
};

using D = exahype::plotters::Patch2CSV_Dataset;
static std::vector<exahype::plotters::ascii::CSVWriter::Column> patch2csv_columns = {
	CSVWRITER_DOUBLE_COLUMN(D, timeStamp, "Time of this cell (interesting in case of local time stepping)"),
	CSVWRITER_DOUBLE_COLUMN(D, offsetOfPatch[0], "Position of patch lower left corner (x-direction)"),
	CSVWRITER_DOUBLE_COLUMN(D, offsetOfPatch[1], "Position of patch lower left corner (y-direction)"),
	#if DIMENSIONS > 2
	CSVWRITER_DOUBLE_COLUMN(D, offsetOfPatch[2], "Position of patch lower left corner (z-direction)"),
	#endif

	CSVWRITER_DOUBLE_COLUMN(D, sizeOfPatch[0], "Size of patch (x-direction)"),
	CSVWRITER_DOUBLE_COLUMN(D, sizeOfPatch[1], "Size of patch (y-direction)"),
	#if DIMENSIONS > 2
	CSVWRITER_DOUBLE_COLUMN(D, sizeOfPatch[2], "Size of patch (z-direction"),
	#endif
	
	CSVWRITER_INTEGER_COLUMN(D, cellDescriptionIndex, "Peanos cell index"),
	CSVWRITER_INTEGER_COLUMN(D, element, "Patch counter"),
	CSVWRITER_INTEGER_COLUMN(D, level, "AMR level"),
	
	CSVWRITER_INTEGER_COLUMN(D, refinementStatus, "LimitingADERDG: Refinement status (not relevant in pure DG or FV)"),
	CSVWRITER_INTEGER_COLUMN(D, previousRefinementStatus, "LimitingADERDG: Refinement status in previous timestep (not relevant in pure DG or FV)"),
	
	CSVWRITER_INTEGER_COLUMN(D, rank, "MPI rank which holds this patch (not relevant without MPI)"),
};

tarch::logging::Log exahype::plotters::Patch2CSV::_log("exahype::plotters::Patch2CSV");

std::string exahype::plotters::Patch2CSV::getIdentifier() {
  return "csv::patches::ascii";
}

exahype::plotters::Patch2CSV::Patch2CSV(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    exahype::solvers::Solver::Type solverType):
  Device(postProcessing),
  _solverType(solverType),
  _isLimitingSolver(_solverType == exahype::solvers::Solver::Type::LimitingADERDG),
  #ifdef Parallel
  _hasMPIenabled(true),
  #else
  _hasMPIenabled(false),
  #endif
  _fileCounter(-1),
  _solverUnknowns(-1),
  _writtenUnknowns(-1),
  _writer(new exahype::plotters::ascii::CSVWriter)
  {}

void exahype::plotters::Patch2CSV::init(
  const std::string& filename,
  int                numberOfCellsPerAxis,
  int                unknowns,
  int                writtenUnknowns,
  exahype::parser::ParserView plotterParameters
){
	_filename             = filename;
	_solverUnknowns       = unknowns; // not even needed
	_plotterParameters    = plotterParameters;
	_writtenUnknowns      = writtenUnknowns;

	slicer = Slicer::bestFromSelectionQuery(plotterParameters);
	if(slicer) {
		logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<filename);
	}
}


void exahype::plotters::Patch2CSV::startPlotting( double time ) {
	_fileCounter++;
	_postProcessing->startPlotting( time );
	_time = time;

	std::ostringstream snapshotFileName;
	snapshotFileName << _filename;
	if(_one_file_per_timestep)
		snapshotFileName << "-" << _fileCounter;
	if(_hasMPIenabled) {
		int rank = tarch::parallel::Node::getInstance().getRank();
		snapshotFileName << "-rank" << rank;
	}
	
	if(_one_file_per_timestep || _fileCounter==0) {
		_writer->openFile(snapshotFileName.str());
		_writer->columns = patch2csv_columns;
		_writer->writeHeader();
	}
}


void exahype::plotters::Patch2CSV::finishPlotting() {
	_postProcessing->finishPlotting();
	if(_one_file_per_timestep) _writer->closeFile();
}

exahype::plotters::Patch2CSV::~Patch2CSV() {}

void exahype::plotters::Patch2CSV::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
	exahype::plotters::Patch2CSV_Dataset data;
	data.cellDescriptionIndex = cellInfo._cellDescriptionsIndex;
	data.rank = _hasMPIenabled ? tarch::parallel::Node::getInstance().getRank() : -1;
	data.timeStamp = _time;
	tarch::la::Vector<DIMENSIONS, double> offsetOfPatch, sizeOfPatch;
	
	// we need this code doubling as we have different C++ types. Could probably use templates instead.
	switch(_solverType) {
		case exahype::solvers::Solver::Type::LimitingADERDG:
		case exahype::solvers::Solver::Type::ADERDG: { // scope for variables
			data.element = cellInfo.indexOfADERDGCellDescription(solverNumber);
			auto& solverPatch  = cellInfo._ADERDGCellDescriptions[data.element];
			if(solverPatch.getType()!=exahype::solvers::ADERDGSolver::CellDescription::Type::Cell)
				return; // plot only cells
			//double* solution = static_cast<double*>(solverPatch.getSolution());
			data.timeStamp = solverPatch.getTimeStamp();
			offsetOfPatch = solverPatch.getOffset(),
			sizeOfPatch = solverPatch.getSize();
			data.level = solverPatch.getLevel();
			
			if(_isLimitingSolver) {
				data.refinementStatus         = solverPatch.getRefinementStatus();
				data.previousRefinementStatus = solverPatch.getPreviousRefinementStatus();

				// this comes from LimitingADERDG2CartesianVTK.cpp:
				// ignore limiter status on coarser mesh levels
				assertion(static_cast<unsigned int>(solverPatch.getSolverNumber())<exahype::solvers::RegisteredSolvers.size());
				if (data.level < exahype::solvers::RegisteredSolvers[solverPatch.getSolverNumber()]->getMaximumAdaptiveMeshLevel()) {
					data.refinementStatus         = 0;
					data.previousRefinementStatus = 0;
				}

				assertion(data.refinementStatus >= -1);
			}
			break;
		}
		case exahype::solvers::Solver::Type::FiniteVolumes: {
			data.element = cellInfo.indexOfFiniteVolumesCellDescription(solverNumber);
			auto& solverPatch  = cellInfo._FiniteVolumesCellDescriptions[data.element];
			if(solverPatch.getType()!=exahype::solvers::FiniteVolumesSolver::CellDescription::Type::Cell)
				return; // plot only cells
			// double* solution = static_cast<double*>(solverPatch.getSolution());
			data.timeStamp = solverPatch.getTimeStamp();
			offsetOfPatch = solverPatch.getOffset(),
			sizeOfPatch = solverPatch.getSize();
			data.level = solverPatch.getLevel();
			break;
		}
	} // switch _solverType
	
	data.geometryFromTarch(offsetOfPatch, offsetOfPatch);
	if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
		CSVWRITER_WRITE_ROW(*_writer, data);
	} // if slicer
}
