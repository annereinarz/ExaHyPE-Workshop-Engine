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
#include "Patch2VTK.h"

#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"

// @todo 16/05/03:Dominic Etienne Charreir Plotter depends now on kernels.
// Should thus be placed in kernel module or the solver
// should provide a function that computes solution values
// at equidistant grid points
#include "kernels/DGMatrices.h"
#include "kernels/KernelUtils.h" // index functions
#include "peano/utils/Loop.h"

tarch::logging::Log exahype::plotters::Patch2VTK::_log("exahype::plotters::Patch2VTK");


exahype::plotters::Patch2VTKBoxesAscii::Patch2VTKBoxesAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::ASCIIVTK,false,solvertype) {
}


std::string exahype::plotters::Patch2VTKBoxesAscii::getIdentifier() {
  return "vtk::patches::boxes::ascii";
}


exahype::plotters::Patch2VTKBoxesBinary::Patch2VTKBoxesBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::BinaryVTK,false,solvertype) {
}


std::string exahype::plotters::Patch2VTKBoxesBinary::getIdentifier() {
  return "vtk::patches::boxes::binary";
}


exahype::plotters::Patch2VTUBoxesAscii::Patch2VTUBoxesAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::ASCIIVTU,false,solvertype) {
}


std::string exahype::plotters::Patch2VTUBoxesAscii::getIdentifier() {
  return "vtu::patches::boxes::ascii";
}


exahype::plotters::Patch2VTUBoxesBinary::Patch2VTUBoxesBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::BinaryVTU,false,solvertype) {
}


std::string exahype::plotters::Patch2VTUBoxesBinary::getIdentifier() {
  return "vtk::patches::boxes::binary";
}


exahype::plotters::Patch2VTKGapsAscii::Patch2VTKGapsAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::ASCIIVTK,true,solvertype) {
}


std::string exahype::plotters::Patch2VTKGapsAscii::getIdentifier() {
  return "vtk::patches::gaps::ascii";
}


exahype::plotters::Patch2VTKGapsBinary::Patch2VTKGapsBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::BinaryVTK,true,solvertype) {
}


std::string exahype::plotters::Patch2VTKGapsBinary::getIdentifier() {
  return "vtk::patches::gaps::binary";
}


exahype::plotters::Patch2VTUGapsAscii::Patch2VTUGapsAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::ASCIIVTU,true,solvertype) {
}


std::string exahype::plotters::Patch2VTUGapsAscii::getIdentifier() {
  return "vtu::patches::gaps::ascii";
}


exahype::plotters::Patch2VTUGapsBinary::Patch2VTUGapsBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  exahype::solvers::Solver::Type solvertype
):
  exahype::plotters::Patch2VTK(postProcessing,PlotterType::BinaryVTU,true,solvertype) {
}


std::string exahype::plotters::Patch2VTUGapsBinary::getIdentifier() {
  return "vtk::patches::gaps::binary";
}

exahype::plotters::Patch2VTK::Patch2VTK(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    PlotterType plotterType,
    bool plotGaps,
    exahype::solvers::Solver::Type solverType):
  Device(postProcessing),
  _solverType(solverType),
  _isLimitingSolver(_solverType == exahype::solvers::Solver::Type::LimitingADERDG),
  _plotterType(plotterType),
  _plotGaps(plotGaps),
  #ifdef Parallel
  _hasMPIenabled(true),
  #else
  _hasMPIenabled(false),
  #endif
  _fileCounter(-1),
  _solverUnknowns(-1),
  _writtenUnknowns(-1),
  _gridWriter(nullptr),
  _vertexWriter(nullptr),
  _cellWriter(nullptr),
  _cellDataWriter(nullptr) {
}


void exahype::plotters::Patch2VTK::init(
  const std::string& filename,
  int                numberOfCellsPerAxis,
  int                unknowns,
  int                writtenUnknowns,
  exahype::parser::ParserView plotterParameters
){
	_filename             = filename;
	_solverUnknowns       = unknowns; // not even needed
	_plotterParameters               = plotterParameters;
	_writtenUnknowns      = writtenUnknowns;

	slicer = Slicer::bestFromSelectionQuery(plotterParameters);
	if(slicer) {
		logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<filename);
	}
}


void exahype::plotters::Patch2VTK::startPlotting( double time ) {
  _fileCounter++;

    switch (_plotterType) {
      case PlotterType::BinaryVTK:
        _gridWriter = new tarch::plotter::griddata::unstructured::vtk::VTKBinaryFileWriter();
        break;
      case PlotterType::ASCIIVTK:
        _gridWriter = new tarch::plotter::griddata::unstructured::vtk::VTKTextFileWriter();
        break;
      case PlotterType::BinaryVTU:
        _gridWriter = new tarch::plotter::griddata::unstructured::vtk::VTUBinaryFileWriter();
        break;
      case PlotterType::ASCIIVTU:
        _gridWriter = new tarch::plotter::griddata::unstructured::vtk::VTUTextFileWriter();
        break;
    }

    _vertexWriter                = _gridWriter->createVertexWriter();
    _cellWriter                  = _gridWriter->createCellWriter();
    _cellDataWriter              = (_writtenUnknowns>0) ? _gridWriter->createCellDataWriter("Q", _writtenUnknowns) : nullptr;
    _cellTimeStampDataWriter = _gridWriter->createCellDataWriter("time", 1);
    _cellDescriptionIndexWriter = _gridWriter->createCellDataWriter("descriptionIndex", 1);
    _cellElementWriter = _gridWriter->createCellDataWriter("element", 1);
    _cellLevelWriter = _gridWriter->createCellDataWriter("level", 1);
    _cellMpiRankWriter = _hasMPIenabled ? _gridWriter->createCellDataWriter("MpiRank", 1) : nullptr;
    _cellRefinementStatusWriter     = _isLimitingSolver ? _gridWriter->createCellDataWriter("Limiter-Status(0-O,1..2-DG,3..4-FV,5-T)", 1) : nullptr;
    _cellPreviousRefinementStatusWriter = _isLimitingSolver ? _gridWriter->createCellDataWriter("Previous-Limiter-Status(0-O,1..2-DG,3..4-FV,5-T)", 1) : nullptr;

  _postProcessing->startPlotting( time );

  _time = time;
}


void exahype::plotters::Patch2VTK::finishPlotting() {
  _postProcessing->finishPlotting();
	assertion( _gridWriter!=nullptr );

	_vertexWriter->close();
	_cellWriter->close();
	if (_cellDataWriter!=nullptr)            _cellDataWriter->close();
	if (_cellTimeStampDataWriter!=nullptr)   _cellTimeStampDataWriter->close();
	if (_cellMpiRankWriter!=nullptr)      _cellMpiRankWriter->close();
	if (_cellRefinementStatusWriter!=nullptr) _cellRefinementStatusWriter->close();
	if (_cellPreviousRefinementStatusWriter!=nullptr) _cellPreviousRefinementStatusWriter->close();
	if (_cellElementWriter!=nullptr)    _cellElementWriter->close();
	if (_cellDescriptionIndexWriter!=nullptr) _cellDescriptionIndexWriter->close();
	if (_cellLevelWriter!=nullptr)  _cellLevelWriter->close();

    
	std::ostringstream snapshotFileName;
	snapshotFileName << _filename
			<< "-" << _fileCounter;

	switch (_plotterType) {
	case PlotterType::BinaryVTK:
		break;
	case PlotterType::ASCIIVTK:
		break;
	case PlotterType::BinaryVTU:
		_timeSeriesWriter.addSnapshot( snapshotFileName.str(), _time);
		_timeSeriesWriter.writeFile(_filename);
		break;
	case PlotterType::ASCIIVTU:
		_timeSeriesWriter.addSnapshot( snapshotFileName.str(), _time);
		_timeSeriesWriter.writeFile(_filename);
		break;
	}

	const bool hasBeenSuccessful =
	_gridWriter->writeToFile(snapshotFileName.str());
	if (!hasBeenSuccessful) {
		exit(-1);
	}
  
	if (_vertexWriter!=nullptr)        delete _vertexWriter;
	if (_cellWriter!=nullptr)          delete _cellWriter;
	if (_cellMpiRankWriter!=nullptr)   delete _cellMpiRankWriter;
	if (_cellRefinementStatusWriter!=nullptr) delete _cellRefinementStatusWriter;
	if (_cellPreviousRefinementStatusWriter!=nullptr) delete _cellPreviousRefinementStatusWriter;
	if (_cellDataWriter!=nullptr)       delete _cellDataWriter;
	if (_cellTimeStampDataWriter!=nullptr)    delete _cellTimeStampDataWriter;
	if (_gridWriter!=nullptr)           delete _gridWriter;
	if (_cellElementWriter!=nullptr)    delete _cellElementWriter;
	if (_cellDescriptionIndexWriter!=nullptr) delete _cellDescriptionIndexWriter;
	if (_cellLevelWriter!=nullptr) delete _cellLevelWriter;

	_cellDataWriter = nullptr;
	_vertexWriter   = nullptr;
	_cellWriter     = nullptr;
	_cellTimeStampDataWriter   = nullptr;
	_gridWriter          = nullptr;
	_cellMpiRankWriter = nullptr;
	_cellRefinementStatusWriter = nullptr;
	_cellPreviousRefinementStatusWriter = nullptr;
	_cellDescriptionIndexWriter = nullptr;
	_cellElementWriter = nullptr;
	_cellLevelWriter = nullptr;
}


exahype::plotters::Patch2VTK::~Patch2VTK() {
}

std::pair<int,int> exahype::plotters::Patch2VTK::plotCellBoundary(
	tarch::la::Vector<DIMENSIONS, double> offsetOfPatch,
	tarch::la::Vector<DIMENSIONS, double> sizeOfPatch) {
	
	if(_plotGaps) {
		constexpr double  gapScaleFactor = 0.9305681558; // mimics p=3 ADERDG
		// TODO: Expose _cellScaleFactor as runtime parameter instead of a boolean _plotGaps.
		_cellScaleFactor = gapScaleFactor;
	
		assert(_cellScaleFactor <= 1.0 && _cellScaleFactor >= 0.0);
	
		// scale the apparent patch size
		tarch::la::Vector<DIMENSIONS, double> gap = sizeOfPatch * (1-_cellScaleFactor);
		offsetOfPatch += gap;
		sizeOfPatch -= 2.0*gap;
	}
	
	int firstVertex=-1;
	
	// draw the cell outline. Taken from PatchWriterUnstructured.cpp with #cells = 1.
	#if DIMENSIONS==2
	for(int y=0; y<2; y++)
	for(int x=0; x<2; x++) {
		tarch::la::Vector<2, double> p;
		p(0) = offsetOfPatch(0) + x * sizeOfPatch(0);
		p(1) = offsetOfPatch(1) + y * sizeOfPatch(1);
		const int newVertexNumber = _vertexWriter->plotVertex(p);
		if(firstVertex<0) firstVertex = newVertexNumber;
	}
	constexpr int numVertices = 4;
	int cellsVertexIndices[numVertices];
	for(int i=0; i<numVertices; i++) cellsVertexIndices[i] = firstVertex + i;
	const int firstCell = _cellWriter->plotQuadrangle(cellsVertexIndices);

	#elif DIMENSIONS==3
	for (int z=0; z<2; z++)
	for (int y=0; y<2; y++)
	for (int x=0; x<2; x++) {
		tarch::la::Vector<3, double> p;
		p(0) = offsetOfPatch(0) + x * sizeOfPatch(0);
		p(1) = offsetOfPatch(1) + y * sizeOfPatch(1);
		p(2) = offsetOfPatch(2) + z * sizeOfPatch(2);
		const int newVertexNumber = _vertexWriter->plotVertex(p);
		//std::cout << "New vertex with number " << newVertexNumber << " at position " << p << "\n";
		if(firstVertex<0) firstVertex = newVertexNumber;
	}
	
	constexpr int numVertices = 8;
	int cellsVertexIndices[numVertices];
	for(int i=0; i<numVertices; i++) cellsVertexIndices[i] = firstVertex + i;
	//printf("CellsVertexIndices:\n"); for(int i=0; i<numVertices; i++) printf("%d -> %d\n", i, cellsVertexIndices[i]);
	const int firstCell = _cellWriter->plotHexahedron(cellsVertexIndices);
	#endif

	return std::pair<int,int>(firstVertex,firstCell);
}

// just a local shorthand
inline void plotInt(tarch::plotter::griddata::Writer::CellDataWriter *writer, int cellIndex, int data) {
	writer->plotCell(cellIndex, static_cast<double>(data));
}

void exahype::plotters::Patch2VTK::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
	double *solution=nullptr, timeStamp=-1;
	int RefinementStatus=-1, previousRefinementStatus=-1, level=-1;
	tarch::la::Vector<DIMENSIONS, double> offsetOfPatch, sizeOfPatch;
	
	int element = -1;
	// we need this code doubling as we have different C++ types. Could probably use templates instead.
	switch(_solverType) {
		case exahype::solvers::Solver::Type::LimitingADERDG:
		case exahype::solvers::Solver::Type::ADERDG: { // scope for variables
			element = cellInfo.indexOfADERDGCellDescription(solverNumber);
			auto& solverPatch  = cellInfo._ADERDGCellDescriptions[element];
			if(solverPatch.getType()!=exahype::solvers::ADERDGSolver::CellDescription::Type::Cell)
				return; // plot only cells
			solution = static_cast<double*>(solverPatch.getSolution());
			timeStamp = solverPatch.getTimeStamp();
			offsetOfPatch = solverPatch.getOffset(),
			sizeOfPatch = solverPatch.getSize();
			level = solverPatch.getLevel();
			
			if(_isLimitingSolver) {
				RefinementStatus         = solverPatch.getRefinementStatus();
				previousRefinementStatus = solverPatch.getPreviousRefinementStatus();

				// this comes from LimitingADERDG2CartesianVTK.cpp:
				// ignore limiter status on coarser mesh levels
				assertion(static_cast<unsigned int>(solverPatch.getSolverNumber())<exahype::solvers::RegisteredSolvers.size());
				if (level<exahype::solvers::RegisteredSolvers[solverPatch.getSolverNumber()]->getMaximumAdaptiveMeshLevel()) {
					RefinementStatus         = 0;
					previousRefinementStatus = 0;
				}

				assertion(RefinementStatus >= -1);
			}
			break;
		}
		case exahype::solvers::Solver::Type::FiniteVolumes: {
			element = cellInfo.indexOfFiniteVolumesCellDescription(solverNumber);
			auto& solverPatch  = cellInfo._FiniteVolumesCellDescriptions[element];
			if(solverPatch.getType()!=exahype::solvers::FiniteVolumesSolver::CellDescription::Type::Cell)
				return; // plot only cells
			solution = static_cast<double*>(solverPatch.getSolution());
			timeStamp = solverPatch.getTimeStamp();
			offsetOfPatch = solverPatch.getOffset(),
			sizeOfPatch = solverPatch.getSize();
			level = solverPatch.getLevel();
			break;
		}
	} // switch _solverType

	if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
		std::pair<int,int> vertexAndCellIndex = plotCellBoundary(offsetOfPatch, sizeOfPatch);
		const int cellIndex = vertexAndCellIndex.second; // we only need the cellIndex in the following code
		
		// plot generic data about cell
		plotInt(_cellDescriptionIndexWriter, cellIndex, cellInfo._cellDescriptionsIndex);
		plotInt(_cellElementWriter, cellIndex, element);
		plotInt(_cellLevelWriter, cellIndex, level);

		// plot data about limiter
		if(_isLimitingSolver) {
			plotInt(_cellRefinementStatusWriter, cellIndex, RefinementStatus);
			plotInt(_cellPreviousRefinementStatusWriter, cellIndex, previousRefinementStatus);
		}

		// plot data about MPI rank
		if(_hasMPIenabled) {
			int rank = tarch::parallel::Node::getInstance().getRank();
			plotInt(_cellMpiRankWriter, cellIndex, rank);
		}
		
		// plot time information
		_cellTimeStampDataWriter->plotCell(cellIndex, timeStamp);

		// plot user payload data
		plotAverageData(cellIndex, offsetOfPatch, sizeOfPatch, solution, timeStamp);
	} // if slicer
}

void exahype::plotters::Patch2VTK::plotAverageData(
      int cellIndex,
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp) {

	assertion( _writtenUnknowns==0 || _cellDataWriter!=nullptr );
	assertion( _writtenUnknowns==0 || _cellTimeStampDataWriter!=nullptr );

	double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

	// Dummy position index.
	tarch::la::Vector<DIMENSIONS, int>  pos;
	for(int i=0; i<DIMENSIONS; i++) pos(i) = 0;

	// "abuse" the mapQuantities call to map a whole patch onto a single value.
	_postProcessing->mapQuantities(offsetOfPatch, sizeOfPatch,
		offsetOfPatch + 0.5 * sizeOfPatch,
		pos, // just a dummy
		u,   // points to first first luh in cell
		value,
		timeStamp);

	if (_writtenUnknowns>0) {
		_cellDataWriter->plotCell(cellIndex, value, _writtenUnknowns);
	}

	delete[] value;
}

