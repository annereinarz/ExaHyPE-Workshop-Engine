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
 
#include "LimitingADERDG2CartesianVTK.h"
#include "tarch/parallel/Node.h"

// @todo 16/05/03:Dominic Etienne Charreir Plotter depends now on kernels.
// Should thus be placed in kernel module or the solver
// should provide a function that computes solution values
// at equidistant grid points
#include "kernels/DGMatrices.h"
#include "peano/utils/Loop.h"


#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"

#include "kernels/DGBasisFunctions.h"

#include "exahype/solvers/LimitingADERDGSolver.h"


// VTK subclasses
std::string exahype::plotters::LimitingADERDG2CartesianVerticesVTKAscii::getIdentifier() {
  return "vtk::Cartesian::vertices::limited::ascii";
}
exahype::plotters::LimitingADERDG2CartesianVerticesVTKAscii::LimitingADERDG2CartesianVerticesVTKAscii(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
  LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTK,false) {
}

std::string exahype::plotters::LimitingADERDG2CartesianVerticesVTKBinary::getIdentifier() {
  return "vtk::Cartesian::vertices::limited::binary";
}
exahype::plotters::LimitingADERDG2CartesianVerticesVTKBinary::LimitingADERDG2CartesianVerticesVTKBinary(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTK,false) {
}

std::string exahype::plotters::LimitingADERDG2CartesianCellsVTKAscii::getIdentifier() {
  return "vtk::Cartesian::cells::limited::ascii";
}
exahype::plotters::LimitingADERDG2CartesianCellsVTKAscii::LimitingADERDG2CartesianCellsVTKAscii(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTK,true) {
}

std::string exahype::plotters::LimitingADERDG2CartesianCellsVTKBinary::getIdentifier() {
 return "vtk::Cartesian::cells::limited::binary";
}
exahype::plotters::LimitingADERDG2CartesianCellsVTKBinary::LimitingADERDG2CartesianCellsVTKBinary(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTK,true) {
}

// VTU subclasses
std::string exahype::plotters::LimitingADERDG2CartesianVerticesVTUAscii::getIdentifier() {
  return "vtu::Cartesian::vertices::limited::ascii";
}
exahype::plotters::LimitingADERDG2CartesianVerticesVTUAscii::LimitingADERDG2CartesianVerticesVTUAscii(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTU,false) {
}

std::string exahype::plotters::LimitingADERDG2CartesianVerticesVTUBinary::getIdentifier() {
  return "vtu::Cartesian::vertices::limited::binary";
}
exahype::plotters::LimitingADERDG2CartesianVerticesVTUBinary::LimitingADERDG2CartesianVerticesVTUBinary(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTU,false) {
}

std::string exahype::plotters::LimitingADERDG2CartesianCellsVTUAscii::getIdentifier() {
  return "vtu::Cartesian::cells::limited::ascii";
}
exahype::plotters::LimitingADERDG2CartesianCellsVTUAscii::LimitingADERDG2CartesianCellsVTUAscii(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTU,true) {
}

std::string exahype::plotters::LimitingADERDG2CartesianCellsVTUBinary::getIdentifier() {
 return "vtu::Cartesian::cells::limited::binary";
}
exahype::plotters::LimitingADERDG2CartesianCellsVTUBinary::LimitingADERDG2CartesianCellsVTUBinary(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth):
    LimitingADERDG2CartesianVTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTU,true) {
}

tarch::logging::Log exahype::plotters::LimitingADERDG2CartesianVTK::_log("exahype::plotters::LimitingADERDG2CartesianVTK");

exahype::plotters::LimitingADERDG2CartesianVTK::LimitingADERDG2CartesianVTK(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth,
    PlotterType plotterType,
    const bool plotCells)
  :
  Device(postProcessing),
  _plotterType(plotterType),
  _plotCells(plotCells),
  _ghostLayerWidth(ghostLayerWidth)
{}


void exahype::plotters::LimitingADERDG2CartesianVTK::init(
  const std::string& filename,
  int                orderPlusOne,
  int                unknowns,
  int                writtenUnknowns,
  exahype::parser::ParserView plotterParameters
) {
  _filename          = filename;
  _order             = orderPlusOne-1;
  _solverUnknowns    = unknowns;
  _plotterParameters = plotterParameters;
  _patchWriter       = nullptr;
  _writtenUnknowns   = writtenUnknowns;

  _slicer = Slicer::bestFromSelectionQuery(plotterParameters);

  unsigned int nodes = (DIMENSIONS == 3 ? _order  : 0 ) + 1;
  nodes *= (_order + 1) * (_order + 1);
  _tempSolution.resize(_solverUnknowns * nodes);
  _tempGradient.resize(DIMENSIONS * _solverUnknowns * nodes);
  assertion(_tempSolution.size()== _solverUnknowns * nodes);
  assertion(_tempGradient.size()==DIMENSIONS * _solverUnknowns * nodes);

  _resolution = 0;
  if (_plotterParameters.hasKey("resolution")) {
    _resolution = _plotterParameters.getValueAsIntOrDefault("resolution",0);
  }
  logInfo("init", "Plotting with resolution "<<_resolution);

  if(_slicer) {
    logInfo("init", "Plotting selection "<<_slicer->toString()<<" to Files "<<filename);
  }
}


void exahype::plotters::LimitingADERDG2CartesianVTK::startPlotting( double time ) {
  _fileCounter++;

  assertion( _patchWriter==nullptr );

  if (_writtenUnknowns>0) {
    switch (_plotterType) {
    case PlotterType::BinaryVTK:
      _patchWriter =
          new tarch::plotter::griddata::blockstructured::PatchWriterUnstructured(
              new tarch::plotter::griddata::unstructured::vtk::VTKBinaryFileWriter());
      break;
    case PlotterType::ASCIIVTK:
      _patchWriter =
          new tarch::plotter::griddata::blockstructured::PatchWriterUnstructured(
              new tarch::plotter::griddata::unstructured::vtk::VTKTextFileWriter());
      break;
    case PlotterType::BinaryVTU:
      _patchWriter =
          new tarch::plotter::griddata::blockstructured::PatchWriterUnstructured(
              new tarch::plotter::griddata::unstructured::vtk::VTUBinaryFileWriter());
      break;
    case PlotterType::ASCIIVTU:
      _patchWriter =
          new tarch::plotter::griddata::blockstructured::PatchWriterUnstructured(
              new tarch::plotter::griddata::unstructured::vtk::VTUTextFileWriter());
      break;
    }

    _gridWriter                  = _patchWriter->createSinglePatchWriter();
    if (_plotCells) {
      _cellDataWriter            = _patchWriter->createCellDataWriter("Q", _writtenUnknowns);
      _vertexDataWriter          = nullptr;

      _cellRefinementStatusWriter           = _patchWriter->createCellDataWriter("RefinementStatus", 1);
      _vertexRefinementStatusWriter         = nullptr;
      _cellPreviousRefinementStatusWriter   = _patchWriter->createCellDataWriter("PreviousRefinementStatus", 1);
      _vertexPreviousRefinementStatusWriter = nullptr;
    }
    else {
      _cellDataWriter            = nullptr;
      _vertexDataWriter          = _patchWriter->createVertexDataWriter("Q", _writtenUnknowns);

      _cellRefinementStatusWriter   = nullptr;
      _vertexRefinementStatusWriter = _patchWriter->createVertexDataWriter("RefinementStatus", 1);

      _cellPreviousRefinementStatusWriter   = nullptr;
      _vertexPreviousRefinementStatusWriter = _patchWriter->createVertexDataWriter("PreviousRefinementStatus", 1);
    }
    _timeStampVertexDataWriter = _patchWriter->createVertexDataWriter("time", 1);
//    _timeStampCellDataWriter   = _patchWriter->createCellDataWriter("time", 1);

    assertion( _patchWriter!=nullptr );
    assertion( _gridWriter!=nullptr );
    assertion( _timeStampVertexDataWriter!=nullptr );
//    assertion( _timeStampCellDataWriter!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
}


void exahype::plotters::LimitingADERDG2CartesianVTK::finishPlotting() {
  _postProcessing->finishPlotting();

  if (_writtenUnknowns>0) {
    assertion( _patchWriter!=nullptr );
    assertion( _gridWriter!=nullptr );
    assertion( _timeStampVertexDataWriter!=nullptr );

    _gridWriter->close();
//    if (_timeStampCellDataWriter!=nullptr) _timeStampCellDataWriter->close();
    if (_vertexDataWriter!=nullptr)                  _vertexDataWriter->close();
    if (_cellDataWriter!=nullptr)                    _cellDataWriter->close();
    if (_cellRefinementStatusWriter!=nullptr)           _cellRefinementStatusWriter->close();
    if (_vertexRefinementStatusWriter!=nullptr)         _vertexRefinementStatusWriter->close();
    if (_cellPreviousRefinementStatusWriter!=nullptr)   _cellPreviousRefinementStatusWriter->close();
    if (_vertexPreviousRefinementStatusWriter!=nullptr) _vertexPreviousRefinementStatusWriter->close();
    _timeStampVertexDataWriter->close();

    std::ostringstream snapshotFileName;
    snapshotFileName << _filename << "-" << _fileCounter;

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
      _patchWriter->writeToFile(snapshotFileName.str());
    if (!hasBeenSuccessful) {
      exit(-1);
    }
  }

  if (_vertexDataWriter!=nullptr)                  delete _vertexDataWriter;
  if (_cellDataWriter!=nullptr)                    delete _cellDataWriter;
  if (_timeStampVertexDataWriter!=nullptr)         delete _timeStampVertexDataWriter;
  if (_timeStampCellDataWriter!=nullptr)           delete _timeStampCellDataWriter;
  if (_cellRefinementStatusWriter!=nullptr)           delete _cellRefinementStatusWriter;
  if (_vertexRefinementStatusWriter!=nullptr)         delete _vertexRefinementStatusWriter;
  if (_cellPreviousRefinementStatusWriter!=nullptr)   delete _cellPreviousRefinementStatusWriter;
  if (_vertexPreviousRefinementStatusWriter!=nullptr) delete _vertexPreviousRefinementStatusWriter;
  if (_gridWriter!=nullptr)                        delete _gridWriter;
  if (_patchWriter!=nullptr)                       delete _patchWriter;

  _vertexDataWriter                  = nullptr;
  _cellDataWriter                    = nullptr;
  _patchWriter                       = nullptr;
  _timeStampVertexDataWriter         = nullptr;
  _timeStampCellDataWriter           = nullptr;
  _cellRefinementStatusWriter           = nullptr;
  _vertexRefinementStatusWriter         = nullptr;
  _cellPreviousRefinementStatusWriter   = nullptr;
  _vertexPreviousRefinementStatusWriter = nullptr;
  _gridWriter                        = nullptr;
}



exahype::plotters::LimitingADERDG2CartesianVTK::~LimitingADERDG2CartesianVTK() {
}


void exahype::plotters::LimitingADERDG2CartesianVTK::writeTimeStampDataToADERDGPatch( double timeStamp, int vertexIndex ) {
  if (_writtenUnknowns>0) {
    dfor(i,_order+1) {
      _timeStampVertexDataWriter->plotVertex(vertexIndex, timeStamp);
      vertexIndex++;
    }
  }
}


void exahype::plotters::LimitingADERDG2CartesianVTK::plotVertexData(
  int firstVertexIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u,
  double timeStamp,
  const int RefinementStatusAsInt,
  const int previousRefinementStatusAsInt
) {
  assertion( _vertexDataWriter!=nullptr || _writtenUnknowns==0 );

  double* interpoland = new double[_solverUnknowns];
  double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

  dfor(i,_order+1) {
    for (int unknown=0; unknown < _solverUnknowns; unknown++) {
      interpoland[unknown] = 0.0;
      dfor(ii,_order+1) { // Gauss-Legendre node indices
        int iGauss = peano::utils::dLinearisedWithoutLookup(ii,_order + 1);
        interpoland[unknown] += kernels::equidistantGridProjector1d[_order][ii(1)][i(1)] *
                 kernels::equidistantGridProjector1d[_order][ii(0)][i(0)] *
                 #if DIMENSIONS==3
                 kernels::equidistantGridProjector1d[_order][ii(2)][i(2)] *
                 #endif
                 u[iGauss * _solverUnknowns + unknown];
        assertion3(interpoland[unknown] == interpoland[unknown], offsetOfPatch, sizeOfPatch, iGauss);
      }
      if ( !std::isfinite(interpoland[unknown]) ) {
        logError("plotVertexData(...)","plotted value not finite.");
        std::abort();
      }
    }

    assertion(sizeOfPatch(0)==sizeOfPatch(1));
    _postProcessing->mapQuantities(
      offsetOfPatch,
      sizeOfPatch,
      offsetOfPatch + i.convertScalar<double>()* (sizeOfPatch(0)/(_order)),
      i,
      interpoland,
      value,
      timeStamp
    );

    if (_writtenUnknowns>0) {
      _vertexDataWriter->plotVertex(firstVertexIndex, value, _writtenUnknowns );
    }

    _vertexRefinementStatusWriter->plotVertex(firstVertexIndex, static_cast<double>(RefinementStatusAsInt));
    _vertexPreviousRefinementStatusWriter->plotVertex(firstVertexIndex, static_cast<double>(previousRefinementStatusAsInt));

    firstVertexIndex++;
  }

  if (interpoland!=nullptr)  delete[] interpoland;
  if (value!=nullptr)        delete[] value;
}


void exahype::plotters::LimitingADERDG2CartesianVTK::plotCellData(
  int firstCellIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u,
  double timeStamp,
  const int RefinementStatusAsInt,
  const int previousRefinementStatusAsInt
) {
  assertion( _cellDataWriter!=nullptr || _writtenUnknowns==0 );

  double* interpoland = new double[_solverUnknowns];
  double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

  dfor(i,_order) {
    for (int unknown=0; unknown < _solverUnknowns; unknown++) {
      interpoland[unknown] = kernels::interpolate(
        offsetOfPatch.data(),
        sizeOfPatch.data(),
        (offsetOfPatch + (i.convertScalar<double>()+0.5)* (sizeOfPatch(0)/(_order))).data(),
        _solverUnknowns,
        unknown,
        _order,
        u
      );
    }

    assertion(sizeOfPatch(0)==sizeOfPatch(1));
    _postProcessing->mapQuantities(
      offsetOfPatch,
      sizeOfPatch,
      offsetOfPatch + (i.convertScalar<double>()+0.5)* (sizeOfPatch(0)/(_order)),
      i,
      interpoland,
      value,
      timeStamp
    );

    if (_writtenUnknowns>0) {
      _cellDataWriter->plotCell(firstCellIndex, value, _writtenUnknowns );
    }

    _cellRefinementStatusWriter->plotCell(firstCellIndex, static_cast<double>(RefinementStatusAsInt));
    _cellPreviousRefinementStatusWriter->plotCell(firstCellIndex, static_cast<double>(previousRefinementStatusAsInt));

    firstCellIndex++;
  }

  if (interpoland!=nullptr)  delete[] interpoland;
  if (value!=nullptr)        delete[] value;
}

void exahype::plotters::LimitingADERDG2CartesianVTK::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  // look up ADER-DG solver
  solvers::ADERDGSolver* aderdgSolver = nullptr;
  switch ( solvers::RegisteredSolvers[solverNumber]->getType() ) {
  case solvers::Solver::Type::ADERDG:
    aderdgSolver = static_cast<solvers::ADERDGSolver*>( solvers::RegisteredSolvers[solverNumber] );
    break;
  case solvers::Solver::Type::LimitingADERDG:
    aderdgSolver = static_cast<solvers::LimitingADERDGSolver*>( solvers::RegisteredSolvers[solverNumber] )->getSolver().get();
    break;
  default:
    logError("plotPatch(...)","Encountered unexpected solver type: "<<solvers::Solver::toString(solvers::RegisteredSolvers[solverNumber]->getType()));
    std::abort();
    break;
  }

  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& solverPatch  = cellInfo._ADERDGCellDescriptions[element];

  if (solverPatch.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    int refinementStatus         = solverPatch.getRefinementStatus();
    int previousRefinementStatus = solverPatch.getPreviousRefinementStatus();

    // ignore limiter status on coarser mesh levels
    assertion(static_cast<unsigned int>(solverPatch.getSolverNumber())<exahype::solvers::RegisteredSolvers.size());
    if (solverPatch.getLevel()<exahype::solvers::RegisteredSolvers[solverPatch.getSolverNumber()]->getMaximumAdaptiveMeshLevel()) {
      refinementStatus         = 0;
      previousRefinementStatus = 0;
    }

    if(true) {  // TODO(Dominic): Plot FVM solution instead if < Troubled-1
      double* solution = static_cast<double*>(solverPatch.getSolution());
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch = solverPatch.getOffset();

      const int subcellsPerDim = tarch::la::aPowI(_resolution,3);
      const tarch::la::Vector<DIMENSIONS, double>& subcellSize = solverPatch.getSize() / static_cast<double>(subcellsPerDim);

      dfor(subcellIndex,subcellsPerDim) {
        tarch::la::Vector<DIMENSIONS, double> subcellOffset = offsetOfPatch;
        double* u = solution;
        if ( subcellsPerDim > 1 ) {
          u = _tempSolution.data();
          for (int d=0; d<DIMENSIONS; d++) {
            subcellOffset[d] = offsetOfPatch[d] + subcellSize[d] * subcellIndex[d];
          }
          aderdgSolver->volumeUnknownsProlongation(u,solution,0,_resolution,subcellIndex);
        }

        plotADERDGPatch(
            subcellOffset,
            subcellSize,
            u,
            solverPatch.getTimeStamp(),
            refinementStatus,
            previousRefinementStatus);
      }
    }
  }
}


void exahype::plotters::LimitingADERDG2CartesianVTK::plotADERDGPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp,
    const int RefinementStatusAsInt,
    const int previousRefinementStatusAsInt) {
  if (!_slicer || _slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    assertion( _writtenUnknowns==0 || _patchWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _gridWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _timeStampVertexDataWriter!=nullptr );

    std::pair<int,int> vertexAndCellIndex(0,0);
    if (_writtenUnknowns>0) {
      vertexAndCellIndex = _gridWriter->plotPatch(offsetOfPatch, sizeOfPatch, _order);
    }

    writeTimeStampDataToADERDGPatch( timeStamp, vertexAndCellIndex.first );

    if (_plotCells) {
      plotCellData( vertexAndCellIndex.second, offsetOfPatch, sizeOfPatch, u, timeStamp, RefinementStatusAsInt, previousRefinementStatusAsInt );
    }
    else {
      plotVertexData( vertexAndCellIndex.first, offsetOfPatch, sizeOfPatch, u, timeStamp, RefinementStatusAsInt, previousRefinementStatusAsInt );
    }
  }
}

void exahype::plotters::LimitingADERDG2CartesianVTK::plotFiniteVolumesPatch(
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
  double timeStamp) {
  if (!_slicer || _slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    logDebug("plotPatch(...)","offset of patch: "<<offsetOfPatch
    <<", size of patch: "<<sizeOfPatch
    <<", time stamp: "<<timeStamp);

    assertion( _writtenUnknowns==0 || _patchWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _gridWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _timeStampVertexDataWriter!=nullptr );

    const int numberOfCellsPerAxis = 2*_order+1;

    int cellIndex = _writtenUnknowns==0 ? -1 : _gridWriter->plotPatch(offsetOfPatch, sizeOfPatch, numberOfCellsPerAxis).second;

    double* sourceValue = new double[_solverUnknowns];
    double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

    dfor(i,numberOfCellsPerAxis+_ghostLayerWidth) {
      if (tarch::la::allSmaller(i,numberOfCellsPerAxis+_ghostLayerWidth)
          && tarch::la::allGreater(i,_ghostLayerWidth-1)) {
        if (_writtenUnknowns>0) {
          _timeStampCellDataWriter->plotCell(cellIndex, timeStamp);
        }

        for (int unknown=0; unknown < _solverUnknowns; unknown++) {
          sourceValue[unknown] =
            u[peano::utils::dLinearisedWithoutLookup(i,numberOfCellsPerAxis+2*_ghostLayerWidth)*_solverUnknowns+unknown];
        } // !!! Be aware of the "2*_ghostLayerWidth" !!!

        assertion(sizeOfPatch(0)==sizeOfPatch(1));

        _postProcessing->mapQuantities(
          offsetOfPatch,
          sizeOfPatch,
          offsetOfPatch + (i-_ghostLayerWidth).convertScalar<double>()* (sizeOfPatch(0)/(numberOfCellsPerAxis)),
          i-_ghostLayerWidth,
          sourceValue,
          value,
          timeStamp
        );

        if (_writtenUnknowns>0) {
          _cellDataWriter->plotCell(cellIndex, value, _writtenUnknowns);
        }
        cellIndex++;
      }
    }

    delete[] sourceValue;
    delete[] value;
  }
}
