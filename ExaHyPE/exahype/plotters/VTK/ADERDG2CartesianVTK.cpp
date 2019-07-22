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
 
#include "ADERDG2CartesianVTK.h"
#include "tarch/parallel/Node.h"

// @todo 16/05/03:Dominic Etienne Charreir Plotter depends now on kernels.
// Should thus be placed in kernel module or the solver
// should provide a function that computes solution values
// at equidistant grid points
#include "kernels/DGMatrices.h"
#include "peano/utils/Loop.h"

#include "exahype/plotters/slicing/Slicer.h"
#include "exahype/solvers/LimitingADERDGSolver.h"

#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"


#include "kernels/DGBasisFunctions.h"
#include "kernels/aderdg/generic/Kernels.h" // prolongation

tarch::logging::Log exahype::plotters::ADERDG2CartesianVTK::_log("exahype::plotters::ADERDG2CartesianVTK");


std::string exahype::plotters::ADERDG2CartesianVerticesVTKAscii::getIdentifier() {
  return "vtk::Cartesian::vertices::ascii";
}


exahype::plotters::ADERDG2CartesianVerticesVTKAscii::ADERDG2CartesianVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2CartesianVTK(postProcessing,PlotterType::ASCIIVTK,false) {
}


std::string exahype::plotters::ADERDG2CartesianVerticesVTKBinary::getIdentifier() {
  return "vtk::Cartesian::vertices::binary";
}


exahype::plotters::ADERDG2CartesianVerticesVTKBinary::ADERDG2CartesianVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2CartesianVTK(postProcessing,PlotterType::BinaryVTK,false) {
}



std::string exahype::plotters::ADERDG2CartesianCellsVTKAscii::getIdentifier() {
  return "vtk::Cartesian::cells::ascii";
}


exahype::plotters::ADERDG2CartesianCellsVTKAscii::ADERDG2CartesianCellsVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2CartesianVTK(postProcessing,PlotterType::ASCIIVTK,true) {
}


std::string exahype::plotters::ADERDG2CartesianCellsVTKBinary::getIdentifier() {
 return "vtk::Cartesian::cells::binary";
}


exahype::plotters::ADERDG2CartesianCellsVTKBinary::ADERDG2CartesianCellsVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2CartesianVTK(postProcessing,PlotterType::BinaryVTK,true) {
}


std::string exahype::plotters::ADERDG2CartesianVerticesVTUAscii::getIdentifier() {
  return "vtu::Cartesian::vertices::ascii";
}


exahype::plotters::ADERDG2CartesianVerticesVTUAscii::ADERDG2CartesianVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2CartesianVTK(postProcessing,PlotterType::ASCIIVTU,false) {
}


std::string exahype::plotters::ADERDG2CartesianVerticesVTUBinary::getIdentifier() {
  return "vtu::Cartesian::vertices::binary";
}


exahype::plotters::ADERDG2CartesianVerticesVTUBinary::ADERDG2CartesianVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2CartesianVTK(postProcessing,PlotterType::BinaryVTU,false) {
}



std::string exahype::plotters::ADERDG2CartesianCellsVTUAscii::getIdentifier() {
  return "vtu::Cartesian::cells::ascii";
}


exahype::plotters::ADERDG2CartesianCellsVTUAscii::ADERDG2CartesianCellsVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2CartesianVTK(postProcessing,PlotterType::ASCIIVTU,true) {
}


std::string exahype::plotters::ADERDG2CartesianCellsVTUBinary::getIdentifier() {
 return "vtu::Cartesian::cells::binary";
}


exahype::plotters::ADERDG2CartesianCellsVTUBinary::ADERDG2CartesianCellsVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2CartesianVTK(postProcessing,PlotterType::BinaryVTU,true) {
}



exahype::plotters::ADERDG2CartesianVTK::ADERDG2CartesianVTK(
	exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, 
	PlotterType plotterType, 
	bool plotCells):
  Device(postProcessing),
  _plotterType(plotterType),
  _plotCells(plotCells)
{
}


void exahype::plotters::ADERDG2CartesianVTK::init(
  const std::string& filename,
  int                orderPlusOne,
  int                unknowns,
  int                writtenUnknowns,
  exahype::parser::ParserView plotterParameters
) {
  _filename          = filename;
  _order             = orderPlusOne-1;
  _solverUnknowns    = unknowns;
  _plotterParameters            = plotterParameters;
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


void exahype::plotters::ADERDG2CartesianVTK::startPlotting( double time ) {
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

    _gridWriter                = _patchWriter->createSinglePatchWriter();
    if (_plotCells) {
      _cellDataWriter          = _patchWriter->createCellDataWriter("Q", _writtenUnknowns);
      _vertexDataWriter        = nullptr;
      _cellTimeStampDataWriter = _patchWriter->createCellDataWriter("time", 1);
    }
    else {
      _cellDataWriter            = nullptr;
      _vertexDataWriter          = _patchWriter->createVertexDataWriter("Q", _writtenUnknowns);
      _vertexTimeStampDataWriter = _patchWriter->createVertexDataWriter("time", 1);
    }

    assertion( _patchWriter!=nullptr );
    assertion( _gridWriter!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
}


void exahype::plotters::ADERDG2CartesianVTK::finishPlotting() {
  _postProcessing->finishPlotting();

  if (_writtenUnknowns>0) {
    assertion( _patchWriter!=nullptr );
    assertion( _gridWriter!=nullptr );

    _gridWriter->close();
    if (_vertexDataWriter!=nullptr) _vertexDataWriter->close();
    if (_cellDataWriter!=nullptr)   _cellDataWriter->close();
    if (_vertexTimeStampDataWriter!=nullptr) _vertexTimeStampDataWriter->close();
    if (_cellTimeStampDataWriter!=nullptr)   _cellTimeStampDataWriter->close();

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

  if (_vertexDataWriter!=nullptr)          delete _vertexDataWriter;
  if (_cellDataWriter!=nullptr)            delete _cellDataWriter;
  if (_vertexTimeStampDataWriter!=nullptr) delete _vertexTimeStampDataWriter;
  if (_cellTimeStampDataWriter!=nullptr)   delete _cellTimeStampDataWriter;
  if (_gridWriter!=nullptr)                delete _gridWriter;
  if (_patchWriter!=nullptr)               delete _patchWriter;

  _vertexDataWriter          = nullptr;
  _cellDataWriter            = nullptr;
  _patchWriter               = nullptr;
  _vertexTimeStampDataWriter = nullptr;
  _cellTimeStampDataWriter   = nullptr;
  _gridWriter                = nullptr;
}



exahype::plotters::ADERDG2CartesianVTK::~ADERDG2CartesianVTK() {
}


void exahype::plotters::ADERDG2CartesianVTK::writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex ) {
  if (_writtenUnknowns>0 && _vertexTimeStampDataWriter!=nullptr) {
    dfor(i,_order+1) {
      _vertexTimeStampDataWriter->plotVertex(vertexIndex, timeStamp);
      vertexIndex++;
    }
  }

  if (_writtenUnknowns>0 && _cellTimeStampDataWriter!=nullptr) {
    dfor(i,_order) {
      _cellTimeStampDataWriter->plotCell(cellIndex, timeStamp);
      cellIndex++;
    }
  }
}


void exahype::plotters::ADERDG2CartesianVTK::plotVertexData(
  int firstVertexIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u,
  double timeStamp
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

    firstVertexIndex++;
  }

  if (interpoland!=nullptr)  delete[] interpoland;
  if (value!=nullptr)        delete[] value;
}


void exahype::plotters::ADERDG2CartesianVTK::plotCellData(
  int firstCellIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u,
  double timeStamp
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

    firstCellIndex++;
  }

  if (interpoland!=nullptr)  delete[] interpoland;
  if (value!=nullptr)        delete[] value;
}

void exahype::plotters::ADERDG2CartesianVTK::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
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
  auto& aderdgCellDescription  = cellInfo._ADERDGCellDescriptions[element];

  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    double* solution = static_cast<double*>(aderdgCellDescription.getSolution());
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch = aderdgCellDescription.getOffset();

    const int subcellsPerDim = tarch::la::aPowI(_resolution,3);
    const tarch::la::Vector<DIMENSIONS, double>& subcellSize = aderdgCellDescription.getSize() / static_cast<double>(subcellsPerDim);

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

      plotPatch(
          subcellOffset,subcellSize,u,
          aderdgCellDescription.getTimeStamp());
    }
  }
}

void exahype::plotters::ADERDG2CartesianVTK::plotPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp) {
  if (!_slicer || _slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    assertion( _writtenUnknowns==0 || _patchWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _gridWriter!=nullptr );

    std::pair<int,int> vertexAndCellIndex(0,0);
    if (_writtenUnknowns>0) {
      vertexAndCellIndex = _gridWriter->plotPatch(offsetOfPatch, sizeOfPatch, _order);
    }

    writeTimeStampDataToPatch( timeStamp, vertexAndCellIndex.first, vertexAndCellIndex.second );

    if (_plotCells) {
      plotCellData( vertexAndCellIndex.second, offsetOfPatch, sizeOfPatch, u, timeStamp );
    }
    else {
      plotVertexData( vertexAndCellIndex.first, offsetOfPatch, sizeOfPatch, u, timeStamp );
    }
  } // slicer
}
