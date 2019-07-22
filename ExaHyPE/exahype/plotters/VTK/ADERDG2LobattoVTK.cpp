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
 
#include "ADERDG2LobattoVTK.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"

#include "kernels/GaussLobattoQuadrature.h"

#include "kernels/DGBasisFunctions.h"

#include "peano/utils/Loop.h"

#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"

#include "exahype/plotters/slicing/Slicer.h"
#include "exahype/solvers/ADERDGSolver.h"

#include "kernels/aderdg/generic/c/computeGradients.cpph" // derivatives

tarch::logging::Log exahype::plotters::ADERDG2LobattoVTK::_log("exahype::plotters::ADERDG2LobattoVTK");

std::string exahype::plotters::ADERDG2LobattoVerticesVTKAscii::getIdentifier() {
  return "vtk::Lobatto::vertices::ascii";
}


exahype::plotters::ADERDG2LobattoVerticesVTKAscii::ADERDG2LobattoVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::ASCIIVTK,false) {
}


std::string exahype::plotters::ADERDG2LobattoVerticesVTKBinary::getIdentifier() {
  return "vtk::Lobatto::vertices::binary";
}


exahype::plotters::ADERDG2LobattoVerticesVTKBinary::ADERDG2LobattoVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::BinaryVTK,false) {
}



std::string exahype::plotters::ADERDG2LobattoCellsVTKAscii::getIdentifier() {
  return "vtk::Lobatto::cells::ascii";
}


exahype::plotters::ADERDG2LobattoCellsVTKAscii::ADERDG2LobattoCellsVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::ASCIIVTK,true) {
}


std::string exahype::plotters::ADERDG2LobattoCellsVTKBinary::getIdentifier() {
 return "vtk::Lobatto::cells::binary";
}


exahype::plotters::ADERDG2LobattoCellsVTKBinary::ADERDG2LobattoCellsVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::BinaryVTK,true) {
}



std::string exahype::plotters::ADERDG2LobattoVerticesVTUAscii::getIdentifier() {
  return "vtu::Lobatto::vertices::ascii";
}


exahype::plotters::ADERDG2LobattoVerticesVTUAscii::ADERDG2LobattoVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::ASCIIVTU,false) {
}


std::string exahype::plotters::ADERDG2LobattoVerticesVTUBinary::getIdentifier() {
  return "vtu::Lobatto::vertices::binary";
}


exahype::plotters::ADERDG2LobattoVerticesVTUBinary::ADERDG2LobattoVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::BinaryVTU,false) {
}



std::string exahype::plotters::ADERDG2LobattoCellsVTUAscii::getIdentifier() {
  return "vtu::Lobatto::cells::ascii";
}


exahype::plotters::ADERDG2LobattoCellsVTUAscii::ADERDG2LobattoCellsVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::ASCIIVTU,true) {
}


std::string exahype::plotters::ADERDG2LobattoCellsVTUBinary::getIdentifier() {
 return "vtu::Lobatto::cells::binary";
}


exahype::plotters::ADERDG2LobattoCellsVTUBinary::ADERDG2LobattoCellsVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LobattoVTK(postProcessing,PlotterType::BinaryVTU,true) {
}



exahype::plotters::ADERDG2LobattoVTK::ADERDG2LobattoVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType plotterType, bool plotCells):
  Device(postProcessing),
  _fileCounter(-1),
  _plotterType(plotterType),
  _plotCells(plotCells),
  _gridWriter(nullptr),
  _vertexWriter(nullptr),
  _cellWriter(nullptr),
  _vertexTimeStampDataWriter(nullptr),
  _cellTimeStampDataWriter(nullptr),
  _vertexDataWriter(nullptr),
  _cellDataWriter(nullptr) {
}


void exahype::plotters::ADERDG2LobattoVTK::init(
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
  _writtenUnknowns   = writtenUnknowns;

  slicer = Slicer::bestFromSelectionQuery(plotterParameters);

  if(slicer) {
    logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<filename);
  }
}


void exahype::plotters::ADERDG2LobattoVTK::startPlotting( double time ) {
  _fileCounter++;

  if (_writtenUnknowns>0) {
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

    if (_plotCells) {
      _cellDataWriter          = _gridWriter->createCellDataWriter("Q", _writtenUnknowns);
      _vertexDataWriter        = nullptr;
      _cellTimeStampDataWriter = _gridWriter->createCellDataWriter("time", 1);
    }
    else {
      _cellDataWriter          = nullptr;
      _vertexDataWriter        = _gridWriter->createVertexDataWriter("Q", _writtenUnknowns);
      _vertexTimeStampDataWriter = _gridWriter->createVertexDataWriter("time", 1);
    }


    assertion( _gridWriter!=nullptr );
    assertion( _vertexWriter!=nullptr );
    assertion( _cellWriter!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
}


void exahype::plotters::ADERDG2LobattoVTK::finishPlotting() {
  _postProcessing->finishPlotting();

  if ( _writtenUnknowns>0 ) {
    assertion( _gridWriter!=nullptr );

    _vertexWriter->close();
    _cellWriter->close();
    if (_vertexDataWriter!=nullptr) _vertexDataWriter->close();
    if (_cellDataWriter!=nullptr)   _cellDataWriter->close();
    if (_vertexTimeStampDataWriter!=nullptr) _vertexTimeStampDataWriter->close();
    if (_cellTimeStampDataWriter!=nullptr)   _cellTimeStampDataWriter->close();

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
  }

  if (_vertexDataWriter!=nullptr)    delete _vertexDataWriter;
  if (_cellDataWriter!=nullptr)      delete _cellDataWriter;
  if (_vertexWriter!=nullptr)        delete _vertexWriter;
  if (_cellWriter!=nullptr)          delete _cellWriter;
  if (_vertexTimeStampDataWriter!=nullptr) delete _vertexTimeStampDataWriter;
  if (_cellTimeStampDataWriter!=nullptr)   delete _cellTimeStampDataWriter;
  if (_gridWriter!=nullptr)          delete _gridWriter;

  _vertexDataWriter           = nullptr;
  _cellDataWriter             = nullptr;
  _vertexWriter               = nullptr;
  _cellWriter                 = nullptr;
  _vertexTimeStampDataWriter  = nullptr;
  _cellTimeStampDataWriter    = nullptr;
  _gridWriter                 = nullptr;
}


exahype::plotters::ADERDG2LobattoVTK::~ADERDG2LobattoVTK() {
}


void exahype::plotters::ADERDG2LobattoVTK::writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex ) {
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


std::pair<int,int> exahype::plotters::ADERDG2LobattoVTK::plotLobattoPatch(
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch
) {
  int firstVertex = -1;
  int firstCell   = -1;

  if (_writtenUnknowns>0) {
    assertion(_vertexWriter!=nullptr);
    dfor(i,_order+1) {
      tarch::la::Vector<DIMENSIONS, double> p;

      //p = offsetOfPatch + tarch::la::multiplyComponents( i.convertScalar<double>(), sizeOfPatch) * (1.0/_order);
      for (int d=0; d<DIMENSIONS; d++) {
        p(d) = offsetOfPatch(d) + kernels::gaussLobattoNodes[_order][_order-i(d)] * sizeOfPatch(d);
      }


      const int newVertexNumber = _vertexWriter->plotVertex(p);
      firstVertex = firstVertex==-1 ? newVertexNumber : firstVertex;
    }

    assertion(_cellWriter!=nullptr);
    dfor(i,_order) {
      #if DIMENSIONS==2
      int cellsVertexIndices[4];
      cellsVertexIndices[0] = firstVertex + (i(0)+0) + (i(1)+0) * (_order+1);
      cellsVertexIndices[1] = firstVertex + (i(0)+1) + (i(1)+0) * (_order+1);
      cellsVertexIndices[2] = firstVertex + (i(0)+0) + (i(1)+1) * (_order+1);
      cellsVertexIndices[3] = firstVertex + (i(0)+1) + (i(1)+1) * (_order+1);
      const int newCellNumber = _cellWriter->plotQuadrangle(cellsVertexIndices);
      firstCell = firstCell==-1 ? newCellNumber : firstCell;
      #elif DIMENSIONS==3
      int cellsVertexIndices[8];
      cellsVertexIndices[0] = firstVertex + (i(0)+0) + (i(1)+0) * (_order+1) + (i(2)+0) * (_order+1) * (_order+1);
      cellsVertexIndices[1] = firstVertex + (i(0)+1) + (i(1)+0) * (_order+1) + (i(2)+0) * (_order+1) * (_order+1);
      cellsVertexIndices[2] = firstVertex + (i(0)+0) + (i(1)+1) * (_order+1) + (i(2)+0) * (_order+1) * (_order+1);
      cellsVertexIndices[3] = firstVertex + (i(0)+1) + (i(1)+1) * (_order+1) + (i(2)+0) * (_order+1) * (_order+1);
      cellsVertexIndices[4] = firstVertex + (i(0)+0) + (i(1)+0) * (_order+1) + (i(2)+1) * (_order+1) * (_order+1);
      cellsVertexIndices[5] = firstVertex + (i(0)+1) + (i(1)+0) * (_order+1) + (i(2)+1) * (_order+1) * (_order+1);
      cellsVertexIndices[6] = firstVertex + (i(0)+0) + (i(1)+1) * (_order+1) + (i(2)+1) * (_order+1) * (_order+1);
      cellsVertexIndices[7] = firstVertex + (i(0)+1) + (i(1)+1) * (_order+1) + (i(2)+1) * (_order+1) * (_order+1);
      const int newCellNumber = _cellWriter->plotHexahedron(cellsVertexIndices);
      firstCell = firstCell==-1 ? newCellNumber : firstCell;
      #endif
    }
  }

  return std::pair<int,int>(firstVertex,firstCell);
}


void exahype::plotters::ADERDG2LobattoVTK::plotVertexData(
  int firstVertexIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u, double* gradU,
  double timeStamp
) {
  assertion( _vertexDataWriter!=nullptr || _writtenUnknowns==0 );

  double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

  // this should go to the header or similar
  const int basisX = _order + 1;
  const int basisY = _order + 1;
  const int basisZ = (DIMENSIONS == 3 ? _order  : 0 ) + 1;

  kernels::index idx_u(basisZ, basisY, basisX, _solverUnknowns);
  kernels::index idx_gradU(basisZ, basisY, basisX, DIMENSIONS, _solverUnknowns);

  dfor(i,_order+1) {
    tarch::la::Vector<DIMENSIONS, double> p;
      for (int d=0; d<DIMENSIONS; d++) {
        p(d) = offsetOfPatch(d) + kernels::gaussLobattoNodes[_order][_order-i(d)] * sizeOfPatch(d);
      }

    if(_postProcessing->mapWithDerivatives()) {
      _postProcessing->mapQuantities(
        offsetOfPatch,
        sizeOfPatch,
        p,
        i,
        u + idx_u(DIMENSIONS == 3 ? i(2) : 0, i(1), i(0), 0),
        gradU + idx_gradU(DIMENSIONS == 3 ? i(2) : 0, i(1), i(0), 0, 0),
        value,
        timeStamp
      );
    } else {
      _postProcessing->mapQuantities(
        offsetOfPatch,
        sizeOfPatch,
        p,
        i,
        u + idx_u(DIMENSIONS == 3 ? i(2) : 0, i(1), i(0), 0),
        value,
        timeStamp
      );
    }

    if (_writtenUnknowns>0) {
      _vertexDataWriter->plotVertex(firstVertexIndex, value, _writtenUnknowns );
    }

    firstVertexIndex++;
  }

  if (value!=nullptr)        delete[] value;

}


void exahype::plotters::ADERDG2LobattoVTK::plotCellData(
  int firstCellIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u, double* gradU,
  double timeStamp
) {
  assertion( _cellDataWriter!=nullptr || _writtenUnknowns==0 );

  double* interpoland = new double[_solverUnknowns];
  double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];
  
  /****************************
   *  Note: The vtk::Lobatto::cells::... plotter has not been tested yet
   *        because it tends to fail with the assertion
   *     ADERDG2LobattoVTK.cpp, line 516 failed: _writtenUnknowns==0 || _vertexTimeStampDataWriter!=nullptr
   * So this has to be fixed, afterwards this very method can be checked
   * for correctness.
   ****************************/

  // this should go to the header or similar
  const int basisX = _order + 1;
  const int basisY = _order + 1;
  const int basisZ = (DIMENSIONS == 3 ? _order  : 0 ) + 1;

  const bool interpolateDerivatives = _postProcessing->mapWithDerivatives();
  double* inter_gradQ = interpolateDerivatives ? new double[DIMENSIONS * _solverUnknowns] : nullptr;
  kernels::index idx_gradU(basisZ, basisY, basisX, DIMENSIONS, _solverUnknowns);
  kernels::index idx_inter_gradU(DIMENSIONS, _solverUnknowns);

  dfor(i,_order) {
    // This is inefficient but works. We could look it up directly from the arrays
    tarch::la::Vector<DIMENSIONS, double> p;

    for (int d=0; d<DIMENSIONS; d++) {
      p(d) = offsetOfPatch(d) + (kernels::gaussLobattoNodes[_order][_order-i(d)]+kernels::gaussLobattoNodes[_order][_order-(i(d)+1)]) * sizeOfPatch(d)/2.0;
    }
    
    for (int unknown=0; unknown < _solverUnknowns; unknown++) {
      interpoland[unknown] = kernels::interpolate(
        offsetOfPatch.data(),
        sizeOfPatch.data(),
        p.data(), // das ist die Position
        _solverUnknowns,
        unknown,
        _order,
        u
      );
    }

    if(interpolateDerivatives) {
      // Inteprolate the given gradients between the cells. We abuse the interpolate' argument
      // `numberOfUnknowns` with the value `nDim*nVar` in order to circumvent the gradU data ordering.
      for (int d=0; d < DIMENSIONS; d++) {
        for (int unknown=0; unknown < _solverUnknowns; unknown++) {
          inter_gradQ[idx_inter_gradU(d,unknown)] = kernels::interpolate(
            offsetOfPatch.data(),
            sizeOfPatch.data(),
            p.data(),
            DIMENSIONS * _solverUnknowns,
            d * _solverUnknowns + unknown,
            _order,
            gradU
          );
        }
      }

      _postProcessing->mapQuantities(
        offsetOfPatch,
        sizeOfPatch,
        p,
        i,
        interpoland,
	inter_gradQ,
        value,
        timeStamp
      );
    } else {
      _postProcessing->mapQuantities(
        offsetOfPatch,
        sizeOfPatch,
        p,
        i,
        interpoland,
        value,
        timeStamp
      );
    }

    if (_writtenUnknowns>0) {
      _cellDataWriter->plotCell(firstCellIndex, value, _writtenUnknowns );
    }

    firstCellIndex++;
  }

  if (interpoland!=nullptr)  delete[] interpoland;
  if (inter_gradQ!=nullptr)  delete[] inter_gradQ;
  if (value!=nullptr)        delete[] value;
}

void exahype::plotters::ADERDG2LobattoVTK::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  assertion( element!=solvers::Solver::NotFound );
  auto& aderdgCellDescription = cellInfo._ADERDGCellDescriptions[element];

  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    double* solverSolution = static_cast<double*>(aderdgCellDescription.getSolution());

    plotPatch(
        aderdgCellDescription.getOffset(),
        aderdgCellDescription.getSize(), solverSolution,
        aderdgCellDescription.getTimeStamp());
  }
}

void exahype::plotters::ADERDG2LobattoVTK::plotPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp) {
  if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    assertion( _writtenUnknowns==0 || ( _vertexWriter && _cellWriter && _gridWriter ));
    assertion( _writtenUnknowns==0 || (_plotCells && _cellTimeStampDataWriter!=nullptr) || (!_plotCells && _vertexTimeStampDataWriter!=nullptr ));
    assertion(sizeOfPatch(0)==sizeOfPatch(1));

    std::pair<int,int> vertexAndCellIndex = plotLobattoPatch(offsetOfPatch, sizeOfPatch);

    writeTimeStampDataToPatch( timeStamp, vertexAndCellIndex.first, vertexAndCellIndex.second );

    // this should go to the header or similar
    const int basisX = _order + 1;
    const int basisY = _order + 1;
    const int basisZ = (DIMENSIONS == 3 ? _order  : 0 ) + 1;

    double *gradU = nullptr;
    if(_postProcessing->mapWithDerivatives()) {
      gradU = new double[basisZ*basisY*basisX * DIMENSIONS * _solverUnknowns];
      kernels::aderdg::generic::c::computeGradQ(gradU, u, sizeOfPatch, _solverUnknowns, _order);
    }

    if (_plotCells) {
      plotCellData( vertexAndCellIndex.second, offsetOfPatch, sizeOfPatch, u, gradU, timeStamp );
    }
    else {
      plotVertexData( vertexAndCellIndex.first, offsetOfPatch, sizeOfPatch, u, gradU, timeStamp );
    }

    if(gradU!=nullptr) delete[] gradU;
  } // if slicer
}

