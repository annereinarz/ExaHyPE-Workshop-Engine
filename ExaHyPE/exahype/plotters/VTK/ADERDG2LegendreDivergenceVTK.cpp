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
 
#include "ADERDG2LegendreDivergenceVTK.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"
#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/DGBasisFunctions.h"

#include "peano/utils/Loop.h"

#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"

#include "exahype/solvers/ADERDGSolver.h"


tarch::logging::Log exahype::plotters::ADERDG2LegendreDivergenceVTK::_log("exahype::plotters::ADERDG2LegendreDivergenceVTK");

std::string exahype::plotters::ADERDG2LegendreDivergenceVerticesVTKAscii::getIdentifier() {
  return "vtk::Legendre::vertices::div::ascii";
}


exahype::plotters::ADERDG2LegendreDivergenceVerticesVTKAscii::ADERDG2LegendreDivergenceVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LegendreDivergenceVTK(postProcessing,PlotterType::ASCIIVTK) {
}


std::string exahype::plotters::ADERDG2LegendreDivergenceVerticesVTKBinary::getIdentifier() {
  return "vtk::Legendre::vertices::div::binary";
}


exahype::plotters::ADERDG2LegendreDivergenceVerticesVTKBinary::ADERDG2LegendreDivergenceVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LegendreDivergenceVTK(postProcessing,PlotterType::BinaryVTK) {
}


std::string exahype::plotters::ADERDG2LegendreDivergenceVerticesVTUAscii::getIdentifier() {
  return "vtu::Legendre::vertices::div::ascii";
}


exahype::plotters::ADERDG2LegendreDivergenceVerticesVTUAscii::ADERDG2LegendreDivergenceVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LegendreDivergenceVTK(postProcessing,PlotterType::ASCIIVTU) {
}


std::string exahype::plotters::ADERDG2LegendreDivergenceVerticesVTUBinary::getIdentifier() {
  return "vtu::Legendre::vertices::div::binary";
}


exahype::plotters::ADERDG2LegendreDivergenceVerticesVTUBinary::ADERDG2LegendreDivergenceVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
    ADERDG2LegendreDivergenceVTK(postProcessing,PlotterType::BinaryVTU) {
}




exahype::plotters::ADERDG2LegendreDivergenceVTK::ADERDG2LegendreDivergenceVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType plotterType):
  Device(postProcessing),
  _fileCounter(-1),
  _plotterType(plotterType),
  _gridWriter(nullptr),
  _vertexWriter(nullptr),
  _cellWriter(nullptr),
  _vertexTimeStampDataWriter(nullptr),
  _vertexDataWriter(nullptr) {
}


void exahype::plotters::ADERDG2LegendreDivergenceVTK::init(
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


void exahype::plotters::ADERDG2LegendreDivergenceVTK::startPlotting( double time ) {
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

    _vertexDataWriter        = _gridWriter->createVertexDataWriter("Q", _writtenUnknowns);
    _vertexTimeStampDataWriter = _gridWriter->createVertexDataWriter("time", 1);

    assertion( _gridWriter!=nullptr );
    assertion( _vertexWriter!=nullptr );
    assertion( _cellWriter!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
}


void exahype::plotters::ADERDG2LegendreDivergenceVTK::finishPlotting() {
  _postProcessing->finishPlotting();

  if ( _writtenUnknowns>0 ) {
    assertion( _gridWriter!=nullptr );

    _vertexWriter->close();
    _cellWriter->close();
    if (_vertexDataWriter!=nullptr) _vertexDataWriter->close();
    if (_vertexTimeStampDataWriter!=nullptr) _vertexTimeStampDataWriter->close();

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
  if (_vertexWriter!=nullptr)        delete _vertexWriter;
  if (_cellWriter!=nullptr)          delete _cellWriter;
  if (_vertexTimeStampDataWriter!=nullptr) delete _vertexTimeStampDataWriter;
  if (_gridWriter!=nullptr)          delete _gridWriter;

  _vertexDataWriter           = nullptr;
  _vertexWriter               = nullptr;
  _cellWriter                 = nullptr;
  _vertexTimeStampDataWriter  = nullptr;
  _gridWriter                 = nullptr;
}


exahype::plotters::ADERDG2LegendreDivergenceVTK::~ADERDG2LegendreDivergenceVTK() {
}


void exahype::plotters::ADERDG2LegendreDivergenceVTK::writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex ) {
  if (_writtenUnknowns>0 && _vertexTimeStampDataWriter!=nullptr) {
    dfor(i,_order+1) {
      _vertexTimeStampDataWriter->plotVertex(vertexIndex, timeStamp);
      vertexIndex++;
    }
  }
}


std::pair<int,int> exahype::plotters::ADERDG2LegendreDivergenceVTK::plotLegendrePatch(
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
        p(d) = offsetOfPatch(d) + kernels::gaussLegendreNodes[_order][i(d)] * sizeOfPatch(d);
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


void exahype::plotters::ADERDG2LegendreDivergenceVTK::plotVertexData(
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
    // This is inefficient but works. We could look it up directly from the arrays
    tarch::la::Vector<DIMENSIONS, double> p;
    for (int d=0; d<DIMENSIONS; d++) {
      p(d) = offsetOfPatch(d) + kernels::gaussLegendreNodes[_order][i(d)] * sizeOfPatch(d);
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

    assertion(sizeOfPatch(0)==sizeOfPatch(1));
    _postProcessing->mapQuantities(
      offsetOfPatch,
      sizeOfPatch,
      p,
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


void exahype::plotters::ADERDG2LegendreDivergenceVTK::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& aderdgCellDescription  = cellInfo._ADERDGCellDescriptions[element];

  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    double* solverSolution = static_cast<double*>(aderdgCellDescription.getSolution());

    plotPatch(
        aderdgCellDescription.getOffset(),
        aderdgCellDescription.getSize(), solverSolution,
        aderdgCellDescription.getTimeStamp());
  }
}

void exahype::plotters::ADERDG2LegendreDivergenceVTK::plotPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp) {
  if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    assertion( _writtenUnknowns==0 || _vertexWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _cellWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _gridWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _vertexTimeStampDataWriter!=nullptr );

    std::pair<int,int> vertexAndCellIndex = plotLegendrePatch(offsetOfPatch, sizeOfPatch);

    writeTimeStampDataToPatch( timeStamp, vertexAndCellIndex.first, vertexAndCellIndex.second );

    plotVertexData( vertexAndCellIndex.first, offsetOfPatch, sizeOfPatch, u, timeStamp );
  }
}
