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
 
#include "ADERDG2CartesianPeanoPatchFileFormat.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"
#include "peano/utils/Loop.h"

#include "exahype/solvers/ADERDGSolver.h"

#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"


#include "tarch/plotter/griddata/blockstructured/PeanoTextPatchFileWriter.h"
#include "tarch/plotter/griddata/blockstructured/PeanoHDF5PatchFileWriter.h"


#include "kernels/DGBasisFunctions.h"

tarch::logging::Log exahype::plotters::ADERDG2CartesianPeanoFileFormat::_log("exahype::plotters::ADERDG2CartesianPeanoFileFormat");


std::string exahype::plotters::ADERDG2CartesianVerticesPeanoFileFormatAscii::getIdentifier() {
  return "Peano::Cartesian::vertices::ascii";
}


exahype::plotters::ADERDG2CartesianVerticesPeanoFileFormatAscii::ADERDG2CartesianVerticesPeanoFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2CartesianPeanoFileFormat(postProcessing,false,PlotterType::Text) {
}


std::string exahype::plotters::ADERDG2CartesianCellsPeanoFileFormatAscii::getIdentifier() {
  return "Peano::Cartesian::cells::ascii";
}


exahype::plotters::ADERDG2CartesianCellsPeanoFileFormatAscii::ADERDG2CartesianCellsPeanoFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2CartesianPeanoFileFormat(postProcessing,true,PlotterType::Text) {
}








std::string exahype::plotters::ADERDG2CartesianVerticesPeanoFileFormatHDF5::getIdentifier() {
  return "Peano::Cartesian::vertices::hdf5";
}


exahype::plotters::ADERDG2CartesianVerticesPeanoFileFormatHDF5::ADERDG2CartesianVerticesPeanoFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2CartesianPeanoFileFormat(postProcessing,false,PlotterType::Hdf5) {
}


std::string exahype::plotters::ADERDG2CartesianCellsPeanoFileFormatHDF5::getIdentifier() {
  return "Peano::Cartesian::cells::hdf5";
}


exahype::plotters::ADERDG2CartesianCellsPeanoFileFormatHDF5::ADERDG2CartesianCellsPeanoFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2CartesianPeanoFileFormat(postProcessing,true,PlotterType::Hdf5) {
}


exahype::plotters::ADERDG2CartesianPeanoFileFormat::ADERDG2CartesianPeanoFileFormat(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  bool plotCells,
  PlotterType type
):
  Device(postProcessing),
  _fileCounter(-1),
  _plotCells(plotCells),
  _plotType(type),
  _order(-1),
  _solverUnknowns(-1),
  _writtenUnknowns(-1),
  _vertexDataWriter(nullptr),
  _cellDataWriter(nullptr),
  _vertexTimeStampDataWriter(nullptr),
  _cellTimeStampDataWriter(nullptr),
  _writer(nullptr) {
}


void exahype::plotters::ADERDG2CartesianPeanoFileFormat::init(
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
  _writer            = nullptr;
  _writtenUnknowns   = writtenUnknowns;

  slicer = Slicer::bestFromSelectionQuery(plotterParameters);

  if(slicer) {
    logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<filename);
  }
}


void exahype::plotters::ADERDG2CartesianPeanoFileFormat::startPlotting( double time ) {
  assertion( _writer==nullptr );

  if (_writtenUnknowns>0) {
    switch (_plotType) {
      case PlotterType::Text:
        _writer = new tarch::plotter::griddata::blockstructured::PeanoTextPatchFileWriter(
          DIMENSIONS,_order,_filename,
          _fileCounter>0
        );
        break;
      case PlotterType::Hdf5:
        _writer = new tarch::plotter::griddata::blockstructured::PeanoHDF5PatchFileWriter(
          DIMENSIONS,_order,_filename,
          _fileCounter>0,
          true
        );
        break;
    }

    if (_plotCells) {
      _cellDataWriter          = _writer->createCellDataWriter("Q", _writtenUnknowns);
      _vertexDataWriter        = nullptr;
      _cellTimeStampDataWriter = _writer->createCellDataWriter("time", 1);
    }
    else {
      _cellDataWriter            = nullptr;
      _vertexDataWriter          = _writer->createVertexDataWriter("Q", _writtenUnknowns);
      _vertexTimeStampDataWriter = _writer->createVertexDataWriter("time", 1);
    }

    assertion( _writer!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
  _fileCounter++;
}


void exahype::plotters::ADERDG2CartesianPeanoFileFormat::finishPlotting() {
  _postProcessing->finishPlotting();

  if (_writtenUnknowns>0) {
    assertion( _writer!=nullptr );

    if (_vertexDataWriter!=nullptr) _vertexDataWriter->close();
    if (_cellDataWriter!=nullptr)   _cellDataWriter->close();
    if (_vertexTimeStampDataWriter!=nullptr) _vertexTimeStampDataWriter->close();
    if (_cellTimeStampDataWriter!=nullptr)   _cellTimeStampDataWriter->close();

    std::ostringstream snapshotFileName;
    snapshotFileName << _filename
                     << "-" << _fileCounter;

    const bool hasBeenSuccessful =
      _writer->writeToFile(snapshotFileName.str());
    if (!hasBeenSuccessful) {
      exit(-1);
    }
  }

  if (_vertexDataWriter!=nullptr)           delete _vertexDataWriter;
  if (_cellDataWriter!=nullptr)             delete _cellDataWriter;
  if (_vertexTimeStampDataWriter!=nullptr)  delete _vertexTimeStampDataWriter;
  if (_cellTimeStampDataWriter!=nullptr)    delete _cellTimeStampDataWriter;
  if (_writer!=nullptr)                     delete _writer;

  _vertexDataWriter          = nullptr;
  _cellDataWriter            = nullptr;
  _writer                    = nullptr;
  _vertexTimeStampDataWriter = nullptr;
  _cellTimeStampDataWriter   = nullptr;
}



exahype::plotters::ADERDG2CartesianPeanoFileFormat::~ADERDG2CartesianPeanoFileFormat() {
}


void exahype::plotters::ADERDG2CartesianPeanoFileFormat::writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex ) {
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


void exahype::plotters::ADERDG2CartesianPeanoFileFormat::plotVertexData(
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


void exahype::plotters::ADERDG2CartesianPeanoFileFormat::plotCellData(
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

void exahype::plotters::ADERDG2CartesianPeanoFileFormat::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
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

void exahype::plotters::ADERDG2CartesianPeanoFileFormat::plotPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp) {
  if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    assertion( _writtenUnknowns==0 || _writer!=nullptr );

    std::pair<int,int> vertexAndCellIndex(0,0);
    if (_writtenUnknowns>0) {
      vertexAndCellIndex = _writer->plotPatch(offsetOfPatch, sizeOfPatch);
    }

    writeTimeStampDataToPatch( timeStamp, vertexAndCellIndex.first, vertexAndCellIndex.second );

    if (_plotCells) {
      plotCellData( vertexAndCellIndex.second, offsetOfPatch, sizeOfPatch, u, timeStamp );
    }
    else {
      plotVertexData( vertexAndCellIndex.first, offsetOfPatch, sizeOfPatch, u, timeStamp );
    }
  }
}
