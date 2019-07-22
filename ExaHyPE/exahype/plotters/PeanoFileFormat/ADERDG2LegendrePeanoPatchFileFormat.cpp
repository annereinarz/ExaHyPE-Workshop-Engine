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
 
#include "ADERDG2LegendrePeanoPatchFileFormat.h"
#include "tarch/parallel/Node.h"

#include "peano/utils/Loop.h"

#include "kernels/KernelUtils.h"
#include "kernels/DGMatrices.h"
#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/DGBasisFunctions.h"

#include "exahype/solvers/ADERDGSolver.h"

#include "tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTKBinaryFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUTextFileWriter.h"
#include "tarch/plotter/griddata/unstructured/vtk/VTUBinaryFileWriter.h"


#include "tarch/plotter/griddata/blockstructured/PeanoTextPatchFileWriter.h"
#include "tarch/plotter/griddata/blockstructured/PeanoHDF5PatchFileWriter.h"


#include "kernels/DGBasisFunctions.h"
#include "kernels/aderdg/generic/c/computeGradients.cpph" // derivatives

#include "exahype/plotters/slicing/Slicer.h"

std::string exahype::plotters::ADERDG2LegendreVerticesPeanoFileFormatAscii::getIdentifier() {
  return "Peano::Legendre::vertices::ascii";
}


exahype::plotters::ADERDG2LegendreVerticesPeanoFileFormatAscii::ADERDG2LegendreVerticesPeanoFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2LegendrePeanoPatchFileFormat(postProcessing,false,PlotterType::Text) {
}


std::string exahype::plotters::ADERDG2LegendreCellsPeanoFileFormatAscii::getIdentifier() {
  return "Peano::Legendre::cells::ascii";
}


exahype::plotters::ADERDG2LegendreCellsPeanoFileFormatAscii::ADERDG2LegendreCellsPeanoFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2LegendrePeanoPatchFileFormat(postProcessing,true,PlotterType::Text) {
}








std::string exahype::plotters::ADERDG2LegendreVerticesPeanoFileFormatHDF5::getIdentifier() {
  return "Peano::Legendre::vertices::hdf5";
}


exahype::plotters::ADERDG2LegendreVerticesPeanoFileFormatHDF5::ADERDG2LegendreVerticesPeanoFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2LegendrePeanoPatchFileFormat(postProcessing,false,PlotterType::Hdf5) {
}


std::string exahype::plotters::ADERDG2LegendreCellsPeanoFileFormatHDF5::getIdentifier() {
  return "Peano::Legendre::cells::hdf5";
}


exahype::plotters::ADERDG2LegendreCellsPeanoFileFormatHDF5::ADERDG2LegendreCellsPeanoFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  ADERDG2LegendrePeanoPatchFileFormat(postProcessing,true,PlotterType::Hdf5) {
}



tarch::logging::Log  exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::_log( "exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat" );


exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::ADERDG2LegendrePeanoPatchFileFormat(
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


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::init(
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




std::vector<double> exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::getMapping() const {
  std::vector<double> result;
  dfor(i,_order+1) {
    for (int d=0; d<DIMENSIONS; d++) {
      const double x = kernels::gaussLegendreNodes[_order][i(d)];
      result.push_back(x);
    }
  }
  return result;
}


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::startPlotting( double time ) {
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
      _cellDataWriter          = _writer->createCellDataWriter("Q", _writtenUnknowns, "", getMapping().data() );
      _vertexDataWriter        = nullptr;
      _cellTimeStampDataWriter = _writer->createCellDataWriter("time", 1, "", getMapping().data());
    }
    else {
      _cellDataWriter            = nullptr;
      _vertexDataWriter          = _writer->createVertexDataWriter("Q", _writtenUnknowns, "", getMapping().data());
      _vertexTimeStampDataWriter = _writer->createVertexDataWriter("time", 1, "", getMapping().data());
    }

    assertion( _writer!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
  _fileCounter++;
}


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::finishPlotting() {
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

  if (_vertexDataWriter!=nullptr)     delete _vertexDataWriter;
  if (_cellDataWriter!=nullptr)       delete _cellDataWriter;
  if (_vertexTimeStampDataWriter!=nullptr)  delete _vertexTimeStampDataWriter;
  if (_cellTimeStampDataWriter!=nullptr)    delete _cellTimeStampDataWriter;
  if (_writer!=nullptr)                     delete _writer;

  _vertexDataWriter    = nullptr;
  _cellDataWriter      = nullptr;
  _writer              = nullptr;
  _vertexTimeStampDataWriter = nullptr;
  _cellTimeStampDataWriter   = nullptr;
}



exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::~ADERDG2LegendrePeanoPatchFileFormat() {
}


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex ) {
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


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::plotVertexData(
  int firstVertexIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u,
  double* gradU,
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
      p(d) = offsetOfPatch(d) + kernels::gaussLegendreNodes[_order][i(d)] * sizeOfPatch(d);
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
}


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::plotCellData(
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
   *  Note: The vtk::Legendre::cells::... plotter has not been tested yet
   *        because it tends to fail with the assertion
   *     ADERDG2LegendreVTK.cpp, line 516 failed: _writtenUnknowns==0 || _vertexTimeStampDataWriter!=nullptr
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
      p(d) = offsetOfPatch(d) + (kernels::gaussLegendreNodes[_order][i(d)]+kernels::gaussLegendreNodes[_order][i(d)+1]) * sizeOfPatch(d)/2.0;
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


void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
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

void exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat::plotPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp) {
  if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
    assertion( _writtenUnknowns==0 || (_plotCells && _cellTimeStampDataWriter!=nullptr) || (!_plotCells && _vertexTimeStampDataWriter!=nullptr ));
    assertion(sizeOfPatch(0)==sizeOfPatch(1));

    std::pair<int,int> vertexAndCellIndex = _writer->plotPatch(offsetOfPatch, sizeOfPatch);

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
