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

#include "exahype/solvers/FiniteVolumesSolver.h"
#include "exahype/plotters/slicing/Slicer.h"
#include "FiniteVolumes2PeanoPatchFileFormat.h"

#include "tarch/plotter/griddata/blockstructured/PeanoTextPatchFileWriter.h"
#include "tarch/plotter/griddata/blockstructured/PeanoHDF5PatchFileWriter.h"


// @todo 16/05/03:Dominic Etienne Charreir Plotter depends now on kernels.
// Should thus be placed in kernel module or the solver
// should provide a function that computes solution values
// at equidistant grid points
#include "kernels/DGMatrices.h"
#include "kernels/KernelUtils.h" // index functions
#include "peano/utils/Loop.h"


std::string exahype::plotters::FiniteVolumes2PeanoPatchFileFormatAscii::getIdentifier() {
  return "Peano::Cartesian::cells::ascii";
}


exahype::plotters::FiniteVolumes2PeanoPatchFileFormatAscii::FiniteVolumes2PeanoPatchFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth):
  FiniteVolumes2PeanoPatchFileFormat(postProcessing,ghostLayerWidth,PlotterType::Text) {
}


std::string exahype::plotters::FiniteVolumes2PeanoPatchFileFormatHDF5::getIdentifier() {
  return "Peano::Cartesian::cells::hdf5";
}


exahype::plotters::FiniteVolumes2PeanoPatchFileFormatHDF5::FiniteVolumes2PeanoPatchFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth):
  FiniteVolumes2PeanoPatchFileFormat(postProcessing,ghostLayerWidth,PlotterType::Hdf5) {
}


tarch::logging::Log exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::_log( "exahype::plotters::FiniteVolumes2PeanoPatchFileFormat" );


exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::FiniteVolumes2PeanoPatchFileFormat(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing*   postProcessing,
  const int                                                 ghostLayerWidth,
  PlotterType                                               plotterType):
  Device(postProcessing),
  _fileCounter(-1),
  _plotterType(plotterType),
  _numberOfCellsPerAxis(-1),
  _ghostLayerWidth(ghostLayerWidth),
  _solverUnknowns(-1),
  _writtenUnknowns(-1),
  _cellDataWriter(nullptr),
  _cellTimeStampDataWriter(nullptr),
  _writer(nullptr) {
}


void exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::init(
  const std::string& filename,
  int                numberOfCellsPerAxis,
  int                unknowns,
  int                writtenUnknowns,
  exahype::parser::ParserView plotterParameters
) {
  _filename             = filename;
  _numberOfCellsPerAxis = numberOfCellsPerAxis;
  _solverUnknowns       = unknowns;
  _plotterParameters               = plotterParameters;
  _writer               = nullptr;
  _writtenUnknowns      = writtenUnknowns;

  slicer = Slicer::bestFromSelectionQuery(plotterParameters);

  if(slicer) {
    logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<filename);
  }
}


void exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::startPlotting( double time ) {
  assertion( _writer==nullptr );

  if (_writtenUnknowns>0) {
    switch (_plotterType) {
      case PlotterType::Text:
        _writer = new tarch::plotter::griddata::blockstructured::PeanoTextPatchFileWriter(
          DIMENSIONS,_numberOfCellsPerAxis,_filename,
          _fileCounter>0
        );
        break;
      case PlotterType::Hdf5:
        _writer = new tarch::plotter::griddata::blockstructured::PeanoHDF5PatchFileWriter(
          DIMENSIONS,_numberOfCellsPerAxis,_filename,
          _fileCounter>0,
          true
        );
        break;
    }

    _cellDataWriter          = _writer->createCellDataWriter("Q", _writtenUnknowns);
    _cellTimeStampDataWriter = _writer->createCellDataWriter("time", 1);

    assertion( _writer!=nullptr );
  }

  _postProcessing->startPlotting( time );

  _time = time;
  _fileCounter++;
}


void exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::finishPlotting() {
  _postProcessing->finishPlotting();

  if (_writtenUnknowns>0) {
    assertion( _writer!=nullptr );

    if (_cellDataWriter!=nullptr)   _cellDataWriter->close();
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

  if (_cellDataWriter!=nullptr)             delete _cellDataWriter;
  if (_cellTimeStampDataWriter!=nullptr)    delete _cellTimeStampDataWriter;
  if (_writer!=nullptr)                     delete _writer;

  _cellDataWriter            = nullptr;
  _writer                    = nullptr;
  _cellTimeStampDataWriter   = nullptr;
}


exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::~FiniteVolumes2PeanoPatchFileFormat() {
}


void exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::plotCellData(
  int firstCellIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u,
  double timeStamp
) {
  assertion( _cellDataWriter!=nullptr || _writtenUnknowns==0 );

  double* sourceValue = new double[_solverUnknowns];
  double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

  dfor(i,_numberOfCellsPerAxis+_ghostLayerWidth) {
    if (
      tarch::la::allSmaller(i,_numberOfCellsPerAxis+_ghostLayerWidth)
      &&
      tarch::la::allGreater(i,_ghostLayerWidth-1)
    ) {
      // Comment(SVEN): This copying is actually not neccessary and could be skipped
      // in favour of just using the pointer &u[...dLinearisedWithoutLookup(...)*_solverUnknowns]
      for (int unknown=0; unknown < _solverUnknowns; unknown++) {
        sourceValue[unknown] =
          u[peano::utils::dLinearisedWithoutLookup(i,_numberOfCellsPerAxis+2*_ghostLayerWidth)*_solverUnknowns+unknown];
      } // !!! Be aware of the "2*_ghostLayerWidth" !!!

      assertion(sizeOfPatch(0)==sizeOfPatch(1));

      _postProcessing->mapQuantities(
        offsetOfPatch,
        sizeOfPatch,
        offsetOfPatch + (i-_ghostLayerWidth).convertScalar<double>()* (sizeOfPatch(0)/(_numberOfCellsPerAxis)),
        i-_ghostLayerWidth,
        sourceValue,
        value,
        timeStamp
      );

      _cellDataWriter->plotCell(firstCellIndex, value, _writtenUnknowns );

      firstCellIndex++;
    }
  }

  if (sourceValue!=nullptr)  delete[] sourceValue;
  if (value!=nullptr)        delete[] value;
}


void exahype::plotters::FiniteVolumes2PeanoPatchFileFormat::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfFiniteVolumesCellDescription(solverNumber);
  auto& cellDescription  = cellInfo._FiniteVolumesCellDescriptions[element];

  if (cellDescription.getType()==exahype::solvers::FiniteVolumesSolver::CellDescription::Type::Cell) {
    const tarch::la::Vector<DIMENSIONS, double> &offsetOfPatch = cellDescription.getOffset(), &sizeOfPatch = cellDescription.getSize();

    // Slicing debugging:
    /*
    if(slicer && slicer->getIdentifier() == "CartesianSlicer" && offsetOfPatch(0) == 0.0) {
      logInfo("slicing", "Having " << slicer->toString() << ", isPatchActive("<<offsetOfPatch<<","<<sizeOfPatch<<") = "<< slicer->isPatchActive(offsetOfPatch, sizeOfPatch) );
      logInfo("debug", ((exahype::plotters::CartesianSlicer*)slicer)->debugVerbose());
    }
    */

    if (!slicer || slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) {
      double* solution = static_cast<double*>(cellDescription.getSolution());
      std::pair<int,int> vertexAndCellIndex(0,0);

      if (_writtenUnknowns>0) {
        vertexAndCellIndex = _writer->plotPatch(offsetOfPatch, sizeOfPatch);
      }

      plotCellData(vertexAndCellIndex.second, offsetOfPatch, sizeOfPatch, solution, cellDescription.getTimeStamp());
    } // if slicer
  } // if is cell
}
