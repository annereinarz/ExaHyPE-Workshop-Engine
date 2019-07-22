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

#include "exahype/solvers/FiniteVolumesSolver.h"
#include "exahype/plotters/slicing/Slicer.h"
#include "FiniteVolumes2VTK.h"

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

tarch::logging::Log exahype::plotters::FiniteVolumes2VTK::_log("exahype::plotters::FiniteVolumes2VTK");


exahype::plotters::FiniteVolumesCells2VTKAscii::FiniteVolumesCells2VTKAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTK,true) {
}


std::string exahype::plotters::FiniteVolumesCells2VTKAscii::getIdentifier() {
  return ADERDG2CartesianCellsVTKAscii::getIdentifier();
}


exahype::plotters::FiniteVolumesCells2VTKBinary::FiniteVolumesCells2VTKBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTK,true) {
}


std::string exahype::plotters::FiniteVolumesCells2VTKBinary::getIdentifier() {
  return ADERDG2CartesianCellsVTKBinary::getIdentifier();
}


exahype::plotters::FiniteVolumesCells2VTUAscii::FiniteVolumesCells2VTUAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTU,true) {
}


std::string exahype::plotters::FiniteVolumesCells2VTUAscii::getIdentifier() {
  return ADERDG2CartesianCellsVTUAscii::getIdentifier();
}


exahype::plotters::FiniteVolumesCells2VTUBinary::FiniteVolumesCells2VTUBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTU,true) {
}


std::string exahype::plotters::FiniteVolumesCells2VTUBinary::getIdentifier() {
  return ADERDG2CartesianCellsVTUBinary::getIdentifier();
}


exahype::plotters::FiniteVolumesVertices2VTKAscii::FiniteVolumesVertices2VTKAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTK,false) {
}


std::string exahype::plotters::FiniteVolumesVertices2VTKAscii::getIdentifier() {
  return ADERDG2CartesianVerticesVTKAscii::getIdentifier();
}


exahype::plotters::FiniteVolumesVertices2VTKBinary::FiniteVolumesVertices2VTKBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTK,false) {
}


std::string exahype::plotters::FiniteVolumesVertices2VTKBinary::getIdentifier() {
  return ADERDG2CartesianVerticesVTKBinary::getIdentifier();
}


exahype::plotters::FiniteVolumesVertices2VTUAscii::FiniteVolumesVertices2VTUAscii(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::ASCIIVTU,false) {
}


std::string exahype::plotters::FiniteVolumesVertices2VTUAscii::getIdentifier() {
  return ADERDG2CartesianVerticesVTUAscii::getIdentifier();
}


exahype::plotters::FiniteVolumesVertices2VTUBinary::FiniteVolumesVertices2VTUBinary(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int ghostLayerWidth
):
  exahype::plotters::FiniteVolumes2VTK(postProcessing,ghostLayerWidth,PlotterType::BinaryVTU,false) {
}


std::string exahype::plotters::FiniteVolumesVertices2VTUBinary::getIdentifier() {
  return ADERDG2CartesianVerticesVTUBinary::getIdentifier();
}



exahype::plotters::FiniteVolumes2VTK::FiniteVolumes2VTK(
    exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
    const int ghostLayerWidth,
    PlotterType plotterType,
    bool plotCells):
  Device(postProcessing),
  _plotterType(plotterType),
  _plotCells(plotCells),
  _fileCounter(-1),
  _numberOfCellsPerAxis(-1),
  _ghostLayerWidth(ghostLayerWidth),
  _solverUnknowns(-1),
  _writtenUnknowns(-1),
  _patchWriter(nullptr),
  _gridWriter(nullptr),
  _vertexDataWriter(nullptr),
  _cellDataWriter(nullptr),
  _vertexTimeStampDataWriter(nullptr),
  _cellTimeStampDataWriter(nullptr){
}


void exahype::plotters::FiniteVolumes2VTK::init(
  const std::string& filename,
  int                numberOfCellsPerAxis,
  int                unknowns,
  int                writtenUnknowns,
  exahype::parser::ParserView plotterParameters
){
  _filename             = filename;
  _numberOfCellsPerAxis = numberOfCellsPerAxis;
  _solverUnknowns       = unknowns;
  _plotterParameters               = plotterParameters;
  _patchWriter          = nullptr;
  _writtenUnknowns      = writtenUnknowns;

  slicer = Slicer::bestFromSelectionQuery(plotterParameters);

  if(slicer) {
  	logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<filename);
  }
}


void exahype::plotters::FiniteVolumes2VTK::startPlotting( double time ) {
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

    _gridWriter          = _patchWriter->createSinglePatchWriter();
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


void exahype::plotters::FiniteVolumes2VTK::finishPlotting() {
  _postProcessing->finishPlotting();
  if ( _writtenUnknowns>0 ) {
    assertion( _patchWriter!=nullptr );
    assertion( _gridWriter!=nullptr );

    _gridWriter->close();
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
      _patchWriter->writeToFile(snapshotFileName.str());
    if (!hasBeenSuccessful) {
      exit(-1);
    }
  }

  if (_vertexDataWriter!=nullptr)     delete _vertexDataWriter;
  if (_cellDataWriter!=nullptr)       delete _cellDataWriter;
  if (_vertexTimeStampDataWriter!=nullptr)  delete _vertexTimeStampDataWriter;
  if (_cellTimeStampDataWriter!=nullptr)    delete _cellTimeStampDataWriter;
  if (_gridWriter!=nullptr)           delete _gridWriter;
  if (_patchWriter!=nullptr)          delete _patchWriter;

  _vertexDataWriter    = nullptr;
  _cellDataWriter      = nullptr;
  _patchWriter         = nullptr;
  _vertexTimeStampDataWriter = nullptr;
  _cellTimeStampDataWriter   = nullptr;
  _gridWriter          = nullptr;
}


exahype::plotters::FiniteVolumes2VTK::~FiniteVolumes2VTK() {
}

#include "exahype/plotters/slicing/CartesianSlicer.h"

void exahype::plotters::FiniteVolumes2VTK::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
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
        vertexAndCellIndex = _gridWriter->plotPatch(offsetOfPatch, sizeOfPatch, _numberOfCellsPerAxis);
      }
      if (_plotCells) {
        plotCellData(vertexAndCellIndex.second, offsetOfPatch, sizeOfPatch, solution, cellDescription.getTimeStamp());
      } else {
        plotVertexData(vertexAndCellIndex.first, offsetOfPatch, sizeOfPatch, solution, cellDescription.getTimeStamp());
      }
    } // if slicer
  } // if is cell
}

void exahype::plotters::FiniteVolumes2VTK::plotCellData(
  int cellIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
  double timeStamp) {
    logDebug("plotPatch(...)","offset of patch: "<<offsetOfPatch
    <<", size of patch: "<<sizeOfPatch
    <<", time stamp: "<<timeStamp);

    assertion( _writtenUnknowns==0 || _patchWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _gridWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _cellTimeStampDataWriter!=nullptr );

    double* sourceValue = new double[_solverUnknowns];
    double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

    dfor(i,_numberOfCellsPerAxis+_ghostLayerWidth) {
      if (tarch::la::allSmaller(i,_numberOfCellsPerAxis+_ghostLayerWidth)
          && tarch::la::allGreater(i,_ghostLayerWidth-1)) {
        if (_writtenUnknowns>0) {
          _cellTimeStampDataWriter->plotCell(cellIndex, timeStamp);
        }

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

        if (_writtenUnknowns>0) {
          _cellDataWriter->plotCell(cellIndex, value, _writtenUnknowns);
        }
        cellIndex++;
      }
    }

    delete[] sourceValue;
    delete[] value;
}

void exahype::plotters::FiniteVolumes2VTK::plotVertexData(
  int vertexIndex,
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
  double timeStamp) {
    typedef tarch::la::Vector<DIMENSIONS,int> ivec;
    assertion( _writtenUnknowns==0 || _patchWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _gridWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _vertexDataWriter!=nullptr );
    assertion( _writtenUnknowns==0 || _vertexTimeStampDataWriter!=nullptr );

    double* vertexValue = new double[_solverUnknowns];
    double* outputValue = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

    // the following assumes quadratic cells.
    assertion(sizeOfPatch(0)==sizeOfPatch(1));
    kernels::dindex patchPos(_numberOfCellsPerAxis + 2*_ghostLayerWidth); // including ghost zones

    // Initial idea:
    // the following algorithm goes over the vertices and sums up the surrounding cell
    // values in order to determine an interpolation value for the vertex. It thus
    // touches one ghost layer:
    assertion(_ghostLayerWidth >= 1);
    
    /** Lession learned: Do not rely on ghost zones. In the initial data, they are not
     *  filled; in each timestep with all our schemes, the edges are never set.
     *  Of course we could include this when interpolating here but its very ugly
     *  and scheme dependent. So this should go into the kernels then.
     *  Like all of the interpolation code probably.
     **/
    const int numberOfVerticesPerAxis = _numberOfCellsPerAxis + 1;
    dfor(ivertex, numberOfVerticesPerAxis) {
      if (_writtenUnknowns>0) {
        _vertexTimeStampDataWriter->plotVertex(vertexIndex, timeStamp);
      }

      // We do no smearing, so we only take into account the 2 nearest neighbours.
      const int neighbourCellsPerAxis = 2;
      // const int neighbourCellsMax = std::pow(neighbourCellsPerAxis, DIMENSIONS); // maximum possible cells (ie. 4 in 2D)
        std::fill_n(vertexValue, _solverUnknowns, 0.0);
	    int neighbourCells = 0; // actual neighbour cells taken into account
	      dfor(icells, neighbourCellsPerAxis) {
		        ivec icell = _ghostLayerWidth + ivertex + (icells - neighbourCellsPerAxis / 2);
		
	  	// if the target cell position in the patch is *not* in the ghost layers:
		  if (
		    tarch::la::allSmaller(icell,_numberOfCellsPerAxis+_ghostLayerWidth)
		    &&
		    tarch::la::allGreater(icell,_ghostLayerWidth-1)
		  ) {
			  double *cell = u + patchPos.rowMajor(icell)*_solverUnknowns;
		  	for (int unknown=0; unknown < _solverUnknowns; unknown++) {
			  	vertexValue[unknown] += cell[unknown];
		  	}
		  	neighbourCells++;
		  }
	  }

	// normalize value
	for (int unknown=0; unknown < _solverUnknowns; unknown++) {
		vertexValue[unknown] = vertexValue[unknown] / neighbourCells;
	}
	
	// The following code could be used instead of the neighbour contributions as
	// above and was used for the start. Just one  cell.
	// This works and shows how badly it is if we rely on ghost zones.
	/*
	double *cell = u + patchPos.rowMajor(_ghostLayerWidth + ivertex)*_solverUnknowns;
	for (int unknown=0; unknown < _solverUnknowns; unknown++) {
		vertexValue[unknown] = cell[unknown];
	}
	*/

        _postProcessing->mapQuantities(
          offsetOfPatch,
          sizeOfPatch,
          offsetOfPatch + ivertex.convertScalar<double>()* (sizeOfPatch(0)/(numberOfVerticesPerAxis)), // coordinate of vertex
          ivertex,
          vertexValue,
          outputValue,
          timeStamp
        );

        if (_writtenUnknowns>0) {
          _vertexDataWriter->plotVertex(vertexIndex, outputValue, _writtenUnknowns);
        }
        vertexIndex++;
    }

    delete[] vertexValue;
    delete[] outputValue;
}

