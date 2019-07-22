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
 
#ifndef _EXAHYPE_PLOTTERS_FINITE_VOLUMES_2_VTK_H_
#define _EXAHYPE_PLOTTERS_FINITE_VOLUMES_2_VTK_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/parser/ParserView.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"

#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

namespace exahype {
  namespace plotters {
    class FiniteVolumes2VTK;

    class FiniteVolumesCells2VTKAscii;
    class FiniteVolumesCells2VTKBinary;

    class FiniteVolumesCells2VTUAscii;
    class FiniteVolumesCells2VTUBinary;
    
    class FiniteVolumesVertices2VTKAscii;
    class FiniteVolumesVertices2VTKBinary;

    class FiniteVolumesVertices2VTUAscii;
    class FiniteVolumesVertices2VTUBinary;

    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
  }
}

class exahype::plotters::FiniteVolumes2VTK: public exahype::plotters::Plotter::Device {
  protected:
   enum class PlotterType {
     BinaryVTK,
     ASCIIVTK,
     BinaryVTU,
     ASCIIVTU
   };
 private:
  const PlotterType _plotterType;
  const bool    _plotCells;  
  int           _fileCounter;
  std::string   _filename;
  int           _numberOfCellsPerAxis;
  int           _ghostLayerWidth;
  int           _solverUnknowns;
  int           _writtenUnknowns;
  exahype::parser::ParserView   _plotterParameters;

  /**
   * Is obviously only used if we use vtu instead of the vtk legacy format.
   */
  tarch::plotter::griddata::VTUTimeSeriesWriter _timeSeriesWriter;

  /**
   * To memorise the time argument from startPlotter(). We need it when we close the plotter for the time series.
   */
  double _time;

  exahype::plotters::Slicer *slicer;

  tarch::plotter::griddata::blockstructured::PatchWriterUnstructured*
      _patchWriter;
  tarch::plotter::griddata::blockstructured::PatchWriter::SinglePatchWriter*
      _gridWriter;

  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellDataWriter;
  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexTimeStampDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellTimeStampDataWriter;

  static tarch::logging::Log _log;

 public:
  FiniteVolumes2VTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth, PlotterType plotterType, bool plotCells);
  virtual ~FiniteVolumes2VTK();

  virtual void init(const std::string& filename, int numberOfCellsPerAxis, int unknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  void plotCellData(
      int firstCellIndex,
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  void plotVertexData(
      int firstCellIndex,
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::FiniteVolumesCells2VTKAscii: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesCells2VTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesCells2VTKBinary: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesCells2VTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesCells2VTUAscii: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesCells2VTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesCells2VTUBinary: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesCells2VTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesVertices2VTKAscii: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesVertices2VTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesVertices2VTKBinary: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesVertices2VTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesVertices2VTUAscii: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesVertices2VTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};


class exahype::plotters::FiniteVolumesVertices2VTUBinary: public exahype::plotters::FiniteVolumes2VTK {
  public:
    FiniteVolumesVertices2VTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);

    static std::string getIdentifier();
};



#endif
