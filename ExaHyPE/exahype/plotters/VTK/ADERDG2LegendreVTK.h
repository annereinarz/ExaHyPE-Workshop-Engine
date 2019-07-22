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

#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_VTK_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_VTK_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/parser/ParserView.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/unstructured/UnstructuredGridWriter.h"
#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

namespace exahype {
  namespace plotters {
    class ADERDG2LegendreVTK;

    class ADERDG2LegendreVerticesVTKAscii;
    class ADERDG2LegendreVerticesVTKBinary;
    class ADERDG2LegendreCellsVTKAscii;
    class ADERDG2LegendreCellsVTKBinary;

    class ADERDG2LegendreVerticesVTUAscii;
    class ADERDG2LegendreVerticesVTUBinary;
    class ADERDG2LegendreCellsVTUAscii;
    class ADERDG2LegendreCellsVTUBinary;

    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
  }
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::ADERDG2LegendreVTK: public exahype::plotters::Plotter::Device {
protected:
  enum class PlotterType {
    BinaryVTK,
    ASCIIVTK,
    BinaryVTU,
    ASCIIVTU
  };
private:
  const PlotterType            _plotterType;
  const bool                   _plotCells;

  int                          _fileCounter = 0;
  std::string                  _filename;

  int                          _order             = 0;
  int                          _solverUnknowns    = 0;
  int                          _writtenUnknowns   = 0;

  exahype::parser::ParserView  _plotterParameters;

  /**
   * To resolve features of a high order polynomial solution
   * in a VTK compatible way, we apply the volume interpolation operator
   * to the DG solution polynomial.
   *
   * @return number of times the prolongation operators should be applied.
   */
  int                          _resolution = 0;

  /**
   * Temporary solution and gradient arrays used for interpolating the solution
   * onto finer grids.
   */
  DataHeap::HeapEntries _tempSolution;
  DataHeap::HeapEntries _tempGradient;

  /**
   * Is obviously only used if we use vtu instead of the vtk legacy format.
   */
  tarch::plotter::griddata::VTUTimeSeriesWriter _timeSeriesWriter;

  /**
   * To memorise the time argument from startPlotter(). We need it when we close the plotter for the time series.
   */
  double _time = 0;

  exahype::plotters::Slicer *_slicer = nullptr;
  static tarch::logging::Log _log;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter*               _gridWriter = nullptr;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::VertexWriter* _vertexWriter = nullptr;
  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::CellWriter*   _cellWriter   = nullptr;

  tarch::plotter::griddata::Writer::VertexDataWriter*                           _vertexTimeStampDataWriter = nullptr;
  tarch::plotter::griddata::Writer::CellDataWriter*                             _cellTimeStampDataWriter   = nullptr;
  tarch::plotter::griddata::Writer::VertexDataWriter*                           _vertexDataWriter          = nullptr;
  tarch::plotter::griddata::Writer::CellDataWriter*                             _cellDataWriter            = nullptr;

  void writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex );

  void plotVertexData(
      int firstVertexIndex,
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
      double* u, double* gradU,
      double timeStamp
  );

  void plotCellData(
      int firstCellIndex,
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
      double* u, double* gradU,
      double timeStamp
  );

  std::pair<int,int> plotLegendrePatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch
  );
public:
  ADERDG2LegendreVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType isBinary, bool plotCells);
  virtual ~ADERDG2LegendreVTK();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
      double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::ADERDG2LegendreVerticesVTKAscii: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreVerticesVTKBinary: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreCellsVTKAscii: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreCellsVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreCellsVTKBinary: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreCellsVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreVerticesVTUAscii: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreVerticesVTUBinary: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreCellsVTUAscii: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreCellsVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreCellsVTUBinary: public exahype::plotters::ADERDG2LegendreVTK {
public:
  static std::string getIdentifier();
  ADERDG2LegendreCellsVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


#endif
