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
 
#ifndef _EXAHYPE_PLOTTERS_PATCH_2_VTK_H_
#define _EXAHYPE_PLOTTERS_PATCH_2_VTK_H_

#include "exahype/plotters/Plotter.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/unstructured/UnstructuredGridWriter.h"

#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

#include <map>

namespace exahype {
  namespace plotters {
    class Patch2VTK;

    class Patch2VTKBoxesAscii;
    class Patch2VTKBoxesBinary;
    class Patch2VTKGapsAscii;
    class Patch2VTKGapsBinary;

    class Patch2VTUBoxesAscii;
    class Patch2VTUBoxesBinary;
    class Patch2VTUGapsAscii;
    class Patch2VTUGapsBinary;

    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
  }
}

class exahype::plotters::Patch2VTK: public exahype::plotters::Plotter::Device {
  protected:
   enum class PlotterType {
     BinaryVTK,
     ASCIIVTK,
     BinaryVTU,
     ASCIIVTU
   };
 private:
  const exahype::solvers::Solver::Type _solverType;
  const bool _isLimitingSolver; // Shorthand
  const PlotterType _plotterType;
  const bool    _plotGaps; ///< whether to plot cells with artificial gap
  const bool    _hasMPIenabled;
  int           _fileCounter;
  std::string   _filename;
  //int           _numberOfCellsPerAxis;
  int           _solverUnknowns;
  int           _writtenUnknowns;
  exahype::parser::ParserView   _plotterParameters;
  double        _cellScaleFactor; ///< a factor to visually reduce the box size of a cell, should be between [0,1] where 1 means full size

  /**
   * Is obviously only used if we use vtu instead of the vtk legacy format.
   */
  tarch::plotter::griddata::VTUTimeSeriesWriter _timeSeriesWriter;

  /**
   * To memorise the time argument from startPlotter(). We need it when we close the plotter for the time series.
   */
  double _time;

  exahype::plotters::Slicer *slicer;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter*                    _gridWriter;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::VertexWriter*      _vertexWriter;
  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::CellWriter*        _cellWriter;

  // doesn't bring too much:
  /*
  /// since we have so much celldatawriters, manage them in a map where the label indicates the
  /// field label.
  std::map<std::string, tarch::plotter::griddata::Writer::CellDataWriter*> _cellScalarWriters;
  
  // we want to use these labels. If you add another, don't forget to add it to the plotPatch() registry.
  const char* const dataLabel = "Q";
  const char* const timeLabel = "time";
  const char* const mpiLabel = "MpiRank";
  const char* const limiterLabel = "Limiter-Status(0-O,1..2-DG,3..4-FV,5-T)";
  const char* const previousLimiterLabel = "Previous-Limiter-Status(0-O,1..2-DG,3..4-FV,5-T)";
  */
  
  /// as a leftover, we only store this vector of celldata with lnength _writtenUnknowns.
  tarch::plotter::griddata::Writer::CellDataWriter *_cellDataWriter, *_cellMpiRankWriter, *_cellRefinementStatusWriter, *_cellPreviousRefinementStatusWriter, *_cellTimeStampDataWriter, *_cellDescriptionIndexWriter, *_cellElementWriter, *_cellLevelWriter;

  static tarch::logging::Log _log;

 public:
  Patch2VTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType plotterType, bool plotCells, exahype::solvers::Solver::Type solvertype);
  virtual ~Patch2VTK();

  virtual void init(const std::string& filename, int numberOfCellsPerAxis, int unknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;
  std::pair<int,int> plotCellBoundary(
	tarch::la::Vector<DIMENSIONS, double> offsetOfPatch,
	tarch::la::Vector<DIMENSIONS, double> sizeOfPatch);

  void plotAverageData(int cellIndex,
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::Patch2VTKBoxesAscii: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTKBoxesAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTKBoxesBinary: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTKBoxesBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTUBoxesAscii: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTUBoxesAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTUBoxesBinary: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTUBoxesBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTKGapsAscii: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTKGapsAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTKGapsBinary: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTKGapsBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTUGapsAscii: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTUGapsAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};


class exahype::plotters::Patch2VTUGapsBinary: public exahype::plotters::Patch2VTK {
  public:
    Patch2VTUGapsBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
    static std::string getIdentifier();
};



#endif
