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
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_CARTESIAN_VTK_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_CARTESIAN_VTK_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/parser/ParserView.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

namespace exahype {
  namespace plotters {
    class ADERDG2CartesianVTK;

    class ADERDG2CartesianVerticesVTKAscii;
    class ADERDG2CartesianVerticesVTKBinary;
    class ADERDG2CartesianCellsVTKAscii;
    class ADERDG2CartesianCellsVTKBinary;

    class ADERDG2CartesianVerticesVTUAscii;
    class ADERDG2CartesianVerticesVTUBinary;
    class ADERDG2CartesianCellsVTUAscii;
    class ADERDG2CartesianCellsVTUBinary;

    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
  }
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::ADERDG2CartesianVTK: public exahype::plotters::Plotter::Device {
 protected:
   enum class PlotterType {
     BinaryVTK,
     ASCIIVTK,
     BinaryVTU,
     ASCIIVTU
   };
 private:
  int           _fileCounter     = -1;
  const PlotterType _plotterType = PlotterType::BinaryVTU;
  const bool    _plotCells       = false;
  std::string   _filename        = "";
  int           _order           = -1;
  int           _solverUnknowns  = -1;
  int           _writtenUnknowns = -1;
  exahype::parser::ParserView   _plotterParameters ;

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
  std::vector<double>          _tempSolution;
  std::vector<double>          _tempGradient;

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

  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexDataWriter = nullptr;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellDataWriter   = nullptr;
  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexTimeStampDataWriter = nullptr;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellTimeStampDataWriter   = nullptr;

  tarch::plotter::griddata::blockstructured::PatchWriter::SinglePatchWriter* _gridWriter  = nullptr;
  tarch::plotter::griddata::blockstructured::PatchWriterUnstructured*        _patchWriter = nullptr;

  void writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex );

  void plotVertexData(
    int firstVertexIndex,
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp
  );

  void plotCellData(
    int firstCellIndex,
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp
  );
 public:
  ADERDG2CartesianVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType plotterType, bool plotCells);
  virtual ~ADERDG2CartesianVTK();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::ADERDG2CartesianVerticesVTKAscii: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianVerticesVTKBinary: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianCellsVTKAscii: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianCellsVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianCellsVTKBinary: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianCellsVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianVerticesVTUAscii: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianVerticesVTUBinary: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianCellsVTUAscii: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianCellsVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2CartesianCellsVTUBinary: public exahype::plotters::ADERDG2CartesianVTK {
  public:
    static std::string getIdentifier();
    ADERDG2CartesianCellsVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};

#endif
