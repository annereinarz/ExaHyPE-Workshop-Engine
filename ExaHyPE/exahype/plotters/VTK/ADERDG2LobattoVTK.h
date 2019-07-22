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
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_VTK_LOBATTO_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_VTK_LOBATTO_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/parser/ParserView.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/unstructured/UnstructuredGridWriter.h"
#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

namespace exahype {
  namespace plotters {
    class ADERDG2LobattoVTK;

    class ADERDG2LobattoVerticesVTKAscii;
    class ADERDG2LobattoVerticesVTKBinary;
    class ADERDG2LobattoCellsVTKAscii;
    class ADERDG2LobattoCellsVTKBinary;

    class ADERDG2LobattoVerticesVTUAscii;
    class ADERDG2LobattoVerticesVTUBinary;
    class ADERDG2LobattoCellsVTUAscii;
    class ADERDG2LobattoCellsVTUBinary;

    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
  }
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::ADERDG2LobattoVTK: public exahype::plotters::Plotter::Device {
  protected:
   enum class PlotterType {
     BinaryVTK,
     ASCIIVTK,
     BinaryVTU,
     ASCIIVTU
   };
 private:
  int           _fileCounter;
  const PlotterType _plotterType;
  const bool    _plotCells;
  std::string   _filename;
  int           _order;
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
  static tarch::logging::Log _log;
  
  tarch::plotter::griddata::unstructured::UnstructuredGridWriter*                    _gridWriter;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::VertexWriter*              _vertexWriter;
  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::CellWriter*                _cellWriter;

  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexTimeStampDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellTimeStampDataWriter;
  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellDataWriter;

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

  std::pair<int,int> plotLobattoPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch
  );
 public:
  ADERDG2LobattoVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType isBinary, bool plotCells);
  virtual ~ADERDG2LobattoVTK();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::ADERDG2LobattoVerticesVTKAscii: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoVerticesVTKBinary: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoCellsVTKAscii: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoCellsVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoCellsVTKBinary: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoCellsVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoVerticesVTUAscii: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoVerticesVTUBinary: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoCellsVTUAscii: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoCellsVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LobattoCellsVTUBinary: public exahype::plotters::ADERDG2LobattoVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LobattoCellsVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


#endif
