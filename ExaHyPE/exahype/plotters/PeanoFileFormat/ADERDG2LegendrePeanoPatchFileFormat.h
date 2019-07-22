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
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_LEGENDRE_PEANO_FILE_FORMAT_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_LEGENDRE_PEANO_FILE_FORMAT_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/Slicer.h"

#include "tarch/plotter/griddata/blockstructured/PeanoPatchFileWriter.h"

namespace exahype {
  namespace plotters {
    class ADERDG2LegendrePeanoPatchFileFormat;

    class ADERDG2LegendreCellsPeanoFileFormatAscii;
    class ADERDG2LegendreVerticesPeanoFileFormatAscii;
    class ADERDG2LegendreCellsPeanoFileFormatHDF5;
    class ADERDG2LegendreVerticesPeanoFileFormatHDF5;

    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
  }
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat: public exahype::plotters::Plotter::Device {
 protected:
   enum class PlotterType {
     Text,
     /**
      * Strange uppercase/lowercase convention exists as we do have -DHDF5 as macro.
      */
     Hdf5
   };
 private:
  int           _fileCounter;
  const bool    _plotCells;
  const PlotterType _plotType;
  std::string   _filename;
  int           _order;
  int           _solverUnknowns;
  int           _writtenUnknowns;
  exahype::parser::ParserView   _plotterParameters;

  /**
   * To memorise the time argument from startPlotter(). We need it when we close the plotter for the time series.
   */
  double _time;

  exahype::plotters::Slicer *slicer;
  static tarch::logging::Log _log;

  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellDataWriter;
  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexTimeStampDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellTimeStampDataWriter;

  tarch::plotter::griddata::blockstructured::PeanoPatchFileWriter* _writer;

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

  std::vector<double> getMapping() const;
 public:
  ADERDG2LegendrePeanoPatchFileFormat(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, bool plotCells, PlotterType type);
  virtual ~ADERDG2LegendrePeanoPatchFileFormat();

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


class exahype::plotters::ADERDG2LegendreVerticesPeanoFileFormatAscii: public exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreVerticesPeanoFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreCellsPeanoFileFormatAscii: public exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreCellsPeanoFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreVerticesPeanoFileFormatHDF5: public exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreVerticesPeanoFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreCellsPeanoFileFormatHDF5: public exahype::plotters::ADERDG2LegendrePeanoPatchFileFormat {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreCellsPeanoFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};

#endif
