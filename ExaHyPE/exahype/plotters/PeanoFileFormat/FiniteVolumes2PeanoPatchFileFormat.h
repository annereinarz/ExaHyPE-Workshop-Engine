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
 
#ifndef _EXAHYPE_PLOTTERS_FINITE_VOLUMES_2_PEANO_PATH_FILE_FORMAT_H_
#define _EXAHYPE_PLOTTERS_FINITE_VOLUMES_2_PEANO_PATH_FILE_FORMAT_H_

#include "exahype/plotters/Plotter.h"

#include "tarch/plotter/griddata/blockstructured/PeanoPatchFileWriter.h"

namespace exahype {
  namespace plotters {
    class FiniteVolumes2PeanoPatchFileFormat;

    class FiniteVolumes2PeanoPatchFileFormatAscii;
    class FiniteVolumes2PeanoPatchFileFormatHDF5;
  }
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::FiniteVolumes2PeanoPatchFileFormat: public exahype::plotters::Plotter::Device {
 protected:
   enum class PlotterType {
     Text,
     /**
      * Strange uppercase/lowercase convention exists as we do have -DHDF5 as macro.
      */
     Hdf5
   };
 private:
  static tarch::logging::Log _log;

  int           _fileCounter;
  const PlotterType _plotterType;
  std::string   _filename;
  int           _numberOfCellsPerAxis;
  int           _ghostLayerWidth;
  int           _solverUnknowns;
  int           _writtenUnknowns;
  exahype::parser::ParserView   _plotterParameters;


  /**
   * To memorise the time argument from startPlotter(). We need it when we close the plotter for the time series.
   */
  double _time;


  tarch::la::Vector<DIMENSIONS, double>  _regionOfInterestLeftBottomFront;
  tarch::la::Vector<DIMENSIONS, double>  _regionOfInterestRightTopBack;

  exahype::plotters::Slicer *slicer;

  tarch::plotter::griddata::Writer::CellDataWriter*    _cellDataWriter;
  tarch::plotter::griddata::Writer::CellDataWriter*    _cellTimeStampDataWriter;

  tarch::plotter::griddata::blockstructured::PeanoPatchFileWriter* _writer;

  void plotCellData(
    int firstCellIndex,
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp
  );
 public:
  FiniteVolumes2PeanoPatchFileFormat(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth, PlotterType plotterType);
  virtual ~FiniteVolumes2PeanoPatchFileFormat();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns,exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::FiniteVolumes2PeanoPatchFileFormatAscii: public exahype::plotters::FiniteVolumes2PeanoPatchFileFormat {
  public:
    static std::string getIdentifier();
    FiniteVolumes2PeanoPatchFileFormatAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);
};


class exahype::plotters::FiniteVolumes2PeanoPatchFileFormatHDF5: public exahype::plotters::FiniteVolumes2PeanoPatchFileFormat {
  public:
    static std::string getIdentifier();
    FiniteVolumes2PeanoPatchFileFormatHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,const int ghostLayerWidth);
};



#endif
