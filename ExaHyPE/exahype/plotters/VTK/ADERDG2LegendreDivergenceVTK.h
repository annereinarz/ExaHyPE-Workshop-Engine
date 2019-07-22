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
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_LEGENDRE_DIVERGENCE_VTK_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_LEGENDRE_DIVERGENCE_VTK_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/Slicer.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/unstructured/UnstructuredGridWriter.h"
#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

namespace exahype {
  namespace plotters {
    class ADERDG2LegendreDivergenceVTK;

    class ADERDG2LegendreDivergenceVerticesVTKAscii;
    class ADERDG2LegendreDivergenceVerticesVTKBinary;

    class ADERDG2LegendreDivergenceVerticesVTUAscii;
    class ADERDG2LegendreDivergenceVerticesVTUBinary;
  }
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::ADERDG2LegendreDivergenceVTK: public exahype::plotters::Plotter::Device {
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
  
  static tarch::logging::Log _log;
  exahype::plotters::Slicer *slicer;

//  tarch::la::Vector<DIMENSIONS, double>  _regionOfInterestLeftBottomFront;
//  tarch::la::Vector<DIMENSIONS, double>  _regionOfInterestRightTopBack;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter*                    _gridWriter;

  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::VertexWriter*              _vertexWriter;
  tarch::plotter::griddata::unstructured::UnstructuredGridWriter::CellWriter*                _cellWriter;

  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexTimeStampDataWriter;
  tarch::plotter::griddata::Writer::VertexDataWriter*  _vertexDataWriter;

  void writeTimeStampDataToPatch( double timeStamp, int vertexIndex, int cellIndex );

  void plotVertexData(
    int firstVertexIndex,
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp
  );

  std::pair<int,int> plotLegendrePatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch
  );
 public:
  ADERDG2LegendreDivergenceVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, PlotterType isBinary);
  virtual ~ADERDG2LegendreDivergenceVTK();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};


class exahype::plotters::ADERDG2LegendreDivergenceVerticesVTKAscii: public exahype::plotters::ADERDG2LegendreDivergenceVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreDivergenceVerticesVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreDivergenceVerticesVTKBinary: public exahype::plotters::ADERDG2LegendreDivergenceVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreDivergenceVerticesVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreDivergenceVerticesVTUAscii: public exahype::plotters::ADERDG2LegendreDivergenceVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreDivergenceVerticesVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


class exahype::plotters::ADERDG2LegendreDivergenceVerticesVTUBinary: public exahype::plotters::ADERDG2LegendreDivergenceVTK {
  public:
    static std::string getIdentifier();
    ADERDG2LegendreDivergenceVerticesVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
};


#endif
