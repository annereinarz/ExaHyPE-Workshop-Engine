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
 * 
 * @authors: Sven Koeppel
 **/
 
#ifndef _EXAHYPE_PLOTTERS_FV_2_CARPET_H_
#define _EXAHYPE_PLOTTERS_FV_2_CARPET_H_

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/CartesianSlicer.h"
#include "exahype/plotters/Carpet/CarpetWriter.h"

namespace exahype {
  namespace plotters {
    class FiniteVolume2Carpet;
    class FiniteVolume2CarpetHDF5;
    class FiniteVolume2CarpetASCII;
    class CarpetWriter;
  }
}

namespace kernels {
  class index; // instead #include "kernels/KernelUtils.h"
}

/**
 * <h2>Writing Carpet files from FiniteVolume solvers</h2>
 * 
 * This plotter is a hack. It tries to map the finite volume solutions onto the Carpet
 * finite differencing data representation. Currently, the mapping is poor but at least
 * there are data.
 * 
 * In detail, the "problematic" thing is that we map cell-average data to nodal vertex data.
 * This interpolation is known to be poor on a coarse resolution. This decision is made to
 * allow for plotting components which have no gaps. Of course we could define the
 * components in a shifted way that this interpolation is not neccessary, but then
 * plotting artifacts might be visible because the components no more touch.
 * 
 **/
class exahype::plotters::FiniteVolume2Carpet : public exahype::plotters::Plotter::Device {
 public:
  CarpetWriter* writer;
  const int ghostLayerWidth;
  exahype::plotters::CarpetWriter::FileFormat format;

  // set at init(...) time
  int numberOfCellsPerAxis; //< what is called nodesPerCoordinatesAxis in the (FV) Solver
  int numberOfVerticesPerAxis;
  int solverUnknowns;

  /// See parent class for documentation
  FiniteVolume2Carpet(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth, exahype::plotters::CarpetWriter::FileFormat format);

  virtual ~FiniteVolume2Carpet();

  virtual void init(const std::string& filename, int basisSize, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView  plotterParameters);

  virtual void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo);
  
  virtual void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);
  
  virtual void startPlotting( double time );
  virtual void finishPlotting();

  // TODO: These FV interpolating routines should be in some generic library
  void interpolateVertexPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double *u,
    double *mappedCell,
    double timeStamp
  );

  void interpolateCartesianSlicedVertexPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double *u,
    double *mappedCell,
    double timeStamp,
    const exahype::plotters::CartesianSlicer& slicer
  );
  
  void interpolateFVCellAtPoint(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& interpolation_point,
    const tarch::la::Vector<DIMENSIONS, int>& cell_index,
    double *u, // solution vector
    double* vertexValue,
    double* outputWrittenQuantities,
    double timeStamp
  );
};


class exahype::plotters::FiniteVolume2CarpetHDF5 : public exahype::plotters::FiniteVolume2Carpet {
  public:
    static std::string getIdentifier();
    FiniteVolume2CarpetHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth)
       : FiniteVolume2Carpet(postProcessing, ghostLayerWidth, exahype::plotters::CarpetWriter::FileFormat::FileFormatHDF5) {}
};

class exahype::plotters::FiniteVolume2CarpetASCII : public exahype::plotters::FiniteVolume2Carpet {
  public:
    static std::string getIdentifier();
    FiniteVolume2CarpetASCII(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth)
       : FiniteVolume2Carpet(postProcessing, ghostLayerWidth, exahype::plotters::CarpetWriter::FileFormat::FileFormatASCII) {}
};

#endif/* _EXAHYPE_PLOTTERS_FiniteVolume_2_CARPET_H_ */
