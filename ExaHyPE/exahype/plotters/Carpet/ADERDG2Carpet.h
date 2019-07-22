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
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_CARPETHDF5_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_CARPETHDF5_H_

#include "exahype/plotters/Plotter.h"
#include "CarpetWriter.h"

namespace exahype {
  namespace plotters {
    class ADERDG2CarpetHDF5;
    class ADERDG2CarpetASCII;
    class ADERDG2Carpet;

    class CarpetHDF5Writer;
    class CartesianSlicer; // forward decl
  }
}

namespace kernels {
  class index; // instead #include "kernels/KernelUtils.h"
}

/**
 * <h2>Projecting ADERDG data onto the Carpet file format</h2>
 * 
 * This plotter is very similar to the CartesianVTK format, it projects the ADERDG DOF
 * onto a regular grid which is written out using the CarpetWriter (either HDF5 or ASCII).
 * 
 */
class exahype::plotters::ADERDG2Carpet : public exahype::plotters::Plotter::Device {
 public:
  typedef tarch::la::Vector<DIMENSIONS, double> dvec;

  CarpetWriter* writer;
  exahype::plotters::CarpetWriter::FileFormat format;
  static tarch::logging::Log _log;

  ADERDG2Carpet(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::plotters::CarpetWriter::FileFormat format);

  virtual ~ADERDG2Carpet();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  virtual void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo);
  
  virtual void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp, int limiterStatus);
  
  virtual void startPlotting( double time );
  virtual void finishPlotting();

  // TODO: These ADER interpolating routines should be in some generic library
  void interpolateCartesianPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    double *u,
    double *mappedCell,
    double timeStamp,
    int limiterStatus
  );

  void interpolateCartesianSlicedPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    double *u,
    double *mappedCell,
    double timeStamp,
    int limiterStatus,
    const exahype::plotters::CartesianSlicer& slicer
  );
};

class exahype::plotters::ADERDG2CarpetHDF5 : public exahype::plotters::ADERDG2Carpet {
  public:
    static std::string getIdentifier();
    ADERDG2CarpetHDF5(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing)
       : ADERDG2Carpet(postProcessing, exahype::plotters::CarpetWriter::FileFormat::FileFormatHDF5) {}
};

class exahype::plotters::ADERDG2CarpetASCII : public exahype::plotters::ADERDG2Carpet {
  public:
    static std::string getIdentifier();
    ADERDG2CarpetASCII(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing)
       : ADERDG2Carpet(postProcessing, exahype::plotters::CarpetWriter::FileFormat::FileFormatASCII) {}
};

#endif/* _EXAHYPE_PLOTTERS_ADERDG_2_CARPETHDF5_H_ */
