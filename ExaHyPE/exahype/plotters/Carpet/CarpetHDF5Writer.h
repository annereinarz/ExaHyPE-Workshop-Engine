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

#ifndef _EXAHYPE_PLOTTERS_CARPET_HDF5_WRITER_
#define _EXAHYPE_PLOTTERS_CARPET_HDF5_WRITER_

#include "exahype/plotters/Carpet/CarpetWriter.h"
#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/CartesianSlicer.h"
#include "kernels/KernelUtils.h" // idx::kernels

namespace exahype {
  namespace plotters {
    class CarpetHDF5Writer;

    // internal classes for CarpetHDF5Writer.cpp
    class CarpetHDF5FileSet;
    class CarpetHDF5File;
    class CarpetHDF5MultipleFiles;
  }
}

// Forward declaration for H5Cpp.h
namespace H5 {
  class H5File;
  class DataSpace;
}

/**
 * <h2>Writing CarpetHDF5 files which are compatible to Cactus/EinsteinToolkit</h2>
 * 
 * This writer produces files similar to the CarpetHDF5 file format. This file format is
 * produced by the https://www.carpetcode.org/ Carpet code for http://cactuscode.org/ Cactus
 * by the http://einsteintoolkit.org/ Einsteintoolkit.
 *
 * By using this writer, users can use their Cactus postprocessing tools seamless with ExaHyPE
 * output without even knowing. There are also readers included into Visit and Amira for
 * opening the CarpetHDF5 file format.
 *
 * <h3>Output format variants</h3>
 * 
 * The writer has two major flags to trigger the way how H5 files are produced:
 * 
 * <strong> oneFilePerTimestep </strong> mimics the way how ExaHyPE produces VTK files: There is
 * always (at least) one file per timestep. This is <em>not</em> the way how the CarpetHDF5 file
 * format works and you will have to join the HDF5 files with tools like h5join (shipped with
 * Carpet/Cactus) in order to read these files with ordinary writers. Nevertheless this is helpful
 * for debugging or immediately opening single timestep snapshots in Visit.
 * 
 * <strong> allUnknownsInOneFile </strong> allows you to reduce the number of H5 files by putting
 * the (vector of) all written unknowns in a single file. This also mimics how ExaHyPE's VTK files
 * look like. In contrast, in Cactus one typically has one hdf5 file per group/per physical field.
 * This allows easily to copy only the interesting fields from a supercomputer.
 *
 * <h3>General limitations</h3>
 * 
 * <strong> MPI support not yet completed </strong>. We plan two modes:
 *   (1) Standard approach: Each MPI rank produces its own file
 *   (2) One file is created accross all ranks (HDF5 should provide easy methods to do so).
 * In any way, you find tools at Miscellaneous/CarpetHDF5-Utils to merge several files to a single
 * one, distribute data, or as you whish.
 *
 * <strong> Structure of array vs. array of structures </strong>
 * The CarpetHDF5 file format directly resembles the closest way how to dump Cactus memory into files.
 * Cactus stores structures of arrays, for instance the four hydro variables {rho,velx,vely,velz,eps}
 * are stored as four big arrays in Cactus. In contrast, ExaHyPE stores arrays of structures, hence
 * in principle one big array where at each point there are the four variables. For this very reason,
 * producing CarpetHDF5 files comes with a lot of overhead, ie. a needless amount of over and over
 * repeated metadata and way too much components.
 *
 * By no means this is the fault of the HDF5 binary table format but really the way how CarpetHDF5
 * works or can be "maximally streched" to also fit ExaHyPE in.
 * 
 * <strong>Always vertex-centered data</strong>
 * As the Finite Differencing code Cactus uses by default vertex-centered data and stores them as
 * such, the data layout only supports vertex data and not cell-centered data. Maybe the actual
 * CarpetHDF5 format supports also cell-centered representation.
 * 
 * <h3>A note about time</h3>
 * Since there is no global timestep counter in ExaHyPE (in contrast to Cactus, where the steps
 * on the finest level are counted), the timesteps are just counting the plotting invocations
 * (the "plotting steps", similar as they are counted in the VTK plotters, for instance). That
 * means the number has no physical meaning (there is no equation time = dt * timestep)
 * 
 * <h3>How to build the CarpetHDF5 plotters into your release</h3>
 * 
 * By default, the HDF5 plotters are excluded from compiling. If you enable it in your spec file, the code
 * will stop at startup, throwing a message that this plotter is not supported.
 * 
 * In order to use these plotters, define "HDF5" and provide the HDF5 serial C++ header path and
 * libraries. This can be done by adding to your project
 *
    PROJECT_CFLAGS+=-DHDF5
    PROJECT_CFLAGS+=-I/usr/include/hdf5/serial -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE -Wdate-time -D_FORTIFY_SOURCE=2 -fstack-protector-strong
    PROJECT_LFLAGS+=-L/usr/lib/x86_64-linux-gnu/hdf5/serial -lhdf5 -l hdf5_cpp /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_hl.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.a
    
 * This snippet works on a standard Ubuntu system with hdf5 developer libraries installed.
 * I extracted them from "h5cc -show" and "pkg-config --libs hdf5"
 * 
 * <h3>Connection to other plotters in ExaHyPE</h3>
 *
 * Actually this plotter is quite similar to the CartesianVTK format but just with regular
 * block patches. It is also similar to the new PeanoPatchFileFormat which was developed
 * in the same time.
 *
 * Thanks to Roland Haas for support in Munich and Frankfurt when coming up with the file format.
 *
 * @author Sven Köppel
 *
 **/
class exahype::plotters::CarpetHDF5Writer : public exahype::plotters::CarpetWriter {
  typedef tarch::la::Vector<DIMENSIONS, double> dvec;
  tarch::logging::Log _log;

public:

  // HDF5 specific data types
  std::vector<H5::H5File*> files; ///< List of pointers to H5Files. Has length 1 if allUnknownsInOneFile.
  H5::DataSpace*      patch_space; ///< DataSpaces describing a component/patch: basisSize^D elements.
  H5::DataSpace*      dtuple; ///< DataSpace describing a dim-dimensional tuple, ie dim numbers.

  CarpetHDF5Writer(const std::string& _filename, int _basisSize, int _solverUnknowns, int _writtenUnknowns, exahype::parser::ParserView _plotterParameters,
		   char** writtenQuantitiesNames);
  
  virtual ~CarpetHDF5Writer();

  void writeBasicGroup(H5::H5File* file, int writtenUnknown=-1);
  
  void openFile() override; // was openH5
  void flushFile() override; // was flushH5
  void closeFile() override; // was closeH5

  void startPlotting(double time) override;
  void finishPlotting() override;


  // Default values for limiterStatus for plotPatch* functions, used for instance from
  // a pure FV solver which has no limiter status flag.
  constexpr static int nonLimitingLimiterStatus = -1;

  /**
   * This is 2D and 3D, allows several unknowns, named fields and all that.
   * 
   * Possible problem: Local timestepping / each patch *could* have its own time.
   * Then the whole plotting approach of CarpetHDF5 fails and we have to collect
   * cells belonging to the same time somehow. Or we have to keep track of the
   * "iteration" number.
   **/
  void plotPatch(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp,
      int limiterStatus=nonLimitingLimiterStatus) override;
  
  void plotPatchForSingleUnknown(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus,
      int writtenUnknown, H5::H5File* target);

}; // class ADERDG2CarpetHDF5Impl


#endif /* _EXAHYPE_PLOTTERS_CARPET_HDF5_WRITER_ */
