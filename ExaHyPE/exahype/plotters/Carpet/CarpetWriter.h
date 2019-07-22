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

#ifndef _EXAHYPE_PLOTTERS_CARPET_ABSTRACT_WRITER_
#define _EXAHYPE_PLOTTERS_CARPET_ABSTRACT_WRITER_

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/CartesianSlicer.h"
#include "kernels/KernelUtils.h" // idx::kernels
#include "tarch/la/Vector.h"

namespace exahype {
  namespace plotters {
    class CarpetWriter;
  }
}

/**
 * <h2>Writing output files which are compatible to the ones produced by Cactus/EinsteinToolkit/Carpet</h2>
 * 
 * This is an abstract writer. See CarpetHDF5Writer for details about the well-known
 * HDF5-based file format format, while CarpetASCIIWriter resembles the CSV format.
 * 
 * In general, the H5 format was implemented first and has the more verbose documentation.
 * 
 * @author Sven Köppel
 *
 **/
class exahype::plotters::CarpetWriter {
public:
  typedef tarch::la::Vector<DIMENSIONS, double> dvec;
  typedef tarch::la::Vector<DIMENSIONS, bool> boolvec;
  typedef tarch::la::Vector<DIMENSIONS, int> ivec;

  enum class FileFormat { FileFormatHDF5, FileFormatASCII }; // mind that "HDF5" is a macro (-DHDF5) in ExaHyPE

  // information from the device::init() process
  const int           solverUnknowns; ///< The number of unknowns in the Solver (ie. number of PDEs)
  const int           writtenUnknowns; ///< The number of written out quantities.
  const std::string   basisFilename; ///< The filename prefix as it is common in ExaHyPE plotters
  const int           basisSize; ///< this is _orderPlusOne in ADERDG context and _numberOfCellsPerAxis-2*ghostZones in FV context
  const exahype::parser::ParserView plotterParameters; ///< A plotterParametersion string for passing further parameters throught the ExaHyPE specification file.

  const bool          oneFilePerTimestep; ///< Constant deciding whether to write one file (or series of files) per timestep
  const bool          allUnknownsInOneFile; ///< Constant deciding whether all unknowns should go into a single file or split files instead.

  // set up during construction: Dimensional reduction
  int                 dim; ///< Dimension of the output generated. Do not change this. Setup by constructor.
  exahype::plotters::Slicer   *slicer; ///< Subslice, if present. Otherwise nullptr.
  kernels::index     *patchCellIdx; ///< Regular patch indexer as in ExaHyPE
  kernels::index     *writtenCellIdx; ///< Index of a whole cell as in ExaHyPE
  kernels::index     *singleFieldIdx; ///< index of a whole component as in Carpet: Only one value per point
  int                 patchFieldsSize;  ///< as a service: basisSize^DIMENSIONS * writtenUnkowns, ie. the written without dimensional reduction
  int                 writtenFieldsSize; ///< basisSize^dim * writtenUnknowns, ie the really written incl. dimensional reduction
  int                 singleFieldSize; ///< just basisSize^DIM
  std::string         dimextension; ///< A file extension reflecting good Cactus standards, like "xyz", "xy" or "x" before ".h5"

  // Things to be counted by this instance
  int                 component; ///< An internal counter of the components (=patches) written out in one plot cycle
  int                 iteration; ///< An internal counter of the number of plot cycle runned. It is kind of global.
  char**              writtenQuantitiesNames; // not const as we check for good names in constructor
  std::vector<std::string> qualifiedWrittenQuantitiesNames; // in CarpetHDF5, the field name *must* contain a "::"; ///< The same as writtenQuantitiesNames but with prefix

  /**
   * Construct a new CarpetWriter.
   * 
   * @arg _filename   Basis filename template to use for constructing output
   *                  filenames. This should be without extension or slicing
   *                  information (suffix like "it4-rank3.xyz.h5" will be added).
   * @arg _basisSize  The number of nodes to write. Note, the CarpetWriter is agnostic
   *                  for the underlying scheme. It always writes vertex data.
   * @arg _solverUnknowns  State vector length in the solver (stored for setting up the correct striping)
   * @arg _writtenUnknowns Output vector length
   * @arg _plotterParameters  View on the specfile, allows for runtime parameter estimation
   * @arg writtenQuantitiesNames   Can be provided by postProcessing
   * 
   * The following settings are extracted from the plotterParameters:
   * 
   * oneFilePerTimestep: You might want to have this to view the result during computation
   *     as HDF5 is very lazy at writing. Note that writing out data this form is not compilant
   *     with most CarpetHDF5 readers (ie. the visit reader). You must join seperate files afterwards
   *     manually.
   * 
   * allUnknownsInOneFile: Write different fields in a single combined file. Typically for Cactus
   *     as structure of arrays is to write each unknown in its own file (ie. one file per physical field).
   *
   * For most comments, see also the documentation in the ADERDG2Carpet.h. Note that
   * the ASCII writer was written after the HDF5 writer and is more a hack.
   **/
  CarpetWriter(const std::string& _filename, int _basisSize, int _solverUnknowns, int _writtenUnknowns, exahype::parser::ParserView _plotterParameters, char** writtenQuantitiesNames);

  /**
   * A helper method as class factory.
   * Will return a new CarpetWriterASCII or CarpetWriterHDF5 instance, based on the value of the given FileFormat.
   **/
  static CarpetWriter* newCarpetWriterFor(FileFormat format,
	const std::string& _filename, int _basisSize, int _solverUnknowns, int _writtenUnknowns, exahype::parser::ParserView _plotterParameters, char** writtenQuantitiesNames);

  virtual ~CarpetWriter();

  virtual void openFile() = 0; ///< Opens or switchs the currently active file or the list of files. Closes if neccessary.
  virtual void flushFile() = 0; ///< Flushs all file output buffers. Always flushs before.
  virtual void closeFile() = 0; ///< Closes all files. Closes, deletes and nulls the file objects.

  virtual void startPlotting(double time) = 0;
  virtual void finishPlotting() = 0;


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
  virtual void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& dx,
      double* mappedCell, double timeStamp, int limiterStatus=nonLimitingLimiterStatus) = 0;
}; // class

#endif /* _EXAHYPE_PLOTTERS_CARPET_ABSTRACT_WRITER_ */
