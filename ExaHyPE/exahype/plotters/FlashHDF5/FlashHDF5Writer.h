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

#ifndef _EXAHYPE_PLOTTERS_FLASH_HDF5_WRITER_
#define _EXAHYPE_PLOTTERS_FLASH_HDF5_WRITER_

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/CartesianSlicer.h"
#include "kernels/KernelUtils.h" // idx::kernels

namespace exahype {
  namespace plotters {
    class FlashHDF5Writer;
  }
}

#ifdef HDF5 // Only if H5 is present

// HDF5 library, only available if HDF5 is on the path
#include "H5Cpp.h"

/**
 * <h2>A compact HDF5 file format inspired by the FLASH code output format</h2>
 * 
 * For a description, see my poster from last week. For the time being, code
 * is just ripped of the CarpetHDF5 implementation in ExaHyPE.
 * 
 * That is, the only difference to CarpetHDF5 or the PeanoHDF5 is that we reduce
 * the number of HDF5 objects in favour of big tables which are however
 * extendible datasets and thus we heavily rely on chunking.
 * 
 * The probable advantage of this is to reduce the read-in times when using large
 * number of cells/patches. Another advantage is the idea to exploit filters aka
 * compression (loseless or lossy). Filters <i>seem</i> to work only on single
 * datasets. This could especially perform great on the grid information (cell
 * positions and coordinates) and of course also on the payload.
 *
 * This file format could also be advanteous when using again an Array of structures
 * by means of storing individual fields (number of written unknowns = 1) because
 * then the datasets are still large compared to both CarpetHDF5 and the PeanoHDF5
 * file format. In practise, as users want to look only at one field and not many 
 * of them in parallel, such an AoS approach is quite attractive.
 *
 * @author Sven Köppel
 *
 **/
class exahype::plotters::FlashHDF5Writer {
  typedef tarch::la::Vector<DIMENSIONS, double> dvec;
  tarch::logging::Log _log;

public:
  // information from the device::init() process
  const int           solverUnknowns; ///< The number of unknowns in the Solver (ie. number of PDEs)
  const int           writtenUnknowns; ///< The number of written out quantities.
  const std::string   basisFilename; ///< The filename prefix as it is common in ExaHyPE plotters
  const int           basisSize; ///< this is _orderPlusOne in ADERDG context and _numberOfCellsPerAxis-2*ghostZones in FV context
  exahype::parser::ParserView  plotterParameters; ///< A plotterParametersion string for passing further parameters throught the ExaHyPE specification file.

  const bool          oneFilePerTimestep; ///< Constant deciding whether to write one file (or series of files) per timestep
  const bool          allUnknownsInOneFile; ///< Constant deciding whether all unknowns should go into a single file or split files instead.

  // set up during construction: Dimensional reduction
  int                 dim; ///< Dimension of the output generated. Do not change this. Setup by constructor.
  exahype::plotters::Slicer   *slicer; ///< Subslice, if present. Otherwise nullptr.
  kernels::index     *patchCellIdx; ///< Regular patch indexer as in ExaHyPE
  kernels::index     *writtenCellIdx; ///< Index of a whole cell as in ExaHyPE
  // THIS IS NO MORE PRESENT as we ALWAYS write whole written patches as all ExahyPE code does (AoS vs SoA)
  // kernels::index     *singleFieldIdx; ///< index of a whole component as in Carpet: Only one value per point
  int                 patchFieldsSize;  ///< as a service: basisSize^DIMENSIONS * writtenUnkowns, ie. the written without dimensional reduction
  int                 writtenFieldsSize; ///< basisSize^dim * writtenUnknowns, ie the really written incl. dimensional reduction
  //int                 singleFieldSize; ///< just basisSize^DIM

  // Things to be counted by this instance
  int                 component; ///< An internal counter of the components (=patches) written out in one plot cycle
  int                 iteration; ///< An internal counter of the number of plot cycle runned. It is kind of global.
  char**              writtenQuantitiesNames; // not const as we check for good names in constructor

  // HDF5 specific data types
  std::vector<H5::H5File*> files; ///< List of pointers to H5Files. Has length 1 if allUnknownsInOneFile.
  H5::DataSpace       dtuple; ///< DataSpace describing a dim-dimensional tuple, ie dim numbers.
  
  /**
   * Wraps a DataSet (and DataSpace) which can grow unlimited in *one*
   * direction. This direction has to be given with the "unlimited"
   * number, indicating the unlimited dimension.
   * 
   * We could also adopt this idea and extend also in a second direction,
   * for the time. However, for true AMR this is not a good idea if the
   * number of cells in each timestep varies (very much).
   * 
   **/
  struct ExtendableDataSet {
	typedef hsize_t hsize;
	typedef std::vector<hsize> sizes;
	tarch::logging::Log _log;
	
	/// Returns a copy of v = { val, v[0], v[1], ...  }, leaves v unchanged.
	sizes static copy_front(sizes v, hsize val) { v.insert(v.begin(), val); return v; }
	
	const sizes basic_shape; ///< The actual shape of the data, for instance (a,b,c)
	const sizes initial_dims; ///< The initial dimensions of the  dataspace, for instance (inc,a,b,c)
	const sizes initial_maxdims; ///< the maximum dimensions of the datapsace, for instance (UNLIMITED,a,b,c)
	const sizes chunk_size; ///< The size of an individual chunk
	const H5::DataSpace   initial_dataspace; ///< The space with which a DataSet is started
	const int increase; ///< The number of blocks to add when we have to increase. You want a large number.

	/*const*/ H5::DSetCreatPropList prop; // holding chunk size information
	
	H5::DataSet    *dataset;
	int capacity; ///< The number of blocks which are already allocated
	int size; ///< The current number of blocks
  private:
	void extend(int new_capacity);
  public:
	/// Increase: Initial number of patches to reserve.
	ExtendableDataSet(const sizes& shape, hsize increase);
	
	template <typename H5GroupOrFile>
	void newDataSet(H5GroupOrFile* loc, const std::string& name, const H5::DataType &data_type);
	void write(const H5::DataType &mem_type, const void *buf);
	void shrink_to_fit() { extend(size); }
	void closeDataSet() { delete dataset; dataset=nullptr; size=0; capacity=0; }
	std::string toString() const;
  };

  H5::Group           *timegroup;
  H5::Attribute       num_components;
  ExtendableDataSet   *fields, *origin, *delta;

  /**
   * cf. also the documentation in the ADERDG2FlashHDF5.h
   * 
   * oneFilePerTimestep: You might want to have this to view the result during computation
   *     as HDF5 is very lazy at writing. Note that writing out data this form is not compilant
   *     with most FlashHDF5 readers (ie. the visit reader). You must join seperate HDF5 files afterwards
   *     manually.
   * 
   * allUnknownsInOneFile: Write different fields in a single H5 combined file. Typically for Cactus
   *     as structure of arrays is to write each unknown in its own file (ie. one file per physical field).
   *
   **/
  FlashHDF5Writer(const std::string& _filename, int _basisSize, int _solverUnknowns, int _writtenUnknowns, exahype::parser::ParserView _plotterParameters,
		   char** writtenQuantitiesNames, bool oneFilePerTimestep_=false, bool allUnknownsInOneFile_=false);

  void setupFile(H5::H5File* file);
  
  void openH5(); ///< Opens or switchs the currently active H5 file or the list of H5 files. Closes if neccessary.
  void flushH5(); ///< Flushs all HDF5 file output buffers. Always flushs before.
  void closeH5(); ///< Closes all HDF5 files. Closes, deletes and nulls the H5 objects.

  void startPlotting(double time);
  void finishPlotting();

  /**
   * This is 2D and 3D, allows several unknowns, named fields and all that.
   * 
   * Possible problem: Local timestepping / each patch *could* have its own time.
   * Then the whole plotting approach of FlashHDF5 fails and we have to collect
   * cells belonging to the same time somehow. Or we have to keep track of the
   * "iteration" number.
   **/
  void plotPatch(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp);

}; // class ADERDG2FlashHDF5Impl




#endif /* H5 */
#endif /* _EXAHYPE_PLOTTERS_FLASH_HDF5_WRITER_ */
