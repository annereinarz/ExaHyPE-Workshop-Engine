// If HDF5 support is not enabled, there will be no implementation of FlashHDF5Writer
// in the compiled binary.
#ifdef HDF5

#include "exahype/plotters/FlashHDF5/FlashHDF5Writer.h"
#include "peano/utils/Loop.h" // dfor
#include <sstream>
#include <iterator> // ostream_iterator
#include <stdexcept>

typedef tarch::la::Vector<DIMENSIONS, double> dvec;
typedef tarch::la::Vector<DIMENSIONS, bool> boolvec;
typedef tarch::la::Vector<DIMENSIONS, int> ivec;

using namespace H5;

// my small C++11 to_string-independent workaround.
template <typename T> std::string toString( T Number ) {
	std::ostringstream ss; ss << Number; return ss.str();
}

// Printing any std::vector
// Source: https://stackoverflow.com/a/10758845
template <typename T>
std::ostream& operator<< (std::ostream& out, const std::vector<T>& v) {
  if ( !v.empty() ) {
    out << '[';
    std::copy (v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
    out << "\b\b]";
  }
  return out;
}

// small helper
void writeStringAttribute(H5Object* o, std::string key, std::string value) {
	StrType t_str = H5::StrType(H5::PredType::C_S1, value.size()+1);
	Attribute at = o->createAttribute(key.c_str(), t_str, H5S_SCALAR);
	at.write(t_str, value.c_str());
}


exahype::plotters::FlashHDF5Writer::ExtendableDataSet::ExtendableDataSet(const sizes& _basic_shape, hsize_t _increase)
	:
	_log("FlashHDF5Writer::ExtendableDataSet"), // just for debugging!
	basic_shape(_basic_shape),
	initial_dims(copy_front(basic_shape, _increase)),
	initial_maxdims(copy_front(basic_shape, H5S_UNLIMITED)),
	chunk_size(copy_front(basic_shape, 1)), // chunk size is exactly size of actual data
	initial_dataspace(initial_dims.size(), initial_dims.data(), initial_maxdims.data()),
	increase(_increase),
	dataset(nullptr),
	capacity(0),
	size(0)
	{
	
	prop.setChunk(chunk_size.size(), chunk_size.data());
	
	// the DSetCreatPropList is also the place to go for turning on compression.
}

std::string exahype::plotters::FlashHDF5Writer::ExtendableDataSet::toString() const {
	std::stringstream s;
	s << "ExtendableDataSet:\n";
	s << "  Initial DataSpace: ( rank=" <<  initial_dims.size() << ", dims=" << initial_dims << ", maxdims=" << initial_maxdims << ")\n";
	s << "  Size of Chunks:    (ndims=" << chunk_size.size() << ",    dim =" << chunk_size <<  ")\n";
	s << "  capacity = " << capacity << ", size = " << size << ", increase = " << increase << "\n";
	return s.str();
}

/* In principle, I would like to pass a  H5::Location* pointer (HDF 1.10), however the common interface
 * is called H5::CommonFG* in HDF 1.8 (https://support.hdfgroup.org/HDF5/doc1.8/cpplus_RM/class_h5_1_1_common_f_g.html).
 * To obtain compatibility, I stick to a template here.
 */
template <typename H5GroupOrFile>
void exahype::plotters::FlashHDF5Writer::ExtendableDataSet::newDataSet(H5GroupOrFile* location, const std::string& name, const H5::DataType &data_type) {
	if(dataset == nullptr && size == 0 && capacity == 0) {
		dataset = new DataSet(location->createDataSet(name, data_type, initial_dataspace, prop));
		capacity = increase;
		size = 0;
		//return dataset;
	} else {
		throw std::domain_error("Close old Dataset before opening new one");
	}
}


void exahype::plotters::FlashHDF5Writer::ExtendableDataSet::extend(int _capacity) {
	sizes new_capacity(initial_dims);
	new_capacity[0] = _capacity;
	
	// Debugging:
	/*
	logInfo("extend", "with capacity = " << capacity << ", size = " << size << ", increase = " << increase);
	logInfo("extend", "going to new capacity = " << _capacity << " with shape " << new_capacity);
	*/
	
	dataset->extend(new_capacity.data());
	capacity = _capacity;
}

void exahype::plotters::FlashHDF5Writer::ExtendableDataSet::write(const DataType &mem_type, const void *buf) {
	if(capacity <= size) extend(size + increase);

	// Select a hyperslab in extended portion of the dataset.
	DataSpace *filespace = new DataSpace(dataset->getSpace());
	
	sizes block_offset(initial_dims.size(), 0.0); // initial offset 0
	block_offset[0] = size;
	
	// we default to block size 1, so block count is actually element size.
	sizes block_count(initial_dims);
	block_count[0] = 1; // hyperslab contains only one block/element in extending direction
	
	// Debugging
	/*
	logInfo("write", "size=" << size <<", writing block size " << block_count << " to offset " << block_offset);
	*/
	
	// Hyperslab: https://support.hdfgroup.org/HDF5/doc/cpplus_RM/class_h5_1_1_data_space.html#a3147799b3cd1e741e591175e61785854
	// op	- IN: Operation to perform on current plotterParametersion
	// count	- IN: Number of blocks included in the hyperslab
	// start	- IN: Offset of the start of hyperslab
	// stride	- IN: Hyperslab stride - default to NULL
	// block	- IN: Size of block in the hyperslab - default to NULL
	// C++: (op, count, start, stride, block)
	
	// TODO ALERT FIXME This crucial call is non working for my HDF5 version here. Probably different API?
	///////// filespace->plotterParametersHyperslab(H5S_SELECT_SET, block_count.data(), block_offset.data());
	std::abort();

	// Define memory space.
	DataSpace *memspace = new DataSpace(basic_shape.size(), basic_shape.data(), NULL);
	
	// Write data to the extended portion of the dataset.
	dataset->write(buf, mem_type, *memspace, *filespace);
	
	size++;
}


exahype::plotters::FlashHDF5Writer::FlashHDF5Writer(
	const std::string& _filename,
	int _basisSize,
	int _solverUnknowns,
	int _writtenUnknowns,
	exahype::parser::ParserView  _plotterParameters,
	char** _writtenQuantitiesNames,
	bool _oneFilePerTimestep,
	bool _allUnknownsInOneFile)
	:
	_log("ADERDG2FlashHDF5Writer"),
	solverUnknowns(_solverUnknowns),
	writtenUnknowns(_writtenUnknowns),
	basisFilename(_filename),
	basisSize(_basisSize),
	plotterParameters(_plotterParameters),
	oneFilePerTimestep(_oneFilePerTimestep),
	allUnknownsInOneFile(_allUnknownsInOneFile),
	
	// default values for init(...)-level runtime parameters:
	dim(DIMENSIONS),
	slicer(nullptr),
	component(-100),
	iteration(0),
	writtenQuantitiesNames(_writtenQuantitiesNames),
	
	// hdf5 specific data types
	files(allUnknownsInOneFile ? 1 : writtenUnknowns)
	{

	// todo at this place:  allow _oneFilePerTimestep and _allUnknownsInOneFile to be read off _plotterParameters.

	// just for convenience/a service, store an indexer for the actual ExaHyPE cells.
	switch(DIMENSIONS) { // simulation dimensions
		case 3:
		patchCellIdx = new kernels::index(basisSize, basisSize, basisSize, writtenUnknowns);
		break;
		
		case 2:
		patchCellIdx = new kernels::index(basisSize, basisSize, writtenUnknowns);
		break;
		
		default:
			throw std::domain_error("FlashHDF5Writer: I think ExaHyPE only supports 2D and 3D");
	}
	writtenCellIdx = patchCellIdx; // without slicing, this is true.

	slicer = Slicer::bestFromSelectionQuery(plotterParameters);
	if(slicer) {
		logInfo("init", "Plotting selection "<<slicer->toString()<<" to Files "<<basisFilename);
		if(slicer->getIdentifier() == "CartesianSlicer") {
			dim = static_cast<CartesianSlicer*>(slicer)->targetDim;
		}
	}
	
	typedef std::vector<hsize_t> hsizes;
	hsizes fieldShape; // using c++11 list initialization below.
	// to avoid type truncation warnings (int -> hsize):
	const hsize_t basisSize_h = static_cast<hsize_t>(basisSize), writtenUnknowns_h = static_cast<hsize_t>(writtenUnknowns);

	switch(dim) { // written dimensions
		case 3:
		writtenCellIdx = new kernels::index(basisSize, basisSize, basisSize, writtenUnknowns);
		fieldShape = { basisSize_h, basisSize_h, basisSize_h, writtenUnknowns_h };
		break;
		
		case 2:
		writtenCellIdx = new kernels::index(basisSize, basisSize, writtenUnknowns);
		fieldShape = { basisSize_h, basisSize_h, writtenUnknowns_h };
		break;
		
		case 1:
		writtenCellIdx = new kernels::index(basisSize, writtenUnknowns);
		fieldShape = { basisSize_h, writtenUnknowns_h };
		break;
		
		default:
			logError("FlashHDF5Writer", "Error, only dimensions 1, 2, 3 supported. Slicing requested: " << slicer->toString());
			throw std::domain_error("FlashHDF5Writer does not like your domain"); // har har
	}
	
	// just as shorthands
	patchFieldsSize = patchCellIdx->size;
	writtenFieldsSize = writtenCellIdx->size;
	//singleFieldSize = singleFieldIdx->size;
	
	// make sure there are reasonable names everywhere
	for(int u=0; u<writtenUnknowns; u++) {
		if(!writtenQuantitiesNames[u]) {
			std::string* replacement_name = new std::string("Q_");
			*replacement_name += toString(u);
			writtenQuantitiesNames[u] = const_cast<char*>(replacement_name->c_str());
		}
	}

	// this is the dataspace describing how to write a patch/cell/component.
	fields = new ExtendableDataSet(fieldShape, 10);
	// dataspaces describing the geometry
	origin = new ExtendableDataSet(hsizes(1, dim), 10);
	delta  = new ExtendableDataSet(hsizes(1, dim), 10);
	
	// open file(s) initially, if neccessary
	if(!oneFilePerTimestep) openH5();
	timegroup = nullptr;
	
	// for Debugging:
	logInfo("FlashHDF5DebugInfo", "dim=" << dim);
	logInfo("FlashHDF5DebugInfo", "writtenCellIdx=" << writtenCellIdx->toString());
	
	logInfo("init", "Created fields =  " << fields->toString());
	logInfo("init", "Created origin =  " << origin->toString());
	logInfo("init", "Created delta =  " << delta->toString());
}

void exahype::plotters::FlashHDF5Writer::setupFile(H5::H5File* file) {
	//table.write(componentPatch, PredType::NATIVE_DOUBLE);

	// write basic group. The toString(int) stuff is nonsense and shall be replaced
	Group* parameters = new Group(file->createGroup( "/FlashHDF5-Info" ));
	writeStringAttribute(parameters, "version", "1.0");
	writeStringAttribute(parameters, "dim", toString(dim));
	writeStringAttribute(parameters, "writtenCellIdx", writtenCellIdx->toString());
	writeStringAttribute(parameters, "DIMENSIONS", toString(DIMENSIONS));
	writeStringAttribute(parameters, "slicer", slicer ? slicer->toString() : "none/null");
	
	// write the names of the unknowns
	// TODO: Fix this, does not yet produce correct data but garbage instead
	/*
	DataSpace names_space(1, std::vector<hsize_t>(1, writtenUnknowns).data());
	DataSet names = parameters->createDataSet("FieldNames", PredType::C_S1, names_space);
	names.write(writtenQuantitiesNames, PredType::C_S1);
	*/
	
	// todos:
	// * add MPI information
	// * add information how much files are printed (oneFilePerTimestep)
	// * List all fields which go into this file.
}

/**
 * Opens or switchs the currently active H5 file or the list of H5 files.
 **/
void exahype::plotters::FlashHDF5Writer::openH5() {
	std::string local_filename, suffix, prefix, sep("-");
	prefix = basisFilename;
	suffix = (oneFilePerTimestep?(sep + "it" + toString(iteration)):"") + ".h5";
	
	closeH5(); // just to be sure
	int writtenUnknown=0;
	for(auto& file : files) {
		// @TODO: MPI Rank number should go in here.
		local_filename = prefix + (allUnknownsInOneFile ? "" : (sep + writtenQuantitiesNames[writtenUnknown])) + suffix;

		logInfo("open", "Opening File '"<< local_filename << "'");
		file = new H5File(local_filename, H5F_ACC_TRUNC);
		file->setComment("Created by ExaHyPE");
		setupFile(file);
		writtenUnknown++;
	}
}

void exahype::plotters::FlashHDF5Writer::closeH5() {
	// flush in any case, even when closing the file. Cf. http://stackoverflow.com/a/31301117
	flushH5();
	for(auto& file : files) {
		if(file) {
			file->close();
			delete file;
			file = nullptr;
		}
	}
}

void exahype::plotters::FlashHDF5Writer::flushH5() {
	for(auto& file : files) {
		if(file) {
			file->flush(H5F_SCOPE_GLOBAL);
		}
	}
}

void exahype::plotters::FlashHDF5Writer::startPlotting(double time) {
	if(oneFilePerTimestep) openH5();
	
	if(!allUnknownsInOneFile) {
		throw std::domain_error("Flash format requires all unknowns in one file");
	}
	auto file = files[0];
	
	//if(timegroup) delete timegroup; // properly closing
	timegroup = new Group(file->createGroup(std::string("/it") + toString(iteration)));
	
	// setup extendable (chunked) data tables
	fields->newDataSet<Group>(timegroup, "fields", PredType::NATIVE_FLOAT);
	origin->newDataSet<Group>(timegroup, "origin", PredType::NATIVE_FLOAT);
	delta->newDataSet<Group>(timegroup, "delta", PredType::NATIVE_FLOAT); // dx of subgrid
	
	Attribute atime = timegroup->createAttribute("time", PredType::NATIVE_DOUBLE, H5S_SCALAR);
	atime.write(PredType::NATIVE_DOUBLE, &time);
	Attribute ait = timegroup->createAttribute("iteration", PredType::NATIVE_INT, H5S_SCALAR);
	ait.write(PredType::NATIVE_INT, &iteration);
	
	// has to been kept in sync with fields, origin, delta counters
	//component = 0;
	//num_components = timegroup->createAttribute("components", PredType::NATIVE_INT, H5S_SCALAR);
	//num_components.write(PredType::NATIVE_INT, &component);
	// forget about this -> it should all be extracted from the shape of the data.
}
  
void exahype::plotters::FlashHDF5Writer::finishPlotting() {
	fields->shrink_to_fit();
	origin->shrink_to_fit();
	delta->shrink_to_fit();
	
	fields->closeDataSet();
	origin->closeDataSet();
	delta->closeDataSet();
	
	if(oneFilePerTimestep) closeH5();
	else flushH5();

	iteration++;
}

/**
 * This is 2D and 3D, allows several unknowns, named fields and all that.
 **/
void exahype::plotters::FlashHDF5Writer::plotPatch(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp) {
	// for the moment, we want to have ALL unknowns in ONE file
	
	// write out the mappedCell
	// mappedCells has shape mappedCell[writtenCellIdx->get({ depending on dims: i(2),i(1),i(0) or i(1),i(0) or i(0)}, writtenUnknown)];
	fields->write(PredType::NATIVE_DOUBLE, mappedCell);
	
	// write out the grid information
	origin->write(PredType::NATIVE_DOUBLE, offsetOfPatch.data());
	delta->write(PredType::NATIVE_DOUBLE, dx.data());
	
	// At the end: Count up the patches/components and update it
	// so we know how many components we have.
	// TODO: should rely on fields.size or similar instead
	//component++;
	//num_components.write(PredType::NATIVE_INT, &component);
}


#endif /* HDF5 */
