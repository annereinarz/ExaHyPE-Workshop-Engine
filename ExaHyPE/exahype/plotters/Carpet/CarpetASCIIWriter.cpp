#include "exahype/plotters/Carpet/CarpetASCIIWriter.h"
#include "exahype/plotters/ascii/CSVStackWriter.h"
#include "peano/utils/Loop.h" // dfor
#include "tarch/parallel/Node.h" // for basic MPI rank determination only
#include <sstream>
#include <stdexcept>

// my small C++11 to_string-independent workaround.
template <typename T> inline std::string toString( T Number ) {
	std::ostringstream ss; ss << Number; return ss.str();
}

struct exahype::plotters::CarpetASCIIDatasets::CompatFull { int it, tl, rl, c, ml; };
struct exahype::plotters::CarpetASCIIDatasets::Coord1D { int ix; double time, x; };
struct exahype::plotters::CarpetASCIIDatasets::Coord2D { int ix, iy; double time, x, y; };
struct exahype::plotters::CarpetASCIIDatasets::Coord3D { int ix, iy, iz; double time, x, y, z; };

using namespace exahype::plotters::ascii;
using namespace exahype::plotters::CarpetASCIIDatasets;

static std::vector<exahype::plotters::ascii::CSVWriter::Column> carpet_writer_compat_full = {
	CSVWRITER_INTEGER_COLUMN(CompatFull, it, "iteration (in ExaHyPE: plotting step, not time integration step)"),
	CSVWRITER_INTEGER_COLUMN(CompatFull, tl, "time level (in ExaHyPE: no use)"),
	CSVWRITER_INTEGER_COLUMN(CompatFull, c,  "component (in ExaHYPE: cell number)"),
	CSVWRITER_INTEGER_COLUMN(CompatFull, ml, "multigrid level (in ExaHYPE: Limiter status)")
};
static std::vector<exahype::plotters::ascii::CSVWriter::Column> carpet_writer_coord1d = {
	CSVWRITER_INTEGER_COLUMN(Coord1D, ix, "X-Index of data point (in ExaHYPE: index within a patch)"),
	CSVWRITER_DOUBLE_COLUMN(Coord1D, time, "Time of vertex"),
	CSVWRITER_DOUBLE_COLUMN(Coord1D, x, "Coordinate of vertex")
};
static std::vector<exahype::plotters::ascii::CSVWriter::Column> carpet_writer_coord2d = {
	CSVWRITER_INTEGER_COLUMN(Coord2D, ix, "X-Index of data point (in ExaHYPE: index within a patch)"),
	CSVWRITER_INTEGER_COLUMN(Coord2D, iy, "Y-Index of data point (in ExaHYPE: index within a patch)"),
	CSVWRITER_DOUBLE_COLUMN(Coord2D, time, "Time of vertex"),
	CSVWRITER_DOUBLE_COLUMN(Coord2D, x, "Coordinate of vertex"),
	CSVWRITER_DOUBLE_COLUMN(Coord2D, y, "Coordinate of vertex")
};
static std::vector<exahype::plotters::ascii::CSVWriter::Column> carpet_writer_coord3d = {
	CSVWRITER_INTEGER_COLUMN(Coord3D, ix, "X-Index of data point (in ExaHYPE: index within a patch)"),
	CSVWRITER_INTEGER_COLUMN(Coord3D, iy, "Y-Index of data point (in ExaHYPE: index within a patch)"),
	CSVWRITER_INTEGER_COLUMN(Coord3D, iz, "Z-Index of data point (in ExaHYPE: index within a patch)"),
	CSVWRITER_DOUBLE_COLUMN(Coord3D, time, "Time of vertex"),
	CSVWRITER_DOUBLE_COLUMN(Coord3D, x, "Coordinate of vertex"),
	CSVWRITER_DOUBLE_COLUMN(Coord3D, y, "Coordinate of vertex"),
	CSVWRITER_DOUBLE_COLUMN(Coord3D, z, "Coordinate of vertex")
};

exahype::plotters::CarpetASCIIWriter::~CarpetASCIIWriter() {}
exahype::plotters::CarpetASCIIWriter::CarpetASCIIWriter(
	const std::string& _filename,
	int _basisSize,
	int _solverUnknowns,
	int _writtenUnknowns,
	exahype::parser::ParserView  _plotterParameters,
	char** _writtenQuantitiesNames)
	:
	CarpetWriter(_filename, _basisSize, _solverUnknowns, _writtenUnknowns, _plotterParameters, _writtenQuantitiesNames),
	_log("exahype::plotters::CarpetASCIIWriter"),
	fullCarpetCompatibility(_plotterParameters.getValueAsBoolOrDefault("select/full_carpet_compat", true)),
	files(allUnknownsInOneFile ? 1 : writtenUnknowns)
	{
		
	// initialize the CSV file headers
	int writtenUnknown=0;
	for(auto& file : files) {
		if(fullCarpetCompatibility) {
			CSVWriter fullCompatWriter; fullCompatWriter.columns = carpet_writer_compat_full;
			file.writers.push_back(fullCompatWriter);
		}
		
		// bogus code -.-		
		switch(dim) {
			case 3: {
				CSVWriter coord3dWriter; coord3dWriter.columns = carpet_writer_coord3d;
				file.writers.push_back(coord3dWriter); break;
			}
			case 2: {
				CSVWriter coord2dWriter; coord2dWriter.columns = carpet_writer_coord2d;
				file.writers.push_back(coord2dWriter); break;
			}
			case 1: {
				CSVWriter coord1dWriter; coord1dWriter.columns = carpet_writer_coord1d;
				file.writers.push_back(coord1dWriter); break;
			}
		}
		
		CSVWriter fieldWriter;
		fieldWriter.add_similar_columns(allUnknownsInOneFile ? writtenUnknowns : 1,
			CSVWriter::Column::Type::DOUBLE, "Q%d", "%e"); // we probably want to have %f as format
		if(allUnknownsInOneFile) {
			for(int wu=0; wu<writtenUnknowns; wu++) {
				fieldWriter.columns[wu].name = writtenQuantitiesNames[wu];
				fieldWriter.columns[wu].description = "field value (payload)";
			}
		} else {
			fieldWriter.columns[0].name = writtenQuantitiesNames[writtenUnknown];
			fieldWriter.columns[0].description = "single written field";
		}
		
		file.writers.push_back(fieldWriter);
		writtenUnknown++;
	}
	
	// open file(s) initially, if neccessary
	if(!oneFilePerTimestep) openFile();
}


/**
 * Opens or switchs the currently active H5 file or the list of H5 files.
 * 
 * ATTENTION: The composal of the Carpet filename has to be chosen carefully. Some (!) readers except
 *    for 1D files a pattern *.{x,y,z}.asc
 *    for 2D files a pattern *.{xy,yz,xz}.asc
 *    for 3D files a pattern *.xyz.asc
 *    (all patterns here are bash linux command line globbing patterns)
 * This is especially for the readers from David Radice (https://bitbucket.org/dradice/scidata)
 * and Wolfgang Kastaun (https://bitbucket.org/DrWhat/pycactuset).
 **/
void exahype::plotters::CarpetASCIIWriter::openFile() {
	std::string local_filename, suffix, prefix, sep("-");
	prefix = basisFilename;
	suffix = (oneFilePerTimestep?(sep + "it" + toString(iteration)):"") + dimextension;
	// note that the CSVWriter adds the MPI rank information and ".asc" file extension

	int writtenUnknown=0;
	for(auto& file : files) {
		local_filename = prefix + (allUnknownsInOneFile ? "" : (sep + writtenQuantitiesNames[writtenUnknown])) + suffix;
		file.openFile(local_filename);
		file.writeCommentLine("Created by ExaHyPE/CarpetASCIIWriter");
		file.writeHeader();
		file.flushFile();
		writtenUnknown++;
	}
}

void exahype::plotters::CarpetASCIIWriter::closeFile() {
	for(auto& file : files) file.closeFile();
}

void exahype::plotters::CarpetASCIIWriter::flushFile() {
	for(auto& file : files) file.flushFile();
}


void exahype::plotters::CarpetASCIIWriter::startPlotting(double time) {
	component = 0; // Carpet in general wants the components start with 0.
	if(oneFilePerTimestep) openFile();
}
  
void exahype::plotters::CarpetASCIIWriter::finishPlotting() {
	if(oneFilePerTimestep) closeFile();
	else flushFile();
	iteration++;
}

/**
 * This is 2D and 3D, allows several unknowns, named fields and all that.
 **/
void exahype::plotters::CarpetASCIIWriter::plotPatch(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus) {

	for(int writtenUnknown=0; writtenUnknown < writtenUnknowns; writtenUnknown++) {
		CSVWriterType& target = files[allUnknownsInOneFile ? 0 : writtenUnknown];
		plotPatchForSingleUnknown(offsetOfPatch, sizeOfPatch, dx, mappedCell, timeStamp, limiterStatus, writtenUnknown, target);
	} // for writtenUnknown
	component++;
}

void exahype::plotters::CarpetASCIIWriter::plotPatchForSingleUnknown(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus_data,
      int requestedWrittenUnknown, exahype::plotters::CarpetASCIIWriter::CSVWriterType& target) {

	// the proper replacement for dfor(i,basisSize)
	ivec i;
	for(i(0)=0; i(0)<basisSize; i(0)++)
	for(i(1)=0; i(1)<(dim > 1 ? basisSize : 1); i(1)++)
	#if DIMENSIONS>2
	for(i(2)=0; i(2)<(dim > 2 ? basisSize : 1); i(2)++)
	#endif
	{
		dvec pos = offsetOfPatch + i.convertScalar<double>()* (sizeOfPatch(0)/(basisSize-1));
		
		if(fullCarpetCompatibility) {
			CompatFull full;
			full.it = iteration;
			full.tl = -1;
			full.c = -1; // Cell number, todo, currently no access
			full.ml = limiterStatus_data;
			CSV_STACK_WRITER_WRITE_COLUMNS(target, full);
		}

		switch(dim) {
			case 1: {
				Coord1D coord;
				coord.ix = i(0);
				coord.time = timeStamp;
				coord.x = pos(0);
				CSV_STACK_WRITER_WRITE_COLUMNS(target, coord);
				break;
			}
			case 2: {
				Coord2D coord;
				coord.ix = i(0);
				coord.iy = i(1);
				coord.time = timeStamp;
				coord.x = pos(0);
				coord.y = pos(1);
				CSV_STACK_WRITER_WRITE_COLUMNS(target, coord);
				break;
			}
			case 3: {
				Coord3D coord;
				coord.ix = i(0);
				coord.iy = i(1);
				coord.iz = i(2);
				coord.time = timeStamp;
				coord.x = pos(0);
				coord.y = pos(1);
				coord.z = pos(2);
				CSV_STACK_WRITER_WRITE_COLUMNS(target, coord);
				break;
			}
		}

		
		int writtenUnknownOffset = requestedWrittenUnknown < 0 ? 0 : requestedWrittenUnknown;
		//int writtenUnkownLength = requestedWrittenUnknown < 0 ? writtenUnknowns : 1; // unused
		double* fieldValues;
		
		switch(dim) {
			case 3: fieldValues = &mappedCell[writtenCellIdx->get(i(2),i(1),i(0),writtenUnknownOffset)]; break;
			case 2: fieldValues = &mappedCell[writtenCellIdx->get(i(1),i(0),writtenUnknownOffset)]; break;
			case 1: fieldValues = &mappedCell[writtenCellIdx->get(i(0),writtenUnknownOffset)]; break;
			default: throw std::domain_error("Dimension not supported");
		}
		
		CSV_STACK_WRITER_WRITE_COLUMNS(target, *fieldValues);
		target.finishRow();
	}
}

