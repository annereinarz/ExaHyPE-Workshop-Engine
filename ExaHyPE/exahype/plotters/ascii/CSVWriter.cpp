#include "CSVWriter.h"

#include "tarch/parallel/Node.h" // for neat MPI information
#include "tarch/parallel/NodePool.h" 
#include "tarch/logging/Log.h"

static tarch::logging::Log _log("exahype::plotters::ascii::CSVWriter");

#include <iostream>
#include <ctime>
#include <cerrno>
#include <stdexcept>
#include <string>
#include <cstdarg>
#include <vector>
#include <cstdint>
#include <fstream>
#include <cstring>

namespace exahype {
namespace plotters {
namespace ascii {

// Helper routines

// a buffer-overflow-safe version of sprintf
// source: http://stackoverflow.com/a/69911
inline std::string vformat (const char *fmt, va_list ap) {
    // Allocate a buffer on the stack that's big enough for us almost
    // all the time.  Be prepared to allocate dynamically if it doesn't fit.
    size_t size = 1024;
    char stackbuf[1024];
    std::vector<char> dynamicbuf;
    char *buf = &stackbuf[0];
    va_list ap_copy;

    while (1) {
        // Try to vsnprintf into our buffer.
        va_copy(ap_copy, ap);
        int needed = vsnprintf (buf, size, fmt, ap);
        va_end(ap_copy);

        // NB. C99 (which modern Linux and OS X follow) says vsnprintf
        // failure returns the length it would have needed.  But older
        // glibc and current Windows return -1 for failure, i.e., not
        // telling us how much was needed.

        if (needed <= (int)size && needed >= 0) {
            // It fit fine so we're done.
            return std::string (buf, (size_t) needed);
        }

        // vsnprintf reported that it wanted to write more characters
        // than we allotted.  So try again using a dynamic buffer.  This
        // doesn't happen very often if we chose our initial size well.
        size = (needed > 0) ? (needed+1) : (size*2);
        dynamicbuf.resize (size);
        buf = &dynamicbuf[0];
    }
}

inline std::string sformat(const char *fmt, ...) {
	va_list ap;
	va_start (ap, fmt);
	std::string buf = vformat (fmt, ap);
	va_end (ap);
	return buf;
}

inline std::string sformat(std::string fmt,...) {
	va_list ap;
	va_start (ap, fmt);
	std::string buf = vformat (fmt.c_str(), ap);
	va_end (ap);
	return buf;
}

std::string str_time() {
	time_t rawtime;
	struct tm * timeinfo;
	char buffer[800];

	time (&rawtime);
	timeinfo = localtime(&rawtime);

	strftime(buffer,sizeof(buffer),"%d-%m-%Y %I:%M:%S",timeinfo);
	return std::string(buffer);
}

// END of helper routines
}}} // end of namespace exahype::plotters::ascii

using namespace exahype::plotters::ascii;

std::string exahype::plotters::ascii::CSVWriter::Column::get(voidptr lineStruct) {
	uintptr_t target = (uintptr_t)lineStruct + offset;
	
	switch(type) {
		case CSVWriter::Column::Type::INT:
			return sformat(format, (int&)*(int*)target);
		case CSVWriter::Column::Type::DOUBLE:
			return sformat(format, (double&)*(double*)target);
		case CSVWriter::Column::Type::BOOL:
			return sformat(format, (bool&)*(bool*)target);
		case CSVWriter::Column::Type::STRING:
			return (std::string)*(std::string*)target;
		case CSVWriter::Column::Type::UNDEF:
		default:
			throw std::domain_error("Undefined type");
	}
}

uintptr_t exahype::plotters::ascii::CSVWriter::Column::size_of_type(
	exahype::plotters::ascii::CSVWriter::Column::Type type) {
	
	switch(type) {
		case CSVWriter::Column::Type::INT: return sizeof(int);
		case CSVWriter::Column::Type::DOUBLE: return sizeof(double);
		case CSVWriter::Column::Type::BOOL: return sizeof(bool);
		case CSVWriter::Column::Type::STRING: return sizeof(std::string*); // i.e. void pointer
		case CSVWriter::Column::Type::UNDEF:
		default:
			throw std::domain_error("Undefined type");
	}
}

void exahype::plotters::ascii::CSVWriter::writeCommentLine(const std::string& line) {
	os() << commentIntro << line << newline;
}

void exahype::plotters::ascii::CSVWriter::writeHeader() {
	// Writes a header block which goes at the top of the file
	
	writeCommentLine("exahype::plotters::ascii::CSVWriter ASCII output");
	#ifdef PARALLEL
	os() << commentIntro <<
	"MPI Rank "
	tarch::parallel::Node::getInstance()::getRank()
	<< " of " <<
	tarch::parallel::Node::getInstance()::getNumberOfNodes()
	<< "total ranks" << newline;
	#else
	writeCommentLine("No MPI Setup -- single rank here.");
	#endif
	os() << commentIntro << "Created on " << str_time() << newline;
	
	// Human readable column list including description, in a format
	// suitable for gnuplotting (kind of similar to CarpetASCII)
	writeCommentLine(""); int c=1;
	for(auto&& col : columns) {
		os() << commentIntro
		   << c++ << ":" << col.name
		   << " -- "
		   << col.description
		   << newline;
	}
	writeCommentLine("");
	
	// Columns list
	if(!rawcolumns) os() << commentIntro;
	auto &last = *(--columns.end());
	for(auto&& col : columns) {
		os() << col.name;
		if(&col != &last) os() << seperator;
	}
	os() << newline;
}

void exahype::plotters::ascii::CSVWriter::writeRow(voidptr lineStruct) {
	auto &last = *(--columns.end());
	for(auto&& col : columns) {
		os() << col.get(lineStruct);
		if(&col != &last) os() << seperator;
        }
        os() << newline;
	os().flush(); // important
}

std::string exahype::plotters::ascii::CSVWriter::combineFilename(std::string filename, std::string suffix) {
	// this imitates how the VTK writes compose their filenames, cf.
	// tarch::plotter::griddata::unstructured::vtk::VTKTextFileWriter::writeToFile(...) at
	// Peano/tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.cpp
	#ifdef PARALLEL
	filename += "-rank-" + 	std::to_string(tarch::parallel::Node::getInstance().getRank());
	#endif
	filename += suffix;
	return filename;
}

std::ofstream& exahype::plotters::ascii::CSVWriter::os() {
	if(!ostream) throw std::runtime_error("Trying to write to a null output stream in CSVWriter.");
	return *ostream;
}

void exahype::plotters::ascii::CSVWriter::flushFile() {
	os().flush();
}

void exahype::plotters::ascii::CSVWriter::closeFile() {
	os().close();
	delete ostream;
}

void exahype::plotters::ascii::CSVWriter::openFile(std::string filename, std::string suffix) {
	filename = combineFilename(filename, suffix);
	bool master = tarch::parallel::Node::getInstance().isGlobalMaster();

	if(ostream) delete ostream; // close old file if present
	if (master) logInfo("openFile()", "Opening CSV file '"<< filename << "'");
	
	ostream = new std::ofstream(filename, std::ofstream::out);
	if(os().fail() || !os().is_open()) {
		logError("openFile()", "Cannot open ASCII file at '" << filename << "': " << std::strerror(errno));
		throw std::runtime_error("CSVWriter failed to open a file.");
	}
}

void exahype::plotters::ascii::CSVWriter::add_similar_columns(int n, exahype::plotters::ascii::CSVWriter::Column::Type type, std::string base_name, std::string format, uintptr_t base_offset) {
	std::string description = "";
	uintptr_t diff = Column::size_of_type(type);
	for(int i=0; i<n; i++) {
		Column col { sformat(base_name, i), format, description, type, base_offset + i*diff };
		columns.push_back(col);
	}
}

