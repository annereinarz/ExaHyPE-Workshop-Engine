#ifndef __EXAHYPE_PLOTTERS_ASCII__CSVWRITER__
#define __EXAHYPE_PLOTTERS_ASCII__CSVWRITER__

#include <string>
#include <vector>
#include <fstream>
#include <cstdint>

namespace exahype {
  namespace plotters {
    namespace ascii {
      class CSVWriter;
      
      std::string str_time(); ///< helper routine from CSVWriter.cpp
    }
  }
}

/**
 * CSVWriter is a semi-reflecting writer for ASCII/CSV files (most likely used in time series
 * "plots" in ExaHyPE).
 * 
 * The central idea is that you have a structure in your code/plotter, such as
 *
 *   struct Line { int time; double x, y, z; bool active_flag; }
 *
 * and CSVWriter provides you the library code to write out this structure every now and then
 * without much registration overhead. This is archived by storing relative positions of the
 * data fields in the structures (C macro offsetof). I call this "semi-reflection".
 *
 * What CSVWriter does not:
 *  - Generation of machine readable self-describing data (this should be part of another layer ontop)
 *  - Global reductions or MPI communication (CSVWriter does not know much about MPI)
 **/
struct exahype::plotters::ascii::CSVWriter {
	using voidptr = char*; ///< A pointer to a 1-byte-width thing

	struct Column {
		std::string name;   ///< A short name for a CSV column such as "l1norm"
		std::string format; ///< A formatstring
		std::string description; ///< A one line human-readable description such as "Defines the l1Norm for a certain quantity"

		enum class Type { INT, DOUBLE, BOOL, STRING, UNDEF } type;
		uintptr_t offset; ///< Storage position in structure where it is read off
		static uintptr_t size_of_type(Type type);
		std::string get(voidptr lineStruct); ///< get value formatted as string, given a structure
	};

	std::vector<Column> columns; ///< Column specifications, go set them!
	std::ofstream *ostream = nullptr; ///< The Stream you want to write to. This is a pointer to allow CSVWriter to be a POD
	std::ofstream& os(); ///< Obtains the ostream reference or crashes
	
	std::string seperator = "\t"; ///< Column seperator
	std::string newline = "\n"; ///< Newline character to use
	std::string commentIntro = "# "; ///< Characters how to introduce a comment line
	bool rawcolumns = true; ///< Display the column names without a comment (=machine readable)
	
	void writeCommentLine(const std::string& line);
	virtual void writeHeader(); ///< Write a header block
	
	/**
	 * Write an actual row. Here you should pass the pointer to your structure.
	 **/
	virtual void writeRow(voidptr line);
	
	// I/O helper routines if you want to delegate the file handling to this class:
	
	/// Compose a typical ExaHyPE output filename
	static std::string combineFilename(std::string base, std::string suffix=".asc");
	virtual void openFile(std::string base, std::string suffix=".asc");
	virtual void closeFile();
	virtual void flushFile();
	
	/**
	 * A helper routine for quickly add a similar class of columns (n times).
	 * The base_name will be expanded as C-format string, i.e. "Q%d" yields "Q0","Q1",...
	 * Note, if you want to set descriptions afterwards, you can always change the
	 * columns, i.e. column[2].description="foo".
	 **/
	void add_similar_columns(int n, Column::Type type, std::string base_name, std::string format, uintptr_t base_offset=0);
};


// Syntactic sugar for standard stuff

#include <cstddef> // offsetof macro (not member of std::)

#define CSVWRITER_INTEGER_COLUMN(userStructure, fieldName, description) \
	{ #fieldName, "%d", description, exahype::plotters::ascii::CSVWriter::Column::Type::INT, offsetof(userStructure, fieldName) }
		
#define CSVWRITER_DOUBLE_COLUMN(userStructure, fieldName, description) \
	{ #fieldName, "%e", description, exahype::plotters::ascii::CSVWriter::Column::Type::DOUBLE, offsetof(userStructure, fieldName) }
		
#define CSVWRITER_WRITE_ROW(csvWriterInstance, userStructureInstance) \
	(csvWriterInstance).writeRow( (exahype::plotters::ascii::CSVWriter::voidptr) &userStructureInstance )

// etc.

#endif /* __EXAHYPE_PLOTTERS_ASCII__CSVWRITER__ */
