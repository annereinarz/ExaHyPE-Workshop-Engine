#ifndef __EXAHYPE_PLOTTERS_ASCII__CSVWRITER__STACK__
#define __EXAHYPE_PLOTTERS_ASCII__CSVWRITER__STACK__

namespace exahype {
  namespace plotters {
    namespace ascii {
      class CSVStackWriter;
    }
  }
}

#include "CSVWriter.h"

/**
 * CSVStackWriter is a horizontally stacked CSVWriter, i.e. semi-reflectively allows to write
 * ASCII/CSV files from several structures. This is helpful if you want to use CSVWriter where
 * the column structure is not fixed at compile time but only runtime. An example is that
 * you have two structs
 *
 *   struct Basis { double time; bool active_flag; }
 *   struct Line3d { double x, y, z; }
 *
 * and you can then just write a single CSV file with columns "time,active_flag,x,y,z" using
 * CSVStackWriter. CSVWriter does not allow you that without crafting an intermediate
 * struct with continous storage.
 * 
 * To reach its goal, this class abuses CSVWriter a bit. Rule of thumb: Use the writers only
 * for setting the columns, do not use their file descriptors or anything else...
 **/
struct exahype::plotters::ascii::CSVStackWriter : public exahype::plotters::ascii::CSVWriter {
	using voidptr = char*;
	std::vector<exahype::plotters::ascii::CSVWriter> writers; ///< A list of individual writers
	
	void writeHeader() override; ///< Write a header block
	
	void openFile(std::string base, std::string suffix=".asc") override;
	void closeFile() override;
	
	/**
	 * Writing a row goes like (for convenience)
	 *    writeColumnSet(a);
	 *    writeColumnSet(b);
	 *    finishRow();
	 **/
	int numberOfWrittenCSVWritersInCurrentLine = 0;
	void writeColumns(voidptr line);
	void finishRow();
	
	/** Implemented just to tell you you're using this function wrong */
	void writeRow(voidptr line) override;
};

// syntactic sugar

#define CSV_STACK_WRITER_WRITE_COLUMNS(csvStackWriterInstance, userStructureInstance) \
	(csvStackWriterInstance).writeColumns( (exahype::plotters::ascii::CSVWriter::voidptr) &userStructureInstance )


#endif /* __EXAHYPE_PLOTTERS_ASCII__CSVWRITER__STACK__ */
