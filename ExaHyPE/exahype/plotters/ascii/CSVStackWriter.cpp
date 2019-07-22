#include "CSVStackWriter.h"
#include <exception>


#include <stdlib.h>
#include <stdio.h>
#include <stdexcept>

void exahype::plotters::ascii::CSVStackWriter::openFile(std::string base, std::string suffix) {
	exahype::plotters::ascii::CSVWriter::openFile(base,suffix); // sets ostream / os()
	for(auto& writer : writers) writer.ostream = ostream; // links all writers
}

void exahype::plotters::ascii::CSVStackWriter::closeFile() {
	delete ostream;
	for(auto& writer : writers) delete writer.ostream;
}

void exahype::plotters::ascii::CSVStackWriter::writeHeader() {
	// Writes a header block which goes at the top of the file
	writeCommentLine("exahype::plotters::ascii::CSVStackWriter ASCII output");
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
	for(auto& writer : writers) {
		for(auto&& col : writer.columns) {
			os() << commentIntro
			<< c++ << ":" << col.name
			<< " -- "
			<< col.description
			<< newline;
		}
		//os() << newline;
	}
	writeCommentLine("");
	
	// Columns list in a single line
	auto &last_writer = *(--writers.end());
	for(auto& writer : writers) {
		if(!rawcolumns) os() << commentIntro;
		auto &last = *(--writer.columns.end());
		for(auto&& col : writer.columns) {
			os() << col.name;
			if(&col != &last) os() << seperator;
		}
		if(&writer != &last_writer) os() << seperator;
		if(!rawcolumns) os() << newline;
	}
	os() << newline;
}

void exahype::plotters::ascii::CSVStackWriter::writeColumns(voidptr line) {
	auto& writer = writers[numberOfWrittenCSVWritersInCurrentLine];
	// to be safe, set this every time
	writer.newline = "";
	writer.writeRow(line);
	if(numberOfWrittenCSVWritersInCurrentLine < writers.size()-1)
		os() << seperator;
	numberOfWrittenCSVWritersInCurrentLine++;
}


void exahype::plotters::ascii::CSVStackWriter::finishRow() {
	numberOfWrittenCSVWritersInCurrentLine = 0;
        os() << newline;
	os().flush(); // important
}

void exahype::plotters::ascii::CSVStackWriter::writeRow(voidptr line) {
	throw std::runtime_error("You're using CSVStackWriter wrong. Use writeColumns() instead.");
}
