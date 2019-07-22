/*
 * PeanoPatch.cpp
 *
 *  Created on: 16 Oct 2017
 *      Author: dan
 */

#include "PeanoPatch.h"
#include "PeanoVariable.h"
#include "PeanoPatchData.h"

#include <string>
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <limits>

#include "boost/lexical_cast.hpp"
#include "boost/algorithm/string/predicate.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/algorithm/string/classification.hpp"
#include "boost/algorithm/string/trim.hpp"

PeanoPatch::PeanoPatch() {
	offsets = nullptr;
	sizes = nullptr;
	resolution = nullptr;
	dimensions = -1;
}

PeanoPatch::PeanoPatch(std::vector<std::string> &text, int dimensions, int* patchSize, std::vector<PeanoVariable*> &variables) {
	//std::cout << "Starting PeanoPatch\n";
	this->dimensions = dimensions;
	this->resolution = patchSize;

	//calculate the number of cells in the patch
	int cells = 1;
	for(int i = 0; i < dimensions; i++) {
		cells *= patchSize[i];
	}

	//initialize the data objects for each variable
	for(uint i = 0; i < variables.size(); i++) {
		PeanoPatchData* data = new PeanoPatchData(variables[i]);
		patchData[data->structure->name] = data;
	}

	//std::cout << "text size: " << text.size() << "\n";

	for(uint i = 0; i < text.size(); i++) {
		//std::cout << "Patch line " << i << "\n";
		std::string line = text[i];
		boost::trim(line);

		if(boost::starts_with(line, "offset")) {

			//split the line which will be in the form "offset 0 0"
			std::vector<std::string> split;
			boost::split(split, line, boost::is_any_of(" "));

			//create an array to hold the number  of offsets
			offsets = new double[dimensions];
			for(int j = 0; j < dimensions; j++) {
				offsets[j] = std::stod(split[j+1]);//the 0th element of split is "offset" so skip it
			}
		} else if(boost::starts_with(line, "size")) {
			//split the line which will be in the form "size 0 0"
			std::vector<std::string> split;
			boost::split(split, line, boost::is_any_of(" "));

			//create an array to hold the number  of offsets
			sizes = new double[dimensions];
			for(int j = 0; j < dimensions; j++) {
				sizes[j] = std::stod(split[j+1]);//the 0th element of split is "offset" so skip it
			}
		} else if(boost::starts_with(line,"begin cell-values") || boost::starts_with(line,"begin vertex-values")) {

			//get the variable we are looking for
			std::vector<std::string> split;
			boost::split(split, line, boost::is_any_of(" "));
			std::string variableName = split[2];
			variableName = variableName.substr(1, variableName.size() -2);

			//get the data object for this variable
			PeanoPatchData* data = patchData[variableName];

			//get the line of text containing the values and trim whitespace
			std::string vals = text[i+1];
			boost::trim(vals);

			//split the line of text to get a vector of string values
			std::vector<std::string> splitValues;
			boost::split(splitValues, vals, boost::is_any_of(" "));

			//convert the strings to doubles and add to the array
			for(int j = 0; j < data->structure->totalValues; j++) {
				data->values[j] = std::stod(splitValues[j]);
			}
		}
	}
}

bool PeanoPatch::saveToFile(std::string filename) {
	std::cout << "\nSaving patch to file " << filename << "\n";
	std::ofstream file;
	file.open(filename.c_str(), std::ofstream::out | std::ofstream::trunc);


	file << "#\n";
	file << "# Peano output file\n";
	file << "# Version 0.1\n";
	file << "#\n";
	file << "format ASCII\n";
	file << "dimensions " << dimensions << "\n";
	file << "patch-size " << resolution[0] << " " << resolution[1] << " " << resolution[2] << "\n\n";

	//metadata about the variables in this form
	//begin cell-values "Q"
	//  number-of-unknowns 23
	//end cell-values

	for (auto it : patchData) {
		PeanoPatchData* data = it.second;
		file << "begin " << (data->structure->type == Cell_Values?"cell":"vertex") << "-values";
		file << " \"" << data->structure->name << "\"\n";
		file << "  number-of-unknowns " << data->structure->unknowns << "\n";
		file << "end " << (data->structure->type == Cell_Values?"cell":"vertex") << "-values\n\n";
	}

	//output the actual patch
	file << "begin patch\n";
	file << "  offset " << offsets[0] << " " << offsets[1] << " " << offsets[2] << "\n";
	file << "  size " << sizes[0] << " " << sizes[1] << " " << sizes[2] << "\n";

	//output the variable values
	for (auto it : patchData) {
		PeanoPatchData* data = it.second;
		file << "  begin " << (data->structure->type == Cell_Values?"cell":"vertex") << "-values";
		file << " \"" << data->structure->name << "\"\n";
		file << "   ";
		for(int i = 0; i < data->structure->totalValues; i++) {
			//file << " " << data->values[i];

			double value = data->values[i];
			if(value < 0.000000000001 && value > 0) {
				file << " 0";
			} else {
				file << " " << value;
			}

		}
		file << "\n  end " << (data->structure->type == Cell_Values?"cell":"vertex") << "-values\n";
	}
	file << "end patch\n";

	file.close();

	return false;
}

PeanoVariable* PeanoPatch::getStructure() {
	for (auto it : this->patchData) {
		return it.second->structure;
	}
	return nullptr;
}

bool PeanoPatch::hasMappings(){
	for (auto it : this->patchData) {
		return it.second->structure->mappings != -1;
	}
	return false;
}

double* PeanoPatch::getPositionMap(int x, int y, int z) {
	double* position = new double[3];
	return position;
}

double* PeanoPatch::getPositionVertex(int x, int y, int z) {
	double* position = new double[3];

	position[0] = offsets[0] + sizes[0]*x/resolution[0];
	position[1] = offsets[1] + sizes[1]*y/resolution[1];
	position[2] = offsets[2] + sizes[2]*z/resolution[2];

	return position;
}

double* PeanoPatch::getPositionCellCenter(int x, int y, int z) {
	double* position = new double[3];

	position[0] = offsets[0] + sizes[0]*(0.5+x)/resolution[0];
	position[1] = offsets[1] + sizes[1]*(0.5+y)/resolution[1];
	position[2] = offsets[2] + sizes[2]*(0.5+z)/resolution[2];

	return position;
}

int PeanoPatch::getIndexCellData(int x, int y, int z) {
	return z + y * resolution[2] + x*resolution[2]*resolution[1];
	//return x + y*(resolution[0]) + z*(resolution[0])*(resolution[1]);
}

int PeanoPatch::getIndexVertexData(int x, int y, int z) {
	return z + y * (resolution[2]+1) + x*(resolution[2]+1)*(resolution[1]+1);
	//return x + y*(resolution[0]) + z*(resolution[0])*(resolution[1]);
}


PeanoPatch::~PeanoPatch() {
	delete [] offsets;
	delete [] sizes;
	for (auto it : patchData) {
		delete it.second;
	}
}
