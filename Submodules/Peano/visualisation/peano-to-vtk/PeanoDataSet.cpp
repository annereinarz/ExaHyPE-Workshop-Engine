/*
 * PeanoDataSet.cpp
 *
 *  Created on: 29 Jan 2018
 *      Author: dan
 */

#include "PeanoDataSet.h"
#include "PeanoConverter.h"

#include "boost/algorithm/string/predicate.hpp"
#include "boost/algorithm/string/erase.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/algorithm/string/replace.hpp"

#include <vector>
#include <string>

PeanoDataSet::PeanoDataSet(std::vector<std::string> &lines, std::string directory) {
	this->directory = directory;
	fullData = new std::vector<std::string>();
	resolutionData = new std::vector<std::string>();
	resolutions = new std::vector<std::vector<int>>();

	for(std::string line: lines) {

		//skip the lineif it commented out
		if(boost::starts_with(line, "#")) continue;

		//split the line
		std::vector<std::string> split;
		boost::split(split, line, boost::is_any_of(" "));

		//contains resolution data
		if(split.size() == 5) {
			int x = std::stoi(split[1]);
			int y = std::stoi(split[2]);
			int z = std::stoi(split[3]);

			std::string fileName = split[4];
			boost::erase_all(fileName, "\"");

			std::vector<int> dimensions;
			dimensions.push_back(x);
			dimensions.push_back(y);
			dimensions.push_back(z);

			//std::vector<std::string> resolutionData;
			//std::vector<std::vector<int>> resolutions;

			resolutions->push_back(dimensions);
			resolutionData->push_back(fileName);
		} else if(split.size() == 2) {
			std::string fileName = split[1];
			boost::erase_all(fileName, "\"");
			fullData->push_back(fileName);
		}
	}
}


std::vector<std::string> PeanoDataSet::toString() {
	std::vector<std::string> lines;

	lines.push_back("begin dataset");

	//print the full data
	for(uint i = 0; i < fullData->size(); i++) {
		std::string data = fullData->at(i);
		lines.push_back("  include " + data);
	}

	//print the lower resolution versions
	for(uint i = 0; i < resolutions->size(); i++) {
		int x = resolutions->at(i)[0];
		int y = resolutions->at(i)[1];
		int z = resolutions->at(i)[2];
		std::string file = resolutionData->at(i);
		lines.push_back("  include-resolution " + std::to_string(x) + " "
				+ std::to_string(y) + " " + std::to_string(z) + " " + file);
	}


	lines.push_back("end dataset");

	return lines;
}

std::vector<PeanoReader*>* PeanoDataSet::createReadersFull() {
  std::vector<PeanoReader*>* readers = new std::vector<PeanoReader*>();
  const int maxSize = fullData->size();
  #pragma omp parallel for
  for(uint i = 0; i < maxSize; i++) {
    PeanoReader* newReader = new PeanoReader(directory + fullData->at(i));
    #pragma omp critical
	readers->push_back(newReader);
  }
  return readers;
}

std::vector<PeanoReader*>* PeanoDataSet::createReadersResolution(int res) {
	std::vector<PeanoReader*>* readers = new std::vector<PeanoReader*>();

	readers->push_back(new PeanoReader(directory + resolutionData->at(res)));

	return readers;
}

/*
 * Creates a new regular PeanoPatch of the specified resolution and
 * adds it to this data set
 * This function returns a pointer to this patch which must be deleted by
 * the caller, otherwise a memory leak will occur.
 */
PeanoPatch* PeanoDataSet::createSubSample(int x, int y, int z, bool saveToFile) {
	std::vector<PeanoReader*>* readers = createReadersFull();
	PeanoPatch* sample = PeanoConverter::subSample(*readers, x, y, z);

	for(uint i = 0; i < readers->size(); i++) {
		delete readers->at(i);
	}


	if(saveToFile) {
		std::string fileName = fullData->at(0);
		boost::replace_all(fileName, ".peano-patch-file",
				"-(" + std::to_string(x) + "-" + std::to_string(y) + "-" + std::to_string(z) + ").peano-patch-file");

		std::vector<int> dimensions;
		dimensions.push_back(x);
		dimensions.push_back(y);
		dimensions.push_back(z);
		resolutions->push_back(dimensions);
		resolutionData->push_back(fileName);

		sample->saveToFile(directory + fileName);
	}

	return sample;
}

std:: string PeanoDataSet::getDirectory() {
	return directory;
}

std::vector<std::string>* PeanoDataSet::getFullData() {
	return fullData;
}
std::vector<std::vector<int>>* PeanoDataSet::getResolutions() {
	return resolutions;
}
std::string PeanoDataSet::getResolution(int index) {
	return resolutionData->at(index);
}

std::string PeanoDataSet::getSimpleName() {
	return fullData->at(0);
}

PeanoDataSet::~PeanoDataSet() {
	delete fullData;
	delete resolutionData;
	delete resolutions;
}
