/*
 * PeanoReader.cpp
 *
 *  Created on: 16 Oct 2017
 *      Author: dan
 */

#include "PeanoReader.h"
#include "PeanoPatch.h"
#include "PeanoVariable.h"

#include "boost/algorithm/string/predicate.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/algorithm/string/classification.hpp"
#include "boost/algorithm/string/erase.hpp"
#include "boost/algorithm/string/trim.hpp"

#include <string>
#include <iostream>
#include <vector>
#include <fstream>

PeanoReader::PeanoReader(const std::string &file) {
  patchSize = new int[3];

  #pragma omp critical
  {
    std::cout << "Reading file " << file << std::endl;
  }

  std::ifstream ifs(file);
  std::vector<std::string> lines;
  for (std::string line; std::getline(ifs, line); /**/ ) {
	lines.push_back(line);
  }
  ifs.close();

  for(uint i = 0; i < lines.size(); i++) {
    std::string line = lines[i];
    if(boost::starts_with(line, "#")) {
      continue;
	}
	else if(boost::starts_with(line, "begin cell-values")) {//define a cell variable
      std::vector<std::string> split;
      boost::split(split, line, boost::is_any_of(" "));
		std::string variableName = split[2];
        variableName = variableName.substr(1, variableName.size() -2);

        std::string nextLine = lines[i+1];
        boost::trim(nextLine);
        std::vector<std::string> splitUnknowns;
        boost::split(splitUnknowns, nextLine, boost::is_any_of(" "));

        int unknowns = std::stoi(splitUnknowns[1]);

        //get mapping
        double* mapping = nullptr;
        int mappings = -1;
	    std::string line2 = lines[i+2];
        boost::trim(line2);
        if(boost::starts_with(line2, "begin mapping")) {

				std::string line3 = lines[i+3];
				boost::erase_all(line3, "end mapping");
				boost::trim(line3);
				std::vector<std::string> splitMappings;
				boost::split(splitMappings, line3, boost::is_any_of(" "));

				mappings = splitMappings.size();

				mapping = new double[mappings];
				for(uint j = 0; j < splitMappings.size(); j++) {
					mapping[j] = std::stod(splitMappings[j]);
				}
				i+= 4;
			}
			PeanoVariable* variable = new PeanoVariable(variableName, unknowns, Cell_Values, cells, mapping, mappings);
			variables.push_back(variable);

		} else if(boost::starts_with(line, "begin vertex-values")) {//define a vertex variable
			//get variable name
			std::vector<std::string> split;
			boost::split(split, line, boost::is_any_of(" "));
			std::string variableName = split[2];
			variableName = variableName.substr(1, variableName.size() -2);

			//get number of unknowns
			std::string line1 = lines[i+1];
			boost::trim(line1);
			std::vector<std::string> splitUnknowns;
			boost::split(splitUnknowns, line1, boost::is_any_of(" "));
			int unknowns = std::stoi(splitUnknowns[1]);

			//get mapping
			double* mapping = nullptr;
			int mappings = -1;
			std::string line2 = lines[i+2];
			boost::trim(line2);
			if(boost::starts_with(line2, "begin mapping")) {

				std::string line3 = lines[i+3];
				boost::erase_all(line3, "end mapping");
				boost::trim(line3);
				std::vector<std::string> splitMappings;
				boost::split(splitMappings, line3, boost::is_any_of(" "));

				mappings = splitMappings.size();

				mapping = new double[mappings];
				for(int j = 0; j < mappings; j++) {
					mapping[j] = std::stod(splitMappings[j]);
				}
				i+=4;
			}
			PeanoVariable* variable = new PeanoVariable(variableName, unknowns, Vertex_Values, vertices, mapping, mappings);
			variables.push_back(variable);
		} else if(boost::starts_with(line, "dimensions")) {
			std::vector<std::string> split;
			boost::split(split, line, boost::is_any_of(" "));
			dimensions = std::stoi(split[1]);
		} else if(boost::starts_with(line, "patch-size")) {
			std::vector<std::string> split;
			boost::split(split, line, boost::is_any_of(" "));
			for(int j = 1; j <= dimensions; j++) {
				patchSize[j-1] = std::stoi(split[j]);
			}

			//update number of cells and vertices
			cells = 1;
			vertices = 1;
			for(int i = 0; i < dimensions; i++) {
				cells *= patchSize[i];
				vertices *= patchSize[i] +1;
			}
		} else if(boost::starts_with(line, "begin patch")) {
			//build a list of lines for this patch
			std::vector<std::string> patchLines;
			for(i++; i < lines.size(); i++) {
				std::string patchLine = lines[i];
				if(boost::starts_with(patchLine, "end patch")) {
					patches.push_back(new PeanoPatch(patchLines, dimensions, patchSize, variables));
					break;
				} else {
					patchLines.push_back(patchLine);
				}
			}
		}
	}
}


PeanoReader::~PeanoReader() {
  delete [] patchSize;

  for(uint i = 0; i < patches.size(); i++) {
    delete patches[i];
  }

  for(uint i = 0; i < variables.size(); i++) {
    delete variables[i];
  }
}
