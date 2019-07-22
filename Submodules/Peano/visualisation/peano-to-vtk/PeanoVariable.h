/*
 * PeanoVariable.h
 *
 *  Created on: 26 Oct 2017
 *      Author: dan
 */

#ifndef PEANOVARIABLE_H_
#define PEANOVARIABLE_H_

#include <string>

enum PeanoDataType {Cell_Values, Vertex_Values};

class PeanoVariable {
public:
	PeanoVariable(std::string variableName, int unknowns, PeanoDataType type, int points, double mapping[], int mappings);
	virtual ~PeanoVariable();
	std::string name;
	int unknowns;
	int points;
	int totalValues;
	int mappings;
	double* mapping;
	PeanoDataType type;
};

#endif /* PEANOVARIABLE_H_ */
