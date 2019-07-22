/*
 * PeanoVariableData.h
 *
 *  Created on: 26 Oct 2017
 *      Author: dan
 */

#ifndef PEANOPATCHDATA_H_
#define PEANOPATCHDATA_H_

#include "PeanoVariable.h"

class PeanoPatchData {
public:
	PeanoPatchData(PeanoVariable* variable);
	virtual ~PeanoPatchData();
	PeanoVariable* structure;
	double* values;
	void setData(int index, double* pointer);
};

#endif /* PEANOPATCHDATA_H_ */
