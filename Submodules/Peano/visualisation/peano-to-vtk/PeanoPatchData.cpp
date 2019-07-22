/*
 * PeanoVariableData.cpp
 *
 *  Created on: 26 Oct 2017
 *      Author: dan
 */

#include "PeanoPatchData.h"
#include "PeanoVariable.h"

#include <iostream>
#include <limits>

PeanoPatchData::PeanoPatchData(PeanoVariable* variable) {
	structure = variable;
	values = new double[structure->totalValues];

}

/*
 * Copies data from the pointer to this patch data
 * at the specified index
 * The number of values copied is structure->unknowns
 */
void PeanoPatchData::setData(int index, double* pointer) {
	int unknowns = structure->unknowns;
	for(int i = 0; i < unknowns; i++) {
		values[index*unknowns + i] = pointer[i];
	}
}

PeanoPatchData::~PeanoPatchData() {
	delete [] values;
}
