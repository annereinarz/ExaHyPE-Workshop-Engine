/*
 * PeanoReader.h
 *
 *  Created on: 16 Oct 2017
 *      Author: dan
 */

#ifndef PEANOREADER_H_
#define PEANOREADER_H_

#include <string>
#include <vector>

#include "PeanoVariable.h"
#include "PeanoPatch.h"

/**
 * Represent a Peano reader
 *
 * A reader is an object which basically takes one file and gives the content
 * of this file.
 *
 * If you just wanna in one file, open it with this file and pass the field
 * patches into the PeanoConverter.
 *
 * @author Dan Tuthill-Jones, Tobias Weinzierl
 */
class PeanoReader {
  public:
	/**
	 * Read in one file.
	 *
	 * <h2> Parse process </h2>
	 *
	 * - Load the whole files into a vector called lines, i.e. we load line by line.
	 * -
	 */
	PeanoReader(const std::string &file);
	virtual ~PeanoReader();

	int dimensions = -1;
	int cells = -1;
	int vertices = -1;
	int* patchSize;

	std::vector<PeanoVariable*> variables;
	std::vector<PeanoPatch*> patches;
};

#endif /* PEANOREADER_H_ */
