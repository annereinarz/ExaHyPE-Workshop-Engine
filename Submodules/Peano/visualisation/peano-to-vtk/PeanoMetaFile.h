/*
 * PeanoMetaReader.h
 *
 *  Created on: 26 Nov 2017
 *      Author: dan
 */

#ifndef PEANOMETAFILE_H_
#define PEANOMETAFILE_H_

#include <string>
#include <vector>

#include "PeanoDataSet.h"
#include "PeanoReader.h"

/**
 * Meta files are files that hold time series or reduced resolution images.
 */
class PeanoMetaFile {
  public:
//	PeanoMetaFile();
	/**
	 * Open a meta file and parse its content
	 */
	PeanoMetaFile(std::string file);
	virtual ~PeanoMetaFile();

	std::vector<PeanoDataSet*>* getDataSets();
	PeanoDataSet* getDataSet(int index);
	int numberOfDataSets();
	void save();
  private:
	static std::string getDirectory(const std::string &fileName);
	std::string fileName;
	std::vector<PeanoDataSet*>* dataSets;
	std::string directory;
};

#endif /* PEANOMETAFILE_H_ */
