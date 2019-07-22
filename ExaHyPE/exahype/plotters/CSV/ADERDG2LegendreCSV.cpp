/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon 
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/
 
#include "ADERDG2LegendreCSV.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"
#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/DGBasisFunctions.h"
#include "kernels/KernelUtils.h"

#include "peano/utils/Loop.h"

#include "exahype/solvers/ADERDGSolver.h"

#include <iomanip>

std::string exahype::plotters::ADERDG2LegendreCSV::getIdentifier() {
  return "csv::Legendre::vertices::ascii";
}


exahype::plotters::ADERDG2LegendreCSV::ADERDG2LegendreCSV(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing):
  Device(postProcessing),
  oneFilePerTimestep(false),
  allUnknownsInOneFile(true),
  writeCSVHeader(true),
  writeCommentHeader(true),
  writeDebugLines(true),
  writeTimeColumn(true),
  comment("#"),
  seperator("\t"),
  endl("\n"),
  precision(8),
  fileCounter(-1),
  basicFilename("undefined"),
  appendix(".csv"),
  order(-1),
  solverUnknowns(-1),
  writtenUnknowns(-1),
  writtenQuantitiesNames(nullptr)
  {}

exahype::plotters::ADERDG2LegendreCSV::~ADERDG2LegendreCSV() {}

void exahype::plotters::ADERDG2LegendreCSV::init(const std::string& filename, int orderPlusOne, int  unknowns, int writtenUnknowns,  exahype::parser::ParserView plotterParameters) {
	this->basicFilename     = basicFilename;
	this->order             = orderPlusOne-1;
	this->solverUnknowns    = unknowns;
	this->writtenUnknowns   = writtenUnknowns;
	
	// Determine names of output fields
	writtenQuantitiesNames = new char*[writtenUnknowns];
	std::fill_n(writtenQuantitiesNames, writtenUnknowns, nullptr);
	_postProcessing->writtenQuantitiesNames(writtenQuantitiesNames);
	
	if(allUnknownsInOneFile) openNewFile();
}

void exahype::plotters::ADERDG2LegendreCSV::startPlotting(double time) {
	if(!allUnknownsInOneFile) openNewFile();
	_postProcessing->startPlotting(time);
}

void exahype::plotters::ADERDG2LegendreCSV::openNewFile() {
	fileCounter++;
	std::stringstream cur_filename;
	cur_filename << basicFilename << "-" << fileCounter << appendix;
	ofs.open(cur_filename.str(), std::ofstream::out);
	ofs << std::setprecision(precision);
	writeHeader();
}

void exahype::plotters::ADERDG2LegendreCSV::finishPlotting() {
	if(!allUnknownsInOneFile)
		ofs.close();
	_postProcessing->finishPlotting();
}

void exahype::plotters::ADERDG2LegendreCSV::writeHeader() {
	const char* coordinates = "xyz";
	const char* timecolname = "t";
	int timeColLength = writeTimeColumn ? 1 : 0;
	if(writeCommentHeader) {
		ofs << comment << " ExaHyPE ADERDG2LegendreCSV text data" << endl;
		ofs << comment << " Written by ... at ... on ... etc." << endl;
		ofs << comment << " Self-Describing: Print ADERDG2LegendreCSV status information" << endl;
		ofs << comment << endl;
		
		// in any case, write in comment the columns, this is handy for
		// gnuplot and many more (Carpet does it, too). This is primarily
		// for human readableness
		if(writtenUnknowns>0) {
			const int breakAfter = 10;
			const std::string shortColInd(":"); ///< indicator, like in 0:t 1:x 2:y
			const std::string shortColSep(" "); ///< Seperator in short column list
			const std::string defaultNamePrefix("Q"); ///< if no writtenQuantitiesNames given, this becomes Q0, Q1, ...
			ofs << comment << shortColSep;
			// start with coordinates
			int i=0;
			if(writeTimeColumn)
				ofs << i << shortColInd << timecolname << shortColSep;
			for(; i<DIMENSIONS+timeColLength; i++) {
				ofs << i << shortColInd;
				ofs << coordinates[i];
				ofs << shortColSep;
			}
			for(; i<writtenUnknowns+DIMENSIONS+timeColLength; i++) {
				if(i > breakAfter)
					ofs << endl << comment << shortColSep; 
				ofs << i << shortColInd;
				if(writtenQuantitiesNames[i])
					ofs << writtenQuantitiesNames[i];
				else	ofs << defaultNamePrefix << i;
				if(i<writtenUnknowns+DIMENSIONS+timeColLength-1)
					ofs << shortColSep;
			}
			ofs << endl;
		} // end of column description in comments
	} // end of comment header
	if(writeCSVHeader) {
		// this header line is for machine readbleness.
		// this is the only line where strings appear.
		int i=0;
		if(writeTimeColumn)
			ofs << timecolname << seperator;
		for(;i<DIMENSIONS+timeColLength;i++)
			ofs << coordinates[i] << seperator;
		for(; i<writtenUnknowns+DIMENSIONS+timeColLength;i++) {
			if(writtenQuantitiesNames[i])
				ofs << writtenQuantitiesNames[i];
			else	ofs << "Q" << i;
			if(i<writtenUnknowns+DIMENSIONS+timeColLength-1)
				ofs << seperator;
		}
		ofs << endl;
	}
}

void exahype::plotters::ADERDG2LegendreCSV::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& aderdgCellDescription  = cellInfo._ADERDGCellDescriptions[element];

  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    double* solverSolution = static_cast<double*>(aderdgCellDescription.getSolution());

	// Old Debugging information by Vasco. We can probably recycle some of them
	// or obtain them directly from the aderdgCellDescription.
	// 
	// cout << offsetOfPatch(0) << endl;

	// int treeDepth = log(sizeOfPatch(0))/log(1./3.) + eps;
	// double increment = pow(1./3., treeDepth);
	// int xIndex = offsetOfPatch(0)/increment + eps;
	// int yIndex = offsetOfPatch(1)/increment + eps;
	// int elementsPerAxis = pow(3., treeDepth) + eps;

	// cout << offsetOfPatch << endl;
	// cout << sizeOfPatch << endl;
	// cout << "treeDepth= " << treeDepth << endl;
	// cout << "elementsPerAxis= " << elementsPerAxis << endl;
	// cout << "increment= " << increment << endl;
	// cout << "yIndex= " << yIndex << endl;
	// cout << "xIndex= " << xIndex << endl;
	// cout << "index= " << yIndex*elementsPerAxis + xIndex << endl;
    
    plotPatch(
        aderdgCellDescription.getOffset(),
        aderdgCellDescription.getSize(), solverSolution,
        aderdgCellDescription.getTimeStamp());
  }
} // plotPatch(cellDescription)

void exahype::plotters::ADERDG2LegendreCSV::plotPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u,
    double timeStamp) {
	
	double* mappedUnknowns       = writtenUnknowns==0 ? nullptr : new double[writtenUnknowns];

	// this should go to the header or similar
	const int basisX = order + 1;
	const int basisY = order + 1;
	const int basisZ = (DIMENSIONS == 3 ? order  : 0 ) + 1;
	kernels::index idx_u(basisZ, basisY, basisX, solverUnknowns);


	dfor(i,order+1) {
		tarch::la::Vector<DIMENSIONS, double> pos;
		for (int d=0; d<DIMENSIONS; d++) {
			pos(d) = offsetOfPatch(d) + kernels::gaussLegendreNodes[order][i(d)] * sizeOfPatch(d);
		}
		
		_postProcessing->mapQuantities(offsetOfPatch, sizeOfPatch, pos, i,
			u + idx_u(DIMENSIONS == 3 ? i(2) : 0, i(1), i(0), 0),
			mappedUnknowns, timeStamp);

		// early NaN check:
		for( int i=0; i<writtenUnknowns; i++) {
			assertion3( std::isfinite(mappedUnknowns[i]), mappedUnknowns[i], offsetOfPatch, sizeOfPatch);
		}

		if(writtenUnknowns>0)
			writeRow(mappedUnknowns, pos.data(), timeStamp);
	}
} // plotPatch(offsetOfPatch,sizeofPatch,u,timeStamp)

void exahype::plotters::ADERDG2LegendreCSV::writeRow(const double* const mappedUnknowns, const double* const coordinates, const double timeStamp) {
	if(writeTimeColumn)
		ofs << timeStamp << seperator;
	for(int d=0; d<DIMENSIONS; d++) {
		ofs << coordinates[d] << seperator;
	}
	for(int i=0; i<writtenUnknowns; i++) {
		ofs << mappedUnknowns[i] << seperator;
	}
	ofs << endl;
	// in CSV, a trailing newline can be a problem. Instead, we should
	// probably track the newline status with a dirty flag.
}
