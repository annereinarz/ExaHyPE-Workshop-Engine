#include "exahype/plotters/Carpet/ADERDG2Carpet.h"

#include <cstdlib>
#include <stdio.h>
#include <sstream>
#include <memory>
#include <limits> // signaling_NaN

#include "exahype/plotters/Carpet/CarpetHDF5Writer.h"

#include "kernels/KernelUtils.h" // indexing
#include "peano/utils/Loop.h" // dfor
#include "kernels/DGMatrices.h"
#include "exahype/solvers/ADERDGSolver.h"
#include "kernels/DGBasisFunctions.h"
#include "tarch/logging/Log.h"
#include <sstream>


std::string exahype::plotters::ADERDG2CarpetHDF5::getIdentifier() {
	return std::string("Carpet::Cartesian::Vertices::HDF5");
}

std::string exahype::plotters::ADERDG2CarpetASCII::getIdentifier() {
	return std::string("Carpet::Cartesian::Vertices::ASCII");
}


typedef tarch::la::Vector<DIMENSIONS, double> dvec;
typedef tarch::la::Vector<DIMENSIONS, int> ivec;

tarch::logging::Log exahype::plotters::ADERDG2Carpet::_log("exahype::plotters::ADERDG2Carpet");

exahype::plotters::ADERDG2Carpet::ADERDG2Carpet(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::plotters::CarpetWriter::FileFormat format) :
    Device(postProcessing), format(format) { writer = nullptr; }

exahype::plotters::ADERDG2Carpet::~ADERDG2Carpet() {
	if(writer) delete writer;
}

void exahype::plotters::ADERDG2Carpet::init(const std::string& filename, int basisSize, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView  plotterParameters) {
	// Determine names of output fields
	char **writtenQuantitiesNames = new char*[writtenUnknowns];
	std::fill_n(writtenQuantitiesNames, writtenUnknowns, nullptr);
	_postProcessing->writtenQuantitiesNames(writtenQuantitiesNames);
	
	writer = exahype::plotters::CarpetWriter::newCarpetWriterFor(format, filename, basisSize, solverUnknowns, writtenUnknowns, plotterParameters, writtenQuantitiesNames);
}

void exahype::plotters::ADERDG2Carpet::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& aderdgCellDescription  = cellInfo._ADERDGCellDescriptions[element];

  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    double* solverSolution = static_cast<double*>(aderdgCellDescription.getSolution());

    plotPatch(
        aderdgCellDescription.getOffset(),
        aderdgCellDescription.getSize(), solverSolution,
        aderdgCellDescription.getTimeStamp(),
	aderdgCellDescription.getRefinementStatus() // interestingly this method is always available, even in non-limiter context
    );
  }
}

void exahype::plotters::ADERDG2Carpet::plotPatch(const dvec& offsetOfPatch, const dvec& sizeOfPatch, double* u, double timeStamp, int limiterStatus) {
    if(writer->slicer && !writer->slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) return;

    const int basisSize = writer->basisSize;
    const int order     = basisSize - 1;
    const dvec dx       = 1./order * sizeOfPatch;
    double* mappedCell;

    if(writer->slicer && writer->slicer->getIdentifier() == "CartesianSlicer") {
	mappedCell = new double[writer->writtenFieldsSize];
	
	interpolateCartesianSlicedPatch(offsetOfPatch, sizeOfPatch, dx, u, mappedCell, timeStamp, limiterStatus,
		static_cast<exahype::plotters::CartesianSlicer&>(*writer->slicer));
    } else {
	mappedCell = new double[writer->patchFieldsSize];
	interpolateCartesianPatch(offsetOfPatch, sizeOfPatch, dx, u, mappedCell, timeStamp, limiterStatus);
    }

    delete[] mappedCell;
}


void exahype::plotters::ADERDG2Carpet::startPlotting(double time) {
	_postProcessing->startPlotting(time);
	writer->startPlotting(time);
}

void exahype::plotters::ADERDG2Carpet::finishPlotting() {
	_postProcessing->finishPlotting();
	writer->finishPlotting();
}

void exahype::plotters::ADERDG2Carpet::interpolateCartesianPatch(const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx, double *u, double *mappedCell, double timeStamp, int limiterStatus) {
  const int basisSize = writer->basisSize;
  const int solverUnknowns = writer->solverUnknowns;
  const int order = basisSize-1;

  double* interpoland = new double[solverUnknowns];
  assertion(sizeOfPatch(0)==sizeOfPatch(1)); // expressing this is all for squared cells.
  
  dfor(i,basisSize) {
    for (int unknown=0; unknown < solverUnknowns; unknown++) {
      interpoland[unknown] = 0.0;
      dfor(ii,basisSize) { // Gauss-Legendre node indices
        int iGauss = peano::utils::dLinearisedWithoutLookup(ii,order + 1);
        interpoland[unknown] +=
		kernels::equidistantGridProjector1d[order][ii(0)][i(0)] *
		kernels::equidistantGridProjector1d[order][ii(1)][i(1)] *
		#if DIMENSIONS==3
		kernels::equidistantGridProjector1d[order][ii(2)][i(2)] *
		#endif
		u[iGauss * solverUnknowns + unknown];
        assertion3(interpoland[unknown] == interpoland[unknown], offsetOfPatch, sizeOfPatch, iGauss);
      }
    }

    double *value = mappedCell + (DIMENSIONS == 3 ? writer->patchCellIdx->get(i(2),i(1),i(0),0) : writer->patchCellIdx->get(i(1),i(0),0));
    //value += writer->patchCellIdx(i(1),i(0),0); // Transposed position. Correct.

    _postProcessing->mapQuantities(
      offsetOfPatch,
      sizeOfPatch,
      offsetOfPatch + i.convertScalar<double>()* (sizeOfPatch(0)/(order)),
      i,
      interpoland,
      value,
      timeStamp
    );
  }
  delete[] interpoland;

  writer->plotPatch(offsetOfPatch, sizeOfPatch, dx, mappedCell, timeStamp, limiterStatus);
}


void exahype::plotters::ADERDG2Carpet::interpolateCartesianSlicedPatch(const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx, double *u, double *mappedCell, double timeStamp, int limiterStatus, const exahype::plotters::CartesianSlicer& slicer) {
  const int basisSize = writer->basisSize;
  const int solverUnknowns = writer->solverUnknowns;
  const int order = basisSize-1;

  double* interpoland = new double[solverUnknowns];  
  assertion(sizeOfPatch(0)==sizeOfPatch(1)); // expressing this is all for squared cells.

  // for the reduced offfsetOfPatch, sizeOfPatch to put into the invalid positions
  double empty_slot = std::numeric_limits<double>::signaling_NaN();

  if(slicer.targetDim == 2) {
	// Determine a position ontop the 2d plane
	dvec plane = slicer.project(offsetOfPatch);
	ivec i;
	for(i(1)=0; i(1)<basisSize; i(1)++)
	for(i(0)=0; i(0)<basisSize; i(0)++) {
		dvec pos = plane + slicer.project(i).convertScalar<double>() * (sizeOfPatch(0)/(order));
		
		for (int unknown=0; unknown < solverUnknowns; unknown++) {
			interpoland[unknown] = kernels::interpolate(
				offsetOfPatch.data(),
				sizeOfPatch.data(),
				pos.data(),
				solverUnknowns,
				unknown,
				order,
				u
			);
		}
		
		double *value = mappedCell + writer->writtenCellIdx->get(i(1),i(0),0);
		
		_postProcessing->mapQuantities(
			offsetOfPatch,
			sizeOfPatch,
			pos,
			i,
			interpoland,
			value,
			timeStamp
		);
	}
	
	// project offset and size of 2D patch onto the plane
	// Todo: ifdef dimensions == 2 case adden
	dvec offsetOfPatch_2D(offsetOfPatch(slicer.runningAxes(0)), offsetOfPatch(slicer.runningAxes(1)), empty_slot);
	dvec sizeOfPatch_2D(sizeOfPatch(slicer.runningAxes(0)), sizeOfPatch(slicer.runningAxes(1)), empty_slot);
	dvec dx_2D(dx(slicer.runningAxes(0)), dx(slicer.runningAxes(1)), empty_slot);
	
	writer->plotPatch(offsetOfPatch_2D, sizeOfPatch_2D, dx_2D, mappedCell, timeStamp, limiterStatus);
  } else if(slicer.targetDim == 1) {
	// Determine a position ontop the 1d line
	dvec line = slicer.project(offsetOfPatch);
	ivec i;
	for(i(0)=0; i(0)<basisSize; i(0)++) {
		dvec pos = line + slicer.project(i).convertScalar<double>() * (sizeOfPatch(0)/(order));
		
		for (int unknown=0; unknown < solverUnknowns; unknown++) {
			interpoland[unknown] = kernels::interpolate(
				offsetOfPatch.data(),
				sizeOfPatch.data(),
				pos.data(),
				solverUnknowns,
				unknown,
				order,
				u
			);
		}
		
		double *value = mappedCell + writer->writtenCellIdx->get(i(0));
		
		_postProcessing->mapQuantities(
			offsetOfPatch,
			sizeOfPatch,
			pos,
			i,
			interpoland,
			value,
			timeStamp
		);
	}
	
	// project offset and size of 1D patch onto the plane
	// Todo: ifdef dimensions == 2 case adden
	dvec offsetOfPatch_1D(offsetOfPatch(slicer.runningAxes(0)), empty_slot, empty_slot);
	dvec sizeOfPatch_1D(sizeOfPatch(slicer.runningAxes(0)), empty_slot, empty_slot);
	dvec dx_1D(dx(slicer.runningAxes(0)), empty_slot, empty_slot);
	
	writer->plotPatch(offsetOfPatch_1D, sizeOfPatch_1D, dx_1D, mappedCell, timeStamp, limiterStatus);
  } else {
	  throw std::invalid_argument("Unupported target dimension.");
  }
  
  delete[] interpoland;
}

