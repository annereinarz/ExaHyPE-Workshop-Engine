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
 *
 * @authors: Sven Koeppel
 **/

#include "exahype/plotters/Carpet/FiniteVolume2Carpet.h"
#include "exahype/solvers/FiniteVolumesSolver.h"

#include <cstdlib>
#include <stdio.h>
#include <sstream>
#include <memory>

#include "exahype/plotters/Carpet/CarpetHDF5Writer.h"
#include "kernels/KernelUtils.h" // indexing
#include "peano/utils/Loop.h" // dfor
#include "kernels/DGMatrices.h"
#include "exahype/solvers/ADERDGSolver.h"
#include "kernels/DGBasisFunctions.h"
#include "tarch/logging/Log.h"
#include <sstream>

#include <fenv.h> // NAN tracker

typedef tarch::la::Vector<DIMENSIONS,int> ivec;


std::string exahype::plotters::FiniteVolume2CarpetHDF5::getIdentifier() {
	return std::string("Carpet::Cartesian::Vertices::HDF5");
}

std::string exahype::plotters::FiniteVolume2CarpetASCII::getIdentifier() {
	return std::string("Carpet::Cartesian::Vertices::ASCII");
}


// my small C++11 to_string-independent workaround.
template <typename T> inline std::string toString( T Number ) {
	std::ostringstream ss; ss << Number; return ss.str();
}

typedef tarch::la::Vector<DIMENSIONS, double> dvec;

exahype::plotters::FiniteVolume2Carpet::FiniteVolume2Carpet(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
  const int _ghostLayerWidth, exahype::plotters::CarpetWriter::FileFormat format)
	: Device(postProcessing), ghostLayerWidth(_ghostLayerWidth), format(format)
	{ writer = nullptr; }

exahype::plotters::FiniteVolume2Carpet::~FiniteVolume2Carpet() {
	if(writer) delete writer;
}

void exahype::plotters::FiniteVolume2Carpet::init(const std::string& filename, int _numberOfCellsPerAxis, int _solverUnknowns, int writtenUnknowns, exahype::parser::ParserView  plotterParameters) {
	// Determine names of output fields
	char **writtenQuantitiesNames = new char*[writtenUnknowns];
	std::fill_n(writtenQuantitiesNames, writtenUnknowns, nullptr);
	_postProcessing->writtenQuantitiesNames(writtenQuantitiesNames);
	
	numberOfCellsPerAxis = _numberOfCellsPerAxis;
	numberOfVerticesPerAxis = _numberOfCellsPerAxis + 1;
	solverUnknowns = _solverUnknowns;

	const int basisSize = numberOfVerticesPerAxis;
	writer = exahype::plotters::CarpetWriter::newCarpetWriterFor(format, filename, basisSize, solverUnknowns, writtenUnknowns, plotterParameters, writtenQuantitiesNames);
}

void exahype::plotters::FiniteVolume2Carpet::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfFiniteVolumesCellDescription(solverNumber);
  auto& cellDescription  = cellInfo._FiniteVolumesCellDescriptions[element];
	if (cellDescription.getType()==exahype::solvers::FiniteVolumesSolver::CellDescription::Type::Cell) {
		double* solution = static_cast<double*>(cellDescription.getSolution());

		plotPatch(
			cellDescription.getOffset(),
			cellDescription.getSize(), solution,
			cellDescription.getTimeStamp());
	}
}

void exahype::plotters::FiniteVolume2Carpet::plotPatch(
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u, /* unknown */
  double timeStamp) {

	if(writer->slicer && !writer->slicer->isPatchActive(offsetOfPatch, sizeOfPatch)) return;
	
	dvec dx = 1./numberOfCellsPerAxis * sizeOfPatch;
	double* mappedCell;

	if(writer->slicer && writer->slicer->getIdentifier() == "CartesianSlicer") {
		mappedCell = new double[writer->writtenFieldsSize];
		
		interpolateCartesianSlicedVertexPatch(offsetOfPatch, sizeOfPatch, u, mappedCell, timeStamp,
			static_cast<exahype::plotters::CartesianSlicer&>(*writer->slicer));
	} else {
		mappedCell  = new double[writer->patchFieldsSize];
		interpolateVertexPatch(offsetOfPatch, sizeOfPatch, u, mappedCell, timeStamp);
		writer->plotPatch(offsetOfPatch, sizeOfPatch, dx, mappedCell, timeStamp);
	}
	delete[] mappedCell;
}

void exahype::plotters::FiniteVolume2Carpet::interpolateVertexPatch(
    const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
    const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
    double* u, /* ingoing unknowns in cell, size numberOfCellsPerAxis^DIMENSIONS */
    double *mappedCell, /* outgoing mapped cell, size writtenUnknowns^DIMENSIONS */
    double timeStamp) {

	double* vertexValue = new double[solverUnknowns];

	// the following assumes quadratic cells.
	assertion(sizeOfPatch(0)==sizeOfPatch(1));
	kernels::dindex patchPos(numberOfCellsPerAxis + 2*ghostLayerWidth); // including ghost zones

	dfor(ivertex, numberOfVerticesPerAxis) {
		// We do no smearing, so we only take into account the 2 nearest neighbours.
		constexpr int neighbourCellsPerAxis = 2;
		//constexpr int neighbourCellsMax = std::pow(neighbourCellsPerAxis, DIMENSIONS); // maximum possible cells (ie. 4 in 2D)
		std::fill_n(vertexValue, solverUnknowns, 0.0);
		int neighbourCells = 0; // actual neighbour cells taken into account
		dfor(icells, neighbourCellsPerAxis) {
			ivec icell = ghostLayerWidth + ivertex + (icells - neighbourCellsPerAxis / 2);
			
			// if the target cell position in the patch is *not* in the ghost layers:
			const bool cellTakenIntoAccount =
			   tarch::la::allSmaller(icell,numberOfCellsPerAxis+ghostLayerWidth)
			   &&  tarch::la::allGreater(icell,ghostLayerWidth-1);
			if (cellTakenIntoAccount) {
				double *cell = u + patchPos.rowMajor(icell)*solverUnknowns;
				for (int unknown=0; unknown < solverUnknowns; unknown++) {
					vertexValue[unknown] += cell[unknown];
				}
				neighbourCells++;
			}
		}

		// normalize value
		for (int unknown=0; unknown < solverUnknowns; unknown++) {
			vertexValue[unknown] = vertexValue[unknown] / neighbourCells;
		}
		
		/*
		// The following code could be used instead of the neighbour contributions as
		// above and was used for the start. Just one  cell.
		// This works and shows how badly it is if we rely on ghost zones.
		double *cell = u + patchPos.rowMajor(ghostLayerWidth + ivertex)*solverUnknowns;
		for (int unknown=0; unknown < solverUnknowns; unknown++) {
			vertexValue[unknown] = 42; // cell[unknown];
		}
		*/
		

		double *outputValue = mappedCell + (DIMENSIONS == 3 ?
			writer->writtenCellIdx->get(ivertex(2),ivertex(1),ivertex(0),0) :
			writer->writtenCellIdx->get(ivertex(1),ivertex(0),0));
		_postProcessing->mapQuantities(
		offsetOfPatch,
		sizeOfPatch,
		offsetOfPatch + ivertex.convertScalar<double>()* (sizeOfPatch(0)/(numberOfVerticesPerAxis)), // coordinate of vertex
		ivertex,
		vertexValue,
		outputValue,
		timeStamp
		);
	}

	delete[] vertexValue;
}

void exahype::plotters::FiniteVolume2Carpet::interpolateFVCellAtPoint(
	const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
	const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
	const tarch::la::Vector<DIMENSIONS, double>& manifold_position,
	const tarch::la::Vector<DIMENSIONS, int>& cell_index,
	double* u, /* ingoing unknowns in cell, size numberOfCellsPerAxis^DIMENSIONS */
	double* vertexValue, // intermediate storage, to avoid calling new double[solverUnknowns] all the time
	double* outputWrittenQuantities,
	double timeStamp
) {
	const dvec dx = 1./numberOfCellsPerAxis * sizeOfPatch;
	const kernels::dindex patchPos(numberOfCellsPerAxis + 2*ghostLayerWidth); // including ghost zones
	int neighbourCells = 0; // actual neighbour cells taken into account
	std::fill_n(vertexValue, solverUnknowns, 0.0);
	bool DebugDoNaNcheck = false;
	
	// A dumb way to detect which FV cells to take into account
	std::vector<double> dist, dist2, bari[3];
	dfor(icell, numberOfCellsPerAxis + 2*ghostLayerWidth) {
		// if the target cell position in the patch is *not* in the ghost layers:
		bool isNotInAnyGhostLayer = tarch::la::allSmaller(icell,numberOfCellsPerAxis+ghostLayerWidth) &&  tarch::la::allGreater(icell,ghostLayerWidth-1);
		// Alternatively, allow the exchanged ghost layers but not the corner ones.
		// (This statement is not fully correct, excludes only 2 of 6 corners)
		bool isNotInCornerGhostLayer = !(tarch::la::allSmaller(icell, ghostLayerWidth) || tarch::la::allGreater(icell, numberOfCellsPerAxis+ghostLayerWidth));
		// For the time being, we just ignore the problem ;-)
		bool justIgnoreTheCornerProblem = true;
		if(true) {
			// Determine the baryCenter of a FV cell.
			// Also, attention: Peano seems to compute ivec.convertScalar<double>()*dvec  wrongly.
			dvec baryCenter = offsetOfPatch + (icell - ghostLayerWidth).convertScalar<double>() * dx(0) + 0.5*dx(0);
			
			for(int zz=0;zz<3;zz++) bari[zz].push_back(baryCenter(zz));
			dist.push_back( tarch::la::norm2(baryCenter - manifold_position) );
			dist2.push_back( tarch::la::norm2(manifold_position - baryCenter) );
			
			if(std::pow(tarch::la::norm2(baryCenter - manifold_position), 2) <= dx(0)) {
				for (int unknown=0; unknown < solverUnknowns; unknown++) {
					double *cell = u + patchPos.rowMajor(icell)*solverUnknowns;
					if(DebugDoNaNcheck && cell[unknown] != cell[unknown]) {
						throw std::runtime_error("interpolateFVCellAtPoint: Adding NaN to interpolated value");
					}
					vertexValue[unknown] += cell[unknown];
				}
				neighbourCells++;
			}
		}
	}
	
	if(neighbourCells == 0) {
		throw std::runtime_error("interpolateFVCellAtPoint: Logical flaw: No interpolation points found but slicer told this patch is active.");
	}
	
	// normalize value
	for (int unknown=0; unknown < solverUnknowns; unknown++) {
		// Mind that a division by neighbourCells==0 will give a NaN.
		vertexValue[unknown] = vertexValue[unknown] / neighbourCells;
		if(DebugDoNaNcheck && vertexValue[unknown] != vertexValue[unknown]) {
			throw std::runtime_error("FV2Carpet::interpolateFVCellAtPoint; before Mapping the interpolation gave NaN");
		}
	}
	
	_postProcessing->mapQuantities(
		offsetOfPatch,
		sizeOfPatch,
		manifold_position,
		cell_index, // this value does not have the real meaning as we call the mapper off-FV-grid anyway
		vertexValue,
		outputWrittenQuantities,
		timeStamp
	);
	
	for (int unknown=0; unknown < solverUnknowns; unknown++) {
		if(DebugDoNaNcheck && outputWrittenQuantities[unknown] != outputWrittenQuantities[unknown]) {
			throw std::runtime_error("FV2Carpet::interpolateFVCellAtPoint; after Mapping the interpolation gave NaN");
		}
	}
}

void exahype::plotters::FiniteVolume2Carpet::interpolateCartesianSlicedVertexPatch(
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
  double* u, /* ingoing unknowns in cell, size numberOfCellsPerAxis^DIMENSIONS */
  double *mappedCell, /* outgoing mapped cell, size writtenUnknowns^DIMENSIONS */
  double timeStamp,
  const exahype::plotters::CartesianSlicer& slicer) {
	// const int basisSize = writer->basisSize; // => numberOfVerticesPerAxis
	// const int solverUnknowns = writer->solverUnknowns; // => solverUnknowns
	// const int order = basisSize-1;

	assertion(sizeOfPatch(0)==sizeOfPatch(1)); // expressing this is all for squared cells.
	assertion(sizeOfPatch(0)==sizeOfPatch(DIMENSIONS-1));
	dvec dx = 1./numberOfCellsPerAxis * sizeOfPatch;
	// for the reduced offfsetOfPatch, sizeOfPatch to put into the invalid positions
	double empty_slot = std::numeric_limits<double>::signaling_NaN();
	
	double* vertexValue = new double[solverUnknowns];
	if(slicer.targetDim == 2) {
		// Determine a position on the 2d plane
		dvec plane = slicer.project(offsetOfPatch);
		ivec i(0);
		for(i(1)=0; i(1)<numberOfVerticesPerAxis; i(1)++)
		for(i(0)=0; i(0)<numberOfVerticesPerAxis; i(0)++) {
			// mind the Peano non-working ivec.convertScalar<double>()*dvec!
			dvec planePos = plane + slicer.project(i).convertScalar<double>() * dx(0);
			double *outputValue = mappedCell + writer->writtenCellIdx->get(i(1),i(0),0);
			interpolateFVCellAtPoint(offsetOfPatch, sizeOfPatch, planePos, i, u, vertexValue, outputValue, timeStamp);
		}
		
		// project offset and size of 2D patch onto the plane
		// Todo: ifdef dimensions == 2 case adden
		dvec offsetOfPatch_2D(offsetOfPatch(slicer.runningAxes(0)), offsetOfPatch(slicer.runningAxes(1)), empty_slot);
		dvec sizeOfPatch_2D(sizeOfPatch(slicer.runningAxes(0)), sizeOfPatch(slicer.runningAxes(1)), empty_slot);
		dvec dx_2D(dx(slicer.runningAxes(0)), dx(slicer.runningAxes(1)), empty_slot);
		
		writer->plotPatch(offsetOfPatch_2D, sizeOfPatch_2D, dx_2D, mappedCell, timeStamp);
	} else if(slicer.targetDim == 1) {
		// Determine a position on the 1d line
		dvec line = slicer.project(offsetOfPatch);
		ivec i(0);
		for(i(0)=0; i(0)<numberOfVerticesPerAxis; i(0)++) {
			dvec linePos = line + slicer.project(i).convertScalar<double>() * dx(0);
			double *outputValue = mappedCell + writer->writtenCellIdx->get(i(0));
			interpolateFVCellAtPoint(offsetOfPatch, sizeOfPatch, linePos, i, u, vertexValue, outputValue, timeStamp);
		}
		
		// project offset and size of 1D patch onto the plane
		// Todo: ifdef dimensions == 2 case adden
		dvec offsetOfPatch_1D(offsetOfPatch(slicer.runningAxes(0)), empty_slot, empty_slot);
		dvec sizeOfPatch_1D(sizeOfPatch(slicer.runningAxes(0)), empty_slot, empty_slot);
		dvec dx_1D(dx(slicer.runningAxes(0)), empty_slot, empty_slot);
		
		writer->plotPatch(offsetOfPatch_1D, sizeOfPatch_1D, dx_1D, mappedCell, timeStamp);
	} else {
		throw std::invalid_argument("Unupported target dimension.");
	}
	
	delete[] vertexValue;
}

void exahype::plotters::FiniteVolume2Carpet::startPlotting(double time) {
	_postProcessing->startPlotting(time);
	writer->startPlotting(time);
	//feenableexcept(FE_INVALID | FE_OVERFLOW);
}

void exahype::plotters::FiniteVolume2Carpet::finishPlotting() {
	_postProcessing->finishPlotting();
	writer->finishPlotting();
}
