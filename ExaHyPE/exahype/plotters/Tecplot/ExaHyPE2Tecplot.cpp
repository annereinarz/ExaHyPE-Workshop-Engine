#include "exahype/plotters/Tecplot/ExaHyPE2Tecplot.h"


#include "tarch/logging/Log.h"


#include <cstdlib>
#include <stdio.h>
#include <sstream>
#include <memory>
#include <limits> // signaling_NaN


std::string exahype::plotters::ExaHyPE2Tecplot::getIdentifier() {
	return std::string("Tecplot::Binary");
}

tarch::logging::Log exahype::plotters::ExaHyPE2Tecplot::_log("exahype::plotters::ExaHyPE2Tecplot");


#ifndef TECPLOT
/*************************************************************************************************
 * ExaHyPE2Tecplot Dummy implementation in case TECPLOT support is skipped.
 * Probably such a section is (except the constructor) not neccessary as the methods are never
 * referenced/called.
 *************************************************************************************************/

exahype::plotters::ExaHyPE2Tecplot::ExaHyPE2Tecplot(
  exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, int ghostLayerWidth) : Device(postProcessing) {
	logError("ExaHyPE2Tecplot()", "ERROR: Compile with TECPLOT, otherwise you cannot use the Tecplot plotter.");
	// if(std::getenv("EXAHYPE_STRICT"))
	abort();
}

// all other methods are stubs
exahype::plotters::ExaHyPE2Tecplot::~ExaHyPE2Tecplot() {}

void exahype::plotters::ExaHyPE2Tecplot::init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters) {
	logError("init()","ERROR: Compile with TECPLOT, otherwise you cannot use the Tecplot plotter. There will be no output going to " << filename << " today.");
	logError("init()", "Will fail gracefully. If you want to stop the program in such a case, please set the environment variable EXAHYPE_STRICT=\"Yes\".");
}
void exahype::plotters::ExaHyPE2Tecplot::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {}
void exahype::plotters::ExaHyPE2Tecplot::startPlotting(double time) {
	logError("startPlotting()", "Skipping HDF5 output due to missing support.");
}
void exahype::plotters::ExaHyPE2Tecplot::finishPlotting() {}

#else

// Define the Fortran routines here.

void exahypetecplotwriter_init_(char* filename, int* filename_len, int* solverType, int* basisSize, int* writtenUnkowns) {}

// Additional headers and helper routines

#include "exahype/solvers/ADERDGSolver.h"

/**
 * Tries to convert a C string to a fixed-length Fortran string.
 * Returns true in case of success and false if the memory
 * is too small.
 **/
bool ConvertToFortran(char* fstring, std::size_t fstring_len,
                      const char* cstring) {
    std::size_t inlen = std::strlen(cstring);
    std::size_t cpylen = std::min(inlen, fstring_len);

    if (inlen > fstring_len) return false;

    std::copy(cstring, cstring + cpylen, fstring);
    std::fill(fstring + cpylen, fstring + fstring_len, ' ');
    return true;
}

/*************************************************************************************************
 * ExaHyPE2Tecplot non-dummy implementation
 *************************************************************************************************/

exahype::plotters::ExaHyPE2Tecplot::ExaHyPE2Tecplot(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, int ghostLayerWidth) :
    Device(postProcessing), _ghostLayerWidth(ghostLayerWidth) {}

exahype::plotters::ExaHyPE2Tecplot::~ExaHyPE2Tecplot() {}

void exahype::plotters::ExaHyPE2Tecplot::init(const std::string& filename, int basisSize, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters) {
	// init is called at program startup.
	
	// Remember the parameters for later use
	_filename = filename;
	_orderPlusOne = basisSize;
	_solverUnknowns = solverUnknowns;
	_writtenUnknowns = writtenUnknowns;
	// plotterParameters is for query-based visualization, ignore it here.
	
	// Call Fortran.
	char filename_for_Fortran[1000];
	if(!ConvertToFortran(filename_for_Fortran, 1000, filename.c_str())) {
		logError("init()", "Requested plotting filename exceeds the fixed-length Fortran buffer.");
		std::abort();
	}

	int solverType = 12345; // make 1,2 or 3 depending on DG, FV, Limiter or so.
	int flen = 0; // filename_len
	
	exahypetecplotwriter_init_(filename_for_Fortran, &flen, &solverType, &basisSize, &writtenUnknowns);
}

void exahype::plotters::ExaHyPE2Tecplot::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& aderdgCellDescription  = cellInfo._ADERDGCellDescriptions[element];

  // you can also use _solverType to see whether you have an ordinary ADERDG solver
  // or a limiting solver
  
  // this if allows you to understand whether you have an ADERDG cell or
  // a limiting cell or whatever.
  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    // The internal cell structure is (order,order,order,nVar) in C and 3D.
    double* solverSolution = static_cast<double*>(aderdgCellDescription.getSolution());

    // vectors of length DIMENSIONS:
    double* cellOffset = aderdgCellDescription.getOffset().data();
    double* cellSize = aderdgCellDescription.getSize().data();
    
    double time = aderdgCellDescription.getTimeStamp();
    
    int order     = _orderPlusOne - 1;
    // compute the dx as vector for instance with:
    // vector  dx    = 1./order * sizeOfPatch;

  } // if cellldescription == ADERDG cell
}

void exahype::plotters::ExaHyPE2Tecplot::startPlotting(double time) {
	_postProcessing->startPlotting(time);
	
	// This is called when a new plotting grid swipe happens
}

void exahype::plotters::ExaHyPE2Tecplot::finishPlotting() {
	_postProcessing->finishPlotting();
	
	// This is called when a plotting grid swipe ends.

}

#endif /* TECPLOT */
