#include "exahype/plotters/FiniteVolumes2ProbeAscii.h"

#include <fstream>
#include <cstring>

#include "peano/utils/Loop.h"

#include "exahype/solvers/FiniteVolumesSolver.h"

tarch::logging::Log exahype::plotters::FiniteVolumes2ProbeAscii::_log( "exahype::plotters::FiniteVolumes2ProbeAscii" );


exahype::plotters::FiniteVolumes2ProbeAscii::FiniteVolumes2ProbeAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth):
  Device(postProcessing),
  _out(nullptr),
  _ghostLayerWidth(ghostLayerWidth
) {}


exahype::plotters::FiniteVolumes2ProbeAscii::~FiniteVolumes2ProbeAscii() {
  if (_out!=nullptr) {
    _out->close();
    _out=nullptr;
  }
}


void exahype::plotters::FiniteVolumes2ProbeAscii::startPlotting( double time ) {
  // In the very first time step, the
  if (time==std::numeric_limits<double>::infinity() ) {
    _time = 0.0;
  }
  else {
    _time = time;
  }
}


void exahype::plotters::FiniteVolumes2ProbeAscii::finishPlotting() {
  if (_out!=nullptr && *_out) {
    (*_out) << std::endl;
  }
}


void exahype::plotters::FiniteVolumes2ProbeAscii::init(const std::string& filename,
                                                       int numberOfCellsPerAxis,
                                                       int unknowns,
                                                       int writtenUnknowns,
                                                       exahype::parser::ParserView plotterParameters){
  _numberOfCellsPerAxis = numberOfCellsPerAxis;
  _solverUnknowns       = unknowns;
  _writtenUnknowns      = writtenUnknowns;
  _plotterParameters    = plotterParameters;
  _filename             = filename;

  if (!plotterParameters.isValueValidDouble("select/x") || !plotterParameters.isValueValidDouble("select/y") || ( DIMENSIONS==3 &&!plotterParameters.isValueValidDouble("select/z"))) {
    logError("init()", "Probe location is invalid. Require x,y,z values. Have " << plotterParameters.dump());
  }
  
  _x(0) = plotterParameters.getValueAsDouble("select/x");
  _x(1) = plotterParameters.getValueAsDouble("select/y");
  #if DIMENSIONS==3
  _x(2) = plotterParameters.getValueAsDouble("select/z");
  #endif

  logDebug( "init(...)", "probe at location " << _x << "(plotterParameters=\"" << plotterParameters.dump() << "\")");

  if (!tarch::la::equals(_x,_x)) {
    logError( "init(...)", "Probe location is invalid." );
  }
}


void exahype::plotters::FiniteVolumes2ProbeAscii::openOutputStream() {
  if (_out == nullptr) {
    if (tarch::la::oneEquals(_x,std::numeric_limits<double>::quiet_NaN())) {
      logError( "init(...)", "probe requires valid x, y (and z) coordinates in plotterParameters statement. No plot written as plot location has been " << _x );
    }
    else {
      _out = new std::ofstream;

      std::ostringstream outputFilename;
      outputFilename << _filename
                   #ifdef Parallel
	           << "-rank-" << tarch::parallel::Node::getInstance().getRank()
                   #endif
		           << ".probe";
      _out->open( outputFilename.str() );
      
      // See issue #47 for discussion whether to quit program on failure
      if(*_out && _out->fail()) {
         logError("openOutputStream(...)", "Could not open file '" << outputFilename.str() << "': " << strerror(errno));
	 exit(-2);
      }

      if (*_out) {
        (*_out) << "# plot-time, real-time";
        for (int unknown=0; unknown < _writtenUnknowns; unknown++) {
          std::ostringstream identifier;
          identifier << "Q" << unknown;
          (*_out) << "," << identifier.str();
        }
        (*_out) << std::endl;
      }
    }
  }

  if (_out!=nullptr && *_out  ) {
    (*_out) << _time;
  }
}


std::string exahype::plotters::FiniteVolumes2ProbeAscii::getIdentifier() {
  return "probe::ascii";
}

void exahype::plotters::FiniteVolumes2ProbeAscii::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {

  const int element = cellInfo.indexOfFiniteVolumesCellDescription(solverNumber);
  auto& cellDescription = cellInfo._FiniteVolumesCellDescriptions[element];

  if (cellDescription.getType()==exahype::solvers::FiniteVolumesSolver::CellDescription::Type::Cell) {
    double* solution = static_cast<double*>(cellDescription.getSolution());

    plotPatch(
        cellDescription.getOffset(),
        cellDescription.getSize(), solution,
        cellDescription.getTimeStamp());
  }
}

void exahype::plotters::FiniteVolumes2ProbeAscii::plotPatch(
  const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
  const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
  double timeStamp
) {
  if (
    tarch::la::allSmallerEquals(offsetOfPatch,_x)
    &&
    tarch::la::allGreater(offsetOfPatch+sizeOfPatch,_x)
  ) {
    // lazy opening
    openOutputStream();

    double* interpoland = new double[_solverUnknowns];
    double* value       = _writtenUnknowns==0 ? nullptr : new double[_writtenUnknowns];

    int ind[DIMENSIONS];
    for (int d = 0 ; d < DIMENSIONS ; ++d){
      ind[d] = (int)std::floor((_x[d] - offsetOfPatch[d])/sizeOfPatch[d] * (_numberOfCellsPerAxis)) + _ghostLayerWidth;
    }

    int lin_index=0;
    for (int d = DIMENSIONS-1 ; d > -1 ; --d){
      lin_index *= (_numberOfCellsPerAxis+ 2 * _ghostLayerWidth);
      lin_index += ind[d];
    }
    
    
    for (int unknown=0; unknown < _solverUnknowns; unknown++) {
      interpoland[unknown] = u[lin_index * _solverUnknowns + unknown];
    }

    _postProcessing->mapQuantities(
      offsetOfPatch,
      sizeOfPatch,
      _x,
      tarch::la::Vector<DIMENSIONS, int>(0),
      interpoland,
      value,
      timeStamp
    );

    if (_out!=nullptr && *_out) {
      (*_out) << ", " << timeStamp;
      for (int i=0; i<_writtenUnknowns; i++) {
        (*_out) << ", " << value[i];
      }
    }

    delete[] interpoland;
    delete[] value;
  }
}
