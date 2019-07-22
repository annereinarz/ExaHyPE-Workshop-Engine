#include "matrixfree/TimeStepper.h"

#include "tarch/Assertions.h"
#include "tarch/la/ScalarOperations.h"

#include <limits>


tarch::logging::Log matrixfree::TimeStepper::_log( "matrixfree::TimeStepper" );


matrixfree::TimeStepper::TimeStepper(
  double  initialTimeStepSize,
  double  maxDifferenceFromTimeStepToTimeStepInMaxNorm,
  double  maxDifferenceFromTimeStepToTimeStepInHNorm,
  double  deltaInBetweenTwoSnapshots,
  bool    printInitialSolution
):
  _time(0.0),
  _timeStepSize( initialTimeStepSize ),
  _maxDifferenceFromTimeStepToTimeStepInMaxNorm( maxDifferenceFromTimeStepToTimeStepInMaxNorm ),
  _maxDifferenceFromTimeStepToTimeStepInHNorm( maxDifferenceFromTimeStepToTimeStepInHNorm ),
  _timeInBetweenTwoSnapshots(deltaInBetweenTwoSnapshots),
  _nextSnapshotIsDue(deltaInBetweenTwoSnapshots==0 ? std::numeric_limits<double>::max() : 0.0),
  _printInitialCondition(printInitialSolution) {
  assertion1( _timeStepSize>=0.0, _timeStepSize );
}


void matrixfree::TimeStepper::setMaxDifferenceFromTimeStepToTimeStepInMaxNorm( double maxDifferenceFromTimeStepToTimeStepInMaxNorm ) {
  _maxDifferenceFromTimeStepToTimeStepInMaxNorm = maxDifferenceFromTimeStepToTimeStepInMaxNorm;
  assertion1( _maxDifferenceFromTimeStepToTimeStepInMaxNorm>=0, _maxDifferenceFromTimeStepToTimeStepInMaxNorm );
}


void matrixfree::TimeStepper::setMaxDifferenceFromTimeStepToTimeStepInHNorm( double maxDifferenceFromTimeStepToTimeStepInHNorm ) {
  _maxDifferenceFromTimeStepToTimeStepInHNorm = maxDifferenceFromTimeStepToTimeStepInHNorm;
  assertion1( _maxDifferenceFromTimeStepToTimeStepInHNorm>=0, _maxDifferenceFromTimeStepToTimeStepInHNorm );
}


void matrixfree::TimeStepper::setDeltaInBetweenTwoSnapshots( double deltaInBetweenTwoSnapshots ) {
  if (deltaInBetweenTwoSnapshots==0) {
    _nextSnapshotIsDue = std::numeric_limits<double>::max();
  }
  else {
    _nextSnapshotIsDue = 0;
  }
  _timeInBetweenTwoSnapshots = deltaInBetweenTwoSnapshots;
}


double matrixfree::TimeStepper::getTime() const {
  return _time;
}

double matrixfree::TimeStepper::getTimeStepSize() const {
  return _timeStepSize;
}

void matrixfree::TimeStepper::switchToNextTimeStep() {
  assertion1( _timeStepSize>0.0, _timeStepSize );
  _time += _timeStepSize;
}


bool matrixfree::TimeStepper::shallWriteSnapshot() const {
  return _printInitialCondition || _time > _nextSnapshotIsDue;
}


void matrixfree::TimeStepper::wroteSnapshot() {
  if (_timeInBetweenTwoSnapshots==0.0) {
    _nextSnapshotIsDue = std::numeric_limits<double>::max();
  };

  while (_nextSnapshotIsDue<=_time) {
    _nextSnapshotIsDue += _timeInBetweenTwoSnapshots;
  }

  _printInitialCondition = false;
}


bool matrixfree::TimeStepper::isTimeStepSizeWellSuitedToProceed(
  double updateInHNorm,
  double updateInMaxNorm,
  bool   linearEquationSystemHasTerminatedWithLessIterationsThanNecessary
) const {
  bool hasToReduceTimeStepDueToHNorm    = updateInHNorm   > _maxDifferenceFromTimeStepToTimeStepInHNorm;
  bool hasToReduceTimeStepDueToMaxNorm  = updateInMaxNorm > _maxDifferenceFromTimeStepToTimeStepInMaxNorm;

  if (!linearEquationSystemHasTerminatedWithLessIterationsThanNecessary || hasToReduceTimeStepDueToHNorm || hasToReduceTimeStepDueToMaxNorm) {
    return false;
  }
  else {
    return true;
  }
}


void matrixfree::TimeStepper::computeNewTimeStepSize(
  double updateInHNorm,
  double updateInMaxNorm,
  bool   linearEquationSystemHasTerminatedWithLessIterationsThanNecessary
) {
  logTraceInWith3Arguments( "computeNewTimeStepSize(double,double,bool)", updateInHNorm, updateInMaxNorm, linearEquationSystemHasTerminatedWithLessIterationsThanNecessary );

  assertion(updateInHNorm>=0.0);
  assertion(updateInMaxNorm>=0.0);

  if (_maxDifferenceFromTimeStepToTimeStepInMaxNorm==0.0) {
    logTraceOut( "computeNewTimeStepSize(double,double,bool)" );
    return;
  }
  if (_maxDifferenceFromTimeStepToTimeStepInHNorm==0.0) {
    logTraceOut( "computeNewTimeStepSize(double,double,bool)" );
    return;
  }

  if ( isTimeStepSizeWellSuitedToProceed(updateInHNorm,updateInMaxNorm,linearEquationSystemHasTerminatedWithLessIterationsThanNecessary) ) {
    const double timeStepExtrapolationFactor = 9.0;
    bool couldCoarseTimeStepDueToHNorm    = updateInHNorm*timeStepExtrapolationFactor   < _maxDifferenceFromTimeStepToTimeStepInHNorm;
    bool couldCoarseTimeStepDueToMaxNorm  = updateInMaxNorm*timeStepExtrapolationFactor < _maxDifferenceFromTimeStepToTimeStepInMaxNorm;

    if (couldCoarseTimeStepDueToHNorm && couldCoarseTimeStepDueToMaxNorm) {
      logInfo( "computeNewTimeStepSize(double,double,bool)", "solution update of (max=" << updateInMaxNorm << ", h=" << updateInHNorm << ") is smaller than threshold (max=" << _maxDifferenceFromTimeStepToTimeStepInMaxNorm << ",h=" << _maxDifferenceFromTimeStepToTimeStepInHNorm << "), so we increase time step size" );
      _timeStepSize *= 3.0;
    }
  }
  else {
    logInfo( "computeNewTimeStepSize(double,double,bool)", "reduce time step size" );
    _timeStepSize /= 3.0;
  }

  if ( tarch::la::equals( _timeStepSize, std::numeric_limits<double>::min() ) ) {
    logError( "computeNewTimeStepSize(double,double,bool)", "time step size underruns zero" );
  }

  logTraceOutWith1Argument( "computeNewTimeStepSize(double,double,bool)", _timeStepSize );
}
