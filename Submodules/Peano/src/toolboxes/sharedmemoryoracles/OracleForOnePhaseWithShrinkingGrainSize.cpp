#include "sharedmemoryoracles/OracleForOnePhaseWithShrinkingGrainSize.h"
#include "peano/utils/Globals.h"
#include "tarch/Assertions.h"
#include "tarch/multicore/Core.h"


#include <cstdlib>
#include <limits>
#include <fstream>
#include <stdexcept>


tarch::logging::Log  sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::_log( "sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize" );

const double   sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::_InitialRelativeAccuracy(1e-2);
const double   sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::_TimingMax( 65536.0 );
bool           sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::_hasLearnedSinceLastQuery(false);



sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::OracleForOnePhaseWithShrinkingGrainSize(bool learn, bool restart):
  _activeMethodTrace(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling),
  _learn(learn && tarch::multicore::Core::getInstance().getNumberOfThreads()>1),
  _restart(restart),
  _measurements() {
}


bool sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::operator<(const DatabaseEntry& cmp) const {
  return ( _biggestProblemSize<cmp._biggestProblemSize );
}


sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry&  sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::getDatabaseEntry(
  int problemSize,
  peano::datatraversal::autotuning::MethodTrace askingMethod
) {
  if ( _measurements.count(askingMethod)==0 ) {
    _measurements.insert( std::pair<peano::datatraversal::autotuning::MethodTrace,MethodTraceData >(askingMethod,MethodTraceData()) );
    _measurements[askingMethod].push_back( DatabaseEntry(2) );
    assertion( _measurements.count(askingMethod)==1 );
    logInfo(
      "getDatabaseEntry(int)",
      "inserted trivial entry for " + peano::datatraversal::autotuning::toString(askingMethod)
      << ": " << _measurements[askingMethod].rbegin()->toString()
    );
  }

  while (_measurements[askingMethod].rbegin()->_biggestProblemSize<problemSize) {
    _measurements[askingMethod].push_back( DatabaseEntry(
      *_measurements[askingMethod].rbegin(),
      _measurements[askingMethod].rbegin()->_biggestProblemSize*2
    ));
    logInfo(
      "getDatabaseEntry(int)",
      "inserted new entry for " + peano::datatraversal::autotuning::toString(askingMethod)
      << ": " << _measurements[askingMethod].rbegin()->toString()
    );
  }

  MethodTraceData::iterator entry = _measurements[askingMethod].begin();
  while (entry->_biggestProblemSize<problemSize) {
    entry++;
  }

  DatabaseEntry& result = *entry;
  return result;
}


peano::datatraversal::autotuning::GrainSize  sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::parallelise(int problemSize, peano::datatraversal::autotuning::MethodTrace askingMethod) {
  assertion( askingMethod != peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling );

  assertion(askingMethod!=peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling);
  assertion( _measurements.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0 );

  auto databaseEntry = getDatabaseEntry(problemSize,askingMethod);

  assertion( _measurements.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0 );
  assertion( _measurements.count(askingMethod)>0 );

  const bool trackTime         = (_activeMethodTrace==askingMethod) && (databaseEntry._searchDelta>0);

  logDebug(
    "parallelise()",
    "will track time for " << toString(askingMethod) << "=" << trackTime <<
    " with active trace " << toString(_activeMethodTrace)
  );

  const int chosenGrainSize = databaseEntry._currentGrainSize<problemSize ? databaseEntry._currentGrainSize : 0;
  return peano::datatraversal::autotuning::GrainSize(
    chosenGrainSize,
    trackTime,
    problemSize,
    askingMethod, this
  );
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::widenAccuracyOfCurrentlyStudiedMethodTraceAndRandomlyRestart() {
  bool widenedEntry = false;
  for (auto& p: _measurements[_activeMethodTrace]) {
    const bool widenThisEntry = p._searchDelta>0 && p._currentMeasurement.getAccuracy()<1.0;
    if (widenThisEntry) {
      p._currentMeasurement.increaseAccuracy(0.9);
    }
    widenedEntry |= widenThisEntry;
  }
  if (!widenedEntry && _restart) {
    logInfo( "widenAccuracyOfCurrentlyStudiedMethodTraceAndRandomlyRestart(...)", "restart all measurements of " << toString(_activeMethodTrace) );
    for (auto& p: _measurements[_activeMethodTrace]) {
      if (rand()%100==0) {
        p.restart();
      }
    }
  }
}


bool sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::isStableDatabaseEntrySet( const peano::datatraversal::autotuning::MethodTrace&  askingMethod) const {
  if ( _measurements.count(askingMethod)==0 ) {
    return true;
  }
  else {
    bool result             = true;

    for (auto p: _measurements.at(askingMethod)) {
      result             &= (p._searchDelta==0);
    }

    return result;
  }
}



void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::changeMeasuredMethodTrace() {
  assertion( _measurements.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0 );
  assertion( !_measurements.empty() );

  const bool randomiseSelection   = true;

  // We just make it big enough to run through every variant twice if we use a deterministic scheme.
  int remainingTriesToFindSearchingTrace = (int)(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling);
  if (randomiseSelection) {
    _activeMethodTrace             = peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling;

    while ( _measurements.count(_activeMethodTrace)==0 ) {
      _activeMethodTrace = peano::datatraversal::autotuning::toMethodTrace( rand() % (int)(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling) );
      if (
        remainingTriesToFindSearchingTrace>0
        &&
        _measurements.count(_activeMethodTrace)==1
        &&
        isStableDatabaseEntrySet(_activeMethodTrace)
      ) {
        remainingTriesToFindSearchingTrace--;
        logDebug( "changeMeasuredMethodTrace()", "skip " << toString(_activeMethodTrace) );
        _activeMethodTrace = peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling;
      }
    }
  }
  else {
    do {
      _activeMethodTrace = peano::datatraversal::autotuning::toMethodTrace( static_cast<int>(_activeMethodTrace) + 1 );
      if (_activeMethodTrace>=peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling) {
        _activeMethodTrace = peano::datatraversal::autotuning::toMethodTrace( 0 );
      }
      remainingTriesToFindSearchingTrace--;
    }
    while (
      _measurements.count(_activeMethodTrace)==0
      ||
      ( remainingTriesToFindSearchingTrace>0 && isStableDatabaseEntrySet(_activeMethodTrace))
    );
  }

  assertion(_measurements.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0);
  assertion(_activeMethodTrace!=peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling);

  assertion(_activeMethodTrace!=peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling);
  assertion(_measurements.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0);

  logDebug( "changeMeasuredMethodTrace()", "next active method trace " << toString(_activeMethodTrace) );
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::learn() {
  assertion( _currentMeasurement.isAccurateValue() );
  assertion( _searchDelta>0 );

  if ( _currentMeasurement.getValue() < _previousMeasuredTime ) {
    logInfo( "learn()", "found better scaling parameter choice/serial runtime for: " << toString() );

    while ( _currentGrainSize - _searchDelta <= 0 && _searchDelta>0 ) {
      _searchDelta /= 2;
    }

    _currentGrainSize     -= _searchDelta;
    _previousMeasuredTime  = _currentMeasurement.getValue();
    _currentMeasurement.erase();
    _currentMeasurement.increaseAccuracy(2.0);

    logInfo( "learn()", "continue with " << toString() );
  }
  else {
    logInfo( "learn()", "parameter choice for " << toString() << " does not scale" );

    _currentGrainSize     += _searchDelta;
    _previousMeasuredTime  = _TimingMax;
    _currentMeasurement.erase();
    _currentMeasurement.increaseAccuracy(2.0);

    if ( _biggestProblemSize<=tarch::multicore::Core::getInstance().getNumberOfThreads() ) {
      _searchDelta--;
    }
    else if (
      _searchDelta > tarch::multicore::Core::getInstance().getNumberOfThreads()
    ) {
      _searchDelta /= std::max(2,tarch::multicore::Core::getInstance().getNumberOfThreads());
    }
    else {
      _searchDelta /= 2;
    }

    logInfo( "learn()", "continue with " << toString() );
  }

  assertion1(_currentGrainSize>0,  toString() );
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::parallelSectionHasTerminated(int problemSize, int grainSize, peano::datatraversal::autotuning::MethodTrace askingMethod, double costPerProblemElement) {
  assertion( askingMethod!=peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling );

  auto& databaseEntry = getDatabaseEntry(problemSize,askingMethod);
  if (databaseEntry._currentMeasurement.getAccuracy()==0.0 ) {
    const double computeTime   = costPerProblemElement * static_cast<double>(problemSize);
    databaseEntry._currentMeasurement.setAccuracy( computeTime * _InitialRelativeAccuracy );

    logDebug(
      "parallelSectionHasTerminated(...)",
      "fix accuracy for " << toString(askingMethod) << " to " << databaseEntry.toString()
    );
  }

  databaseEntry._currentMeasurement.setValue( costPerProblemElement );
}


std::string sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::toString() const {
  std::ostringstream msg;
  msg <<        _biggestProblemSize
      << "," << _currentGrainSize
      << "," << _previousMeasuredTime
      << "," << _searchDelta
      << "," << _currentMeasurement.getAccuracy()
      << "," << _currentMeasurement.toString()
      << ")";
  return msg.str();
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::plotStatistics(std::ostream& out, int oracleNumber) const {
  out << "# " << std::endl;
  out << "# trace=biggest problem size, current grain size, previous measured time, search delta, accuracy, current measurement" << std::endl;
  out << "# -------------------------------------------------------------------------------------------------------------------" << std::endl;
  out << "# " << std::endl;
  out << "begin OracleForOnePhaseWithShrinkingGrainSize" << std::endl;
  out << "initial-relative-accuracy=" << _InitialRelativeAccuracy << std::endl;
  out << "adapter-number=" << oracleNumber << std::endl;

  for (auto measurement: _measurements)
  for (auto p: measurement.second) {
    out << peano::datatraversal::autotuning::toString(measurement.first)
        << "=" << p.toString() << std::endl;
  }

  out << "end OracleForOnePhaseWithShrinkingGrainSize" << std::endl;

  logDebug( "plotStatistics(std::ostream,int)", "piped statistics for oracle no " << oracleNumber );
}


sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::DatabaseEntry(int problemSize) {
  assertion(problemSize>0);

  _biggestProblemSize   = problemSize;
  _currentGrainSize     = problemSize;
  _currentMeasurement   = tarch::timing::Measurement( 0.0 );
  _previousMeasuredTime = _TimingMax;

  if (
    problemSize < tarch::multicore::Core::getInstance().getNumberOfThreads()*2
    ||
    tarch::multicore::Core::getInstance().getNumberOfThreads() <= 2
  ) {
    _searchDelta = problemSize/2;
  }
  else {
    _searchDelta = problemSize - problemSize / tarch::multicore::Core::getInstance().getNumberOfThreads();
  }
}


sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::DatabaseEntry( const DatabaseEntry& prototype, int newProblemSize ):
  DatabaseEntry(newProblemSize) {
  assertion(prototype._biggestProblemSize<newProblemSize);
  if (prototype._currentGrainSize < prototype._biggestProblemSize) {
    // These are all integers and it should be possible to  determine all
    // settings through integer arithmetics if we do not determine a rescaling
    // factor (integer) once and then add it but first multiply and then
    // divide. However, if we do so, we quickly run into integer overflows:
    //     _currentGrainSize = prototype._currentGrainSize * newProblemSize / prototype._biggestProblemSize;
    //     _searchDelta      = prototype._searchDelta      * newProblemSize / prototype._biggestProblemSize;
    //
    // So we have to work with first computing the ratio though it might introduce truncation errors:
    int scaleUp = newProblemSize / prototype._biggestProblemSize;
    if (scaleUp==0) {
      scaleUp = 1;
    }

    _currentGrainSize = prototype._currentGrainSize * scaleUp;
    _searchDelta      = prototype._searchDelta      * scaleUp;

  }

  if (_searchDelta==0) {
    restart();
  }

  assertion1(_currentGrainSize<=_biggestProblemSize, toString() );
  assertion1(_currentGrainSize>0,                    toString() );
  assertion1(_biggestProblemSize>0,                  toString() );
  assertion1(_searchDelta>=0,                        toString() );
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::restart() {
  assertion1( _currentGrainSize>0, toString() );

  const int oldCurrentGrainSize = _currentGrainSize;

  int newCurrentGrainSize = 1;

  if (
    _biggestProblemSize>tarch::multicore::Core::getInstance().getNumberOfThreads()
    &&
    _currentGrainSize<_biggestProblemSize/2
  ) {
    // do not loose all the concurrency in one restart
    newCurrentGrainSize = _currentGrainSize * 2;
  }
  else if (
    _biggestProblemSize>tarch::multicore::Core::getInstance().getNumberOfThreads()
  ) {
    // do not loose all the concurrency in one restart
    newCurrentGrainSize = _biggestProblemSize / 2;
  }
  else {
    newCurrentGrainSize = (_biggestProblemSize + _currentGrainSize)/2;
  }

  _currentGrainSize     = newCurrentGrainSize;
  _currentMeasurement   = tarch::timing::Measurement( 0.0 );
  _previousMeasuredTime = _TimingMax;
  _searchDelta          = newCurrentGrainSize - oldCurrentGrainSize;

  if (_searchDelta<=0) {
    _searchDelta = _currentGrainSize/2;
  }

  logInfo( "restart(...)", "restarted " << toString() );
}


sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::DatabaseEntry::DatabaseEntry(std::string  inputLine) {
  std::string leftToken    = "";
  std::string rightString  = inputLine;
  std::string methodTrace;

  leftToken   = rightString.substr( 0, rightString.find(",") );
  rightString = rightString.substr( leftToken.size()+1 );
  _biggestProblemSize = std::stoi(leftToken);
  logDebug( "DatabaseEntry(std::string)", "got biggest problem size " << _biggestProblemSize );

  leftToken   = rightString.substr( 0, rightString.find(",") );
  rightString = rightString.substr( leftToken.size()+1 );
  _currentGrainSize = std::stoi(leftToken);
  logDebug( "DatabaseEntry(std::string)", "got current grain size " <<  _currentGrainSize );

  leftToken   = rightString.substr( 0, rightString.find(",") );
  rightString = rightString.substr( leftToken.size()+1 );
  _previousMeasuredTime = std::stof(leftToken);
  logDebug( "DatabaseEntry(std::string)", "previous measured time is " <<  _previousMeasuredTime );

  leftToken   = rightString.substr( 0, rightString.find(",") );
  rightString = rightString.substr( leftToken.size()+1 );
  _searchDelta = std::stoi(leftToken);
  logDebug( "DatabaseEntry(std::string)", "search delta is " <<  _searchDelta );

  leftToken   = rightString.substr( 0, rightString.find(",") );
  rightString = rightString.substr( leftToken.size()+1 );
  double accuracy = std::stof(leftToken);
  logDebug( "DatabaseEntry(std::string)", "accuracy is " <<  accuracy << ". Ignore remainder of this line");

  _currentMeasurement.erase();
  _currentMeasurement.setAccuracy( accuracy );

  bool isValid = _biggestProblemSize>0
              && _currentGrainSize>0
              && _currentGrainSize<=_biggestProblemSize
              && _previousMeasuredTime>=0
              && _searchDelta>=0
	            && accuracy>=0.0;

  if (!isValid) {
    if (_biggestProblemSize<=0) {
      _biggestProblemSize = 65536;
      logError( "DatabaseEntry(std::string)", "unable to parse file entries w.r.t. biggest problem size" );
    }
    logError( "DatabaseEntry(std::string)", "input file seems to have been corrupted. Restart entry " << toString() << ". Corrupted line: " << inputLine );
    _currentGrainSize     = _biggestProblemSize;
    _currentMeasurement   = tarch::timing::Measurement( 0.0 );
    _previousMeasuredTime = _TimingMax;
    _searchDelta          = 0;
    restart();
  }
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::loadStatistics(const std::string& filename, int oracleNumber) {
  std::ifstream file(filename);
  std::string str = "";

  bool        tagOpen = false;
  while (std::getline(file, str)) {
    tagOpen &= str.compare("end OracleForOnePhaseWithShrinkingGrainSize")!=0;

    if (tagOpen) {
      #ifdef __EXCEPTIONS
      try {
      #endif

      std::string leftToken = "";
      std::string rightString = str;

      leftToken   = rightString.substr( 0, rightString.find("=") );
      rightString = rightString.substr( leftToken.size()+1 );
      peano::datatraversal::autotuning::MethodTrace  methodTrace = peano::datatraversal::autotuning::toMethodTrace(leftToken);
      logDebug( "loadStatistics(...)", "parse properties for " << toString(methodTrace) );

      DatabaseEntry newEntry(rightString);

      _measurements[methodTrace].push_back( newEntry );

      logDebug( "loadStatistics(...)", "added " << newEntry.toString() << " for " << toString(methodTrace) );
      #ifdef __EXCEPTIONS
      }
      catch (std::out_of_range& exception) {
        logError(
          "loadStatistics(...)",
          "failed to parse shared memory configuration file " << filename << " with error in " << exception.what() << " in adapter " << oracleNumber
        );
        logError(
          "loadStatistics(...)",
          "flawed string: " << str
        );
      }
      #endif
    }

    // Older GCC versions require an explicit cast here
    tagOpen |= str.compare( "adapter-number=" + std::to_string( (long long)oracleNumber) )==0;
  }

  assertion( _measurements.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0 );
}


sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::~OracleForOnePhaseWithShrinkingGrainSize() {
}


peano::datatraversal::autotuning::OracleForOnePhase* sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::createNewOracle() const {
  return new OracleForOnePhaseWithShrinkingGrainSize(_learn,_restart);
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::deactivateOracle() {
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::clearAllMeasurementsBesidesActiveOne() {
  for (auto& measurement: _measurements) {
    if (measurement.first!=_activeMethodTrace) {
      for (auto& p: measurement.second) {
        p._currentMeasurement.erase();
      }
    }
  }
}


void sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::activateOracle() {
  if (_learn) {
    //
    // First check is very important. If we skip it, the second statement would
    // insert elements into map
    //
    if (
      _activeMethodTrace!=peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling
    ) {
      bool          oneMeasurementDidLearn           = false;
      bool          oneMeasurementDidTerminateSearch = false;
      DatabaseEntry learningEntry(1);
      for (auto& p: _measurements[_activeMethodTrace]) {
        if (
          p._currentMeasurement.isAccurateValue()
          &&
          p._searchDelta>0
        ) {
          logInfo( "activateOracle()", "found entry that should learn for trace " << toString(_activeMethodTrace) );
          p.learn();
          oneMeasurementDidLearn = true;
          if (p._searchDelta==0) {
            logInfo( "activateOracle()", "entry fixed, so propagate its data: " << p.toString() );
            oneMeasurementDidTerminateSearch = true;
            learningEntry                    = p;
          }
        }
        else if (
          oneMeasurementDidTerminateSearch
          &&
          (
            p._searchDelta>0
            ||
            (p._searchDelta==0 && p._currentGrainSize >= p._biggestProblemSize/2)
          )
        ) {
          // propagate data
          p = DatabaseEntry( learningEntry, p._biggestProblemSize );
          logInfo( "activateOracle()", "have propagated solution from " << learningEntry.toString() << " into " << p.toString() );
        }
        else if (
          oneMeasurementDidTerminateSearch
          &&
          p._searchDelta==0
        ) {
          oneMeasurementDidTerminateSearch = false;
        }
      }

      if (oneMeasurementDidLearn) {
        clearAllMeasurementsBesidesActiveOne();
        _hasLearnedSinceLastQuery = true;
      }
    }

    if (!_measurements.empty() ) {
      changeMeasuredMethodTrace();
      widenAccuracyOfCurrentlyStudiedMethodTraceAndRandomlyRestart();
    }
  }
}


bool sharedmemoryoracles::OracleForOnePhaseWithShrinkingGrainSize::hasLearnedSinceLastQuery() {
  bool result = _hasLearnedSinceLastQuery;
  _hasLearnedSinceLastQuery = false;
  return result;
}

