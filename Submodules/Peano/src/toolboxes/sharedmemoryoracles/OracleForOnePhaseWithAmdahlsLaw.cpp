#include "sharedmemoryoracles/OracleForOnePhaseWithAmdahlsLaw.h"
#include "tarch/Assertions.h"
#include "peano/utils/Globals.h"

#include <cstdlib>
#include <math.h>
#include <fstream>


tarch::logging::Log  sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::_log( "sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw" );
constexpr int        sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::MinSampleInterval;


sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::OracleForOnePhaseWithAmdahlsLaw():
  _executionTimeDatabase(),
  _sampleEveryXQueries(MinSampleInterval),
  _sampleCounter(0) {
}


sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::~OracleForOnePhaseWithAmdahlsLaw() {
}


peano::datatraversal::autotuning::GrainSize sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::parallelise(int problemSize, peano::datatraversal::autotuning::MethodTrace askingMethod) {
  logTraceInWith1Argument( "parallelise(...)", problemSize );

  _sampleCounter++;
  _sampleCounter = _sampleCounter%_sampleEveryXQueries;

  _executionTimeDatabase[askingMethod]._maxProblemSize = std::max( _executionTimeDatabase[askingMethod]._maxProblemSize, problemSize );

  if (_sampleCounter==0 and problemSize>1) {
    const int grainSize = (rand() % (problemSize-1) )+1;
    assertion2(grainSize>=0,grainSize,problemSize);
    assertion2(grainSize<problemSize,grainSize,problemSize);

    return peano::datatraversal::autotuning::GrainSize(
      grainSize,
	  true,
      problemSize,
      askingMethod,
      this
    );
  }
  else {
    const int grainSize = _executionTimeDatabase[askingMethod]._optimalGrainSize >= problemSize ? 0 : _executionTimeDatabase[askingMethod]._optimalGrainSize;
    return peano::datatraversal::autotuning::GrainSize(
      grainSize, false,
      problemSize,
      askingMethod,
      this
    );
  }
}


void sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::parallelSectionHasTerminated(int problemSize, int grainSize, peano::datatraversal::autotuning::MethodTrace askingMethod, double costPerProblemElement) {//  assertion(_executionTimes.count(askingMethod)==1);
  // 0 doesn't fit to Amdahl's law, so we manually have to postprocess
  grainSize = grainSize==0 ? _executionTimeDatabase[askingMethod]._maxProblemSize+1 : grainSize;

  _executionTimeDatabase[askingMethod]._statistics.addMeasurement(grainSize,costPerProblemElement);
}



void sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::loadStatistics(const std::string& filename, int oracleNumber) {
  std::ifstream file(filename);
  std::string str = "";

  bool        tagOpen = false;
  while (std::getline(file, str)) {
    tagOpen &= str.compare("end OracleForOnePhaseWithAmdahlsLaw")!=0;

    if (tagOpen) {
      #ifdef __EXCEPTIONS
      try {
      #endif
      std::string rightString = str;

      std::string traceToken      = rightString.substr( 0, rightString.find("=") );
      rightString                 = rightString.substr( traceToken.size()+1 );
      std::string grainSizeToken  = rightString.substr( 0, rightString.find(",") );
      rightString                 = rightString.substr( grainSizeToken.size()+1 );
      std::string maxToken        = rightString.substr( 0, rightString.find(",") );

      peano::datatraversal::autotuning::MethodTrace  methodTrace      = peano::datatraversal::autotuning::toMethodTrace(traceToken);
      int                                            optimalGrainSize = atoi( grainSizeToken.c_str() );
      int                                            maxProblemSize   = atoi( maxToken.c_str() );

      _executionTimeDatabase[methodTrace]._optimalGrainSize = optimalGrainSize;
      _executionTimeDatabase[methodTrace]._maxProblemSize   = maxProblemSize;

      logInfo( "loadStatistics(...)", "added entry " << toString(methodTrace) << ":" << optimalGrainSize << "," << maxProblemSize );

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

  assertion( _executionTimeDatabase.count(peano::datatraversal::autotuning::MethodTrace::NumberOfDifferentMethodsCalling)==0 );
}


void sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::plotStatistics(std::ostream& out, int oracleNumber) const {
  out << "begin OracleForOnePhaseWithAmdahlsLaw" << std::endl;
  out << "adapter-number=" << oracleNumber << std::endl;

  for (auto measurement: _executionTimeDatabase) {
    out << peano::datatraversal::autotuning::toString(measurement.first)
        << "=" << measurement.second._optimalGrainSize
		<< "," << measurement.second._maxProblemSize
        << "," << measurement.second._statistics.toShortString() << std::endl;
  }

  out << "end OracleForOnePhaseWithAmdahlsLaw" << std::endl;
}


peano::datatraversal::autotuning::OracleForOnePhase* sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::createNewOracle() const {
  return new OracleForOnePhaseWithAmdahlsLaw();
}



void sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::deactivateOracle() {
  bool hasChangedAnEntry = false;
  for (auto& measurement: _executionTimeDatabase) {
	const int oldValue = measurement.second._optimalGrainSize;
	measurement.second._statistics.relaxAmdahlsLawWithThreadStartupCost();
	int newValue = std::min( measurement.second._statistics.getOptimalNumberOfThreads(), measurement.second._maxProblemSize+1 );
	if ( oldValue!=newValue and !hasChangedAnEntry) {
	  newValue = std::abs(oldValue-newValue)>=2 ? (oldValue+newValue)/2 : newValue;
      logInfo(
        "deactivateOracle()",
		"change optimal grain size for " << toString(measurement.first)
		<< " from " << oldValue << " to " << newValue
		<< " (max size=" << measurement.second._maxProblemSize << ")"
	  );
      measurement.second._optimalGrainSize = newValue;
      if ( std::abs(newValue-oldValue) > oldValue / 10 ) { // More than ten percent is significant
        hasChangedAnEntry = true;
      }
	}
  }

  if (hasChangedAnEntry) {
    _sampleEveryXQueries++;
    _sampleEveryXQueries = std::min(_sampleEveryXQueries,65536);
  }
  else {
    _sampleEveryXQueries--;
    _sampleEveryXQueries = std::max(_sampleEveryXQueries,MinSampleInterval);
  }
  logInfo(
    "deactivateOracle()",
     "new sample interval: sample every " << _sampleEveryXQueries << "th request "
  );
}


void sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::activateOracle() {
}


sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw::DataBaseEntry::DataBaseEntry():
  _statistics(),
  _optimalGrainSize(1),
  _maxProblemSize(1) {
}

