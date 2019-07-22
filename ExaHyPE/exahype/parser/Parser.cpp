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
#include "exahype/parser/Parser.h"

#include "tarch/Assertions.h"

#include <fstream>

#include <stdio.h>
#include <string.h>
#include <string>
#include <regex>
#include <cstdlib> // getenv, exit
#include <sstream>
#include <iostream>
#include <iomanip>
#include <typeinfo>
#include <exception>
#include <cstdarg>

#include "tarch/la/ScalarOperations.h"

#include "exahype/parser/ParserView.h"


#include "json.hpp" // this is an ~1MB header file which is included here exclusively
using json = nlohmann::json;

tarch::logging::Log exahype::parser::Parser::_log("exahype::parser::Parser");

namespace exahype {
namespace parser {
/// Using a file-local namespace to avoid conflicts with these common function names.
namespace tools {

// a buffer-overflow-safe version of sprintf
// source: http://stackoverflow.com/a/69911
std::string vformat (const char *fmt, va_list ap) {
  using namespace std;
  // Allocate a buffer on the stack that's big enough for us almost
  // all the time.  Be prepared to allocate dynamically if it doesn't fit.
  size_t size = 1024;
  char stackbuf[1024];
  std::vector<char> dynamicbuf;
  char *buf = &stackbuf[0];
  va_list ap_copy;

  while (1) {
      // Try to vsnprintf into our buffer.
      va_copy(ap_copy, ap);
      int needed = vsnprintf (buf, size, fmt, ap);
      va_end(ap_copy);

      // NB. C99 (which modern Linux and OS X follow) says vsnprintf
      // failure returns the length it would have needed.  But older
      // glibc and current Windows return -1 for failure, i.e., not
      // telling us how much was needed.

      if (needed <= (int)size && needed >= 0) {
          // It fit fine so we're done.
          return std::string (buf, (size_t) needed);
      }

      // vsnprintf reported that it wanted to write more characters
      // than we allotted.  So try again using a dynamic buffer.  This
      // doesn't happen very often if we chose our initial size well.
      size = (needed > 0) ? (needed+1) : (size*2);
      dynamicbuf.resize (size);
      buf = &dynamicbuf[0];
  }
}

std::string sformat(const char *fmt, ...) {
  using namespace std;
  va_list ap;
  va_start (ap, fmt);
  std::string buf = vformat (fmt, ap);
  va_end (ap);
  return buf;
}

} // ns tools
} // ns parser
} // ns exahype

using namespace exahype::parser::tools;

/**
 * Using a file-local "pimpl idiom" to avoid exposing the json.hpp header to anything
 * including the Parser.h
 **/
struct exahype::parser::ParserImpl {
  json data;

  /**
   * Generic method to extract various types (like int,double,bool,string,vector),
   * throws std::runtime_error in case of error.
   * 
   * This is a template method in ParserImpl in order to avoid templates in Parser.h
   **/
  template<typename T>
  T getFromPath(std::string path, T defaultValue, bool isOptional=false) const {
    std::stringstream ss;
    try {
      return data.at(json::json_pointer(path));
    } catch (json::type_error& e) {
      ss << path << " is not a " << typeid(T).name()   << " (" << e.what() << ")";
    } catch(json::out_of_range& e) {
      if(isOptional)
        return defaultValue;
      else
        ss << "Missing entry " << path << " (" << e.what() << ")";
    }
    throw std::runtime_error(ss.str());
  }

  std::vector< std::pair<std::string, std::string> > getObjectAsVectorOfStringPair(std::string path, bool isOptional=false) const {
    std::vector< std::pair<std::string, std::string> > retvec;
    std::stringstream ss;
    try{
      auto object = data.at(json::json_pointer(path));
      for (json::iterator it = object.begin(); it != object.end(); ++it) {
        std::string value;
        if(it.value().is_string()) {
          value = it.value().get<std::string>(); //remove the ""
        } else { //convert to string
          std::ostringstream oss;
          oss << it.value();
          value = oss.str();
        }
        retvec.push_back( make_pair(it.key(), value) );
      }
      return retvec;
    } catch (json::type_error& e) {
      ss << path << " is not an object (" << e.what() << ")";
    } catch(json::out_of_range& e) {
      if(isOptional)
        return retvec; //empty vector
      else
        ss << "Missing entry " << path << " (" << e.what() << ")";
    }
    throw std::runtime_error(ss.str());
  }

}; // end of ParserImpl

/**
 * static constexpr need to declared again when following a
 * C++ standard before C++17.
 */
constexpr bool exahype::parser::Parser::isOptional;
constexpr bool exahype::parser::Parser::isMandatory;

bool exahype::parser::Parser::_interpretationErrorOccured(false);

const std::string exahype::parser::Parser::_noTokenFound("notoken");

double exahype::parser::Parser::getValueFromPropertyString(
    const std::string& parameterString, const std::string& key) {
  std::size_t startIndex = parameterString.find(key);
  startIndex = parameterString.find(":", startIndex);
  std::size_t endIndex = parameterString.find_first_of("}, \n\r", startIndex + 1);

  std::string substring =
      parameterString.substr(startIndex + 1, endIndex - startIndex - 1);

  double result;
  std::istringstream ss(substring);
  ss >> result;

  if (ss) {
    return result;
  } else {
    return std::numeric_limits<double>::quiet_NaN();
  }
}

exahype::parser::Parser::Parser() {
  _impl = new exahype::parser::ParserImpl();

  _identifier2Type.insert(
      std::pair<std::string, exahype::solvers::Solver::Type>(
          "ADER-DG", exahype::solvers::Solver::Type::ADERDG));
  _identifier2Type.insert(
      std::pair<std::string, exahype::solvers::Solver::Type>(
          "Finite-Volumes", exahype::solvers::Solver::Type::FiniteVolumes));
  _identifier2Type.insert(
        std::pair<std::string, exahype::solvers::Solver::Type>(
            "Limiting-ADER-DG", exahype::solvers::Solver::Type::LimitingADERDG));

  _identifier2TimeStepping.insert(
      std::pair<std::string, exahype::solvers::Solver::TimeStepping>(
          "global", exahype::solvers::Solver::TimeStepping::Global));
  _identifier2TimeStepping.insert(
      std::pair<std::string, exahype::solvers::Solver::TimeStepping>(
          "globalfixed", exahype::solvers::Solver::TimeStepping::GlobalFixed));
}

exahype::parser::Parser::~Parser() {
  delete _impl;
}


void exahype::parser::Parser::readFile(const std::string& filename) {
    std::ifstream inputFile;
    inputFile.open(filename.c_str());
    if (!inputFile.good()) {
      logError("readFile(String)", "cannot open file " << filename);
      invalidate();
      return;
    }
    readFile(inputFile, filename);
}

void exahype::parser::Parser::readFile(std::istream& inputStream, std::string filename) {
  _filename = filename;
  try {
    _impl->data = json::parse(inputStream);
  } catch(json::parse_error& e) {
    logError("readFile()", "Could not read specification file " << filename
      << " becaue the file is not a valid JSON file.");
    logError("readFile()", "Syntax Error (Error type " << e.id << "): " << e.what() << " at byte position " << e.byte);
    logError("readFile()", "Hint: Please ensure the given specification file is of the novel JSON format. We recently "
      << "switched formats and if you have an old-style specification file, use the toolkit to translate it to JSON.");
    std::abort(); // come on, there is no reason to invalidate and continue here.
    invalidate();
    return;
  }

  //  For debugging purposes
  if(std::getenv("EXAHYPE_VERBOSE_PARSER")) { // runtime debugging
     logInfo("readFile()", std::setw(4) << _impl->data << std::endl);
  }

  checkValidity();
  //  std::string configuration = getMPIConfiguration();
  //  int ranksPerNode = static_cast<int>(exahype::parser::Parser::getValueFromPropertyString(configuration,"ranks_per_node"));
  //  std::cout << "ranks_per_node="<<ranksPerNode << std::endl;
}

void exahype::parser::Parser::checkValidity() {
  // functions have side-effects: might set _interpretationErrorOccured
  getDomainSize();
  getOffset();
  if (foundSimulationEndTime()) {
    getSimulationEndTime();
  } else {
    getSimulationTimeSteps();
  }

  // TODO: Should call checkSolverConsistency(int solverNumber) for every solver at this point.
  //       Actually I (Sven) Cannot figure out at the moment where checkSolverConsistency is called at all.
  //       This just lead to bugs where we called a project compiled for LimitingADERDG with a pure ADERDG specfile.
}

bool exahype::parser::Parser::isValid() const {
  bool jsonIsValid= !_impl->data.empty(); // at least a simple check to ensure the JSON holds something
  return jsonIsValid && !_interpretationErrorOccured;
}

void exahype::parser::Parser::invalidate() const {
  _interpretationErrorOccured = true;
}

bool exahype::parser::Parser::hasPath(std::string path) const {
  // Q: I'm not sure whethe the iterator end() is also true for nested structures (path pointer)
  // Previous code: bool found = _impl->data.find(json::json_pointer(path)) != _impl->data.end();
  // return found;
  try {
    auto j = _impl->data.at(json::json_pointer(path));
    if (!j.is_null()) {
      return true;
    } else {
      return false;
    }
  } catch(json::out_of_range& e) {
    return false;
  }
}

std::string exahype::parser::Parser::dumpPath(std::string path) const {
  try {
    return _impl->data.at(json::json_pointer(path)).dump();
  } catch (json::type_error& e) {
    logError("dumpPath()", "Path " << path << " contains a non-UTF-8-encodable string (" << e.what() << ")");
  } catch(json::out_of_range& e) {
    logError("dumpPath()", "Path " << path << " not found (" << e.what() << ")");
  }
  invalidate();
  return "{'dumpPath':'error'}";
}

bool exahype::parser::Parser::isValueValidBool(const std::string& path) const {
  return hasPath(path) && _impl->data.at(json::json_pointer(path)).is_boolean();
}

bool exahype::parser::Parser::isValueValidInt(const std::string& path) const {
  return hasPath(path) && _impl->data.at(json::json_pointer(path)).is_number_integer();
}

bool exahype::parser::Parser::isValueValidDouble(const std::string& path) const {
  return hasPath(path) && _impl->data.at(json::json_pointer(path)).is_number();
}

bool exahype::parser::Parser::isValueValidString(const std::string& path) const {
  return hasPath(path) && _impl->data.at(json::json_pointer(path)).is_string();
}

std::string exahype::parser::Parser::getStringFromPath(std::string path, std::string defaultValue, bool isOptional) const {
  assertion(isValid());
  try {
    return _impl->getFromPath(path, defaultValue, isOptional);
  } catch(std::runtime_error& e) {
    logError("getStringFromPath()", e.what());
    invalidate();
    return defaultValue; /* I don't like returning something here */
  }
  /*
  assertion(isValid());
  try {
    return _impl->data.at(json::json_pointer(path));
  } catch (json::type_error& e) {
    logError("getStringFromPath()", path << " is not a string (" << e.what() << ")");
  } catch(json::out_of_range& e) {
    logError("getStringFromPath()", "Missing entry " << path << " (" << e.what() << ")");
  }
  invalidate();
  return "";
  */
}

bool exahype::parser::Parser::stringFromPathEquals(const std::string& path, const std::string& defaultValue,const bool isOptional, const std::string& otherString) const {
  assertion(isValid());
  try {
    return _impl->getFromPath(path, defaultValue, isOptional).compare(otherString);
  } catch(std::runtime_error& e) {
    logError("stringFromPathEquals()", e.what());
    invalidate();
    return defaultValue.compare(otherString)==0; /* I don't like returning something here */
  }
  /*
  assertion(isValid());
  try {
    return _impl->data.at(json::json_pointer(path));
  } catch (json::type_error& e) {
    logError("getStringFromPath()", path << " is not a string (" << e.what() << ")");
  } catch(json::out_of_range& e) {
    logError("getStringFromPath()", "Missing entry " << path << " (" << e.what() << ")");
  }
  invalidate();
  return "";
  */
}

int exahype::parser::Parser::getIntFromPath(std::string path, int defaultValue, bool isOptional) const {
  assertion(isValid());
  try {
    return _impl->getFromPath(path, defaultValue, isOptional);
  } catch(std::runtime_error& e) {
    logError("getIntFromPath()", e.what());
    invalidate();
    return defaultValue; /* I don't like returning something here */
  }
}

double exahype::parser::Parser::getDoubleFromPath(std::string path, double defaultValue, bool isOptional) const {
  assertion(isValid());
  try {
    return _impl->getFromPath(path, defaultValue, isOptional);
  } catch(std::runtime_error& e) {
    logError("getDoubleFromPath()", e.what());
    invalidate();
    return defaultValue; /* I don't like returning something here */
  }
}

bool exahype::parser::Parser::getBoolFromPath(std::string path, bool defaultValue, bool isOptional) const {
  assertion(isValid());
  try {
    return _impl->getFromPath(path, defaultValue, isOptional);
  } catch(std::runtime_error& e) {
    logError("getBoolFromPath()", e.what());
    invalidate();
    return defaultValue; /* I don't like returning something here */
  }
}

std::vector< std::pair<std::string, std::string> > exahype::parser::Parser::getObjectAsVectorOfStringPair(std::string path, bool isOptional) const {
  assertion(isValid());
  try {
    return _impl->getObjectAsVectorOfStringPair(path, isOptional);
  } catch(std::runtime_error& e) {
    logError("getObjectKeysFromPath()", e.what());
    invalidate();
    std::vector< std::pair<std::string, std::string> >  empty;
    return empty; /* I don't like returning something here */
  }
}

tarch::la::Vector<DIMENSIONS,double> exahype::parser::Parser::getDimVectorFromPath(std::string path) const {
  assertion(isValid());
  tarch::la::Vector<DIMENSIONS,double> result;
  // Here we do not rely on _impl->getFromPath because we don't want to
  // copy from a STL vector. Somehow.
  try {
    json::json_pointer p(path);
    result(0) = _impl->data.at(p).at(0);
    if(_impl->data.at(p).size() < DIMENSIONS) {
      logError("getDimVectorFromPath()", path << " holds a vector of size " << _impl->data.at(p).size() << ", however we have " << DIMENSIONS << " spatial dimensions");
    }
    result(1) = _impl->data.at(p).at(1);
    if(DIMENSIONS == 3)
      result(2) = _impl->data.at(p).at(2);
    return result;
  } catch(json::type_error& e) {
    logError("getDimVectorFromPath()", path << " holds not a double-vector of size " << DIMENSIONS << " (" << e.what() << ")");
  } catch(json::out_of_range& e) {
    logError("getDimVectorFromPath()", "Missing entry " << path << " (" << e.what() << ")");
  }

  invalidate();
  return result;
}

std::vector<int> exahype::parser::Parser::getIntVectorFromPath(std::string path) const {
  assertion(isValid());
  std::vector<int> empty;
  try {
    return _impl->getFromPath(path, empty, isMandatory);
  } catch(std::runtime_error& e) {
    logError("getIntVectorFromPath()", e.what());
    invalidate();
    return empty; /* I don't like returning something here */
  }
}

bool exahype::parser::Parser::flagListContains(std::string path, std::string keyword) const {
  assertion(isValid());
  try {
    auto p = json::json_pointer(path);
    if(_impl->data.count(p)) {
      auto j = _impl->data.at(p);
      if(!j.is_array()) {
        logError("flagListContains()", "Expected " << path << " to hold an array, but this is not the case.");
        invalidate();
        return false;
      }
      for (json::iterator it = j.begin(); it != j.end(); ++it) {
        if(! it->is_string() ) {
          logError("flagListContains()", path << " holds a non-string element in its array-content.");
          invalidate();
          return false;
        }
        if( it->get<std::string>().compare(keyword) == 0) return true;
      }
      return false; // not found
    } else {
      logDebug("flagListContains()", "Flag array " << path << " is not existing, defaulting to empty.");
    }
  } catch(json::type_error& e) {
    logError("flagListContains()", path << " holds weird data (" << e.what() << ")");
  } catch(json::out_of_range& e) {
    logError("flagListContains()", "Missing something below or at  " << path << " (" << e.what() << ")");
  }
  
  invalidate();
  return false; // Should not return data
}

int exahype::parser::Parser::getNumberOfThreads() const {
  return getIntFromPath("/shared_memory/cores");
}

int exahype::parser::Parser::getThreadStackSize() const {
  return getIntFromPath("/shared_memory/thread_stack_size",8388608,isOptional);
}

tarch::la::Vector<DIMENSIONS, double> exahype::parser::Parser::getDomainSize() const {
  assertion(isValid());
  tarch::la::Vector<DIMENSIONS, double> result;
  
  int dim = getIntFromPath("/computational_domain/dimension");
  
  if(dim != DIMENSIONS) {
    logError("getDomainSize()",
              "dimension: value "<< dim << " in specification file" <<
              " does not match -DDim"<<DIMENSIONS<<" switch in Makefile. Rerun toolkit!");
    invalidate();
    return result;
  }
  
  result = getDimVectorFromPath("/computational_domain/width");
  logDebug("getDomainSize()", "found size " << result);
  return result;
}

tarch::la::Vector<DIMENSIONS, double> exahype::parser::Parser::getOffset() const {
  assertion(isValid());
  std::string token;
  tarch::la::Vector<DIMENSIONS, double> result;
  result = getDimVectorFromPath("/computational_domain/offset");
  logDebug("getOffset()", "found offset " << result);
  return result;
}

int exahype::parser::Parser::getOutsideCells() const {
  return getIntFromPath("/computational_domain/outside_cells", 2, isOptional);
}

int exahype::parser::Parser::getOutsideCellsLeft() const {
  const int outsideCells = getOutsideCells();
  const int result       = getIntFromPath("/computational_domain/outside_cells_left", outsideCells/2, isOptional);
  if ( result < 0 || result > outsideCells ) {
    logError("getOutsideCellsLeft()", "'outside_cells_left' must not be negative or larger than 'outside_cells' (default: 2); it is: "<<result);
    invalidate();
  }
  return result;
}

bool exahype::parser::Parser::getPlaceOneThirdOfCellsOuside() const {
  return getBoolFromPath("/computational_domain/one_third_of_cells_outside", false, isOptional);
}

bool exahype::parser::Parser::getScaleBoundingBox() const {
  return getPlaceOneThirdOfCellsOuside() || getOutsideCells() > 0;
}

std::string exahype::parser::Parser::getMulticorePropertiesFile() const {
  std::string result = getStringFromPath("/shared_memory/properties_file","shared-memory.properties",isOptional);
  logDebug("getMulticorePropertiesFile()", "found " << result);
  return result;
}

exahype::parser::Parser::MPILoadBalancingType exahype::parser::Parser::getMPILoadBalancingType() const {
  std::string token = getStringFromPath("/distributed_memory/loadbalancing_type","static",isOptional);
  exahype::parser::Parser::MPILoadBalancingType result = MPILoadBalancingType::Static;
  if (token.compare("static") == 0) {
    result = MPILoadBalancingType::Static;
  } else if (token.compare("dynamic") == 0) {
    result = MPILoadBalancingType::Dynamic;
  } else {
    logError("getMPILoadBalancingType()",
             "Invalid distributed memory identifier " << token);
    invalidate();
  }
  return result;
}

double exahype::parser::Parser::getNodePoolAnsweringTimeout() const {
  const std::string path = "/distributed_memory/node_pool_timeout";
  const double defaultResult = 1;
  double result = getDoubleFromPath(path, defaultResult, true);
  if(tarch::la::equals(defaultResult, result)) {
    logWarning( "getNodePoolAnsweringTimeout()", path << " not specified for MPI configuration so use default timeout of " << defaultResult );
  }
  return result;
}

bool exahype::parser::Parser::compareNodePoolStrategy(const std::string& strategy) const {
  return getStringFromPath("/distributed_memory/node_pool_strategy","fair",isOptional).compare(strategy)==0;
}

bool exahype::parser::Parser::compareMPILoadBalancingStrategy(const std::string& strategy) const {
  return getStringFromPath("/distributed_memory/load_balancing_strategy","hotspot",isOptional).compare(strategy)==0;
}

/**
 * @return the number of forks the load balancing
 * mapping is allowed to trigger per mesh refinement iteration.
 */
int exahype::parser::Parser::getMaxForksPerLoadBalancingStep() const {
  return getIntFromPath("/distributed_memory/max_forks_at_once", -1, isOptional);
}

int exahype::parser::Parser::getMPIBufferSize() const {
  int result = getIntFromPath("/distributed_memory/buffer_size");

  // Apparently, in former days an invalid value just yielded in a non-fatal error.
  // all non-castable ints resulted in negative numbers.

  if(result <= 0) {
    logError("getMPIBufferSize()", "Invalid MPI buffer size " << result);
    result = 64;
    invalidate();
  }

  return result;
}

int exahype::parser::Parser::getMPITimeOut() const {
  double result = getIntFromPath("/distributed_memory/timeout");

  // Apparently, in former days an invalid value just yielded in a non-fatal error.
  // all non-castable doubles resulted in negative numbers.

  if (result <= 0) {
    logError("getMPIBufferSize()", "Invalid MPI timeout value " << result);
    result = 0;
    invalidate();
  }
  
  return result;
}

exahype::parser::Parser::MulticoreOracleType exahype::parser::Parser::getMulticoreOracleType() const {
  std::string token = getStringFromPath("/shared-memory/autotuning_strategy","dummy",isOptional);
  exahype::parser::Parser::MulticoreOracleType result = MulticoreOracleType::Dummy;
  if (token.compare("dummy") == 0) {
    result = MulticoreOracleType::Dummy;
  } else if (token.compare("autotuning") == 0) {
    result = MulticoreOracleType::AutotuningWithRestartAndLearning;
  } else if (token.compare("autotuning_without_learning") == 0) {
    result = MulticoreOracleType::AutotuningWithoutLearning;
  } else if (token.compare("autotuning_without_restart") == 0) {
    result = MulticoreOracleType::AutotuningWithLearningButWithoutRestart;
  } else if (token.compare("sampling") == 0) {
    result = MulticoreOracleType::GrainSizeSampling;
  } else {
    logError("getMulticoreOracleType()", "Invalid shared memory identifier "
                                             << token);
    result = MulticoreOracleType::Dummy;
    invalidate();
  }
  return result;
}

double exahype::parser::Parser::getSimulationEndTime() const {
  double result = getDoubleFromPath("/computational_domain/end_time");
  logDebug("getSimulationEndTime()", "found end time " << result);
  
  // Apparently, in former days an invalid value just yielded in a non-fatal error.
  // all non-castable doubles resulted in negative numbers.

  if (result <= 0) {
    logError("getSimulationEndTime()",
             "Invalid simulation end-time: " << result);
    result = 1.0;
    invalidate();
  }
  return result;
}

bool exahype::parser::Parser::foundSimulationEndTime() const {
  const double not_there = -43;
  double result = getDoubleFromPath("/computational_domain/end_time", not_there, isOptional);
  bool found = !tarch::la::equals(result, not_there);
  return found;
}

int exahype::parser::Parser::getSimulationTimeSteps() const {
  int result = getIntFromPath("/computational_domain/time_steps");
  logDebug("getSimulationTimeSteps()", "found result " << result);
  
  // Apparently, in former days an invalid value just yielded in a non-fatal error.
  // all non-castable ints resulted in negative numbers.

  if (result < 0) {
    logError("getSimulationTimeSteps()",
             "Invalid simulation timestep: " << result);
    invalidate();
  }
  return result;
}

int exahype::parser::Parser::getMaxMeshSetupIterations() const {
  int result = getIntFromPath("/computational_domain/max_mesh_setup_iterations", 400, isOptional);
  if ( result < 1 ) {
    logError("getMaxMeshSetupIterations()",
            "'max_mesh_setup_iterations': Use infinite number of mesh setup iterations as given value was found to be smaller than one.");
    result = std::numeric_limits<int>::max();
  }
  return result;
}

bool exahype::parser::Parser::getStaticMeshRefinement() const {
  return getStringFromPath("/optimisation/mesh_refinement", "dynamic", isOptional).compare("static")==0;
}

bool exahype::parser::Parser::getStaticLimiting() const {
  return getStringFromPath("/optimisation/limiting", "dynamic", isOptional).compare("static")==0;
}

bool exahype::parser::Parser::getFuseAllAlgorithmicSteps() const {
  return getStringFromPath("/optimisation/fuse_algorithmic_steps", "none", isOptional).compare("all")==0;
}

bool exahype::parser::Parser::getFuseMostAlgorithmicSteps() const {
  return getStringFromPath("/optimisation/fuse_algorithmic_steps", "none", isOptional).compare("most")==0;
}

double exahype::parser::Parser::getFuseAlgorithmicStepsRerunFactor() const {
  const double default_value = 0.99;
  double result = getDoubleFromPath("/optimisation/fuse_algorithmic_steps_rerun_factor", default_value, isOptional);
  logDebug("getFuseAlgorithmicStepsFactor()", "found fuse_algorithmic_steps_rerun_factor " << result);
  if(result < 0.0 || result > 1.0) {
    logError("getFuseAlgorithmicStepsRerunFactor()",
              "'fuse_algorithmic_steps_rerun_factor': Value must be greater than zero "
              "and smaller than or equal to one. It is: "
                  << result);
    invalidate();
  }
  return result;
}

double exahype::parser::Parser::getFuseAlgorithmicStepsDiffusionFactor() const {
  const double default_value = 0.99;
  double result = getDoubleFromPath("/optimisation/fuse_algorithmic_steps_diffusion_factor", default_value, isOptional);
  logDebug("getFuseAlgorithmicStepsDiffusionFactor()", "found fuse_algorithmic_steps_diffusion_factor" << result);
  if(result < 0.0 || result > 1.0) {
    logError("getFuseAlgorithmicStepsFactor()",
              "'fuse_algorithmic_steps_diffusion_factor': Value must be greater than zero "
              "and smaller than or equal to one. It is: "
                  << result);
    invalidate();
  }
  return result;
}

bool exahype::parser::Parser::getSpawnPredictionAsBackgroundThread() const {
  return getBoolFromPath("/optimisation/spawn_predictor_as_background_thread", false, isOptional);
}

bool exahype::parser::Parser::getSpawnUpdateAsBackgroundThread() const {
  return getBoolFromPath("/optimisation/spawn_update_as_background_thread", false, isOptional);
}

bool exahype::parser::Parser::getSpawnProlongationAsBackgroundThread() const {
  return getBoolFromPath("/optimisation/spawn_prolongation_as_background_thread", false, isOptional);
}

bool exahype::parser::Parser::getSpawnAMRBackgroundThreads() const {
  return getBoolFromPath("/optimisation/spawn_amr_background_threads", false, isOptional);
}

bool exahype::parser::Parser::getSpawnNeighbourMergeAsThread() const {
  return getBoolFromPath("/optimisation/spawn_neighbour_merge_as_thread", false, isOptional);
}

bool exahype::parser::Parser::getDisableMetadataExchangeInBatchedTimeSteps() const {
  return getBoolFromPath("/optimisation/disable_metadata_exchange_in_batched_time_steps", false, isOptional);
}

bool exahype::parser::Parser::getDisablePeanoNeighbourExchangeInTimeSteps() const {
  return getBoolFromPath("/optimisation/disable_vertex_exchange_in_time_steps", false, isOptional);
}

double exahype::parser::Parser::getTimestepBatchFactor() const {
  double result = getDoubleFromPath("/optimisation/time_step_batch_factor", 0.0, isOptional);
  logDebug("getFuseAlgorithmicStepsFactor()", "found time-step-batch-factor " << result);

  if (result < 0.0 || result > 1.0) {
    logError("getFuseAlgorithmicStepsFactor()",
              "'time-step-batch-factor': Value is required in global-optimisation "
              "section and must be greater than zero and smaller than one: "
                  << result);
    result = 0.0;
    invalidate();
  }
  return result;
}


bool exahype::parser::Parser::hasOptimisationSegment() const {
  return hasPath("/optimisation");
}


bool exahype::parser::Parser::getSkipReductionInBatchedTimeSteps() const {
  if (hasOptimisationSegment()) {
    return tarch::la::greater(getTimestepBatchFactor(),0.0);
  }
  else return false;
}


double exahype::parser::Parser::getDoubleCompressionFactor() const {
  double result = getDoubleFromPath("/optimisation/double_compression", 0.0, isOptional);

  if (result < 0.0) {
    logError("getDoubleCompressionFactor()",
            "'double-compression': Value is required in global-optimisation "
            "section and must be greater than or equal to zero: " << result);
    result = 0.0;
    invalidate();
  }

  return result;
}


bool exahype::parser::Parser::getSpawnDoubleCompressionAsBackgroundTask() const {
  return getBoolFromPath("/optimisation/spawn_double_compression_as_background_thread");
}


exahype::solvers::Solver::Type exahype::parser::Parser::getType(int solverNumber) const {
  exahype::solvers::Solver::Type result =
      exahype::solvers::Solver::Type::ADERDG;
  std::string token = getStringFromPath(sformat("/solvers/%d/type", solverNumber));
  if (_identifier2Type.find(token) != _identifier2Type.end()) {
    result = _identifier2Type.at(token);
    logDebug("getType()", "found type " << exahype::solvers::Solver::toString(result));
  } else {
    logError(
        "getType()",
        "'" << getIdentifier(solverNumber) << "': 'type': Value '" << token
            << "' is invalid. See the ExaHyPE documentation for valid values.");
    invalidate();
  }
  return result;
}

std::string exahype::parser::Parser::getIdentifier(int solverNumber) const {
  std::string token = getStringFromPath(sformat("/solvers/%d/name", solverNumber));
  logDebug("getIdentifier()", "found identifier " << token);
  return token;
}

int exahype::parser::Parser::countVariablesType(const int solverNumber, const std::string& identifier, const bool isOptional) const {
  try {
    std::string path = sformat("/solvers/%d/%s", solverNumber, identifier.c_str());
    auto j = _impl->data.at(json::json_pointer(path));

    int result = 0;
    if(j.is_number_integer()) {
      // variables=N
      result += getIntFromPath(path);
    } else if (j.is_array()) {
      // count multiplicities
      for (json::iterator it = j.begin(); it != j.end(); ++it) {
        auto variable = j.at(json::json_pointer(sformat("/%d", it - j.begin())));
        if (variable.is_string()) {  // variables=["foo","bar","baz"], meaning variables=3
          result++;
        } else {
          // the lazy way, constructing the global path again. Side note: We require multiplicity, we don't support an implicit 1.
          result += static_cast<int>(j.at(json::json_pointer(sformat("/%d/multiplicity", it - j.begin()))));
        }
      }
    }
    /*
    //TODO Sven: are parser with 0 variables as in CCZ4 valid?
    if(result == 0) {
      logError("countVariablesType()", "'" << getIdentifier(solverNumber)
               << "': '"<<identifier<<"': Value must be greater than zero (integer), "
                   "a list of identifiers (string), or a list of objects with fields 'name' (string) and 'multiplicity' (integer).");
      invalidate();
    }
    */
    return result;
  } catch(json::out_of_range& e) { // not found
    if( !isOptional ) {
      logError("countVariablesType()","'" << getIdentifier(solverNumber)<< "': Field '"<<identifier<<
               "' could not be found or parsed! Reason: "<<e.what());
      invalidate();
    }
    return 0;
  }
}

int exahype::parser::Parser::getVariables(int solverNumber) const {
  return countVariablesType(solverNumber,"variables");
}

int exahype::parser::Parser::getParameters(int solverNumber) const {
  return countVariablesType(solverNumber,"material_parameters",isOptional);
}

int exahype::parser::Parser::getGlobalObservables(int solverNumber) const {
  return countVariablesType(solverNumber,"global_observables",isOptional);
}

int exahype::parser::Parser::getOrder(int solverNumber) const {
  return getIntFromPath(sformat("/solvers/%d/order", solverNumber));
}


double exahype::parser::Parser::getMaximumMeshSize(int solverNumber) const {
  double result = getDoubleFromPath(sformat("/solvers/%d/maximum_mesh_size", solverNumber));

  if (tarch::la::smallerEquals(result, 0.0)) {
    logError("getMaximumMeshSize(int)",
             "'" << getIdentifier(solverNumber)
                 << "': 'maximum-mesh-size': Value must be greater than zero.");
    invalidate();
  }

  logDebug("getMaximumMeshSize()", "found maximum mesh size " << result);
  return result;
}

int exahype::parser::Parser::getMaximumMeshDepth(int solverNumber) const {
  int result = getIntFromPath(sformat("/solvers/%d/maximum_mesh_depth", solverNumber),0,isOptional);

  if (tarch::la::smaller(result, 0)) {
    logError("getMaximumMeshDepth(int)",
             "'" << getIdentifier(solverNumber)
                 << "': 'maximum-mesh-depth': Value must be greater than or equal to zero.");
    invalidate();
  }

  logDebug("getMaximumMeshDepth()", "found maximum mesh size " << result);
  return result;
}

int exahype::parser::Parser::getHaloCells(int solverNumber) const {
  int result = getIntFromPath(sformat("/solvers/%d/halo_cells", solverNumber),0,isOptional);

  if (tarch::la::smaller(result, 0)) {
    logError("getHaloCells(int)",
             "'" << getIdentifier(solverNumber)
                 << "': 'halo-cells': Value must be greater than or equal to zero.");
    invalidate();
  }

  logDebug("getHaloCells()", "found halo-cells " << result);
  return result;
}

int exahype::parser::Parser::getRegularisedFineGridLevels(int solverNumber) const {
  int result = getIntFromPath(sformat("/solvers/%d/regularised_fine_grid_levels", solverNumber),0,isOptional);
  
  if (tarch::la::smaller(result, 0)) {
    logError("getRegularisedFineGridLevels(int)",
             "'" << getIdentifier(solverNumber)
                 << "': 'regularised-fine-grid-levels': Value must be greater than or equal to zero.");
    invalidate();
  }

  logDebug("getRegularisedFineGridLevels()", "found regularised-fine-grid-levels " << result);
  return result;
}

exahype::solvers::Solver::TimeStepping exahype::parser::Parser::getTimeStepping(
    int solverNumber) const {
  exahype::solvers::Solver::TimeStepping result;
  const std::string default_value = "global";
  std::string token = getStringFromPath(sformat("/solvers/%d/time_stepping", solverNumber), default_value, isOptional);
  if (_identifier2TimeStepping.find(token) != _identifier2TimeStepping.end()) {
    result = _identifier2TimeStepping.at(token);
    // logDebug("getTimeStepping()", "found TimeStepping " << result);
    logDebug("getTimeStepping()", "found TimeStepping "<< token);
    return result;
  } else {
    logError(
        "getTimeStepping()",
        "'" << getIdentifier(solverNumber) << "': 'time-stepping': Value '"
            << token
            << "' is invalid. See the ExaHyPE documentation for valid values.");
    invalidate();
  }
  return exahype::solvers::Solver::TimeStepping::Global; /* keep in sync with default_value above */
}

double exahype::parser::Parser::getDMPRelaxationParameter(int solverNumber) const {
  double result = getDoubleFromPath(sformat("/solvers/%d/limiter/dmp_relaxation_parameter", solverNumber));

  if (result < 0) {
    logError("getDMPRelaxationParameter()",
             "'" << getIdentifier(solverNumber)
                 << "': 'dmp-relaxation-parameter': Value must not be negative.");
    invalidate();
  }

  logDebug("getParameters()", "found dmp-relaxation-parameter " << result);
  return result;
}

double exahype::parser::Parser::getDMPDifferenceScaling(int solverNumber) const {
  double result = getDoubleFromPath(sformat("/solvers/%d/limiter/dmp_difference_scaling", solverNumber));

  if (result < 0) {
    logError("getDMPDifferenceScaling()",
             "'" << getIdentifier(solverNumber)
                 << "': 'dmp-difference-scaling': Value must not be negative.");
    invalidate();
  }

  logDebug("getDMPDifferenceScaling()", "found dmp-difference-scaling " << result);
  return result;
}

int exahype::parser::Parser::getDMPObservables(int solverNumber) const {
  int result = getIntFromPath(sformat("/solvers/%d/limiter/dmp_observables", solverNumber));

  if (result < 0) {
    logError("getDMPObservables()",
             "'" << getIdentifier(solverNumber)
                 << "': 'dmp-observables': Value must not be negative.");
    invalidate();
  }

  logDebug("getDMPObservables()", "found dmp-observables " << result);
  return result;
}

std::string exahype::parser::Parser::getTypeForPlotter(int solverNumber,int plotterNumber) const {
  return getStringFromPath(sformat("/solvers/%d/plotters/%d/type", solverNumber, plotterNumber));
}

std::string exahype::parser::Parser::getNameForPlotter(int solverNumber,int plotterNumber) const {
  return getStringFromPath(sformat("/solvers/%d/plotters/%d/name", solverNumber, plotterNumber));
}

int exahype::parser::Parser::getUnknownsForPlotter(int solverNumber,int plotterNumber) const {
  return countVariablesType(solverNumber,sformat("plotters/%d/variables", plotterNumber),isOptional);
}

double exahype::parser::Parser::getFirstSnapshotTimeForPlotter(
    int solverNumber, int plotterNumber) const {
  return getDoubleFromPath(sformat("/solvers/%d/plotters/%d/time", solverNumber, plotterNumber));
}

double exahype::parser::Parser::getRepeatTimeForPlotter(int solverNumber,
                                                int plotterNumber) const {
  return getDoubleFromPath(sformat("/solvers/%d/plotters/%d/repeat", solverNumber, plotterNumber));
}

std::string exahype::parser::Parser::getFilenameForPlotter(int solverNumber,
                                                   int plotterNumber) const {
  return getStringFromPath(sformat("/solvers/%d/plotters/%d/output", solverNumber, plotterNumber));
}

exahype::parser::ParserView exahype::parser::Parser::getParametersForPlotter(int solverNumber,
                                                   int plotterNumber) const {
  // New style: We expect the "parameters" path, and all user-defined/plotter-defined parameters are
  //    within this section. Example:
  //       { 'parameters': {'output_format': 'zipFoo', 'select': {'x':3} } }
  std::string path = sformat("/solvers/%d/plotters/%d/parameters", solverNumber, plotterNumber);
  if ( hasPath(path) ) {
    logInfo("getParametersForPlotter", "Found parameters at " << path);
    return exahype::parser::ParserView(this,path);
  }

  // Old style: There is only the "select" statement which shall be interpreted as the selection
  // query. Example:
  //       { 'select': { 'x':3 } }
  std::string select_path = sformat("/solvers/%d/plotters/%d/select", solverNumber, plotterNumber);
  std::string plotter_path = sformat("/solvers/%d/plotters/%d", solverNumber, plotterNumber);
  if ( hasPath(select_path) ) {
    logInfo("getParametersForPlotter", "Found parameters at " << select_path);
    return exahype::parser::ParserView(this,plotter_path);
  }

  // No Parameters given for parser.
  return exahype::parser::ParserView();
}

std::string exahype::parser::Parser::getLogFileName() const {
  return getStringFromPath("/paths/log_file", "", isOptional);
}

std::string exahype::parser::Parser::getProfilerIdentifier() const {
  return getStringFromPath("/profiling/profiler", "NoOpProfiler", isOptional);
}

exahype::parser::Parser::ProfilingTarget exahype::parser::Parser::getProfilingTarget() const {
  std::string option = getStringFromPath("/profiling/profiling_target", "whole_code", isOptional);

  if ( option.compare("whole_code")!=0 && (
       #ifdef Parallel
       true ||
       #endif
       foundSimulationEndTime())
  ) {
    logError("getProfilingTarget","Profiling target '"<<option<<"' can not be chosen if a simulation end time is specified or a parallel build is run. Only 'whole_code' is allowed in this case.");
    invalidate();
    return ProfilingTarget::WholeCode;
  }

  if ( option.compare("whole_code")==0 ) {
    return ProfilingTarget::WholeCode;
  } else if ( option.compare("neighbour_merge")==0 ) {
    return ProfilingTarget::NeigbhourMerge;
  } else if ( option.compare("update")==0 ) {
    return ProfilingTarget::Update;
  } else if ( option.compare("predictor")==0 ) {
    return ProfilingTarget::Prediction;
  } else {
    logError("getProfilingTarget","Unknown profiling target: "<<option);
    invalidate();
    return ProfilingTarget::WholeCode;
  }
}

bool exahype::parser::Parser::getMeasureCellProcessingTimes() const {
  return getBoolFromPath("/profiling/measure_cell_processing_times", false, isOptional);
}

int exahype::parser::Parser::getMeasureCellProcessingTimesIterations() const {
  const int result = getIntFromPath("/profiling/measure_cell_processing_times_iter", 100, isOptional);

  if ( result <  1 ) {
    logError("getMeasureCellProcessingTimesIterations(...)",";measure-cell-processing-times-iter' must be greater than 0.");
    invalidate();
  }
  return result;
}

std::string exahype::parser::Parser::getMetricsIdentifierList() const {
  return getStringFromPath("/profiling/metrics", "{}", isOptional);
}

std::string exahype::parser::Parser::getProfilingOutputFilename() const {
  return getStringFromPath("/profiling/profiling_output", "", isOptional);
}

void exahype::parser::Parser::checkSolverConsistency(int solverNumber) const {
  assertion1(solverNumber <
                 static_cast<int>(exahype::solvers::RegisteredSolvers.size()),
             solverNumber);
  exahype::solvers::Solver* solver =
      exahype::solvers::RegisteredSolvers[solverNumber];

  bool recompile = false;
  bool runToolkitAgain = false;
  if (solver->getType() != getType(solverNumber)) {
    logError("checkSolverConsistency",
             "'" << getIdentifier(solverNumber)
                 << "': Solver type in specification file "
		 << "('" << exahype::solvers::Solver::toString(getType(solverNumber)) << "') "
                 << "differs from solver type used in implementation "
		 << "('" << exahype::solvers::Solver::toString(solver->getType()) << "').");
    recompile = true;
    invalidate();
  }

  if (solver->getIdentifier().compare(getIdentifier(solverNumber))) {
    logError("checkSolverConsistency",
             "'" << getIdentifier(solverNumber)
                 << "': Identifier in specification file "
                 << "('" << getIdentifier(solverNumber)
                 << "') differs from identifier used in implementation ('"
                 << solver->getIdentifier() << "').");
    recompile = true;
    invalidate();
  }

  if (solver->getNumberOfVariables() != getVariables(solverNumber)) {
    logError("checkSolverConsistency",
             "'" << getIdentifier(solverNumber)
                 << "': Value for 'variables' in specification file"
                 << "('" << getVariables(solverNumber)
                 << "') differs from number of variables used in "
                    "implementation file ('"
                 << solver->getNumberOfVariables() << "').");
    recompile = true;
    invalidate();
  }

  if (solver->getNumberOfParameters() != getParameters(solverNumber)) {
    logError("checkSolverConsistency",
             "'" << getIdentifier(solverNumber)
                 << "': Value for field 'parameters' in specification file"
                 << "('" << getParameters(solverNumber)
                 << "') differs from  number of parameters used in "
                    "implementation file ('"
                 << solver->getNumberOfParameters() << "').");
    recompile = true;
    invalidate();
  }

  if (solver->getType() == exahype::solvers::Solver::Type::ADERDG &&
      solver->getNodesPerCoordinateAxis() != getOrder(solverNumber) + 1) {
    logError("checkSolverConsistency",
             "'" << getIdentifier(solverNumber)
                 << "': Value for field 'order' in specification file "
                 << "('" << getOrder(solverNumber)
                 << "') differs from value used in implementation file ('"
                 << solver->getNodesPerCoordinateAxis() - 1 << "'). ");
    runToolkitAgain = true;
    invalidate();
  }

  // @todo We should add checks for FV as well
  
  // (Sven:) somehow for me the following lines are never printed. I don't
  // know why.

  if (runToolkitAgain) {
    logError("checkSolverConsistency",
             "Please (1) run the Toolkit again for " << getSpecfileName() << ", and (2) recompile!");
    invalidate();
  }

  if (recompile) {
    logError(
        "checkSolverConsistency",
        "Please (1) adjust the specification file (" << getSpecfileName() <<  ") or the file '"
            << solver->getIdentifier()
            << ".cpp' (and similar) accordingly, and (2) recompile!");
    invalidate();
  }
}

std::string exahype::parser::Parser::getSpecfileName() const {
  return _filename;
}

int exahype::parser::Parser::getRanksPerNode() {
  return getIntFromPath("/distributed_memory/ranks_per_node");
}


int exahype::parser::Parser::getNumberOfBackgroundJobConsumerTasks() {
  int result = getIntFromPath("/shared_memory/background_job_consumers",0,isOptional);
  if (result<=0) {
    logInfo("getNumberOfBackgroundTasks()", "no number of background tasks specified. Use default: #consumers = #threads / 4.");
    result = getNumberOfThreads()/4;
  }
  return result;
}

bool exahype::parser::Parser::compareBackgroundJobProcessing(const std::string& strategy) const {
  return getStringFromPath("/shared_memory/background_job_processing","job_system",isOptional).
      compare(strategy)==0;
}

bool exahype::parser::Parser::compareJobSystemWaitBehaviour(const std::string& strategy) const {
  return getStringFromPath("/shared_memory/job_system_wait_behaviour","process_any_jobs",isOptional).
      compare(strategy)==0;
}

int exahype::parser::Parser::getMaxBackgroundJobsInARush() {
  return getIntFromPath("/shared_memory/max_background_jobs_in_one_rush",std::numeric_limits<int>::max(),isOptional);
}

int exahype::parser::Parser::getMinBackgroundJobsInARush() {
  return getIntFromPath("/shared_memory/min_background_jobs_in_one_rush",1,isOptional);
}

bool exahype::parser::Parser::useManualPinning() {
  return getBoolFromPath("/shared_memory/manual_pinning",false,isOptional);
}

exahype::parser::ParserView exahype::parser::Parser::createParserView(const int solverNumberInSpecificationFile) {
  // Note, we used to call this section "constants" in the past, but constants is really a bad name
  // for run time parameters. Parameters is a bad name, too, since we have material parameters which
  // have this name. Maybe "runtime_parameters" would be a good idea. Or just passing "/solver/%d" and
  // letting the user to decide how to call his variables
  return exahype::parser::ParserView(this,sformat("/solvers/%d/parameters", solverNumberInSpecificationFile));
}

exahype::parser::Parser::TBBInvadeStrategy exahype::parser::Parser::getTBBInvadeStrategy() const {
  if (stringFromPathEquals("/shared_memory/invasion_strategy", "no_invade", true, "no_invade"))                                               return TBBInvadeStrategy::NoInvade;
  if (stringFromPathEquals("/shared_memory/invasion_strategy", "no_invade", true, "analyse_optimal_static_distribution_but_do_not_invade"))   return TBBInvadeStrategy::NoInvadeButAnalyseDistribution;
  if (stringFromPathEquals("/shared_memory/invasion_strategy", "no_invade", true, "occupy_all_cores"))                                        return TBBInvadeStrategy::OccupyAllCores;
  if (stringFromPathEquals("/shared_memory/invasion_strategy", "no_invade", true, "invade_between_time_steps"))                               return TBBInvadeStrategy::InvadeBetweenTimeSteps;
  if (stringFromPathEquals("/shared_memory/invasion_strategy", "no_invade", true, "invade_throughout_computation"))                           return TBBInvadeStrategy::InvadeThroughoutComputation;
  if (stringFromPathEquals("/shared_memory/invasion_strategy", "no_invade", true, "invade_at_time_step_startup_plus_throughout_computation")) return TBBInvadeStrategy::InvadeAtTimeStepStartupPlusThroughoutComputation;

  return TBBInvadeStrategy::Undef;
}
