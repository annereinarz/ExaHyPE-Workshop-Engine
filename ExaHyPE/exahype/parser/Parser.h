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

#ifndef EXAHYPE_PARSER_Parser_H
#define EXAHYPE_PARSER_Parser_H

namespace exahype {
namespace parser {
  class Parser;
  class ParserImpl; // pimpl idiom, forward declaration
}
}

#include <iostream>

#include <map>
#include <vector>
#include <utility> // pair
#include <istream>
#include <ostream>

#include "peano/utils/Globals.h"
#include "tarch/la/Vector.h"
#include "tarch/logging/Log.h"

#include "exahype/solvers/Solver.h"

/**
 * ExaHyPE Specification file (Parameters)
 *
 * Under the hood, the old Specfile parser was replaced here by
 * a JSON parser.
 * 
 * This parser is a singleton "by concept", i.e. to keep all methods
 * const, it stores the internal state statically and thus it only 
 * makes sense to use it as a singleton. The state is the validity of
 * the plotter, ie. holds a flag whether an error occured. The idea
 * of this was probably lazy error reporting but I don't see how.
 * (Well, one thing is that you can copy it freely but it will behave
 * like a singleton)
 *
 * @author Tobias Weinzierl, Dominic Etienne Charrier, Sven Koeppel
 */
class exahype::parser::Parser {
  friend class ParserView;
 private:
  static tarch::logging::Log _log;

  static const std::string   _noTokenFound;

  ParserImpl* _impl = nullptr;

  /**
   * Takes certain parameters from the parameters and checks their validity.
   */
  void checkValidity();

  /*
   * Helper map for converting strings to types.
   */
  std::map<std::string, exahype::solvers::Solver::Type> _identifier2Type;

  /*
   * Helper map for converting strings to types.
   */
  std::map<std::string, exahype::solvers::Solver::TimeStepping>
      _identifier2TimeStepping;

  /**
   * Has to be static. If it is not static, then we can't modify it inside
   * const functions, i.e. all getters have to become non-const. This would
   * be reasonable but then in turn enforce all operations accepting parsers
   * to accept them as non-const.
   */
  static bool _interpretationErrorOccured;

  /**
   * Holds the filename of the specification file parsed by this parser
   **/
  std::string _filename;

  /**
   * Count the size of a field of type 'variables', 'material_parameters', 'global_observables'.
   *
   * @param solverNumber index of solver in registry. First solver has index '0'.
   * @param identifier   one out of 'variables', 'material_parameters', 'global_observables'
   */
  int countVariablesType(const int solverNumber, const std::string& identifier, const bool isOptional=false) const;

 public:
  /**
   * \defgroup JsonFrontend High-level API to read from JSON data
   * 
   * These functions allow to use JSON pointers (RFC 6901), hereby
   * called "paths", to address anything in nested data structures.
   * An example of such a path is "/solvers/0/name". Obviously, it
   * allows to mix dictionary/object and array/list indices.
   * 
   * Suppport for all native data types (bool,int,double,string) is
   * provided.
   * 
   * For the readers, if isOptional is given, will return defaultValue
   * if not found. However, if found but not of the requested type
   * (not castable), will logError().
   * If not isOptional is given, will logError also if path is not
   * found. Will not throw exception but return a nonsense value
   * instead and invalidate the Parser. This follows the old
   * Parser idiom.
   * 
   * There are also functions for high level access to vectors
   * and simple versions of ExaHyPE's favourite "property lists",
   * i.e. arrays containing flag keywords.
   * 
   * @{
   **/
  
  /// Checks whether a path is present, irrelevant from it's type
  bool hasPath(std::string path) const;
  
  /// Will spill out JSON
  std::string dumpPath(std::string path) const;
  
  bool isValueValidBool(const std::string& path) const;
  bool isValueValidInt(const std::string& path) const;
  bool isValueValidDouble(const std::string& path) const;
  bool isValueValidString(const std::string& path) const;
  
  /**
   * @return a string from a JSON path from the configuration file or the default value
   *
   * @note Will logError in case of errors and invalidate the parser.
   *
   * @param path         path to the string
   * @param defaultValue default value
   * @param isOptional   parameter is optional
   */
  std::string getStringFromPath(std::string path, std::string defaultValue="", bool isOptional=false) const;
  int getIntFromPath(std::string path, int defaultValue=-1, bool isOptional=false) const;
  double getDoubleFromPath(std::string path, double defaultValue=-1, bool isOptional=false) const;
  bool getBoolFromPath(std::string path, bool defaultValue=true, bool isOptional=false) const;
  std::vector< std::pair<std::string, std::string> > getObjectAsVectorOfStringPair(std::string path, bool isOptional=false) const;
  
  /**
   * Compare string from a JSON path from the configuration file to
   * another string.
   *
   * @return `true` if the strings match.
   *
   * @note Will logError in case of errors and invalidate the parser.
   *
   * @param path         path to the string
   * @param defaultValue default value
   * @param isOptional   parameter is optional
   */
  bool stringFromPathEquals(const std::string& path, const std::string& defaultValue,const bool isOptional, const std::string& otherString) const;

  /**
   * Read a double-Vector of length DIMENSIONS from a JSON path from the configuration file.
   * Will logError in case of errors and invalidate the parser.
   **/
  tarch::la::Vector<DIMENSIONS,double> getDimVectorFromPath(std::string path) const;
  
  /**
   * Get a STL int-vector with any length from a JSON path from the configuration file.
   * Will logError in case of errors and invalidate the parser.
   **/
  std::vector<int> getIntVectorFromPath(std::string path) const;
  
  
  /**
   * Returns true if a flag list (i.e. an array of strings) contains a keyword, at the 
   * JSON path position.
   * A nonexisting array is interpreted as false.
   **/
  bool flagListContains(std::string path, std::string keyword) const;
  
  /// Readable boolean "enums"
  static constexpr bool isOptional = true;
  static constexpr bool isMandatory = false;
  /**
   * @}
   **/
   
  /**
   * This is old and shout no more be used, since it requires parsing of strings.
   * TODO: Detect any usage of getValueFromPropertyString and replace it with
   * a proper ParserView, map or list where they can look up the value by their
   * selves, or something else.
   *
   * Property strings in ExaHyPE are string alike "{all,left=0.5,Q4}". This
   * operation returns the value of a property, i.e. if you invoke
   * getvalueFromProperyString( "left" ), you obtain 0.5 in the example
   * above. The routine returns nan if now entry is found or the entry's
   * value  is not a valid floating point number.
   */
  static double getValueFromPropertyString(const std::string& parameterString,
                                           const std::string& key);

  Parser();
  virtual ~Parser();

  // Disallow copy and assignment
  Parser(const Parser& other) = delete;
  Parser& operator=(const Parser& other) = delete;

  enum class MulticoreOracleType {
    Dummy,
    AutotuningWithRestartAndLearning,
    AutotuningWithoutLearning,
    AutotuningWithLearningButWithoutRestart,
    GrainSizeSampling
  };

  enum class MPILoadBalancingType { Static, Dynamic };


  void readFile(const std::string& filename);
  void readFile(std::istream& inputFile, std::string filename="");

  bool isValid() const;
  void invalidate() const;

  /**
   * @return How many threads is the code supposed to use?
   */
  int getNumberOfThreads() const;

  /**
   * @return The thread stack size. Default
   * is 0 which is tranlated to a library-specific default value.
   * (TBB default: 2 MB or 4 MB).
   * (Alexey Kukanov (Intel), Thu, 08/12/2010 - 13:10)
   * https://software.intel.com/en-us/forums/intel-threading-building-blocks/topic/288253
   */
  int getThreadStackSize() const;

  tarch::la::Vector<DIMENSIONS, double> getDomainSize() const;

  tarch::la::Vector<DIMENSIONS, double> getOffset() const;

  std::string getMulticorePropertiesFile() const;

  MulticoreOracleType getMulticoreOracleType() const;

  MPILoadBalancingType getMPILoadBalancingType() const;

  /**
   * @return `true` if the parsed strategy matches the argument.
   * @param strategy one of "FCFS" (first-come-first-served), "fair", "sfc-diffusion"
   */
  bool compareNodePoolStrategy(const std::string& strategy) const;

  /**
   * @return `true` if the parsed strategy matches the argument.
   * @param strategy one of "greedy_naive", "greedy_regular", "hotspot"
   */
  bool compareMPILoadBalancingStrategy(const std::string& strategy) const;

  /**
   * @return the number of forks the load balancing
   * mapping is allowed to trigger per mesh refinement iteration.
   */
  int getMaxForksPerLoadBalancingStep() const;

  /**
   * @note It is important to scale the bounding box if MPI
   * experiments are run as this prevents communication
   * with rank 0. More importantly, it prevents that the
   * global master has too many outside cells which it traverses
   * before and after sending a kick off message to its workers.
   *
   * @return number of cells placed outside of the domain per coordinate axis.
   */
  int getOutsideCells() const;

  /**
   * @return number of outside cells which should be placed on the "left" side
   * in each coordinate direction. Default is getOutsideCells()/2.
   */
  int getOutsideCellsLeft() const;

  /**
   * Places one third of the bounding box cells per coordinate direction (plus 2 cells)
   * outside of the domain.
   *
   * This way we can put 2^d ranks on the coarsest grid (cubic domains).
   * This flag overrules the 'outside_cells' and 'outside_cells_left' parameters.
   *
   * @return true if the feature is enabled.
   */
  bool getPlaceOneThirdOfCellsOuside() const;

  /**
   * @return if the bounding box is scaled in any way.
   */
  bool getScaleBoundingBox() const;

  int getMPIBufferSize() const;

  int getMPITimeOut() const;

  double getSimulationEndTime() const;

  /**
   * @return if the simulation end time can be
   * found in the parsed specification file.
   */
  bool  foundSimulationEndTime() const;

  /**
   * @return the number of time steps the
   * simulation shall be run (0 is a valid value)
   */
  int  getSimulationTimeSteps() const;

  /**
   * @return the maximum number of grid setup iterations.
   * This is mainly a debugging option as the grid might be instable when the mesh setup is stopped
   * abruptly.
   */
  int getMaxMeshSetupIterations() const;

  /**
   * @return if static mesh refinement is used. Otherwise, or if not specified,
   * it is assumed dynamic mesh refinement is used.
   */
  bool getStaticMeshRefinement() const;

  /**
   * @return if dynamic mesh refinement is used. Otherwise, or if not specified,
   * it is assumed dynamic mesh refinement is used.
   */
  bool getStaticLimiting() const;

  /**
   * @return Indicates if the user has chosen the ADER-DG time stepping variant
   * where all three phases are fused into one loop.
   *
   * If the parser returns _noTokenFound, we may not issue an error as this is
   * an optional entry in the spec file.
   */
  bool getFuseAllAlgorithmicSteps() const;

  /**
   * @return Indicates if the user has chosen the ADER-DG time stepping variant
   * where the corrector and update loop are fused into one loop.
   *
   * If the parser returns _noTokenFound, we may not issue an error as this is
   * an optional entry in the spec file.
   */
  bool getFuseMostAlgorithmicSteps() const;

  /**
   * @return Time step size factor for the fused ADER-DG time stepping for nonlinear PDEs
   * used when a rerun is performed.
   *
   * @note is only used if a nonlinear PDE is solved and all algorithmic phases are fused.
   */
  double getFuseAlgorithmicStepsRerunFactor() const;

  /**
   * @return Time step size factor for the fused ADER-DG time stepping for nonlinear PDEs
   * used when a rerun is performed.
   *
   * @note is only used if a nonlinear PDE is solved and all algorithmic phases are fused.
   */
  double getFuseAlgorithmicStepsDiffusionFactor() const;

  /**
   * @return if the predictor and the first and intermediate fused time steps should be
   * spawned as background thread.
   */
  bool getSpawnPredictionAsBackgroundThread() const;

  /**
   * @return if the update or the last fused timste iterations
   *  should be spawned as background.
   */
  bool getSpawnUpdateAsBackgroundThread() const;

  /**
   * @return if the prolongation should be spawned as background
   * thread whenever this is possible.
   */
  bool getSpawnProlongationAsBackgroundThread() const;

  /**
   * @return if the mesh refinement iterations should
   * use background-threads whenever this is possible.
   */
  bool getSpawnAMRBackgroundThreads() const;

  /**
   * @return If an additional 4 or 12 tasks (2D and 3D) should be spawned per vertex.
   */
  bool getSpawnNeighbourMergeAsThread() const;

  double getTimestepBatchFactor() const;
  bool getSkipReductionInBatchedTimeSteps() const;

  /**
   * Is used in the runner to set the solver's compression accuracy. For
   * details, please consult Runner::initDataCompression().
   */
  double getDoubleCompressionFactor() const;
  bool   getSpawnDoubleCompressionAsBackgroundTask() const;

  /**
   * If we batch time steps, we can in principle switch off the Peano boundary data
   * exchange, as ExaHyPE's data flow is realised through heaps. However, if we
   * turn off the boundary exchange, we enforce that no AMR and load balancing
   * is done in-between time steps.
   */
  bool getDisablePeanoNeighbourExchangeInTimeSteps() const;

  /**
   * If we batch time steps, we can in principle switch off the
   * exchange of ExaHyPE metadata if and only if no dynamic limiting
   * and no dynamic AMR is used.
   *
   * @note That this is upgraded to all time stepping communication
   * if you turn getDisablePeanoNeighbourExchangeDuringTimeSteps()
   * returns true as well.
   */
  bool getDisableMetadataExchangeInBatchedTimeSteps() const;

  /**
   * @return The type of a solver.
   */
  exahype::solvers::Solver::Type getType(int solverNumber) const;

  /**
   * @return The identifier of a solver.
   */
  std::string getIdentifier(int solverNumber) const;

  /**
   * @return The number of state variables of a solver.
   */
  int getVariables(int solverNumber) const;

  /**
   *
   *
   * @return The number of parameters of a solver, e.g. material values etc.
   */
  int getParameters(int solverNumber) const;

  /**
   * @return The number of global observables, e.g. energies, coordinates, errors, ...
   */
  int getGlobalObservables(int solverNumber) const;

  /**
   * @return The order of the ansatz polynomials of a solver.
   */
  int getOrder(int solverNumber) const;

  /**
   * @return The maximum extent in each coordinate direction a cell is allowed
   * to have.
   */
  double getMaximumMeshSize(int solverNumber) const;

  /**
   * @return The maximum adaptive mesh depth as specified
   * by the user.
   *
   * @note If the user has not specified an adaptive
   * mesh depth, 0 is returned.
   */
  int getMaximumMeshDepth(int solverNumber) const;

  /**
   * @return The number of halo cells that are refined around a
   * a cell on the finest allowed mesh level which wants to be kept
   * or refined further.
   *
   * @note If the user has not specified this optional value, 0 is returned.
   */
  int getHaloCells(int solverNumber) const;

  /**
   * @return The number of regularised fine grid levels.
   *
   * @note If the user has not specified this optional value, 0 is returned.
   */
  int getRegularisedFineGridLevels(int solverNumber) const;

  /**
   * Checks for inconsistencies between the ExaHyPE specification file
   * and the build. Stops the program with an error
   * if both are inconsistent.
   *
   * The fields type, identifier, variables, parameters, and order
   * are considered in the inconsistency check.
   */
  void checkSolverConsistency(int solverNumber) const;

  /**
   * @return The time stepping mode of a solver.
   */
  exahype::solvers::Solver::TimeStepping getTimeStepping(
      int solverNumber) const;

  bool hasOptimisationSegment() const;

  /**
   * @return The relaxation parameter used for the discrete maximum principle (DMP).
   *
   * @note This value can only be read in if the solver \p solverNumber is
   * a limiting ADER-DG solver.
   */
  double getDMPRelaxationParameter(int solverNumber) const;

  /**
   * @return The maximum-minimum difference scaling used for the discrete maximum principle (DMP).
   *
   * @note This value can only be read in if the solver \p solverNumber is
   * a limiting ADER-DG solver.
   */
  double getDMPDifferenceScaling(int solverNumber) const;

  /**
   * @return The number of observables that should be considered
   * within the discrete maximum principle.
   *
   * @note This value can only be read in if the solver \p solverNumber is
   * a limiting ADER-DG solver.
   */
  int getDMPObservables(int solverNumber) const;

  /**
   * In the ExaHyPE specification file, a plotter configuration has
   * the following signature:
   *
   * plot <identifier> <name>
   *  variables = <variables>
   *  time      = <first-snapshot-time>
   *  repeat    = <repeat-time>
   *  output    = <filename>
   *  select    = <selector>
   * end plot
   */
  std::string getTypeForPlotter(int solverNumber,
                                      int plotterNumber) const;
  std::string getNameForPlotter(int solverNumber,
                                int plotterNumber) const;
  int getUnknownsForPlotter(int solverNumber, int plotterNumber) const;
  double getFirstSnapshotTimeForPlotter(int solverNumber,
                                        int plotterNumber) const;
  double getRepeatTimeForPlotter(int solverNumber, int plotterNumber) const;
  std::string getFilenameForPlotter(int solverNumber, int plotterNumber) const;

  /**
   * @return A parser view for the parameters section.
   * Invalid parser view if no parameters section is present.
   *
   * @param solverNumber  index of the solver in the spec file
   * @param plotterNumber index of the plotter in the spec file
   */
  exahype::parser::ParserView getParametersForPlotter(int solverNumber, int plotterNumber) const;

  std::string getProfilerIdentifier() const;
  std::string getMetricsIdentifierList() const;
  std::string getProfilingOutputFilename() const;

  /**
   * The profiling target.
   */
  enum class ProfilingTarget {
    WholeCode,     //!< The whole code is profiled
    NeigbhourMerge,//!< The neighbour merge phase is profiled
    Prediction,    //!< The prediction phase is profiled
    Update         //!< The update phase is profiled
  };

  /**
   * Specify what code part you plan to run/profile.
   */
  ProfilingTarget getProfilingTarget() const;

  /**
   * @return if the cell processing times (time per update, min/max time per prediction) should be measured per solver.
   */
  bool getMeasureCellProcessingTimes() const;

  /**
   * @return number of measurements to er
   *
   * @see getMeasureCellProcessingTimes()
   */
  int getMeasureCellProcessingTimesIterations() const;

  /**
   * @TODO This function should be renamed to createParserViewForSolver, as we also
   * now also create ParserViews for plotters.
   **/
  exahype::parser::ParserView createParserView(int solverNumber);
  exahype::parser::ParserView createParserView(std::string basePath);

  /**
   * Returns an empty string if no log file is specified in the file.
   */
  std::string getLogFileName() const;

  /**
   * Always returns a valid value (or default if not specified).
   */
  double getNodePoolAnsweringTimeout() const;

  int getRanksPerNode();

  /**
   * \return Maximum number of running background job consumer tasks.
   */
  int getNumberOfBackgroundJobConsumerTasks();

  /**
   * @return If the given @p strategy matches the one specified in the specification file.
   */
  bool compareBackgroundJobProcessing(const std::string& strategy) const;

  /**
   * @return If the given @p strategy matches the one specified in the specification file.
   */
  bool compareJobSystemWaitBehaviour(const std::string& strategy) const;

  /**
   * @return Minimum number of background jobs a consumer grabs from the queue in a single rush (default: 1).
   */
  int getMinBackgroundJobsInARush();
  /**
   * @return Maximum number of background jobs a consumer grabs from the queue in a single rush
   * (default: ~maximum integer number)
   */
  int getMaxBackgroundJobsInARush();

  bool useManualPinning();

  /**
   * Returns the filename of the specfile represented by this Parser. Can
   * be empty if the user specified no filename.
   **/
  std::string getSpecfileName() const;

  enum class TBBInvadeStrategy {
    Undef,
    NoInvade,
    OccupyAllCores,
    NoInvadeButAnalyseDistribution,
    InvadeBetweenTimeSteps,
    InvadeThroughoutComputation,
    InvadeAtTimeStepStartupPlusThroughoutComputation
  };

  TBBInvadeStrategy getTBBInvadeStrategy() const;
};

#endif
