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
 
#ifndef _EXAHYPE_RUNNERS_RUNNER_H_
#define _EXAHYPE_RUNNERS_RUNNER_H_

#include "exahype/parser/Parser.h"
#include "tarch/logging/Log.h"

#include "exahype/State.h"

#include "tarch/multicore/MulticoreDefinitions.h"

namespace exahype {
  namespace runners {
    class Runner;
  }
  namespace repositories {
    class Repository;
  }

  namespace records {
    class RepositoryState;
  }
}

#ifdef TBBInvade
#include "shminvade/SHMInvade.h"
#endif

/**
 * Runner
 *
 */
class exahype::runners::Runner {
 private:
  static tarch::logging::Log _log;

  exahype::parser::Parser& _parser;
  std::vector<std::string>& _cmdlineargs;


  /**
   * The computational domain offset as used by the
   * repository.
   *
   * \note Is initialised in ::createRepository.
   */
  tarch::la::Vector<DIMENSIONS,double> _domainOffset;

  /**
   * The computational domain size as used by the
   * repository.
   *
   * \note Is initialised in ::createRepository.
   */
  tarch::la::Vector<DIMENSIONS,double> _domainSize;

  /**
   * The bounding box size used by the repository.
   *
   * The bounding box embeds the computational
   * domain into a cube with extent greater than
   * or equalt to the largest extent of the computational
   * domain (see ::_domainSize).
   *
   * \note Is initialised in ::createRepository.
   */
  double _boundingBoxSize;

  /**
   * The mesh spacing used to partition the
   * bounding box.
   */
  double _boundingBoxMeshSize;

  /**
   * Statistics.
   */
  int _meshRefinements;
  int _localRecomputations;
  int _predictorReruns;

  #ifdef TBBInvade
  shminvade::SHMInvade*  _shmInvade;
  #endif

  /**
   * Updates the statistics.
   */
  void updateStatistics();

  void printStatistics();

  /**
   * Parses global optimisations and
   * prints them out. Must be used for all ranks.
   */
  void initOptimisations() const;

  /**
   *
   */
  void initProfiling();

  /**
   * Setup the oracles for the shared memory parallelisation. Different
   * oracles can be employed:
   *
   * - If no autotuning is used and no valid properties file is provided and
   *   the code is compiled with -DPerformanceAnalysis, we use the grain size
   *   sampling
   * - If no autotuning is used and no valid properties file is provided, we
   *   use the default oracle coming along with the Peano kernel
   * - If autotuning is enabled and no valid properties file is provided, we
   *
   *
   * <h2>Invocation sequence</h2>
   *
   * It is important that we init the shared memory environment after we have
   * created the repository. See Orace::loadStatistics().
   */
  void initSharedMemoryConfiguration();
  void shutdownSharedMemoryConfiguration();

  /**
   * The distributed memory environment has to be set up before we create the
   * repository.
   */
  void initDistributedMemoryConfiguration();
  void shutdownDistributedMemoryConfiguration();

  int runAsMaster(exahype::repositories::Repository& repository);

#ifdef Parallel
  int runAsWorker(exahype::repositories::Repository& repository);

  /**
   * If the master node calls runGlobalStep() on the repository, all MPI
   * ranks besides rank 0 invoke this operation no matter whether they are
   * idle or not. Hence, please call this operation manually within
   * runAsMaster() if you require the master to execute the same function
   * as well.
   */
  void runGlobalStep();
#endif

  /**
   * Initialise the solver time stamps as well as metainformation
   * such as the coarsest mesh level, the maximum
   * adaptive mesh level etc.
   *
   * Runs through the solver registry only,
   * i.e. no grid traversal is required.
   */
  void initSolvers() const;

  /**
   * Measure the time it takes for the solver to perform
   * a single cell update or predictor computation.
   * Print the result to the standard output.
   */
  void measureCellProcessingTimes() const;

  /**
   * Plot some information about the current mesh setup iteration.
   */
  void printMeshSetupInfo(
      exahype::repositories::Repository& repository,
      const int meshSetupIterations) const;

  void validateInitialSolverTimeStepData(const bool fuseADERDGPhases) const;

  /**
   * Initialise the data compression (or switch it off if we don't need it).
   * The routine is called for each and every rank.
   */
  void initDataCompression();

  /**
   * Initialise all the stuff required for measurements, e.g.
   *
   * - Switch off Peano's performance analysis for all ranks.
   *   Otherwise you'll get tons of data for the grid construction.
   *   Codes typically are only interested in time stepping performance data.
   *
   *   Peano's performance analysis is activated again after the mesh refinement
   *   in exahype::mappings::FinaliseMeshRefinement::endIteration(State).
   *
   *   The performance analysis is stopped in the Runner and not in the first
   *   mesh refinement iteration's beginIteration(...) since otherwise a
   *   beginIteration measurement has been started already when the
   *   mesh refinement's beginIteration(...) method is called.
   */
  void initHPCEnvironment();

  /**
   * Initialise the heaps for storing and exchanging
   * degrees of freedom, solver metadata, and solver
   * patches.
   */
  void initHeaps();

  /**
   * Shutdown all heaps.
   */
  void shutdownHeaps();

  /**
   * Print minimum of current solver time stamps and time step sizes.
   *
   * The solver time stamp and step sizes are computed as
   * minimum over the patch quantities.
   *
   * \param numberOfStepsRanSinceLastCall The number of time step done since
   *          the last call. If you pass -1, then you've done only preparatory
   *          time steps so far.
   *
   * \note A value \p numberOfStepsRanSinceLastCall greater than 1 is only interesting
   *       for fixed time stepping runs.
   */
  void printTimeStepInfo(int numberOfStepsRanSinceLastCall, const exahype::repositories::Repository& repository);

  /**
   * Determine how many time steps we can group into one batch until
   * we either reach the user prescribed simulation end time or
   * the user prescribed maximum number of time steps.
   *
   * The number of batched time steps is determined as minimum of
   * the three ingredients:
   * - estimated number of steps till the next plot weighted by a factor smaller 1.0,
   * - (estimated) number of steps till the simulation end time weighted by a factor smaller 1.0,
   * - number of steps till the maximum number of time steps is reached.
   *
   * Time steps are estimated based on the maximum time stamp of all solvers
   * and the minimum time step size of all solvers.
   */
  int determineNumberOfBatchedTimeSteps(const int& currentTimeStep);

  /**
   * Do one time step where all phases are actually fused into one traversal
   *
   * @param numberOfStepsToRun Number of steps to run. If you hand in 0, then
   *           it runs one time step plus does a plot.
   */
  void runTimeStepsWithFusedAlgorithmicSteps(exahype::repositories::Repository& repository, int numberOfStepsToRun);

  /**
   * Run the three (four for MPI) adapters necessary for initialising the
   * limiter domain.
   */
  void initialiseMesh(exahype::repositories::Repository& repository);

  /**
   * TODO(Dominic): Add docu.
   */
  void updateMeshOrLimiterDomain(
      exahype::repositories::Repository& repository,
      const bool fusedTimeStepping);


  /**
   * Do one time step but actually use two algorithmic steps (2 to 3 loops) to do so.

   * @param plot       Do plot before the corrector is applied
   */
  void runOneTimeStepWithTwoSeparateAlgorithmicSteps(
      exahype::repositories::Repository& repository, bool plot);

  /**
   * Do one time step but actually use three algorithmic steps (3 to 4 loops) to do so.
   *
   * @param plot       Do plot before the corrector is applied
   */
  void runOneTimeStepWithThreeSeparateAlgorithmicSteps(
      exahype::repositories::Repository& repository, bool plot);

  /**
   * Print grid statistics, e.g. number of inner cells, unrefined inner cells (leafs) ... .
   */
  void printGridStatistics(repositories::Repository& repository);

  /**
   * Run the prediction step in isolation.
   */
  void runPredictionInIsolation(repositories::Repository& repository);

  /**
   * Per dimension, computes the smallest multiplicity of the coarsest solver mesh size
   * which is larger than the domain size.
   */
  tarch::la::Vector<DIMENSIONS, double> determineDomainSize() const;

  /**
   * @return Bounding box size. If we have a non-cubical domain,
   *         then the bounding box still is cubical and all of its entries are
   *         the biggest dimension along one coordinate axis.
   */
  double determineBoundingBoxSize(const tarch::la::Vector<DIMENSIONS, double>& domainSize) const;

  /**
   * Sets up the geometry, hands it over to a new instance of the repository
   * and returns the repository.
   *
   * Sets the _boundingBoxSize field to the
   * bounding box used for the repository.
   *
   * <h2>Bounding box scaling</h2>
   * If the user switches on the bounding box scaling,
   * we determine a minimum bounding box mesh refinement level lBB
   * and a minimum bounding box width HBB which
   * yields a mesh that resolves the domain boundary accurately.
   *
   * Since there are arbitrary many solutions to this problem,
   * we add the following constraints:
   *
   * 1.Choose the number of elements for resolving
   * the boundary as N=3^lBB - 2, i.e. have two elements
   * outside of the domain.
   *
   * 2. The new mesh size hBB = HBB/(3^lBB) must
   * be smaller than or equal to the user's mesh size hD,
   * i.e, lBB must be larger or equal to the user's
   * coarsest mesh level lD with hD > HD/3^lD.
   *
   * We thus end up with the following problem:
   *
   * Minimise lBB and vary HBB,xBB in order to satisfy
   * N*hBB      = HD, (1)
   * xBB + hBB  = xD, (2)
   *
   * with
   * hBB = HBB/(3^lBB), HBB: bounding box size,
   * xBB: bounding box offset,
   * xD: domain offset,
   * HD: domain size.
   *
   * Solution:
   * From the first constraint, we have that N=3^lBB-2.
   *
   * We then have from (1)-(2) and expanding hBB:
   *
   * HBB = 3^lBB / 3^lBB-2 * HD,
   * xBB = xD - HBB/3^lBB,
   *
   * where lBB >= lD is the first lBB such
   * that hBB <= hD.
   */
  exahype::repositories::Repository* createRepository();

  /**
   * Constructs the initial computational grid
   *
   * The grid generation is an iterative process as it may happen that a grid
   * is refined by a cell initialisation. Cell-based refinements however never
   * are realised in the exactly same iteration, so we always have to wait yet
   * another iteration.
   *
   * This solves immediately another problem: ExaHyPE's adjacency management
   * requires us to run once more over the grid at least once its completely
   * built up to get all the adjacency information correct. If we rely on the
   * grid to become stationary, this is always the case - as long as additional
   * vertices are added, the grid remains instationary. When we've added the
   * last grid entities and run the adapter once again, then it becomes
   * stationary.
   *
   * For the parallel case, I've changed from stationary into balanced which is
   * a slight generalisation. See Peano guidebook.
   *
   * TODO(Dominic): We might not need a few of the other checks anymore after I
   * have introduced the grid refinement requested flag.
   *
   * Background Job Consumer Threads
   * -------------------------------
   *
   * During the mesh refinement iterations, the number of
   * background job consumer threads is set to zero. At
   * the end of each iterations, all consumers are
   * then started up to process the memorised background jobs.
   * This procedure aims to distribute the allocated data
   * more fairly among the sockets of a node.
   *
   * Note that the TBB Jobs class will always run
   * a single consumer thread. So, the behaviour
   * described is only realised approximately.
   */
  bool createMesh(exahype::repositories::Repository& repository);

  /**
   * Run through all the solvers and identify the coarsest grid level in the tree
   * that will be populated by a solver.
   */
  int getCoarsestGridLevelOfAllSolvers(const double boundingBoxSize) const;

  /**
   * The same as ::getCoarsestGridLevelOfAllSolvers but
   * returns at least a value of 3 (Peano) levels.
   */
  int getCoarsestGridLevelForLoadBalancing(const double boundingBoxSize) const;

  /**
   * Run through all the solvers and identify the finest grid level in the tree
   * that will be populated by a solver in a uniform manner.
   */
  int getFinestUniformGridLevelOfAllSolvers(const double boundingBoxSize) const;

  /**
   * The same as ::getFinestUniformGridLevelOfAllSolvers but
   * returns at least a value of 3 (Peano) levels.
   */
  int getFinestUniformGridLevelForLoadBalancing(const double boundingBoxSize) const;

  /**
   * Compute the coarsest mesh size according to the bounding box size.
   */
  double determineCoarsestMeshSize(
      const tarch::la::Vector<DIMENSIONS, double>& boundingBoxSize) const;

  /**
   * Find the smallest scaled computational domain consisting of hypercubes
   * such that the original computational domain fits completely into it.
   *
   * We scale Peano's bounding box such that the longest edge
   * of the rectangular computational domain aligns with it.
   * We further use only hypercubes to build our mesh.
   * The shorter edges of the computational domain thus cannot be resolved
   * exactly in general.
   *
   * They might be either clipped or we scale the computational domain a
   * little bit such that the shorter edges are also integral multiples of the
   * edge lengths of the hypercubes.
   *
   * We do the latter here in order to enable boundary treatment approaches
   * such as the immersed boundary method.
   */
  tarch::la::Vector<DIMENSIONS, double> determineScaledDomainSize(
      const tarch::la::Vector<DIMENSIONS, double>& domainSize,
      const double meshSize) const;


  void preProcessTimeStepInSharedMemoryEnvironment();
  void postProcessTimeStepInSharedMemoryEnvironment();
 public:
  explicit Runner(exahype::parser::Parser& parser, std::vector<std::string>& cmdlineargs);
  virtual ~Runner();

  // Disallow copy and assignment
  Runner(const Runner& other) = delete;
  Runner& operator=(const Runner& other) = delete;

  /**
   * Run
   */
  int run();
};

#endif
