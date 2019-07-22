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
 
#ifndef _EXAHYPE_STATE_H_
#define _EXAHYPE_STATE_H_

#include "exahype/records/State.h"
#include "exahype/records/RepositoryState.h"
#include "peano/grid/State.h"

#include <vector>
#include <memory>

#include "peano/grid/Checkpoint.h"

/**
 * Forward declaration
 */
namespace exahype {
  class State;
  class Vertex;
  class Cell;

  namespace repositories {
    class RepositoryArrayStack;
    class RepositorySTDStack;
  }
}

/**
 * Blueprint for solver state.
 *
 * This file has originally been created by the PDT and may be manually extended
 *to
 * the needs of your application. We do not recommend to remove anything!
 */
class exahype::State : public peano::grid::State<exahype::records::State> {
 private:
  static tarch::logging::Log _log;

  static int CurrentBatchIteration;
  static int NumberOfBatchIterations;

  typedef class peano::grid::State<exahype::records::State> Base;

  /**
   * Needed for checkpointing.
   */
  friend class exahype::repositories::RepositoryArrayStack;
  friend class exahype::repositories::RepositorySTDStack;

  void writeToCheckpoint(
      peano::grid::Checkpoint<Vertex, Cell>& checkpoint) const;
  void readFromCheckpoint(
      const peano::grid::Checkpoint<Vertex, Cell>& checkpoint);

  /**
   * Kick off an iteration, i.e. reset values that are reduced over the cells (and MPI ranks).
   *
   * @param action                  repository action indicating what time step (fused vs non-fused) is run
   * @param currentBatchIteration   the current batch iteration
   * @param numberOfBatchIterations the total number of batch iterations.
   */
  static void kickOffIteration(exahype::records::RepositoryState::Action action,const int currentBatchIteration,const int numberOfIterations);

  /**
   * Static callback to kick off and iteration and perform global broadcasts between working nodes.
   * Calls the other method with same name and then performs a global broadcast.
   *
   * @note We decided to plug ExaHyPE's iteration kickoff into the RepositorySTDStack as we can then ensure that
   * (i)  master ranks do not delay their workers.
   * (ii) boundary data exchange is started in every even sweep (0,2,...) and
   *      finished in every odd sweep if enclave tasking is used.
   *
   * @todo have a tree-based algorithm. Problem: NodePool does not reveal worker nodes.
   *
   * @note private scope since we are friends with the Repositories.
   *
   * @param repositoryState         Stores information about the currently run adapter and the number of batch iterations.
   * @param solverState             Stores information about the grid.
   * @param currentBatchIteration   the current batch iteration.
   */
  static void kickOffIteration(exahype::records::RepositoryState& repositoryState, exahype::State& solverState, const int currentBatchIteration);

// /**                                                                                                                                        //
//  * Wrap up an iterations, i.e. make use of values that are reduced over the cells (and MPI ranks).                                         //
//  *                                                                                                                                         //
//  * @param action                  repository action indicating what time step (fused vs non-fused) is run                                  //
//  * @param currentBatchIteration   the current batch iteration                                                                              //
//  * @param numberOfBatchIterations the total number of batch iterations.                                                                    //
//  */                                                                                                                                        //
// static void wrapUpIteration(exahype::records::RepositoryState::Action action,const int currentBatchIteration,const int numberOfIterations);//

  /**
   * Static callback to wrap up an iteration.
   *
   * @note We decided to plug ExaHyPE's iteration wrap up into the RepositorySTDStack as we can then ensure that
   * boundary data exchange is started in every even sweep (0,2,...) and
   * finished in every odd sweep if enclave tasking is used.
   *
   * @note Reductions are still performed in the respective mappings via
   * Peano's prepareSendToMaster/Worker, mergeWithMaster methods.
   *
   * @note private scope since we are friends with the Repositories.
   *
   * @param repositoryState         Stores information about the currently run adapter and the number of batch iterations.
   * @param solverState             Stores information about the grid.
   * @param currentBatchIteration   the current batch iteration.
   */
  static void wrapUpIteration(exahype::records::RepositoryState& repositoryState, exahype::State& solverState, const int currentBatchIteration);

 public:
  /**
   * This enum is used to select certain solvers
   * in mappings like PredictionRerun, MeshRefinement etc.
   */
  enum class AlgorithmSection {
    /*
     * The runner is currently
     * performing a normal ADER-DG / FV / ... time step.
     */
    TimeStepping,


    /*
     * Currently performing mesh refinement. Only
     * relevant for merging metadata.
     */
    MeshRefinement,

    /*
     * Currently performing limiter status spreading. Only
     * relevant for merging metadata.
     */
    RefinementStatusSpreading,

    /**
     * In this section, the runner overlaps the
     * operations that must be performed after the
     * mesh refinement with operations that
     * must be performed for a local or global
     * recomputation.
     *
     * This marks the end point of the side branch.
     * Triggers a send for all registered solvers.
     */
    PredictionOrLocalRecomputationAllSend,

    /**
     * In this section, all solver have to drop
     * their messages. Then, the ADER-DG solvers which
     * have violated the CFL condition with their
     * estimated time step size are required
     * to reurn the prediction.
     * Finally, all solvers send out again their
     * face data.
     */
    PredictionRerunAllSend
  };

  /**
   * Default Constructor
   *
   * We set the default max mesh width to three, as it does not make sense to
   * kick off with a mesh that is shallower than that.
   */
  State();

  /**
   * Constructor
   *
   * This constructor is required by the framework's data container. Do not
   * remove it. It is kind of a copy constructor that converts an object which
   * comprises solely persistent attributes into a full attribute. This very
   * functionality is implemented within the super type, i.e. this constructor
   * has to invoke the correponsing super type's constructor and not the super
   * type standard constructor.
   */
  State(const Base::PersistentState& argument);

  /**
   * Merge this state with another state
   *
   * @todo Clarify which stuff has to be merged
   */
  void mergeWithMaster(const State& anotherState);

  /**
   * Set to true if we need to exchange local solver
   * data between master and worker at least for one cell at
   * a master-worker boundary.
   *
   * These local solver data are usually restricted or prolongated degrees of freedom.
   * They must not be confused with global solver data such as, e.g.
   * admissible time step sizes.
   *
   * \see exahype::mappings::MeshRefinement
   */
  void setVerticalExchangeOfSolverDataRequired(bool state);
  /**
   * \see setVerticalExchangeOfSolverDataRequired
   */
  bool getVerticalExchangeOfSolverDataRequired() const;

  /**
   * \see exahype/State.def
   */
  void setAllSolversAttainedStableState(const bool state);
  /**
   * \see exahype/State.def
   */
  bool getAllSolversAttainedStableState() const;

  /**
   * \see exahype/State.def
   */
  void setMeshRefinementIsInRefiningMode(const bool state);
  /**
   * \see exahype/State.def
   */
  bool getMeshRefinementIsInRefiningMode() const;

  /**
   * \see exahype/State.def
   */
  void setStableIterationsInARow(const int value);
  /**
   * \see exahype/State.def
   */
  int getStableIterationsInARow() const;


/**
 * Please consult Peano guidebook Section 6.3.2 for details.
 *
 * Mainly used to decide if the refinement can be enforced or
 * must be done more carefully. If there are still
 * idle ranks and the mesh is not stable, the
 * mesh refinement must not be enforced.
 *
 * We switch to immediate, aggressive refinement if and only if
 *
 * @note Has to be called after the iteration!
 */
void endedGridConstructionIteration(int finestGridLevelPossible);

  /**
   * Please consult Peano guidebook Section 6.3.2 for details.
   */
  enum RefinementAnswer {
    DontRefineYet,
    Refine,
    EnforceRefinement
  };
  RefinementAnswer mayRefine(bool isCreationalEvent, int level) const;

  /**
   * We do not follow Peao guidebook Section 6.3.2 as
   * we have other convergence checks in place.
   */
  bool continueToConstructGrid();

  /**
   * @return if we are in an even batch iterations, i.e.
   * CurrentBatchIteration % 2 == 0.
   */
  static bool isEvenBatchIteration();

  /**
   * \return true if we run no batch or if
   * we are in the first iteration of a batch (iteration: 0).
   *
   * \note It makes only sense to query the batch state from
   * within a mapping.
   */
  static bool isFirstIterationOfBatchOrNoBatch();

  /**
   * \return true if we run no batch or if
   * we are in the second iteration of a batch (iteration: 1).
   *
   * \note It makes only sense to query the batch state from
   * within a mapping.
   */
  static bool isSecondIterationOfBatchOrNoBatch();

  /**
   * \return true if we run no batch or if
   * we are in the last iteration of a batch (iteration: #iterations-1)
   *
   * \note It makes only sense to query the batch state from
   * within a mapping.
   */
  static bool isLastIterationOfBatchOrNoBatch();

  /**
   * \return true if we run no batch or if
   * we are in the second to last iteration of a batch (iteration: #iterations-2)
   *
   * \note It makes only sense to query the batch state from
   * within a mapping.
   *
   * \note This function takes the role of isLastIterationOfBatchOrNoBatch() when we use
   * two Prediction or FusedTimeStep sweeps.
   */
  static bool isSecondToLastIterationOfBatchOrNoBatch();

  #ifdef Parallel
  /*!
   * Send data such global solver and plotter
   * time step data down to a worker.
   */
  static void broadcastGlobalDataToWorker(
      const int                                   worker,
      const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
      const int                                   level);

  /*!
   * Merge with global data, such as global solver and plotter
   * time step data, sent down from the master.
   */
  static void mergeWithGlobalDataFromMaster(
      const int                                   master,
      const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
      const int                                   level);

  /*!
   * Send data such global solver
   * time step data up to the master.
   */
  static void reduceGlobalDataToMaster(
    const int                                   master,
    const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
    const int                                   level);

  /*!
   * Merge with global data, such as global solver
   * time step data, sent up from the master.
   */
  static void mergeWithGlobalDataFromWorker(
      const int                                   worker,
      const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
      const int                                   level);
  #endif
};

#endif
