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
 
#ifndef EXAHYPE_MAPPINGS_FinaliseMeshRefinement_H_
#define EXAHYPE_MAPPINGS_FinaliseMeshRefinement_H_

#include "tarch/la/Vector.h"
#include "tarch/logging/Log.h"

#include "peano/CommunicationSpecification.h"
#include "peano/MappingSpecification.h"
#include "peano/grid/VertexEnumerator.h"

#include "tarch/multicore/MulticoreDefinitions.h"

#include "exahype/Cell.h"
#include "exahype/State.h"
#include "exahype/Vertex.h"

namespace exahype {
  namespace mappings {
    class FinaliseMeshRefinement;
  }
}

/**
 * This mapping is used to finalise grid refinement operations and
 * further computes a new time step size for every solver
 * which has requested mesh refinement.
 *
 * <h2>MPI</h2>
 *
 * If you compile with MPI, it will further drop grid metadata messages
 * that have been sent during the grid update iterations.
 *
 * @author Dominic Charrier
 */
class exahype::mappings::FinaliseMeshRefinement {
 private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;

  /**
   * Flag indicating that one solver requested
   * a mesh update.
   * Is set in beginIteration(...).
   */
  static bool OneSolverRequestedMeshUpdate;

  /**
<<<<<<< HEAD
   * A minimum time step size for each solver.
   */
  std::vector<double> _minTimeStepSizes;

  /**
   * The maximum level occupied by cells of a solver.
   */
  std::vector<int> _maxLevels;

   /**
   * A vector of reduced global observables for each solver.
   */
  std::vector<std::vector<double>> _reducedGlobalObservables;

  /**
=======
>>>>>>> master
   * Indicates that the background tasks have terminated.
   * No further checks are required in this case.
   *
   * Is initialised with false for the main thread
   * and for the worker threads.
   * As the worker threads; mappings are destroyed but
   * the main thread's mapping continues to
   * exist we reset this value in endIteration(State) to false.
   *
   * We process background jobs in touchVertexFirstTime(...)
   * and set this flag here as well.
   */
  bool _backgroundJobsHaveTerminated = false;

 public:
  /**
   * Reduce data from the worker to the master.
   *
   * \note Make sure that you return true in a
   * previous iteration in prepareSendToWorker
   * where you performed a broadcast to the worker.
   */
  peano::CommunicationSpecification communicationSpecification() const;

  /**
   * Run over whole tree. Run concurrently on fine grid.
   *
   * Alters state as we perform a reduction.
   */
  peano::MappingSpecification enterCellSpecification(int level) const;

  /* Nop */
  peano::MappingSpecification touchVertexLastTimeSpecification(int level) const;
  peano::MappingSpecification touchVertexFirstTimeSpecification(int level) const;
  peano::MappingSpecification leaveCellSpecification(int level) const;
  peano::MappingSpecification ascendSpecification(int level) const;
  peano::MappingSpecification descendSpecification(int level) const;

  /**
   * Reset the following flags (to true):
   * exahype::mappings::MeshRefinement::IsFirstIteration
   *
   * Initialise the _oneSolverHasRequestedMeshUpdate flag.
   *
   * <h2> Time stepping </h2>
   * Start iteration/grid sweep.
   * Make the state clear its accumulated values.
   *
   * Further initialise temporary variables
   * if they are not initialised yet (or
   * if a new solver was introduced to the grid.
   * This is why we put the initialisation
   * in beginIteration().
   *
   * \note Is called once per rank.
   */
  void beginIteration(exahype::State& solverState);

  /**
   * Sets the MeshRefinement::IsInitialMeshRefinement status to false.
   * MeshRefinement::IsInitialMeshRefinement is set in Runner::run.
   *
   * Further turns on the performance analysis after the initial
   * mesh refinement is done.
   *
   * Performance analysis is turned off initially in exahype::runners::Runner::initHPCEnvironment().
   *
   * <h2>Time Stepping</h2>
   *
   * TODO(Dominic): Update with docu on the meshUpdateRequest
   * and limiterDomainChange flags.
   *
   * Runs over all the registered solvers and sets the
   * reduced minimum time step sizes. Then updates the minimum time stamp
   * of the solvers.
   *
   * Iterate over the solvers and start a new time step
   * on every solver.
   *
   * <h2>Fused ADER-DG time stepping</h2>
   * If we use the fused ADER-DG time stepping algorithm,
   * The solver or (the solver belonging to the global master in the MPI context)
   * is not allowed to perform the time step update
   * directly. It first has to check if the previously used
   * min predictor time step size was stable one.
   * Otherwise, we would corrupt the corrector time stamp
   * with an invalid value.
   *
   * <h2>MPI</h2>
   * Here we start again a new time step "in the small" on the
   * worker rank and overwrite it later on again if a synchronisation is applied
   * by the master rank.
   *
   * It is important to keep in mind that endIteration() on a worker
   * is called before the prepareSendToMaster routine.
   * We thus send out the current time step size from
   * the worker to the master.
   *
   * On the master, the mergeWithMaster routine is however called
   * before endIteration.
   * We thus merge the received time step size with the next
   * time step size on the master.
   *
   * \see exahype::mappings::Sending,exahype::mappings::Merging
   */
  void endIteration(exahype::State& solverState);

  /**
   * Call the solvers finaliseStateUpdates functions.
   * Further, compute the min and max if a solver is of type
   * LimitingADERDG and there is a patch allocated
   * in the \p fineGridCell for this solver.
   *
   * <h2> Time Stepping </h2>
   *
   * If the fine grid cell functions as compute cell for a solver,
   * compute a stable time step size.
   *
   * Then update the time stamp of the compute cell
   * and update the minimum solver time stamp and
   * time step size.
   *
   * Finally, update the minimum and maximum mesh cell size
   * fields per solver with the size of the fine grid cell.
   */
  void enterCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

#if defined(SharedMemoryParallelisation)
  /**
   * Nop
   */
  FinaliseMeshRefinement(const FinaliseMeshRefinement& masterThread);
#endif

  /**
   * Nop
   */
  virtual ~FinaliseMeshRefinement();

#if defined(SharedMemoryParallelisation)
  /**
   * Nop
   */
  void mergeWithWorkerThread(const FinaliseMeshRefinement& workerThread);
#endif

  /**
   * Nop
   */
  FinaliseMeshRefinement();

  /**
   * Nop
   */
  void createInnerVertex(
      exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void createBoundaryVertex(
      exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void createHangingVertex(
      exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void destroyHangingVertex(
      const exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void destroyVertex(
      const exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void createCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * Nop
   */
  void destroyCell(
      const exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

#ifdef Parallel
  /**
   * Drop messages containing meta data associated with the local cells that are
   * adjacent to this vertex sent to those adjacent remote cells that share a complete face
   * with the local cells.
   *
   * These messages have been sent out in eithe preceding refinement
   * status spreading iterations or mesh refinement iterations.
   */
  void mergeWithNeighbour(exahype::Vertex& vertex,
                          const exahype::Vertex& neighbour, int fromRank,
                          const tarch::la::Vector<DIMENSIONS, double>& x,
                          const tarch::la::Vector<DIMENSIONS, double>& h,
                          int level);


  /**
   * Return true since we want to reduce time step data from
   * the worker to the master.
   *
   * \note Has to return true in this iteration and in the previous iteration
   * (by any other adapter).
   */
  bool prepareSendToWorker(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
      int worker);

  /**
   * Merge time step data from the worker with the master
   * for all those solvers which have performed a mesh update.
   */
  void mergeWithMaster(
      const exahype::Cell& workerGridCell,
      exahype::Vertex* const workerGridVertices,
      const peano::grid::VertexEnumerator& workerEnumerator,
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
      int worker, const exahype::State& workerState,
      exahype::State& masterState);

  /**
   * Send time step data to the master for all
   * those solvers which have performed a mesh update.
   */
  void prepareSendToMaster(
      exahype::Cell& localCell, exahype::Vertex* vertices,
      const peano::grid::VertexEnumerator& verticesEnumerator,
      const exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * Nop
   */
  void prepareSendToNeighbour(exahype::Vertex& vertex, int toRank,
                              const tarch::la::Vector<DIMENSIONS, double>& x,
                              const tarch::la::Vector<DIMENSIONS, double>& h,
                              int level);

  /**
   * Nop
   */
  void prepareCopyToRemoteNode(exahype::Vertex& localVertex, int toRank,
                               const tarch::la::Vector<DIMENSIONS, double>& x,
                               const tarch::la::Vector<DIMENSIONS, double>& h,
                               int level);

  /**
   * Nop
   */
  void prepareCopyToRemoteNode(
      exahype::Cell& localCell, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level);

  /**
   * Nop
   */
  void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Vertex& localVertex, const exahype::Vertex& masterOrWorkerVertex,
      int fromRank, const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h, int level);

  /**
   * Nop
   */
  void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Cell& localCell, const exahype::Cell& masterOrWorkerCell,
      int fromRank, const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level);

  /**
   * Nop
   */
  void receiveDataFromMaster(
      exahype::Cell& receivedCell, exahype::Vertex* receivedVertices,
      const peano::grid::VertexEnumerator& receivedVerticesEnumerator,
      exahype::Vertex* const receivedCoarseGridVertices,
      const peano::grid::VertexEnumerator& receivedCoarseGridVerticesEnumerator,
      exahype::Cell& receivedCoarseGridCell,
      exahype::Vertex* const workersCoarseGridVertices,
      const peano::grid::VertexEnumerator& workersCoarseGridVerticesEnumerator,
      exahype::Cell& workersCoarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * Nop
   */
  void mergeWithWorker(exahype::Cell& localCell,
                       const exahype::Cell& receivedMasterCell,
                       const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
                       const tarch::la::Vector<DIMENSIONS, double>& cellSize,
                       int level);

  /**
   * Nop
   */
  void mergeWithWorker(exahype::Vertex& localVertex,
                       const exahype::Vertex& receivedMasterVertex,
                       const tarch::la::Vector<DIMENSIONS, double>& x,
                       const tarch::la::Vector<DIMENSIONS, double>& h,
                       int level);
#endif

  /**
   * Nop
   */
  void touchVertexFirstTime(
      exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void touchVertexLastTime(
      exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);

  /**
   * Nop
   */
  void leaveCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * Nop
   */
  void descend(
      exahype::Cell* const fineGridCells,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell);

  /**
   * Nop
   */
  void ascend(exahype::Cell* const fineGridCells,
              exahype::Vertex* const fineGridVertices,
              const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
              exahype::Vertex* const coarseGridVertices,
              const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
              exahype::Cell& coarseGridCell);
};

#endif
