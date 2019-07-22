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

#ifndef EXAHYPE_MAPPINGS_MeshRefinement_H_
#define EXAHYPE_MAPPINGS_MeshRefinement_H_

#include "tarch/la/Vector.h"
#include "tarch/logging/Log.h"

#include "peano/CommunicationSpecification.h"
#include "peano/MappingSpecification.h"
#include "peano/grid/VertexEnumerator.h"

#include "tarch/multicore/MulticoreDefinitions.h"

#include "exahype/Cell.h"
#include "exahype/State.h"
#include "exahype/Vertex.h"

#include "peano/utils/Globals.h"

#include "tarch/multicore/BooleanSemaphore.h"

namespace exahype {
namespace mappings {
class MeshRefinement;
}
}

/**
 * TODO(Dominic): Update documentation.
 *
 * This mapping builds up the regular base mesh used by all simulations.
 * The regular mesh is determined by the minimal mesh size of all involved
 * solvers. Basically, the only routine that does something in this mapping
 * is the private one. The only thing this private routine does is to call
 * refine().
 *
 *
 * @author Dominic E. Charrier, Tobias Weinzierl
 */
class exahype::mappings::MeshRefinement {
private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;

  /**
   * A state indicating if the mesh refinement has attained a stable state
   * for all solver.
   */
  bool _allSolversAttainedStableState = false;

  /**
   * A state indicating if vertical (master-worker) exchange
   * of face data is required during the time stepping iterations
   * for any of the registered solvers.
   */
  bool _verticalExchangeOfSolverDataRequired = false; // TODO(Dominic): Is Parallel

  /**
   * I use a copy of the state to determine whether I'm allowed to refine or not.
   */
  exahype::State _stateCopy;

  /**
   * We use this semaphore for refining along the
   * boundary of the computational domain in order
   * to avoid hanging nodes.
   */
  static tarch::multicore::BooleanSemaphore BoundarySemaphore;

  /**
   * TODO(Tobias): Add docu.
   */
  void refineSafely(
      exahype::Vertex&                              fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>&  fineGridH,
      int                                           fineGridLevel,
      bool                                          isCalledByCreationalEvent) const;

  /**
   * Ensure that we have no hanging nodes at the domain boundary,
   *
   * In case any inside vertex is refined, refine any boundary vertex as well.
   * In case all inside vertices are unrefined, erase any boundary vertex.
   *
   * This routine has to take the finest permitted resolution into account.
   *
   * \note Thread-safe as reads and writes to the boundary vertices are locked.
   */
  void ensureRegularityAlongBoundary(
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator) const;

public:

  /**
   * If this flag is set, load balancing is performed in all
   * mesh refinement iterations not just in the initial ones.
   *
   * @note Is set in exahype::runners::Runner;
   */
   static bool DynamicLoadBalancing;

  /**
   * Indicates that we perform the initial
   * mesh refinement.
   *
   * \note Must be initialised with true!
   */
  static bool IsInitialMeshRefinement;

  /**
   * We use this state to call adjust solution at the beginning
   * of a batch of mesh refinement iterations.
   *
   * In parallel builds, we further use it to determine if
   * we need to drop MPI messages from neighbours or not.
   * In the first iteration, we don't
   * expect any messages from neighbours.
   *
   * This variable is unset in MeshRefinement::beginIteration(...) in the first iteration
   * of MeshRefinement and then reset in
   * FinaliseMeshRefinement::beginIteration(...).
   *
   * \note Must be initialised with true!
   */
  static bool IsFirstIteration;

  /**
   * Main plug-in point for triggering refinement ans grid erasing events.
   */
  peano::MappingSpecification touchVertexLastTimeSpecification(int level) const;

  /**
   * We merge the limiter status between neighbouring cells.
   * We thus avoid fine grid races.
   */
  peano::MappingSpecification touchVertexFirstTimeSpecification(int level) const;

  /**
   * Avoid fine grid races.  Run through the whole tree.
   * Might be relaxed if vertex semaphores are in place.
   */
  peano::MappingSpecification enterCellSpecification(int level) const;

  /**
   * Avoid fine grid races.  Run through the whole tree.
   * Might be relaxed if vertex semaphores are in place.
   *
   * Alters the state as we perform a reduction.
   */
  peano::MappingSpecification leaveCellSpecification(int level) const;

  /**
   * Switched off
   */
  peano::MappingSpecification ascendSpecification(int level) const;
  peano::MappingSpecification descendSpecification(int level) const;

  peano::CommunicationSpecification communicationSpecification() const;

#if defined(SharedMemoryParallelisation)
  /**
   * We copy over the veto flag from the master thread
   * Initialise the worker's local variables.
   */
  MeshRefinement(const MeshRefinement& masterThread);

  /**
   * Merge with the workers local variables.
   */
  void mergeWithWorkerThread(const MeshRefinement& workerThread);
#endif

  /**
   * Initialise all heaps.
   *
   * For each solver, reset the grid update requested flag
   * to false.
   *
   * Further zero the time step sizes of the solver.
   *
   * <h2>MPI</h2>
   * Finish the previous synchronous sends and
   * start synchronous sending again.
   */
  void beginIteration(exahype::State& solverState);

  /**
   * For each solver, set the grid update requested flag
   * for the next iteration.
   *
   * <h2>MPI</h2>
   * If this rank is the global master, update the
   * initial grid refinement strategy.
   *
   * <h2>Background Jobs</h2>
   * Finish processing background jobs before starting
   * the next iteration.
   */
  void endIteration(exahype::State& solverState);

  /**
   * TODO(Tobias): Add docu.
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
   * TODO(Tobias): Add docu.
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
    * Initialises the cell descriptions index of a new
    * fine grid cell to an invalid default value.
    */
  void createCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * TODO(Dominic) Update docu.
   *
   * <h2>Adjust solution for existing cells</h2>
   * For already existing cells, we only adjust the solution and mark cells for refinement in
   * the first mesh refinement iteration.
   *
   * <h2>Adjust solution for newly introduced cells</h2>
   * We adjust the solution and evaluate the refinement criterion for
   * newly introduced cells during all mesh refinement iterations.
   *
   * <h2>Solution adjustment background jobs</h2>
   * All spawned background jobs must be completed before a new worker rank
   * can be forked or the next iteration begins.
   * We thus place ensure that all spawned background jobs are processed
   * in endIteration(...) and prepareCopyToRemoteNode(...).
   */
  void enterCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * TODO(Dominic): Update docu.
   *
   * <h2>Adjust solution for newly introduced cells</h2>
   * We adjust the solution and evaluate the refinement criterion for
   * newly introduced cells during all mesh refinement iterations.
   *
   * <h2>Solution adjustment background jobs</h2>
   * All spawned background jobs must be completed before a new worker rank
   * can be forked or the next iteration begins.
   * We thus place ensure that all spawned background jobs are processed
   * in endIteration(...) and prepareCopyToRemoteNode(...).
   */
  void leaveCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * After a fork, Peano erases the master's cells.
   * We plugin into these erases and deallocate heap data
   * before the cells are erased.
   */
  void destroyCell(
      const exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * For all solvers, merge the metadata of neighbouring patches.
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
   * TODO(Tobias): Add docu.
   *
   * TODO(Dominic): Update docu.
   */
  void touchVertexLastTime(
      exahype::Vertex& fineGridVertex,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
      const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex);


#ifdef Parallel
  /**
   * Receive metadata from a neighbouring rank.
   *
   * This is not performed in the first iteration of 
   * the initial mesh refinement.
   *
   * It has to be performed in the first iteration 
   * of later mesh refinements as their is
   * some refinement status spreading a-priori.
   */
  void mergeWithNeighbour(exahype::Vertex& vertex,
      const exahype::Vertex& neighbour, int fromRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      int level);

  /**
   * Loop over all cells adjacent to the
   * vertex. If one of the cells is adjacent
   * to a MPI boundary, send the solver type of all solvers
   * registered to the neighbouring MPI rank.
   */
  void prepareSendToNeighbour(exahype::Vertex& vertex, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      int level);


  /**
   * TODO(Dominic): Add docu
   */
  void prepareSendToMaster(
      exahype::Cell& localCell, exahype::Vertex* vertices,
      const peano::grid::VertexEnumerator& verticesEnumerator,
      const exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * TODO(Dominic): Add docu.
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
   * TODO(Dominic): Add docu.
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
   * TODO(Dominic): Add docu.
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
   * Merge the master's grid update requested flag with the one of the workers.
   */
  void mergeWithWorker(exahype::Cell& localCell,
                       const exahype::Cell& receivedMasterCell,
                       const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
                       const tarch::la::Vector<DIMENSIONS, double>& cellSize,
                       int level);

  /**
   * TODO(Dominic): Add docu.
   *
   * <h2>Background Jobs</h2>
   * Finish processing background jobs before sending
   * out any data.
   */
  void prepareCopyToRemoteNode(
      exahype::Cell& localCell, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level);

  /**
   * TODO(Dominic): Add docu.
   */
  void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Vertex& localVertex, const exahype::Vertex& masterOrWorkerVertex,
      int fromRank, const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h, int level);

  /**
   * TODO(Dominic): Add docu.
   */
  void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Cell& localCell, const exahype::Cell& masterOrWorkerCell,
      int fromRank, const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level);

  //
  // All methods below are nop,
  //
  // ==================================


  /**
   * Nop.
   */
  void prepareCopyToRemoteNode(exahype::Vertex& localVertex, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      int level);
  /**
   * Nop.
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
  MeshRefinement();
  /**
   * Nop
   */
  virtual ~MeshRefinement();
/**
 * Nop.
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
  * Nop.
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
  * Nop.
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
  * Nop.
  */
 void descend(
     exahype::Cell* const fineGridCells,
     exahype::Vertex* const fineGridVertices,
     const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
     exahype::Vertex* const coarseGridVertices,
     const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
     exahype::Cell& coarseGridCell);
 /**
  * Nop.
  */
 void ascend(exahype::Cell* const fineGridCells,
     exahype::Vertex* const fineGridVertices,
     const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
     exahype::Vertex* const coarseGridVertices,
     const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
     exahype::Cell& coarseGridCell);
};
#endif
