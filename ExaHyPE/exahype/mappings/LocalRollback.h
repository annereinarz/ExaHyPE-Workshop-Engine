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

#ifndef EXAHYPE_MAPPINGS_LocalRollback_H_
#define EXAHYPE_MAPPINGS_LocalRollback_H_

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
class LocalRollback;
}
}

/**
 * This mapping is part of three mappings (+adapters) which together perform the limiter
 * status spreading the recomputation of troubled cells (and their direct) neighbours.
 *
 * \see exahype::mappings::LocalRecomputation for more details.
 *
 * @author Dominic Charrier
 */
class exahype::mappings::LocalRollback {
private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;

  /**
   * Flag indicating if one solver requested a local recomputation.
   * Is initialised in beginIteration(...).
   */
  static bool OneSolverRequestedLocalRecomputation;

  /**
   * \return if we perform a local recomputation for this solver.
   */
  static bool performLocalRecomputation(exahype::solvers::Solver* solver);

#ifdef Parallel

  /**
   * We only send data for LimitingADERDGSolvers
   * where we have detected a irregular change of the limiter domain
   * but no additional mesh refinement was required.
   * This information should be available on all ranks.
   * We ignore other solver types.
   */
  static void sendDataToNeighbourLoopBody(
      const int                                    toRank,
      const int                                    srcScalar,
      const int                                    destScalar,
      const exahype::Vertex&                       vertex,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level);
#endif

public:
  /**
   * Mask out data exchange between master and worker.
   * Further let Peano handle heap data exchange internally.
   */
  peano::CommunicationSpecification communicationSpecification() const;

  /**
   * Run through the whole grid. Run concurrently on the fine grid.
   */
  peano::MappingSpecification enterCellSpecification(int level) const;

  /////
  // Below all Specifications are nop
  /////
  /**
   * Nop.
   */
  peano::MappingSpecification touchVertexFirstTimeSpecification(int level) const;
  /**
   * Nop.
   */
  peano::MappingSpecification touchVertexLastTimeSpecification(int level) const;
  /**
   * Nop.
   */
  peano::MappingSpecification leaveCellSpecification(int level) const;
  /**
   * Nop.
   */
  peano::MappingSpecification ascendSpecification(int level) const;
  /**
   * Nop.
   */
  peano::MappingSpecification descendSpecification(int level) const;

  /**
   * Initialise a flag stating if any solver
   * requested a local recomputation.
   */
  void beginIteration(exahype::State& solverState);

  /**
   * Rollback to the previous time stamp in all solvers which
   * requested a local recomputation.
   */
  void endIteration(exahype::State& solverState);

  /**
   * For all cells hosting a solver patch of a
   * LimitingADERDGSolver, perform the following operations:
   *
   * 1. Perform a rollback to the previous time step.
   * 2. Reinitialise the solver patch, i.e. perform a rollback in
   *    cells with LimiterStatus other than Ok, and
   *    allocate an additional limiter patch if necessary.
   *    If the solver requested a global recomputation, i.e.
   *    additional mesh refinement,
   *    also perform a rollback in cells with LimiterStatus
   *    Ok.
   * 3. Reset the neighbour merging flags.
   */
  void enterCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

#ifdef Parallel
  /**
   * In case a solver is a LimitingADERDGSolver and we have detected a irregular
   * limiter domain change that does not require a mesh update, we
   * ask the solver to send FV subcell data to neighbouring ranks.
   */
  void prepareSendToNeighbour(exahype::Vertex& vertex, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      int level);



  //
  // Below all methods are nop.
  //
  //===================================

  /**
   * Nop.
   */
  void mergeWithNeighbour(exahype::Vertex& vertex,
      const exahype::Vertex& neighbour, int fromRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      int level);


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
  void prepareCopyToRemoteNode(
      exahype::Cell& localCell, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level);

  /**
   * Nop.
   */
  void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Vertex& localVertex, const exahype::Vertex& masterOrWorkerVertex,
      int fromRank, const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h, int level);

  /**
   * Nop.
   */
  void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Cell& localCell, const exahype::Cell& masterOrWorkerCell,
      int fromRank, const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level);

  /**
   * Nop.
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
   * Nop.
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
   * Nop.
   */
  void prepareSendToMaster(
      exahype::Cell& localCell, exahype::Vertex* vertices,
      const peano::grid::VertexEnumerator& verticesEnumerator,
      const exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * Nop.
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
   * Nop.
   */
  void mergeWithWorker(exahype::Cell& localCell,
      const exahype::Cell& receivedMasterCell,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize,
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
 LocalRollback();

#if defined(SharedMemoryParallelisation)
 /**
  * Nop.
  */
 LocalRollback(const LocalRollback& masterThread);
#endif

 /**
  * Nop.
  */
 virtual ~LocalRollback();

#if defined(SharedMemoryParallelisation)
 /**
  * Nop.
  */
 void mergeWithWorkerThread(const LocalRollback& workerThread);
#endif

 /**
  * Nop.
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
  * Nop.
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
 void createCell(
     exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
     const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
     exahype::Vertex* const coarseGridVertices,
     const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
     exahype::Cell& coarseGridCell,
     const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

 /**
  * Nop.
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
  * Nop.
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
  * Nop.
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
  * Nop.
  */
 void leaveCell(
     exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
     const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
     exahype::Vertex* const coarseGridVertices,
     const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
     exahype::Cell& coarseGridCell,
     const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

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
