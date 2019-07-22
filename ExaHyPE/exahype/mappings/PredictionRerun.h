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
 
#ifndef EXAHYPE_MAPPINGS_PredictionRerun_H_
#define EXAHYPE_MAPPINGS_PredictionRerun_H_

#include "tarch/la/Vector.h"
#include "tarch/logging/Log.h"

#include "peano/CommunicationSpecification.h"
#include "peano/MappingSpecification.h"
#include "peano/grid/VertexEnumerator.h"

#include "tarch/multicore/MulticoreDefinitions.h"

#include "tarch/multicore/BooleanSemaphore.h"


#include "exahype/Cell.h"
#include "exahype/State.h"
#include "exahype/Vertex.h"

namespace exahype {
  namespace mappings {
    class PredictionRerun;
  }
}

/**
 * In principle a copy of Prediction
 * with two differences:
 *
 * - we drop incoming MPI messages from neighbouring ranks.
 * - we only recompute the prediction for solvers which
 *   encountered an unstable time step size
 *   (according to the CFL criterion).
 *
 * @author Dominic E. Charrier and Tobias Weinzierl
 */
class exahype::mappings::PredictionRerun {
private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;

 public:
  /**
   * Level for which we ask what to do. This value is negative
   * if we are on the fine grid level. In this case, the
   * actual level is the absolute value.
   */
  peano::MappingSpecification enterCellSpecification(int level) const;

  /*
   * Nop.
   */
  peano::MappingSpecification touchVertexLastTimeSpecification(int level) const;
  peano::MappingSpecification touchVertexFirstTimeSpecification(int level) const;
  peano::MappingSpecification leaveCellSpecification(int level) const;
  peano::MappingSpecification ascendSpecification(int level) const;
  peano::MappingSpecification descendSpecification(int level) const;

  /**
   * Broadcast global solver data such as time step sizes at the beginning of the traversal.
   * Do not reduce anything.
   */
  peano::CommunicationSpecification communicationSpecification() const;

  /**
   * Initialise the temporary variables (of the master thread
   * in a shared memory build).
   */
  PredictionRerun();

  /**
   * Delete the temporary variables (for
   * master and worker threads in a shared memory build).
   */
  ~PredictionRerun();

  #if defined(SharedMemoryParallelisation)
  /**
   * Initialise the temporary variables of a
   * worker thread.
   */
  PredictionRerun(const PredictionRerun& masterThread);
  /**
   * Nop
   */
  void mergeWithWorkerThread(const PredictionRerun& workerThread);
  #endif

  /**
   * Ensure all background jobs have terminated.
   */
  void beginIteration(exahype::State& solverState);

  /**
   * <h2>Background Jobs</h2>
   *
   * Notify Peano's tarch that we want to start processing
   * background jobs with all available cores.
   */
  void endIteration(exahype::State& solverState);

  /**
   * \copydoc exahype::mappings::Prediction::enterCell
   *
   * \note Only for solvers which have used an instable
   * time step size beforehand.
   */
  void enterCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * \copydoc exahype::mappings::Prediction::leaveCell
   *
   * \note Only for solvers which have used an instable
   * time step size beforehand.
   */
  void leaveCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

#ifdef Parallel

   /** Send out data for all(!) solvers.
    */
   void prepareSendToNeighbour(exahype::Vertex& vertex, int toRank,
                               const tarch::la::Vector<DIMENSIONS, double>& x,
                               const tarch::la::Vector<DIMENSIONS, double>& h,
                               int level);

   /*! Drop incoming MPI messages for all(!) solvers.
    */
   void mergeWithNeighbour(exahype::Vertex& vertex,
                           const exahype::Vertex& neighbour, int fromRank,
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
   void mergeWithWorker(exahype::Cell& localCell,
                        const exahype::Cell& receivedMasterCell,
                        const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
                        const tarch::la::Vector<DIMENSIONS, double>& cellSize,
                        int level);

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
