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
 
#ifndef EXAHYPE_MAPPINGS_Prediction_H_
#define EXAHYPE_MAPPINGS_Prediction_H_

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
    class Prediction;
  }
}

/**
 * This mapping realises the space-time predictor
 *
 * We do run through all the cells and, per solver, run the space-time
 * prediction and the volume integral.
 * Most of the interesting stuff is done in enterCell().
 *
 * The other methods despite leaveCell(...) as well as MPI- and Shared-Mem-specific methods
 * perform no operation.
 *
 * This mapping's enterCell(...) and leaveCell(...) methods are further used to
 * prolongate coarse grid face unknowns down to fine grid cells and to
 * restrict coarse grid face unknowns up to coarse grid cells.
 *
 * As all state data is encoded in the global solver states and as all data
 * accesses are read-only, the mapping has no object attributes.
 *
 * <h2>MPI</h2>
 *
 * For a valid computation of the space-time prediction, we need to know the
 * correct time step on each rank. We thus distribute time step data among the
 * ranks when we start them up. See prepareSendToWorker() and the corresponding
 * receiveDataFromMaster().
 *
 * <h2>Shared Memory</h2>
 *
 * As the mapping accesses the state data in a read-only fashion, no special
 * attention is required here.
 *
 * <h2>Optimisations</h2>
 * We dedicate each thread a fixed size space-time predictor,
 * space-time volume flux, predictor, and volume flux
 * field. There is no need to store these massive
 * quantities on the heap for each cell as it was done
 * in the baseline code.
 * This massively reduces the memory footprint of
 * the method and might lead to a more
 * cache-friendly code since we reuse
 * the temporary variables multiple times
 * per grid traversal (unverified).
 *
 * @author Dominic E. Charrier and Tobias Weinzierl
 */
class exahype::mappings::Prediction {
private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;
 public:
  /**
   * Determine the enter cell specification depending
   * on if background threads are used or not.
   *
   * The mappings FusedTimeStep, PredictionRerun, and
   * PredictionOrLocalRecomputation use the same
   * enter cell specification.
   */
  static peano::MappingSpecification determineEnterLeaveCellSpecification(int level);

  /**
   * This method first synchronises the time step sizes and time stamps, and
   * then resets the Riemann solve flags and the face data exchange counter for all
   * solvers for which a valid cell description was registered on this cell.
   *
   * Directly after, it runs through all solvers assigned to a cell and invoke the solver's
   * spaceTimePredictor(...) as well as the solver's volumeIntegral(...) routines if
   * the fine grid cell functions as a compute cell (Cell) for the solver.
   * Please see the discussion in the class header.
   *
   * Furthermore, this method prolongates face data or prepares a
   * restriction of face data.
   *
   * <h2>LimitingADERDGSolver</h2>
   * We only perform a predictor computation if the cell description's limiter status is not
   * set to Troubled.
   * Cell descriptions with limiter status Troubled do not hold a valid ADER-DG solution and thus
   * cannot provide these data.
   * The other cells require time-extrapolated boundary-extrapolated solution values from their neighbours
   * to compute the normal fluxes/fluctuations at the cell boundary.
   *
   * \see enterCellSpecification()
   *
   * @param[in] issuePredictionJobs issue prediction jobs. Otherwise, perform the prolongation.
   */
  static void performPredictionOrProlongate(
      const exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      const exahype::State::AlgorithmSection& algorithmSection,
      const bool issuePredictionJobs);

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
   * Do not broadcast and reduce anything.
   *
   * If only one prediction sweep is used, delegate heap data exchange to
   * Peano. Otherwise, start and stop it in begin/endIteration(...).
   * Stretch the start/stop window over two sweeps.
   */
  peano::CommunicationSpecification communicationSpecification() const;

  /**
   * Initialise the temporary variables (of the master thread
   * in a shared memory build).
   */
  Prediction();

  /**
   * Delete the temporary variables (for
   * master and worker threads in a shared memory build).
   */
  ~Prediction();

  #if defined(SharedMemoryParallelisation)
  /**
   * Initialise the temporary variables of a
   * worker thread.
   */
  Prediction(const Prediction& masterThread);
  /**
   * Nop
   */
  void mergeWithWorkerThread(const Prediction& workerThread);
  #endif

  /**
   * Signal the solvers that we start a new time step when
   * running the nonfused time stepping variant.
   *
   * Turns ITAC on if required
   * Turn the broadcast off at the end of the first iteration.
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
   * \see performPredictionAndProlongateData
   */
  void enterCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

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

#ifdef Parallel
  /**
    * This routine is called on the master.
    *
    * Broadcast time step data to the workers.
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
    * Receive kick-off message from master
    *
    * Counterpart of prepareSendToWorker(). This operation is called once when
    * we receive data from the master node.
    *
    * \see prepareSendToWorker(...)
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

   /*! Send face data to a neighbouring rank.
    *
    */
   void prepareSendToNeighbour(exahype::Vertex& vertex, int toRank,
                               const tarch::la::Vector<DIMENSIONS, double>& x,
                               const tarch::la::Vector<DIMENSIONS, double>& h,
                               int level);


   /*! Merge metadata and face data sent from the worker.
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

   /*! Send metadata and face data to the master.
    */
   void prepareSendToMaster(
       exahype::Cell& localCell, exahype::Vertex* vertices,
       const peano::grid::VertexEnumerator& verticesEnumerator,
       const exahype::Vertex* const coarseGridVertices,
       const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
       const exahype::Cell& coarseGridCell,
       const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  //
  // Below all methods are nop.
  //
  //===================================


  /**
   * Nop
   */
  void mergeWithNeighbour(exahype::Vertex& vertex,
                          const exahype::Vertex& neighbour, int fromRank,
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
