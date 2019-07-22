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
 *
 * @author Dominic E. Charrier, Tobias Weinzierl
 **/

#ifndef EXAHYPE_MAPPINGS_MergeNeighbours_H_
#define EXAHYPE_MAPPINGS_MergeNeighbours_H_

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
    class MergeNeighbours;
  }
}

/**
 * Run the Riemann solves on the faces
 *
 * @todo Dominic, bitte ordentlich dokumentieren, was hier wann, wo und warum passiert.
 * Bitte auch dokumentieren, falls Du was mal probiert hast und es nicht funktioniert hat.
 *
 * <h2>Synchronisation in multi-rank environment</h2>
 *
 * The individual Riemann solves have to have knowledge about
 * admissible time step sizes. This information is read from the solver
 * instances. In return, they update the solver instances with the maximal
 * permitted time step size for the subsequent iteration. This synchronisation
 * is realised in the mapping NewTimeStep, i.e. no solver synchronisation is to
 * be done in this mapping. It is also realised in SpaceTimePredictor. So
 * please consult the documentation there.
 *
 * This mapping is used to synchronise the compute cell time step sizes
 * with the solver ones. It further is used to reset the Riemann
 * solve flags on compute and helper cells.
 *
 * @author Dominic E. Charrier and Tobias Weinzierl
 */
class exahype::mappings::MergeNeighbours {
private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;

public:

  /**
   * The mapping does synchronise through synchroniseTimeStepping() invoked
   * on the solvers. Yet, no data is transported through the vertices, the
   * cell or the state object.
   *
   * Though we need valid solvers for the actual Riemann solves, we do not
   * do any solver exchange in this mapping. The appropriate data exchange
   * is done in NewTimeStep() and SolutionUpdate().
   *
   * Further let Peano handle heap data exchange internally.
   */
  peano::CommunicationSpecification communicationSpecification() const;

  /**
   * If compression is turned on, run through whole tree and
   * run concurrently over fine grid cells.
   * Otherwise, this specification returns nop.
   */
  peano::MappingSpecification enterCellSpecification(int level) const;

  /**
   * Run through the whole grid. Avoid fine grid races.
   */
  peano::MappingSpecification touchVertexFirstTimeSpecification(int level) const;

  /**
   * Nop.
   */
  peano::MappingSpecification touchVertexLastTimeSpecification(int level) const;
  peano::MappingSpecification leaveCellSpecification(int level) const;
  peano::MappingSpecification ascendSpecification(int level) const;
  peano::MappingSpecification descendSpecification(int level) const;

  /**
   * Solve Riemann problems on all interior faces that are adjacent
   * to this vertex and impose boundary conditions on faces that
   * belong to the boundary. This is done for all cell descriptions
   * belonging to the cells that are an interior face.
   *
   * The routine itself runs the loop over the faces. The actual
   * functionality is outsourced to solveRiemannProblemAtInterface().
   *
   * The function ensures implicitly that interior faces
   * do not align with MPI boundaries. In this case, no operation
   * is performed.
   *
   * This method sets the riemannSolvePerformed flag on a cell description
   * if boundary conditions have been imposed for this cell description.
   * This method sets the riemannSolvePerformed flag on both cell descriptions
   * (per solver) for interior faces if a Riemann solve has been performed for
   * both cell descriptions.
   *
   * \note The function itself is not thread-safe.
   * Thread-safety of this function is ensured by setting
   * RiemannSolver::touchVertexFirstTimeSpecification()
   * to peano::MappingSpecification::AvoidFineGridRaces.
   *
   * <h2>Limiter identification</h2>
   * Each ADER-DG solver analyses the local min and max values within a cell.
   * This information however is not stored in the cell but on the 2d faces
   * of a cell. See Cell::setSolutionMinMaxAndAnalyseValidity() for details.
   * The face information then in the subsequent step has to be merged which
   * is done when we trigger the Riemann solve.
   *
   * @see Cell::mergeSolutionMinMaxOnFace()
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
   * Reset the neighbour merge flags
   * and MPI neighbour exchange counters.
   *
   * Further compress the solver data again if this
   * is required.
   *
   * We cannot validate that all merges have been
   * performed since we only consider MPI neighbour
   * exchange but no local exchange in this mapping.
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
   * Initialise temporary variables on worker thread.
   */
  MergeNeighbours(const MergeNeighbours& masterThread);
#endif

  /**
   * Free previously allocated temporary variables.
   */
  virtual ~MergeNeighbours();

  /**
   * Initialise temporary variables
   * if they are not initialised yet (or
   * if a new solver was introuced to the grid.
   * This is why we put the initialisation
   * in beginIteration().
   *
   * In debug mode, further resets counters for Riemann solves at
   * interior and boundary faces.
   */
  void beginIteration(exahype::State& solverState);

  /**
   * Frees previously allocated temporary variables.
   *
   * In debug mode, prints the output of counters.
   */
  void endIteration(exahype::State& solverState);


#ifdef Parallel
  /**
   * Merge vertex with the incoming vertex from a neighbouring computation node.
   *
   * When Peano is running in parallel the data exchange is done vertex-wise
   * between two grid iterations, i.e. the predictor sends out data in one step
   * and in the following step we receive this data and merge it into the local
   * Riemann integrals. The routine is thus very similar to
   * touchVertexFirstTime():
   *
   * - We identify incoming faces.
   * - We create (temporary) indices on the heap.
   * - We receive the data into these indices.
   *
   * As we always receive data in the iteration following the sends and as
   * Peano inverts the traversal direction after each grid sweep, we have to
   * invert the order in which data is received, too.
   *
   * \note It happens or is possible that this operation is performed
   * after touchVertexLastTime(...) was invoked.
   *
   * <h2>Send</h2>
   * We realise sends within
   * Sending::prepareSendToNeighbour().
   *
   *
   * <h2>Min max analysis</h2>
   * The min/max analysis runs analogously. We do send out min and max from
   * either side to the other rank and then merge min and max on both sides
   * into the local data.
   *
   * <h2>Face data exchange counters</h2>
   * Cell::isInside() does not imply that all adjacent vertices are
   * inside. If we count down the counter only on
   * vertices that are inside we might not send out all faces
   * of cells adjacent to the boundary.
   * @see Prediction::countListingsOfRemoteRankByInsideVerticesAtFace.
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
   * Nop.
   */
  void mergeWithWorker(
      exahype::Cell& localCell,
      const exahype::Cell& receivedMasterCell,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize,
      int level);

  /**
   * Nop.
   */
  void prepareSendToNeighbour(
      exahype::Vertex& vertex, int toRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h, int level);

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
  void mergeWithWorker(exahype::Vertex& localVertex,
                       const exahype::Vertex& receivedMasterVertex,
                       const tarch::la::Vector<DIMENSIONS, double>& x,
                       const tarch::la::Vector<DIMENSIONS, double>& h,
                       int level);
#endif

  /**
   * Nop.
   */
  MergeNeighbours();

#if defined(SharedMemoryParallelisation)
  /**
   * Nop.
   */
  void mergeWithWorkerThread(const MergeNeighbours& workerThread);
#endif

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
