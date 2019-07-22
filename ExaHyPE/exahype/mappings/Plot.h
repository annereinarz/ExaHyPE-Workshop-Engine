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
 
#ifndef EXAHYPE_MAPPINGS_Plot_H_
#define EXAHYPE_MAPPINGS_Plot_H_

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
class Plot;
}
}

/**
 * Plot the data
 *
 * We run through all the cells and evaluate per cell whether this plotter
 * setup is hot. See the plotters' plotDataFromSolver().
 */
class exahype::mappings::Plot {
 private:
  /**
   * Logging device for the trace macros.
   */
  static tarch::logging::Log _log;

 public:

  /**
   * Plot if a plotter is active for a solver.
   */
  static void plotIfRequested(const int cellDescriptionsIndex);
  /**
   * Run through whole tree. Run concurrently on fine grid.
   */
  peano::MappingSpecification enterCellSpecification(int level) const;
  /**
   * Nop.
   */
  peano::MappingSpecification touchVertexLastTimeSpecification(int level) const;
  peano::MappingSpecification touchVertexFirstTimeSpecification(int level) const;
  peano::MappingSpecification leaveCellSpecification(int level) const;
  peano::MappingSpecification ascendSpecification(int level) const;
  peano::MappingSpecification descendSpecification(int level) const;

  /**
   * TODO(Dominic): Add docu. Not sure what we need here. See discussion in beginIteration(...).
   */
  peano::CommunicationSpecification communicationSpecification() const;

  /**
   * If a fine grid cell is a compute cell, plot solution values.
   *
   * Note that we assume here that we always plot the solution values
   * of the previous iteration.
   * Never have this mapping after mapping SolutionUpdate.
   */
  void enterCell(
      exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Vertex* const coarseGridVertices,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell);

  /**
   * Begin an iteration
   *
   * Besides some checks, we do invoke isAPlotterActive() if we are not on the
   * global master. The name isAPlotterActive() is not 100 percent right as
   * this routine on the one hand checks whether a plotter is active. On the
   * other hand, it also switches them on if neccessary. Therefore, it is
   * important that we do this on each and every rank. On the global master,
   * the runner does it himself.
   */
  void beginIteration(exahype::State& solverState);

  /**
   * Iteration is done
   *
   * We run through all the plotters and notify them that the plot has
   * finished. This is triggered by a call to
   * exahype::plotters::finishedPlotting(). They then can prepare to do the
   * next plot if they are repeating plotters.
   */
  void endIteration(exahype::State& solverState);


#ifdef Parallel
  /**
   * Print a warning on a worker rank if this mapping is used whilst
   * no plotter is active at all.
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
  void prepareSendToNeighbour(exahype::Vertex& vertex, int toRank,
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
     * Nop.
     */
    Plot();
  #if defined(SharedMemoryParallelisation)
    /**
     * Nop.
     */
    Plot(const Plot& masterThread);
  #endif
    /**
     * Nop.
     */
    virtual ~Plot();
  #if defined(SharedMemoryParallelisation)
    /**
     * Nop.
     */
    void mergeWithWorkerThread(const Plot& workerThread);
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
