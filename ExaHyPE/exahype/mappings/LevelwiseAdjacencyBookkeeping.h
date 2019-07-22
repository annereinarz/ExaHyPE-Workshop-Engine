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
 */
#ifndef EXAHYPE_MAPPINGS_LevelwiseAdjacencyBookkeeping_H_
#define EXAHYPE_MAPPINGS_LevelwiseAdjacencyBookkeeping_H_


#include "tarch/logging/Log.h"
#include "tarch/multicore/MulticoreDefinitions.h"

#include "peano/MappingSpecification.h"
#include "peano/CommunicationSpecification.h"
#include "peano/grid/VertexEnumerator.h"

#include "exahype/Vertex.h"
#include "exahype/Cell.h"
#include "exahype/State.h"


namespace exahype {
      namespace mappings {
        class LevelwiseAdjacencyBookkeeping;
      } 
}


/**
 * This is an adapter which writes cell-based heap indices into the adjacency
 * maps of each vertex. This is done completely levelwisely.
 *
 * \note Hanging nodes are completely ignored and the indices initialised
 * with invalid indices every time a hanging node is created.
 * Consistency checks must thus not consider cells which
 * have an adjacent hanging node.
 *
 * Codes which want to use this mapping need to ensure that no
 * hanging nodes appear on the boundary of the domain.
 * Refinement must be employed such that this is prevented.
 *
 * \note If you have domains with an aspect-ratio other than exactly 1 (up to machine precision), you need to rescale
 * your computational domain such that the coarsest grid is exactly (up to machine precision) an integral
 * multiple of the bounding box's mesh size.
 *   Otherwise, you will run in situations where a parent may have multiple children which
 * are outside of the computational domain. This is not only inappropriate for AMR prolongation and restriction but
 * also for the adjacency book keeping.
 *
 * CellDescriptionsIndex   Name of the index used for the cell indices within the vertex and 
 *          the cell
 *
 *
 * @author Tobias Weinzierl, Dominic Etienne Charrier
 * @version $Revision: 1.1 $
 */
class exahype::mappings::LevelwiseAdjacencyBookkeeping {
  public:

    /**
     * Since the position of a cell with respect to a particular vertex is unique,
     * there are no races when writing a cell's heap index into the
     * vertex' adjacency list.
     */
    peano::MappingSpecification   enterCellSpecification(int level) const;

    // Below all specifications are nop.
    peano::MappingSpecification   touchVertexLastTimeSpecification(int level) const;
    peano::MappingSpecification   touchVertexFirstTimeSpecification(int level) const;
    peano::MappingSpecification   leaveCellSpecification(int level) const;
    peano::MappingSpecification   ascendSpecification(int level) const;
    peano::MappingSpecification   descendSpecification(int level) const;
    peano::CommunicationSpecification   communicationSpecification() const;

    LevelwiseAdjacencyBookkeeping();

    #if defined(SharedMemoryParallelisation)
    LevelwiseAdjacencyBookkeeping(const LevelwiseAdjacencyBookkeeping& masterThread);
    #endif

    virtual ~LevelwiseAdjacencyBookkeeping();
  
    #if defined(SharedMemoryParallelisation)
    void mergeWithWorkerThread(const LevelwiseAdjacencyBookkeeping& workerThread);
    #endif

    /**
     * Initialises the adjacency map
     * of the fine grid vertex with an invalid index.
     */
    void createInnerVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
    );

    /**
     * Initialises the adjacency map
     * of the fine  grid vertex with a boundary adjacency index.
     */
    void createBoundaryVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
    );

    /**
     * Initialises the adjacency map with an invalid index.
     */
    void createHangingVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
    );

    /**
     * Initialises the fine grid cell's heap index as invalid index if the
     * parent does not have boundary indicator as index.
     * If this is case, then the fine grid cell inherits this index.
     *
     * We try to circumvent this way that Peano introduces inside fine grid cells
     * which do have a coarse grid parent which is outside.
     */
    void createCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const         fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
    );

    /**
     * Updates the adjacency maps of all surrounding fine grid
     * vertices with the fine grid cells cell description.
     *
     * Furthermore, writes boundary indices at a neighbour cell's position in the adjacent vertices' adjacency map.
     * for every face of the cell which belongs to the boundary.
     * We have to check that all adjacent vertices of a face are at the boundary.
     * Otherwise, the face might have only one or two points on the boundary and the remaining
     * points in the interior. Such a face clearly belongs to the interior.
     *
     * \note We do not treat diagonals and edges belonging to the boundary as we assume that the neighbouring
     * cells set the domain boundary indices here correctly. This does not hold for
     * diagonals and edges shared with the global master rank (see the next note).
     *
     * \note This function interplays with function multiscalelinkedcell::HangingVertexBookkeeper::updateCellIndicesInMergeWithNeighbour(...)
     * which ensures that the adjacency indices corresponding to the global master ranks'
     * domain are always set to the value of a domain boundary adjacency index.
     *
     * \note We use the leaveCell plugin point as this is the last time a cell is touched by
     * other mappings.
     */
    void leaveCell(
      exahype::Cell&                          fineGridCell,
      exahype::Vertex * const                 fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      exahype::Vertex * const                 coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&                          coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&      fineGridPositionOfCell
    );

    #ifdef Parallel
    /**
     * Updates the adjacency maps of the fine grid vertices
     * at a remote boundary.
     */
    void mergeWithNeighbour(
      exahype::Vertex&  vertex,
      const exahype::Vertex&  neighbour,
      int                                           fromRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
    );

    /**
     * Updates the adjacency maps of the fine grid vertices
     * at a remote boundary. Only invoked if another mapping in
     * the same adapter requires worker->master communication.
     */
    void mergeWithMaster(
      const exahype::Cell&                     workerGridCell,
      exahype::Vertex * const                  workerGridVertices,
      const peano::grid::VertexEnumerator&     workerEnumerator,
      exahype::Cell&                           fineGridCell,
      exahype::Vertex * const                  fineGridVertices,
      const peano::grid::VertexEnumerator&     fineGridVerticesEnumerator,
      exahype::Vertex * const                  coarseGridVertices,
      const peano::grid::VertexEnumerator&     coarseGridVerticesEnumerator,
      exahype::Cell&                           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>& fineGridPositionOfCell,
      int                                      worker,
      const exahype::State&                    workerState,
      exahype::State&                          masterState
    );

    /**
     * Updates the adjacency map of a fine grid vertex
     * at a remote boundary. Only invoked if another mapping in
     * the same adapter requires master->worker communication.
     */
    void mergeWithWorker(
      exahype::Vertex&        localVertex,
      const exahype::Vertex&  receivedMasterVertex,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
    );

    /**
     * If we are on a new worker, write
     * invalid adjacency indices into the vertex'
     * adjacency map if the entries are not equal
     * to multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex.
     */
    void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Vertex&  localVertex,
      const exahype::Vertex&  masterOrWorkerVertex,
      int                                       fromRank,
      const tarch::la::Vector<DIMENSIONS,double>&  x,
      const tarch::la::Vector<DIMENSIONS,double>&  h,
      int                                       level
    );


    /**
     * Nop.
     */
    void mergeWithRemoteDataDueToForkOrJoin(
      exahype::Cell&  localCell,
      const exahype::Cell&  masterOrWorkerCell,
      int                                       fromRank,
      const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
      int                                       level
    );

    void prepareSendToNeighbour(
      exahype::Vertex&  vertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
    );

    void prepareCopyToRemoteNode(
      exahype::Vertex&  localVertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
    );

    void prepareCopyToRemoteNode(
      exahype::Cell&  localCell,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&   cellSize,
      int                                           level
    );

    bool prepareSendToWorker(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
      int                                                                  worker
    );

    void prepareSendToMaster(
      exahype::Cell&                       localCell,
      exahype::Vertex *                    vertices,
      const peano::grid::VertexEnumerator&       verticesEnumerator, 
      const exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
      const exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
    );


    void receiveDataFromMaster(
      exahype::Cell&                        receivedCell, 
      exahype::Vertex *                     receivedVertices,
      const peano::grid::VertexEnumerator&        receivedVerticesEnumerator,
      exahype::Vertex * const               receivedCoarseGridVertices,
      const peano::grid::VertexEnumerator&        receivedCoarseGridVerticesEnumerator,
      exahype::Cell&                        receivedCoarseGridCell,
      exahype::Vertex * const               workersCoarseGridVertices,
      const peano::grid::VertexEnumerator&        workersCoarseGridVerticesEnumerator,
      exahype::Cell&                        workersCoarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&    fineGridPositionOfCell
    );


    void mergeWithWorker(
      exahype::Cell&           localCell, 
      const exahype::Cell&     receivedMasterCell,
      const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
      int                                          level
    );


    #endif
    void destroyHangingVertex(
        const exahype::Vertex&   fineGridVertex,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
        exahype::Vertex * const  coarseGridVertices,
        const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
        exahype::Cell&           coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
    );

    void destroyVertex(
        const exahype::Vertex&   fineGridVertex,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
        exahype::Vertex * const  coarseGridVertices,
        const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
        exahype::Cell&           coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
    );


    void destroyCell(
        const exahype::Cell&           fineGridCell,
        exahype::Vertex * const        fineGridVertices,
        const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
        exahype::Vertex * const        coarseGridVertices,
        const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
        exahype::Cell&                 coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
    );

    void enterCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
    );

    void touchVertexFirstTime(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
    );


    void touchVertexLastTime(
      exahype::Vertex&         fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
    );

    void beginIteration(
      exahype::State&  solverState
    );


    void endIteration(
      exahype::State&  solverState
    );

    void descend(
      exahype::Cell * const          fineGridCells,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell
    );


    void ascend(
      exahype::Cell * const    fineGridCells,
      exahype::Vertex * const  fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell
    );    
};


#endif
