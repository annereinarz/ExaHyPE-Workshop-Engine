// This file originally was created by pdt (Peano Development Toolkit) as part
// of a code based upon the Peano project by Tobias Weinzierl. For conditions 
// of distribution and use of this project, please see the copyright notice at
// www.peano-framework.org. Feel free to adopt the license and authorship of 
// this file and your project to your needs as long as the license is in 
// agreement with the original Peano user constraints. A reference to/citation  
// of  Peano and its author is highly appreciated.
#ifndef EXAHYPE_MAPPINGS_LoadBalancing_H_
#define EXAHYPE_MAPPINGS_LoadBalancing_H_


#include "tarch/logging/Log.h"
#include "tarch/la/Vector.h"

#include "peano/grid/VertexEnumerator.h"
#include "peano/MappingSpecification.h"
#include "peano/CommunicationSpecification.h"

#include "tarch/multicore/MulticoreDefinitions.h"

#include "exahype/Vertex.h"
#include "exahype/Cell.h"
#include "exahype/State.h"

#include <map>

namespace exahype {
namespace mappings {
class LoadBalancing;
}
}


/**
 * Compute the load balancing metrics bottom-up
 *
 * The mapping plugs into leaveCell() only and basically realises the ideas
 * from the class documentation of mpibalancing::HotSpotBalancing.
 * 
 * TODO(Dominic): Multicoreise this mapping.
 *
 * @author Tobias Weinzierl
 */
class exahype::mappings::LoadBalancing {
  public:
    enum class LoadBalancingAnalysis { Greedy, GreedyWithRegularityAnalysis, Hotspot };
  private:
   /**
     * Logging device for the trace macros.
     */
    static tarch::logging::Log  _log;

    static LoadBalancingAnalysis  _loadBalancingAnalysis;

    /**
     * Number of local cells. We only count compute cells.
     */
    int _numberOfLocalCells;
    
    #ifdef Parallel //TODO Dominic fix, had to comment for single core to compile (JMG)
    /** \see determineLastLevelToPopulateUniformly */
    static int LastLevelToPopulateUniformly;
    /**
     * Compute the last level of the mesh we want to give deploy
     * to available ranks in an uniform manner before we
     * use the remaining ranks for adaptively refined areas.
     *
     * The computed level depends on the domain's shape
     * as we can typically place less workers on the levels
     * of a non-cubic domain than on the cubic bounding box
     * of that domain.
     */
    static int determineLastLevelToPopulateUniformly();
    #endif

  public:
    static void setLoadBalancingAnalysis(LoadBalancingAnalysis loadBalancingAnalysis);
    static LoadBalancingAnalysis getLoadBalancingAnalysis();

    /**
     * Nothing to be done
     */
    peano::MappingSpecification   touchVertexLastTimeSpecification(int level) const;
    peano::MappingSpecification   touchVertexFirstTimeSpecification(int level) const;
    peano::MappingSpecification   ascendSpecification(int level) const;
    peano::MappingSpecification   descendSpecification(int level) const;

    /**
     * Operation degenerates to nop if we translate without MPI.
     *
     * @see enterCell() where we clear the workload flags
     */
    peano::MappingSpecification   enterCellSpecification(int level) const;

    /**
     * Operation degenerates to nop if we translate without MPI.
     *
     * @see leaveCell() for a discussion of the restriction mechanism.
     */
    peano::MappingSpecification   leaveCellSpecification(int level) const;

    /**
     * The load balancing does rely on an analysed tree grammar, i.e.
     * information propagates from fine grid levels to coarser levels.
     * However, we do not enforce any worker-master data exchange in this
     * mapping. If another mapping does require worker-master data exchange
     * (as a global time step size or time stamp has to be computed for
     * example), we do plug into the event and also update the load balancing.
     * But in general, we do not enforce and worker-master synchronisation at
     * all.
     */
    peano::CommunicationSpecification   communicationSpecification() const;

    /**
     * TODO(Tobias): Add docu.
     */
    void enterCell(
        exahype::Cell&                 fineGridCell,
        exahype::Vertex * const        fineGridVertices,
        const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
        exahype::Vertex * const        coarseGridVertices,
        const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
        exahype::Cell&                 coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
    );
    /**
     * TODO(Tobias): Add docu.
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
      * TODO(Tobias): Add docu.
      */
     void mergeWithMaster(
         const exahype::Cell&                       workerGridCell,
         exahype::Vertex * const                    workerGridVertices,
         const peano::grid::VertexEnumerator&             workerEnumerator,
         exahype::Cell&                             fineGridCell,
         exahype::Vertex * const                    fineGridVertices,
         const peano::grid::VertexEnumerator&             fineGridVerticesEnumerator,
         exahype::Vertex * const                    coarseGridVertices,
         const peano::grid::VertexEnumerator&             coarseGridVerticesEnumerator,
         exahype::Cell&                             coarseGridCell,
         const tarch::la::Vector<DIMENSIONS,int>&         fineGridPositionOfCell,
         int                                              worker,
         const exahype::State&                      workerState,
         exahype::State&                            masterState
     );
     /**
      * TODO(Tobias): Add docu.
      */
     void mergeWithWorker(
         exahype::Cell&           localCell,
         const exahype::Cell&     receivedMasterCell,
         const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
         const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
         int                                          level
     );
    /**
     * Nop.
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
     * Nop.
     */
    void prepareSendToNeighbour(
        exahype::Vertex&  vertex,
        int                                           toRank,
        const tarch::la::Vector<DIMENSIONS,double>&   x,
        const tarch::la::Vector<DIMENSIONS,double>&   h,
        int                                           level
    );
    /**
     * Nop.
     */
    void prepareCopyToRemoteNode(
        exahype::Vertex&  localVertex,
        int                                           toRank,
        const tarch::la::Vector<DIMENSIONS,double>&   x,
        const tarch::la::Vector<DIMENSIONS,double>&   h,
        int                                           level
    );
    /**
     * Nop.
     */
    void prepareCopyToRemoteNode(
        exahype::Cell&  localCell,
        int                                           toRank,
        const tarch::la::Vector<DIMENSIONS,double>&   cellCentre,
        const tarch::la::Vector<DIMENSIONS,double>&   cellSize,
        int                                           level
    );
    /**
     * Nop.
     */
    void mergeWithRemoteDataDueToForkOrJoin(
        exahype::Vertex&  localVertex,
        const exahype::Vertex&  masterOrWorkerVertex,
        int                                          fromRank,
        const tarch::la::Vector<DIMENSIONS,double>&  x,
        const tarch::la::Vector<DIMENSIONS,double>&  h,
        int                                          level
    );
    /**
     * Nop.
     */
    void mergeWithRemoteDataDueToForkOrJoin(
        exahype::Cell&  localCell,
        const exahype::Cell&  masterOrWorkerCell,
        int                                          fromRank,
        const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
        const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
        int                                          level
    );
    /**
     * Nop.
     */
    bool prepareSendToWorker(
        exahype::Cell&                       fineGridCell,
        exahype::Vertex * const              fineGridVertices,
        const peano::grid::VertexEnumerator&       fineGridVerticesEnumerator,
        exahype::Vertex * const              coarseGridVertices,
        const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
        exahype::Cell&                       coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell,
        int                                        worker
    );
    /**
     * Nop.
     */
    void prepareSendToMaster(
        exahype::Cell&                       localCell,
        exahype::Vertex *                    vertices,
        const peano::grid::VertexEnumerator&       verticesEnumerator,
        const exahype::Vertex * const        coarseGridVertices,
        const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
        const exahype::Cell&                 coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
    );
    /**
     * Nop.
     */
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
    /**
     * Nop.
     */
    void mergeWithWorker(
        exahype::Vertex&        localVertex,
        const exahype::Vertex&  receivedMasterVertex,
        const tarch::la::Vector<DIMENSIONS,double>&   x,
        const tarch::la::Vector<DIMENSIONS,double>&   h,
        int                                           level
    );
  #endif
    /**
     * Nop.
     */
    LoadBalancing();
  #if defined(SharedMemoryParallelisation)
    /**
     * Nop.
     */
    LoadBalancing(const LoadBalancing& masterThread);
  #endif
    /**
     * Nop.
     */
    virtual ~LoadBalancing();
  #if defined(SharedMemoryParallelisation)
    /**
     * Nop.
     */
    void mergeWithWorkerThread(const LoadBalancing& workerThread);
  #endif
    /**
     * Nop.
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
     * Nop.
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
     * Nop.
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
     * Nop.
     */
    void destroyHangingVertex(
        const exahype::Vertex&   fineGridVertex,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
        exahype::Vertex * const  coarseGridVertices,
        const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
        exahype::Cell&           coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
    );
    /**
     * Nop.
     */
    void destroyVertex(
        const exahype::Vertex&   fineGridVertex,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
        exahype::Vertex * const  coarseGridVertices,
        const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
        exahype::Cell&           coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
    );
    /**
     * Nop.
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
     * Nop.
     */
    void destroyCell(
        const exahype::Cell&           fineGridCell,
        exahype::Vertex * const        fineGridVertices,
        const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
        exahype::Vertex * const        coarseGridVertices,
        const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
        exahype::Cell&                 coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
    );
    /**
     * Nop.
     */
    void touchVertexFirstTime(
        exahype::Vertex&               fineGridVertex,
        const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
        const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
        exahype::Vertex * const        coarseGridVertices,
        const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
        exahype::Cell&                 coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
    );
    /**
     * Nop.
     */
    void touchVertexLastTime(
        exahype::Vertex&         fineGridVertex,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
        const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
        exahype::Vertex * const  coarseGridVertices,
        const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
        exahype::Cell&           coarseGridCell,
        const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
    );
    /**
     * Nop.
     */
    void beginIteration(
        exahype::State&  solverState
    );
    /**
     * Nop.
     */
    void endIteration(
        exahype::State&  solverState
    );
    /**
     * Nop.
     */
    void descend(
        exahype::Cell * const          fineGridCells,
        exahype::Vertex * const        fineGridVertices,
        const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
        exahype::Vertex * const        coarseGridVertices,
        const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
        exahype::Cell&                 coarseGridCell
    );
    /**
     * Nop.
     */
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
