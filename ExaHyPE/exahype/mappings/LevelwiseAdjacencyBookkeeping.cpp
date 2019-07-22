#include "exahype/mappings/LevelwiseAdjacencyBookkeeping.h"

#include <sstream>

#include "peano/utils/Loop.h"
#include "peano/grid/CellFlags.h"


#include "multiscalelinkedcell/HangingVertexBookkeeper.h"
#include "exahype/VertexOperations.h"


peano::CommunicationSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::communicationSpecification() const {
  return peano::CommunicationSpecification::getMinimalSpecification(true);
}

peano::MappingSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::leaveCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::AvoidFineGridRaces,true);
}

// All specifications below are Nop
peano::MappingSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::enterCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
}
peano::MappingSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidFineGridRaces,true);
}
peano::MappingSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::touchVertexFirstTimeSpecification(int level) const { 
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidFineGridRaces,true);
}
peano::MappingSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::ascendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidFineGridRaces,true);
}
peano::MappingSpecification   exahype::mappings::LevelwiseAdjacencyBookkeeping::descendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidFineGridRaces,true);
}


exahype::mappings::LevelwiseAdjacencyBookkeeping::LevelwiseAdjacencyBookkeeping() {
}


exahype::mappings::LevelwiseAdjacencyBookkeeping::~LevelwiseAdjacencyBookkeeping() {
}


#if defined(SharedMemoryParallelisation)
exahype::mappings::LevelwiseAdjacencyBookkeeping::LevelwiseAdjacencyBookkeeping(const LevelwiseAdjacencyBookkeeping&  masterThread) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithWorkerThread(const LevelwiseAdjacencyBookkeeping& workerThread) {
}
#endif


void exahype::mappings::LevelwiseAdjacencyBookkeeping::createHangingVertex(
  exahype::Vertex&     fineGridVertex,
  const tarch::la::Vector<DIMENSIONS,double>&                fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&                fineGridH,
  exahype::Vertex * const   coarseGridVertices,
  const peano::grid::VertexEnumerator&      coarseGridVerticesEnumerator,
  exahype::Cell&       coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                   fineGridPositionOfVertex
) {
  VertexOperations::writeCellDescriptionsIndex(
      fineGridVertex,multiscalelinkedcell::HangingVertexBookkeeper::createVertexLinkMapForNewVertex());
  VertexOperations::writeADERDGCellDescriptions(
      fineGridVertex,static_cast<void*>(nullptr));
  VertexOperations::writeFiniteVolumesCellDescriptions(
        fineGridVertex,static_cast<void*>(nullptr));
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::createInnerVertex(
  exahype::Vertex&               fineGridVertex,
  const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  VertexOperations::writeCellDescriptionsIndex(
      fineGridVertex,multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
  VertexOperations::writeADERDGCellDescriptions(
      fineGridVertex,static_cast<void*>(nullptr));
  VertexOperations::writeFiniteVolumesCellDescriptions(
      fineGridVertex,static_cast<void*>(nullptr));
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::createBoundaryVertex(
  exahype::Vertex&               fineGridVertex,
  const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  VertexOperations::writeCellDescriptionsIndex(
      fineGridVertex,multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex);
  VertexOperations::writeADERDGCellDescriptions(
      fineGridVertex,static_cast<void*>(nullptr));
  VertexOperations::writeFiniteVolumesCellDescriptions(
      fineGridVertex,static_cast<void*>(nullptr));
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::createCell(
  exahype::Cell&                 fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  fineGridCell.setCellDescriptionsIndex(
      multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
}



void exahype::mappings::LevelwiseAdjacencyBookkeeping::leaveCell(
      exahype::Cell&           fineGridCell,
      exahype::Vertex * const  fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfCell
) {
  // Write cell's index into adjacent vertices
  dfor2(k)
    if ( !fineGridVertices[fineGridVerticesEnumerator(k) ].isHangingNode() ) {
      const int index = TWO_POWER_D-kScalar-1;
      VertexOperations::writeCellDescriptionsIndex(
          fineGridVertices[fineGridVerticesEnumerator(k)], index, fineGridCell.getCellDescriptionsIndex());
      VertexOperations::writeADERDGCellDescriptions(
          fineGridVertices[fineGridVerticesEnumerator(k)], index, fineGridCell.getADERDGCellDescriptions());
      VertexOperations::writeFiniteVolumesCellDescriptions(
          fineGridVertices[fineGridVerticesEnumerator(k)], index, fineGridCell.getFiniteVolumesCellDescriptions());
    }
  enddforx

  // Write boundary index at neighbour cell's position in adjacent vertices' adjacency map per face
  for (int direction = 0; direction < DIMENSIONS; ++direction) {
    for (int orientation = 0; orientation < 2; ++orientation) {
      bool isBoundaryFace = true;
      dfor2(v)
        isBoundaryFace &=
            v(direction)!=orientation ||
            fineGridVertices[ fineGridVerticesEnumerator(v) ].isBoundary();
      enddforx

      if ( isBoundaryFace ) {
        dfor2(v)
          dfor2(c)
          if (
              v(direction)==orientation &&
              c(direction)==orientation
          ) {
            assertion( !fineGridVertices[ fineGridVerticesEnumerator(v) ].isHangingNode() );

            VertexOperations::writeCellDescriptionsIndex(
                fineGridVertices[ fineGridVerticesEnumerator(v) ], cScalar,
                multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex);
          }
        enddforx
        enddforx
      }
    }
  }
}

#ifdef Parallel
void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithNeighbour(
  exahype::Vertex&  vertex,
  const exahype::Vertex&  neighbour,
  int                                           fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridH,
  int                                           level
) {
  VertexOperations::writeCellDescriptionsIndex(
    vertex,
    multiscalelinkedcell::HangingVertexBookkeeper::updateCellIndicesInMergeWithNeighbour(
      vertex.getAdjacentRanks(),
      VertexOperations::readCellDescriptionsIndex(vertex)
    )
  );
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithMaster(
  const exahype::Cell&           workerGridCell,
  exahype::Vertex * const        workerGridVertices,
  const peano::grid::VertexEnumerator& workerEnumerator,
  exahype::Cell&                 fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
  int                                                                  worker,
  const exahype::State&          workerState,
  exahype::State&                masterState
) {
  dfor2(k)
    VertexOperations::writeCellDescriptionsIndex(
      fineGridVertices[ fineGridVerticesEnumerator(k) ],
      multiscalelinkedcell::HangingVertexBookkeeper::updateCellIndicesInMergeWithNeighbour(
        fineGridVertices[ fineGridVerticesEnumerator(k) ].getAdjacentRanks(),
        VertexOperations::readCellDescriptionsIndex(fineGridVertices[ fineGridVerticesEnumerator(k) ])
      )
    );
  enddforx
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithWorker(
      exahype::Vertex&        localVertex,
      const exahype::Vertex&  receivedMasterVertex,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
  VertexOperations::writeCellDescriptionsIndex(
    localVertex,
    multiscalelinkedcell::HangingVertexBookkeeper::updateCellIndicesInMergeWithNeighbour(
      localVertex.getAdjacentRanks(),
      VertexOperations::readCellDescriptionsIndex(localVertex)
    )
  );
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Cell&  localCell,
  const exahype::Cell&  masterOrWorkerCell,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
  // do not reset heap indices here. This will result in wrong behaviour when
  // merging remote data if this mapping follows another mapping
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Vertex&  localVertex,
  const exahype::Vertex&  masterOrWorkerVertex,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
  if ( exahype::State::isNewWorkerDueToForkOfExistingDomain() ) {
    dfor2(c)
      if ( localVertex.getCellDescriptionsIndex()[cScalar] != multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex ) {
        exahype::VertexOperations::writeCellDescriptionsIndex(
            localVertex,
            cScalar,
            multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
        exahype::VertexOperations::writeADERDGCellDescriptions(
            localVertex,
            cScalar,
            nullptr);
        exahype::VertexOperations::writeFiniteVolumesCellDescriptions(
            localVertex,
            cScalar,
            nullptr);
      }
    enddforx
  }
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::prepareSendToNeighbour(
      exahype::Vertex&  vertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::prepareCopyToRemoteNode(
      exahype::Vertex&  localVertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::prepareCopyToRemoteNode(
      exahype::Cell&  localCell,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&   cellSize,
      int                                           level
) {
}


bool exahype::mappings::LevelwiseAdjacencyBookkeeping::prepareSendToWorker(
  exahype::Cell&                 fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
  int                                                                  worker
) {
  return false;
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::prepareSendToMaster(
      exahype::Cell&                       localCell,
      exahype::Vertex *                    vertices,
      const peano::grid::VertexEnumerator&       verticesEnumerator, 
      const exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
      const exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
) {
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::receiveDataFromMaster(
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
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::mergeWithWorker(
      exahype::Cell&           localCell, 
      const exahype::Cell&     receivedMasterCell,
      const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
      int                                          level
) {
}
#endif


void exahype::mappings::LevelwiseAdjacencyBookkeeping::destroyHangingVertex(
  const exahype::Vertex&   fineGridVertex,
  const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
  exahype::Vertex * const  coarseGridVertices,
  const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
  exahype::Cell&           coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::destroyVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::destroyCell(
  const exahype::Cell&           fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  // Write invalid index into adjacent vertices
  dfor2(k)
    if ( !fineGridVertices[fineGridVerticesEnumerator(k) ].isHangingNode() ) {
      VertexOperations::writeCellDescriptionsIndex(
          fineGridVertices[fineGridVerticesEnumerator(k)], TWO_POWER_D-kScalar-1,
              multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
      VertexOperations::writeADERDGCellDescriptions(
          fineGridVertices[fineGridVerticesEnumerator(k)], TWO_POWER_D-kScalar-1,nullptr);
      VertexOperations::writeFiniteVolumesCellDescriptions(
          fineGridVertices[fineGridVerticesEnumerator(k)], TWO_POWER_D-kScalar-1,nullptr);
    }
  enddforx
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::touchVertexFirstTime(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::touchVertexLastTime(
      exahype::Vertex&         fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::enterCell(
  exahype::Cell&                 fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
}

void exahype::mappings::LevelwiseAdjacencyBookkeeping::beginIteration(
  exahype::State&  solverState
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::endIteration(
  exahype::State&  solverState
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::descend(
  exahype::Cell * const          fineGridCells,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell
) {
}


void exahype::mappings::LevelwiseAdjacencyBookkeeping::ascend(
  exahype::Cell * const    fineGridCells,
  exahype::Vertex * const  fineGridVertices,
  const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
  exahype::Vertex * const  coarseGridVertices,
  const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
  exahype::Cell&           coarseGridCell
) {
}
