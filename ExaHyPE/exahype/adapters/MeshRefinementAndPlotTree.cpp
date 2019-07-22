#include "exahype/adapters/MeshRefinementAndPlotTree.h"


peano::CommunicationSpecification   exahype::adapters::MeshRefinementAndPlotTree::communicationSpecification() const {
  return peano::CommunicationSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.communicationSpecification()
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.communicationSpecification()
    &  _map2AugmentedAMRTreePlot2d.communicationSpecification()
    &  _map2LoadBalancing.communicationSpecification()
    &  _map2LevelwiseAdjacencyBookkeeping.communicationSpecification()

  ;
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.touchVertexLastTimeSpecification(level)
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.touchVertexLastTimeSpecification(level)
    &  _map2AugmentedAMRTreePlot2d.touchVertexLastTimeSpecification(level)
    &  _map2LoadBalancing.touchVertexLastTimeSpecification(level)
    &  _map2LevelwiseAdjacencyBookkeeping.touchVertexLastTimeSpecification(level)

  ;
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree::touchVertexFirstTimeSpecification(int level) const { 
  return peano::MappingSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.touchVertexFirstTimeSpecification(level)
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.touchVertexFirstTimeSpecification(level)
    &  _map2AugmentedAMRTreePlot2d.touchVertexFirstTimeSpecification(level)
    &  _map2LoadBalancing.touchVertexFirstTimeSpecification(level)
    &  _map2LevelwiseAdjacencyBookkeeping.touchVertexFirstTimeSpecification(level)

  ;
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree::enterCellSpecification(int level) const {
  return peano::MappingSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.enterCellSpecification(level)
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.enterCellSpecification(level)
    &  _map2AugmentedAMRTreePlot2d.enterCellSpecification(level)
    &  _map2LoadBalancing.enterCellSpecification(level)
    &  _map2LevelwiseAdjacencyBookkeeping.enterCellSpecification(level)

  ;
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree::leaveCellSpecification(int level) const {
  return peano::MappingSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.leaveCellSpecification(level)
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.leaveCellSpecification(level)
    &  _map2AugmentedAMRTreePlot2d.leaveCellSpecification(level)
    &  _map2LoadBalancing.leaveCellSpecification(level)
    &  _map2LevelwiseAdjacencyBookkeeping.leaveCellSpecification(level)

  ;
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree::ascendSpecification(int level) const {
  return peano::MappingSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.ascendSpecification(level)
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.ascendSpecification(level)
    &  _map2AugmentedAMRTreePlot2d.ascendSpecification(level)
    &  _map2LoadBalancing.ascendSpecification(level)
    &  _map2LevelwiseAdjacencyBookkeeping.ascendSpecification(level)

  ;
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree::descendSpecification(int level) const {
  return peano::MappingSpecification::getMinimalSpecification()
    &  _map2MeshRefinement.descendSpecification(level)
    &  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.descendSpecification(level)
    &  _map2AugmentedAMRTreePlot2d.descendSpecification(level)
    &  _map2LoadBalancing.descendSpecification(level)
    &  _map2LevelwiseAdjacencyBookkeeping.descendSpecification(level)

  ;
}


exahype::adapters::MeshRefinementAndPlotTree::MeshRefinementAndPlotTree() {
}


exahype::adapters::MeshRefinementAndPlotTree::~MeshRefinementAndPlotTree() {
}


#if defined(SharedMemoryParallelisation)
exahype::adapters::MeshRefinementAndPlotTree::MeshRefinementAndPlotTree(const MeshRefinementAndPlotTree&  masterThread):
  _map2MeshRefinement(masterThread._map2MeshRefinement) , 
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1(masterThread._map2MeshRefinementAndPlotTree2VTKGridVisualiser_1) , 
  _map2AugmentedAMRTreePlot2d(masterThread._map2AugmentedAMRTreePlot2d) , 
  _map2LoadBalancing(masterThread._map2LoadBalancing) , 
  _map2LevelwiseAdjacencyBookkeeping(masterThread._map2LevelwiseAdjacencyBookkeeping) 

{
}


void exahype::adapters::MeshRefinementAndPlotTree::mergeWithWorkerThread(const MeshRefinementAndPlotTree& workerThread) {
  _map2MeshRefinement.mergeWithWorkerThread(workerThread._map2MeshRefinement);
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithWorkerThread(workerThread._map2MeshRefinementAndPlotTree2VTKGridVisualiser_1);
  _map2AugmentedAMRTreePlot2d.mergeWithWorkerThread(workerThread._map2AugmentedAMRTreePlot2d);
  _map2LoadBalancing.mergeWithWorkerThread(workerThread._map2LoadBalancing);
  _map2LevelwiseAdjacencyBookkeeping.mergeWithWorkerThread(workerThread._map2LevelwiseAdjacencyBookkeeping);

}
#endif


void exahype::adapters::MeshRefinementAndPlotTree::createHangingVertex(
      exahype::Vertex&     fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridH,
      exahype::Vertex * const   coarseGridVertices,
      const peano::grid::VertexEnumerator&      coarseGridVerticesEnumerator,
      exahype::Cell&       coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                   fineGridPositionOfVertex
) {
  _map2MeshRefinement.createHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.createHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.createHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.createHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.createHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );


}


void exahype::adapters::MeshRefinementAndPlotTree::destroyHangingVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  _map2MeshRefinement.destroyHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.destroyHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.destroyHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.destroyHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.destroyHangingVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );

}


void exahype::adapters::MeshRefinementAndPlotTree::createInnerVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  _map2MeshRefinement.createInnerVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.createInnerVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.createInnerVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.createInnerVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.createInnerVertex(fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );

}


void exahype::adapters::MeshRefinementAndPlotTree::createBoundaryVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  _map2MeshRefinement.createBoundaryVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.createBoundaryVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.createBoundaryVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.createBoundaryVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.createBoundaryVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );

}


void exahype::adapters::MeshRefinementAndPlotTree::destroyVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  _map2MeshRefinement.destroyVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.destroyVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.destroyVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.destroyVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.destroyVertex( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );

}


void exahype::adapters::MeshRefinementAndPlotTree::createCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  _map2MeshRefinement.createCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.createCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2AugmentedAMRTreePlot2d.createCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LoadBalancing.createCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LevelwiseAdjacencyBookkeeping.createCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );

}


void exahype::adapters::MeshRefinementAndPlotTree::destroyCell(
      const exahype::Cell&           fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  _map2MeshRefinement.destroyCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.destroyCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2AugmentedAMRTreePlot2d.destroyCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LoadBalancing.destroyCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LevelwiseAdjacencyBookkeeping.destroyCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );

}


#ifdef Parallel
void exahype::adapters::MeshRefinementAndPlotTree::mergeWithNeighbour(
  exahype::Vertex&  vertex,
  const exahype::Vertex&  neighbour,
  int                                           fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridH,
  int                                           level
) {
   _map2MeshRefinement.mergeWithNeighbour( vertex, neighbour, fromRank, fineGridX, fineGridH, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithNeighbour( vertex, neighbour, fromRank, fineGridX, fineGridH, level );
   _map2AugmentedAMRTreePlot2d.mergeWithNeighbour( vertex, neighbour, fromRank, fineGridX, fineGridH, level );
   _map2LoadBalancing.mergeWithNeighbour( vertex, neighbour, fromRank, fineGridX, fineGridH, level );
   _map2LevelwiseAdjacencyBookkeeping.mergeWithNeighbour( vertex, neighbour, fromRank, fineGridX, fineGridH, level );

}


void exahype::adapters::MeshRefinementAndPlotTree::prepareSendToNeighbour(
  exahype::Vertex&  vertex,
  int                                           toRank,
  const tarch::la::Vector<DIMENSIONS,double>&   x,
  const tarch::la::Vector<DIMENSIONS,double>&   h,
  int                                           level
) {
   _map2MeshRefinement.prepareSendToNeighbour( vertex, toRank, x, h, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.prepareSendToNeighbour( vertex, toRank, x, h, level );
   _map2AugmentedAMRTreePlot2d.prepareSendToNeighbour( vertex, toRank, x, h, level );
   _map2LoadBalancing.prepareSendToNeighbour( vertex, toRank, x, h, level );
   _map2LevelwiseAdjacencyBookkeeping.prepareSendToNeighbour( vertex, toRank, x, h, level );

}


void exahype::adapters::MeshRefinementAndPlotTree::prepareCopyToRemoteNode(
  exahype::Vertex&  localVertex,
  int                                           toRank,
  const tarch::la::Vector<DIMENSIONS,double>&   x,
  const tarch::la::Vector<DIMENSIONS,double>&   h,
  int                                           level
) {
   _map2MeshRefinement.prepareCopyToRemoteNode( localVertex, toRank, x, h, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.prepareCopyToRemoteNode( localVertex, toRank, x, h, level );
   _map2AugmentedAMRTreePlot2d.prepareCopyToRemoteNode( localVertex, toRank, x, h, level );
   _map2LoadBalancing.prepareCopyToRemoteNode( localVertex, toRank, x, h, level );
   _map2LevelwiseAdjacencyBookkeeping.prepareCopyToRemoteNode( localVertex, toRank, x, h, level );

}


void exahype::adapters::MeshRefinementAndPlotTree::prepareCopyToRemoteNode(
  exahype::Cell&  localCell,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
   _map2MeshRefinement.prepareCopyToRemoteNode( localCell, toRank, x, h, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.prepareCopyToRemoteNode( localCell, toRank, x, h, level );
   _map2AugmentedAMRTreePlot2d.prepareCopyToRemoteNode( localCell, toRank, x, h, level );
   _map2LoadBalancing.prepareCopyToRemoteNode( localCell, toRank, x, h, level );
   _map2LevelwiseAdjacencyBookkeeping.prepareCopyToRemoteNode( localCell, toRank, x, h, level );

}


void exahype::adapters::MeshRefinementAndPlotTree::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Vertex&  localVertex,
  const exahype::Vertex&  masterOrWorkerVertex,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
   _map2MeshRefinement.mergeWithRemoteDataDueToForkOrJoin( localVertex, masterOrWorkerVertex, fromRank, x, h, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithRemoteDataDueToForkOrJoin( localVertex, masterOrWorkerVertex, fromRank, x, h, level );
   _map2AugmentedAMRTreePlot2d.mergeWithRemoteDataDueToForkOrJoin( localVertex, masterOrWorkerVertex, fromRank, x, h, level );
   _map2LoadBalancing.mergeWithRemoteDataDueToForkOrJoin( localVertex, masterOrWorkerVertex, fromRank, x, h, level );
   _map2LevelwiseAdjacencyBookkeeping.mergeWithRemoteDataDueToForkOrJoin( localVertex, masterOrWorkerVertex, fromRank, x, h, level );

}


void exahype::adapters::MeshRefinementAndPlotTree::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Cell&  localCell,
  const exahype::Cell&  masterOrWorkerCell,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
   _map2MeshRefinement.mergeWithRemoteDataDueToForkOrJoin( localCell, masterOrWorkerCell, fromRank, x, h, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithRemoteDataDueToForkOrJoin( localCell, masterOrWorkerCell, fromRank, x, h, level );
   _map2AugmentedAMRTreePlot2d.mergeWithRemoteDataDueToForkOrJoin( localCell, masterOrWorkerCell, fromRank, x, h, level );
   _map2LoadBalancing.mergeWithRemoteDataDueToForkOrJoin( localCell, masterOrWorkerCell, fromRank, x, h, level );
   _map2LevelwiseAdjacencyBookkeeping.mergeWithRemoteDataDueToForkOrJoin( localCell, masterOrWorkerCell, fromRank, x, h, level );

}


bool exahype::adapters::MeshRefinementAndPlotTree::prepareSendToWorker(
  exahype::Cell&                 fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
  int                                                                  worker
) {
  bool result = false;
   result |= _map2MeshRefinement.prepareSendToWorker( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker );
   result |= _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.prepareSendToWorker( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker );
   result |= _map2AugmentedAMRTreePlot2d.prepareSendToWorker( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker );
   result |= _map2LoadBalancing.prepareSendToWorker( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker );
   result |= _map2LevelwiseAdjacencyBookkeeping.prepareSendToWorker( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker );

  return result;
}


void exahype::adapters::MeshRefinementAndPlotTree::prepareSendToMaster(
  exahype::Cell&                       localCell,
  exahype::Vertex *                    vertices,
  const peano::grid::VertexEnumerator&       verticesEnumerator, 
  const exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
  const exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
) {
   _map2MeshRefinement.prepareSendToMaster( localCell, vertices, verticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.prepareSendToMaster( localCell, vertices, verticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
   _map2AugmentedAMRTreePlot2d.prepareSendToMaster( localCell, vertices, verticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
   _map2LoadBalancing.prepareSendToMaster( localCell, vertices, verticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
   _map2LevelwiseAdjacencyBookkeeping.prepareSendToMaster( localCell, vertices, verticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );

}


void exahype::adapters::MeshRefinementAndPlotTree::mergeWithMaster(
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
   _map2MeshRefinement.mergeWithMaster( workerGridCell, workerGridVertices, workerEnumerator, fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker, workerState, masterState );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithMaster( workerGridCell, workerGridVertices, workerEnumerator, fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker, workerState, masterState );
   _map2AugmentedAMRTreePlot2d.mergeWithMaster( workerGridCell, workerGridVertices, workerEnumerator, fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker, workerState, masterState );
   _map2LoadBalancing.mergeWithMaster( workerGridCell, workerGridVertices, workerEnumerator, fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker, workerState, masterState );
   _map2LevelwiseAdjacencyBookkeeping.mergeWithMaster( workerGridCell, workerGridVertices, workerEnumerator, fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell, worker, workerState, masterState );

}


void exahype::adapters::MeshRefinementAndPlotTree::receiveDataFromMaster(
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
   _map2MeshRefinement.receiveDataFromMaster( receivedCell, receivedVertices, receivedVerticesEnumerator, receivedCoarseGridVertices, receivedCoarseGridVerticesEnumerator, receivedCoarseGridCell, workersCoarseGridVertices, workersCoarseGridVerticesEnumerator, workersCoarseGridCell, fineGridPositionOfCell );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.receiveDataFromMaster( receivedCell, receivedVertices, receivedVerticesEnumerator, receivedCoarseGridVertices, receivedCoarseGridVerticesEnumerator, receivedCoarseGridCell, workersCoarseGridVertices, workersCoarseGridVerticesEnumerator, workersCoarseGridCell, fineGridPositionOfCell );
   _map2AugmentedAMRTreePlot2d.receiveDataFromMaster( receivedCell, receivedVertices, receivedVerticesEnumerator, receivedCoarseGridVertices, receivedCoarseGridVerticesEnumerator, receivedCoarseGridCell, workersCoarseGridVertices, workersCoarseGridVerticesEnumerator, workersCoarseGridCell, fineGridPositionOfCell );
   _map2LoadBalancing.receiveDataFromMaster( receivedCell, receivedVertices, receivedVerticesEnumerator, receivedCoarseGridVertices, receivedCoarseGridVerticesEnumerator, receivedCoarseGridCell, workersCoarseGridVertices, workersCoarseGridVerticesEnumerator, workersCoarseGridCell, fineGridPositionOfCell );
   _map2LevelwiseAdjacencyBookkeeping.receiveDataFromMaster( receivedCell, receivedVertices, receivedVerticesEnumerator, receivedCoarseGridVertices, receivedCoarseGridVerticesEnumerator, receivedCoarseGridCell, workersCoarseGridVertices, workersCoarseGridVerticesEnumerator, workersCoarseGridCell, fineGridPositionOfCell );

}


void exahype::adapters::MeshRefinementAndPlotTree::mergeWithWorker(
  exahype::Cell&           localCell, 
  const exahype::Cell&     receivedMasterCell,
  const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
  const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
  int                                          level
) {
   _map2MeshRefinement.mergeWithWorker( localCell, receivedMasterCell, cellCentre, cellSize, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithWorker( localCell, receivedMasterCell, cellCentre, cellSize, level );
   _map2AugmentedAMRTreePlot2d.mergeWithWorker( localCell, receivedMasterCell, cellCentre, cellSize, level );
   _map2LoadBalancing.mergeWithWorker( localCell, receivedMasterCell, cellCentre, cellSize, level );
   _map2LevelwiseAdjacencyBookkeeping.mergeWithWorker( localCell, receivedMasterCell, cellCentre, cellSize, level );

}


void exahype::adapters::MeshRefinementAndPlotTree::mergeWithWorker(
  exahype::Vertex&        localVertex,
  const exahype::Vertex&  receivedMasterVertex,
  const tarch::la::Vector<DIMENSIONS,double>&   x,
  const tarch::la::Vector<DIMENSIONS,double>&   h,
  int                                           level
) {
   _map2MeshRefinement.mergeWithWorker( localVertex, receivedMasterVertex, x, h, level );
   _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.mergeWithWorker( localVertex, receivedMasterVertex, x, h, level );
   _map2AugmentedAMRTreePlot2d.mergeWithWorker( localVertex, receivedMasterVertex, x, h, level );
   _map2LoadBalancing.mergeWithWorker( localVertex, receivedMasterVertex, x, h, level );
   _map2LevelwiseAdjacencyBookkeeping.mergeWithWorker( localVertex, receivedMasterVertex, x, h, level );

}
#endif


void exahype::adapters::MeshRefinementAndPlotTree::touchVertexFirstTime(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  _map2MeshRefinement.touchVertexFirstTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.touchVertexFirstTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.touchVertexFirstTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.touchVertexFirstTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.touchVertexFirstTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );

}


void exahype::adapters::MeshRefinementAndPlotTree::touchVertexLastTime(
      exahype::Vertex&         fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  _map2MeshRefinement.touchVertexLastTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.touchVertexLastTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2AugmentedAMRTreePlot2d.touchVertexLastTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LoadBalancing.touchVertexLastTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );
  _map2LevelwiseAdjacencyBookkeeping.touchVertexLastTime( fineGridVertex, fineGridX, fineGridH, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfVertex );

}


void exahype::adapters::MeshRefinementAndPlotTree::enterCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  _map2MeshRefinement.enterCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.enterCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2AugmentedAMRTreePlot2d.enterCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LoadBalancing.enterCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LevelwiseAdjacencyBookkeeping.enterCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );

}


void exahype::adapters::MeshRefinementAndPlotTree::leaveCell(
      exahype::Cell&           fineGridCell,
      exahype::Vertex * const  fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfCell
) {
  _map2MeshRefinement.leaveCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.leaveCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2AugmentedAMRTreePlot2d.leaveCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LoadBalancing.leaveCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );
  _map2LevelwiseAdjacencyBookkeeping.leaveCell( fineGridCell, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell, fineGridPositionOfCell );

}


void exahype::adapters::MeshRefinementAndPlotTree::beginIteration(
  exahype::State&  solverState
) {
  _map2MeshRefinement.beginIteration( solverState );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.beginIteration( solverState );
  _map2AugmentedAMRTreePlot2d.beginIteration( solverState );
  _map2LoadBalancing.beginIteration( solverState );
  _map2LevelwiseAdjacencyBookkeeping.beginIteration( solverState );

}


void exahype::adapters::MeshRefinementAndPlotTree::endIteration(
  exahype::State&  solverState
) {
  _map2MeshRefinement.endIteration( solverState );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.endIteration( solverState );
  _map2AugmentedAMRTreePlot2d.endIteration( solverState );
  _map2LoadBalancing.endIteration( solverState );
  _map2LevelwiseAdjacencyBookkeeping.endIteration( solverState );

}




void exahype::adapters::MeshRefinementAndPlotTree::descend(
  exahype::Cell * const          fineGridCells,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell
) {
  _map2MeshRefinement.descend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.descend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2AugmentedAMRTreePlot2d.descend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2LoadBalancing.descend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2LevelwiseAdjacencyBookkeeping.descend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );

}


void exahype::adapters::MeshRefinementAndPlotTree::ascend(
  exahype::Cell * const    fineGridCells,
  exahype::Vertex * const  fineGridVertices,
  const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
  exahype::Vertex * const  coarseGridVertices,
  const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
  exahype::Cell&           coarseGridCell
) {
  _map2MeshRefinement.ascend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2MeshRefinementAndPlotTree2VTKGridVisualiser_1.ascend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2AugmentedAMRTreePlot2d.ascend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2LoadBalancing.ascend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );
  _map2LevelwiseAdjacencyBookkeeping.ascend( fineGridCells, fineGridVertices, fineGridVerticesEnumerator, coarseGridVertices, coarseGridVerticesEnumerator, coarseGridCell );

}
