#include "exahype/adapters/MeshRefinementAndPlotTree2VTKGridVisualiser_1.h"

#include <sstream>

#include "peano/utils/Loop.h"
#include "peano/grid/CellFlags.h"

#ifdef Parallel
#include "tarch/parallel/Node.h"
#endif


int exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::_snapshotCounter = 0;



peano::CommunicationSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::communicationSpecification() const {
  return peano::CommunicationSpecification::getPessimisticSpecification(true);
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::touchVertexFirstTimeSpecification(int level) const { 
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::Serial,true);
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::enterCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidFineGridRaces,true);
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::leaveCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::Serial,true);
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::ascendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}


peano::MappingSpecification   exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::descendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}


std::map<tarch::la::Vector<DIMENSIONS,double> , int, tarch::la::VectorCompare<DIMENSIONS> >  exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::_vertex2IndexMap;


exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::MeshRefinementAndPlotTree2VTKGridVisualiser_1():
  _vtkWriter(0),
  _vertexWriter(0),
  _cellWriter(0),
  _vertexTypeWriter(0),
  _vertexRefinementControlWriter(0),
  _vertexAdjacentCellsHeight(0),
  _cellStateWriter(0) {
}


exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::~MeshRefinementAndPlotTree2VTKGridVisualiser_1() {
}


#if defined(SharedMemoryParallelisation)
exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::MeshRefinementAndPlotTree2VTKGridVisualiser_1(const MeshRefinementAndPlotTree2VTKGridVisualiser_1&  masterThread):
  _vtkWriter(masterThread._vtkWriter),
  _vertexWriter(masterThread._vertexWriter),
  _cellWriter(masterThread._cellWriter),
  _vertexTypeWriter(masterThread._vertexTypeWriter),
  _vertexRefinementControlWriter(masterThread._vertexRefinementControlWriter),
  _vertexAdjacentCellsHeight(masterThread._vertexAdjacentCellsHeight),
  _cellStateWriter(masterThread._cellStateWriter)
{
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithWorkerThread(const MeshRefinementAndPlotTree2VTKGridVisualiser_1& workerThread) {
}
#endif





void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::plotVertex(
  const exahype::Vertex&                 fineGridVertex,
  const tarch::la::Vector<DIMENSIONS,double>&  fineGridX
) {
  if ( _vertex2IndexMap.find(fineGridX) == _vertex2IndexMap.end() ) {
    assertion( _vertexWriter                  != nullptr );
    assertion( _vertexTypeWriter              != nullptr );
    assertion( _vertexRefinementControlWriter != nullptr );
    assertion( _vertexAdjacentCellsHeight     != nullptr );
    
    #if defined(Dim2) || defined(Dim3)
    _vertex2IndexMap[fineGridX] = _vertexWriter->plotVertex(fineGridX);
    #else
    _vertex2IndexMap[fineGridX] = _vertexWriter->plotVertex(tarch::la::Vector<3,double>(fineGridX.data()));
    #endif
    const int boundaryFlag = fineGridVertex.isHangingNode() ? -1 : fineGridVertex.isBoundary() ? 1 : 0;
    _vertexTypeWriter->plotVertex             (_vertex2IndexMap[fineGridX], boundaryFlag);
    _vertexRefinementControlWriter->plotVertex(_vertex2IndexMap[fineGridX],fineGridVertex.getRefinementControl() );
    _vertexAdjacentCellsHeight->plotVertex    (_vertex2IndexMap[fineGridX],fineGridVertex.getAdjacentCellsHeightOfPreviousIteration() );
  }
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::createHangingVertex(
      exahype::Vertex&     fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridH,
      exahype::Vertex * const   coarseGridVertices,
      const peano::grid::VertexEnumerator&      coarseGridVerticesEnumerator,
      exahype::Cell&       coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                   fineGridPositionOfVertex
) {
  plotVertex( fineGridVertex, fineGridX ); 
}



void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::destroyHangingVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::createInnerVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::createBoundaryVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::destroyVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::createCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::destroyCell(
      const exahype::Cell&           fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
}


#ifdef Parallel
void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithNeighbour(
  exahype::Vertex&  vertex,
  const exahype::Vertex&  neighbour,
  int                                           fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridH,
  int                                           level
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::prepareSendToNeighbour(
      exahype::Vertex&  vertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::prepareCopyToRemoteNode(
      exahype::Vertex&  localVertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::prepareCopyToRemoteNode(
      exahype::Cell&  localCell,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&   cellSize,
      int                                           level
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Vertex&  localVertex,
  const exahype::Vertex&  masterOrWorkerVertex,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Cell&  localCell,
  const exahype::Cell&  masterOrWorkerCell,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
}


bool exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::prepareSendToWorker(
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


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::prepareSendToMaster(
      exahype::Cell&                       localCell,
      exahype::Vertex *                    vertices,
      const peano::grid::VertexEnumerator&       verticesEnumerator, 
      const exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
      const exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithMaster(
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
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::receiveDataFromMaster(
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


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithWorker(
      exahype::Cell&           localCell, 
      const exahype::Cell&     receivedMasterCell,
      const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
      int                                          level
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::mergeWithWorker(
      exahype::Vertex&        localVertex,
      const exahype::Vertex&  receivedMasterVertex,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
}
#endif


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::touchVertexFirstTime(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  if (
    fineGridVertex.getRefinementControl()==exahype::Vertex::Records::Unrefined ||
    fineGridVertex.getRefinementControl()==exahype::Vertex::Records::RefinementTriggered ||
    fineGridVertex.getRefinementControl()==exahype::Vertex::Records::Erasing
  ) {
    plotVertex( fineGridVertex, fineGridX ); 
  }
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::touchVertexLastTime(
      exahype::Vertex&         fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::enterCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::leaveCell(
      exahype::Cell&           fineGridCell,
      exahype::Vertex * const  fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfCell
) {
  #ifdef Parallel
  if (fineGridCell.isLeaf() && !fineGridCell.isAssignedToRemoteRank()) {
  #else
  if (fineGridCell.isLeaf()) {
  #endif
    assertion( DIMENSIONS==2 || DIMENSIONS==3 );
    int vertexIndex[TWO_POWER_D];
     dfor2(i)
      tarch::la::Vector<DIMENSIONS,double> currentVertexPosition = fineGridVerticesEnumerator.getVertexPosition(i);
      assertion2 ( _vertex2IndexMap.find(currentVertexPosition) != _vertex2IndexMap.end(), currentVertexPosition, fineGridVertices[ fineGridVerticesEnumerator(i) ].toString() );
      vertexIndex[iScalar] = _vertex2IndexMap[currentVertexPosition];
    enddforx
  
    int cellIndex;
    if (DIMENSIONS==2) {
      cellIndex = _cellWriter->plotQuadrangle(vertexIndex);
    }
    if (DIMENSIONS==3) {
      cellIndex = _cellWriter->plotHexahedron(vertexIndex);
    }
    
    _cellStateWriter->plotCell(cellIndex,fineGridVerticesEnumerator.getCellFlags());
  }
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::beginIteration(
  exahype::State&  solverState
) {
  assertion( _vtkWriter==0 );
  
  _vtkWriter = new UsedWriter();
  
  _vertexWriter     = _vtkWriter->createVertexWriter();
  _cellWriter       = _vtkWriter->createCellWriter();
  
  _vertexTypeWriter               = _vtkWriter->createVertexDataWriter(exahype::Vertex::Records::getInsideOutsideDomainMapping()+"/Hanging=-1" ,1);
  _vertexRefinementControlWriter  = _vtkWriter->createVertexDataWriter(exahype::Vertex::Records::getRefinementControlMapping() ,1);
  _vertexAdjacentCellsHeight      = _vtkWriter->createVertexDataWriter( peano::grid::getCellFlagsLegend(),1);

  _cellStateWriter                = _vtkWriter->createCellDataWriter( "cell-flag(>=-1=stationary,-1=parallel-boundary,<=-2=not-stationary" ,1);
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::endIteration(
  exahype::State&  solverState
) {
  _vertexWriter->close();
  _cellWriter->close();
  
  _vertexTypeWriter->close();
  _vertexRefinementControlWriter->close();
  _vertexAdjacentCellsHeight->close();
  _cellStateWriter->close();
  
  delete _vertexWriter;
  delete _cellWriter;
  delete _vertexTypeWriter;
  delete _vertexRefinementControlWriter;
  delete _vertexAdjacentCellsHeight;
  delete _cellStateWriter;
  
  _vertexWriter                  = nullptr;
  _cellWriter                    = nullptr;
  _vertexTypeWriter              = nullptr;
  _vertexRefinementControlWriter = nullptr;
  _vertexAdjacentCellsHeight     = nullptr;
  _cellStateWriter               = nullptr;
  
  std::ostringstream snapshotFileName;
  snapshotFileName << "grid"
                   << "-" << _snapshotCounter;
  _vtkWriter->writeToFile( snapshotFileName.str() );
  
  _snapshotCounter++;                  
  
  _vertex2IndexMap.clear();
  
  delete _vtkWriter;
  _vtkWriter = nullptr;
}




void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::descend(
  exahype::Cell * const          fineGridCells,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell
) {
}


void exahype::adapters::MeshRefinementAndPlotTree2VTKGridVisualiser_1::ascend(
  exahype::Cell * const    fineGridCells,
  exahype::Vertex * const  fineGridVertices,
  const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
  exahype::Vertex * const  coarseGridVertices,
  const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
  exahype::Cell&           coarseGridCell
) {
}
