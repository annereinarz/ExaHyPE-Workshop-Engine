#include "petsc/mappings/Enumerate.h"
#include "petsc/VertexOperations.h"


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::CommunicationSpecification   petsc::mappings::Enumerate::communicationSpecification() const {
  return peano::CommunicationSpecification(
      peano::CommunicationSpecification::ExchangeMasterWorkerData::SendDataAndStateBeforeFirstTouchVertexFirstTime,
      peano::CommunicationSpecification::ExchangeWorkerMasterData::SendDataAndStateAfterLastTouchVertexLastTime,
      false
  );
}


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::MappingSpecification   petsc::mappings::Enumerate::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::MappingSpecification   petsc::mappings::Enumerate::touchVertexFirstTimeSpecification(int level) const { 
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::MappingSpecification   petsc::mappings::Enumerate::enterCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::AvoidFineGridRaces,true);
}


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::MappingSpecification   petsc::mappings::Enumerate::leaveCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::AvoidFineGridRaces,true);
}


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::MappingSpecification   petsc::mappings::Enumerate::ascendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::AvoidCoarseGridRaces,true);
}


/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::MappingSpecification   petsc::mappings::Enumerate::descendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::AvoidCoarseGridRaces,true);
}


tarch::logging::Log                petsc::mappings::Enumerate::_log( "petsc::mappings::Enumerate" ); 


petsc::mappings::Enumerate::Enumerate() {
  logTraceIn( "Enumerate()" );
  // @todo Insert your code here
  logTraceOut( "Enumerate()" );
}


petsc::mappings::Enumerate::~Enumerate() {
  logTraceIn( "~Enumerate()" );
  // @todo Insert your code here
  logTraceOut( "~Enumerate()" );
}


#if defined(SharedMemoryParallelisation)
petsc::mappings::Enumerate::Enumerate(const Enumerate&  masterThread) {
  logTraceIn( "Enumerate(Enumerate)" );
  // @todo Insert your code here
  logTraceOut( "Enumerate(Enumerate)" );
}


void petsc::mappings::Enumerate::mergeWithWorkerThread(const Enumerate& workerThread) {
  logTraceIn( "mergeWithWorkerThread(Enumerate)" );
  // @todo Insert your code here
  logTraceOut( "mergeWithWorkerThread(Enumerate)" );
}
#endif


void petsc::mappings::Enumerate::createHangingVertex(
      petsc::Vertex&     fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridH,
      petsc::Vertex * const   coarseGridVertices,
      const peano::grid::VertexEnumerator&      coarseGridVerticesEnumerator,
      petsc::Cell&       coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                   fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "createHangingVertex(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );
  // @todo Insert your code here
  logTraceOutWith1Argument( "createHangingVertex(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::destroyHangingVertex(
      const petsc::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      petsc::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      petsc::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "destroyHangingVertex(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );
  // @todo Insert your code here
  logTraceOutWith1Argument( "destroyHangingVertex(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::createInnerVertex(
      petsc::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      petsc::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      petsc::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "createInnerVertex(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );
  // @todo Insert your code here
  logTraceOutWith1Argument( "createInnerVertex(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::createBoundaryVertex(
      petsc::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      petsc::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      petsc::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "createBoundaryVertex(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );
  // @todo Insert your code here
  logTraceOutWith1Argument( "createBoundaryVertex(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::destroyVertex(
      const petsc::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      petsc::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      petsc::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "destroyVertex(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );
  // @todo Insert your code here
  logTraceOutWith1Argument( "destroyVertex(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::createCell(
      petsc::Cell&                 fineGridCell,
      petsc::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      petsc::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      petsc::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  logTraceInWith4Arguments( "createCell(...)", fineGridCell, fineGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfCell );
  // @todo Insert your code here
  logTraceOutWith1Argument( "createCell(...)", fineGridCell );
}


void petsc::mappings::Enumerate::destroyCell(
      const petsc::Cell&           fineGridCell,
      petsc::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      petsc::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      petsc::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  logTraceInWith4Arguments( "destroyCell(...)", fineGridCell, fineGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfCell );
  // @todo Insert your code here
  logTraceOutWith1Argument( "destroyCell(...)", fineGridCell );
}

#ifdef Parallel
void petsc::mappings::Enumerate::mergeWithNeighbour(
  petsc::Vertex&  vertex,
  const petsc::Vertex&  neighbour,
  int                                           fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridH,
  int                                           level
) {
  logTraceInWith6Arguments( "mergeWithNeighbour(...)", vertex, neighbour, fromRank, fineGridX, fineGridH, level );
  // @todo Insert your code here
  logTraceOut( "mergeWithNeighbour(...)" );
}

void petsc::mappings::Enumerate::prepareSendToNeighbour(
  petsc::Vertex&  vertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
  logTraceInWith3Arguments( "prepareSendToNeighbour(...)", vertex, toRank, level );
  // @todo Insert your code here
  logTraceOut( "prepareSendToNeighbour(...)" );
}

void petsc::mappings::Enumerate::prepareCopyToRemoteNode(
  petsc::Vertex&  localVertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
  logTraceInWith5Arguments( "prepareCopyToRemoteNode(...)", localVertex, toRank, x, h, level );
  // @todo Insert your code here
  logTraceOut( "prepareCopyToRemoteNode(...)" );
}

void petsc::mappings::Enumerate::prepareCopyToRemoteNode(
  petsc::Cell&  localCell,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&   cellSize,
      int                                           level
) {
  logTraceInWith5Arguments( "prepareCopyToRemoteNode(...)", localCell, toRank, cellCentre, cellSize, level );
  // @todo Insert your code here
  logTraceOut( "prepareCopyToRemoteNode(...)" );
}

void petsc::mappings::Enumerate::mergeWithRemoteDataDueToForkOrJoin(
  petsc::Vertex&  localVertex,
  const petsc::Vertex&  masterOrWorkerVertex,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
  logTraceInWith6Arguments( "mergeWithRemoteDataDueToForkOrJoin(...)", localVertex, masterOrWorkerVertex, fromRank, x, h, level );
  // @todo Insert your code here
  logTraceOut( "mergeWithRemoteDataDueToForkOrJoin(...)" );
}

void petsc::mappings::Enumerate::mergeWithRemoteDataDueToForkOrJoin(
  petsc::Cell&  localCell,
  const petsc::Cell&  masterOrWorkerCell,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
  const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
  int                                       level
) {
  logTraceInWith3Arguments( "mergeWithRemoteDataDueToForkOrJoin(...)", localCell, masterOrWorkerCell, fromRank );
  // @todo Insert your code here
  logTraceOut( "mergeWithRemoteDataDueToForkOrJoin(...)" );
}

bool petsc::mappings::Enumerate::prepareSendToWorker(
  petsc::Cell&                 fineGridCell,
  petsc::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  petsc::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  petsc::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
  int                                                                  worker
) {
  logTraceIn( "prepareSendToWorker(...)" );
  // @todo Insert your code here
  logTraceOutWith1Argument( "prepareSendToWorker(...)", true );
  return true;
}

void petsc::mappings::Enumerate::prepareSendToMaster(
  petsc::Cell&                       localCell,
  petsc::Vertex *                    vertices,
  const peano::grid::VertexEnumerator&       verticesEnumerator, 
  const petsc::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
  const petsc::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
) {
  logTraceInWith2Arguments( "prepareSendToMaster(...)", localCell, verticesEnumerator.toString() );
  // @todo Insert your code here
  logTraceOut( "prepareSendToMaster(...)" );
}


void petsc::mappings::Enumerate::mergeWithMaster(
  const petsc::Cell&           workerGridCell,
  petsc::Vertex * const        workerGridVertices,
 const peano::grid::VertexEnumerator& workerEnumerator,
  petsc::Cell&                 fineGridCell,
  petsc::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  petsc::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  petsc::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
  int                                                                  worker,
  const petsc::State&          workerState,
  petsc::State&                masterState
) {
  logTraceIn( "mergeWithMaster(...)" );
  // @todo Insert your code here
  logTraceOut( "mergeWithMaster(...)" );
}


void petsc::mappings::Enumerate::receiveDataFromMaster(
      petsc::Cell&                        receivedCell, 
      petsc::Vertex *                     receivedVertices,
      const peano::grid::VertexEnumerator&        receivedVerticesEnumerator,
      petsc::Vertex * const               receivedCoarseGridVertices,
      const peano::grid::VertexEnumerator&        receivedCoarseGridVerticesEnumerator,
      petsc::Cell&                        receivedCoarseGridCell,
      petsc::Vertex * const               workersCoarseGridVertices,
      const peano::grid::VertexEnumerator&        workersCoarseGridVerticesEnumerator,
      petsc::Cell&                        workersCoarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&    fineGridPositionOfCell
) {
  logTraceIn( "receiveDataFromMaster(...)" );
  // @todo Insert your code here
  logTraceOut( "receiveDataFromMaster(...)" );
}


void petsc::mappings::Enumerate::mergeWithWorker(
  petsc::Cell&           localCell, 
  const petsc::Cell&     receivedMasterCell,
  const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
  const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
  int                                          level
) {
  logTraceInWith2Arguments( "mergeWithWorker(...)", localCell.toString(), receivedMasterCell.toString() );
  // @todo Insert your code here
  logTraceOutWith1Argument( "mergeWithWorker(...)", localCell.toString() );
}


void petsc::mappings::Enumerate::mergeWithWorker(
  petsc::Vertex&        localVertex,
  const petsc::Vertex&  receivedMasterVertex,
  const tarch::la::Vector<DIMENSIONS,double>&   x,
  const tarch::la::Vector<DIMENSIONS,double>&   h,
  int                                           level
) {
  logTraceInWith2Arguments( "mergeWithWorker(...)", localVertex.toString(), receivedMasterVertex.toString() );
  // @todo Insert your code here
  logTraceOutWith1Argument( "mergeWithWorker(...)", localVertex.toString() );
}
#endif

void petsc::mappings::Enumerate::touchVertexFirstTime(
      petsc::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      petsc::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      petsc::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "touchVertexFirstTime(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );

  if (
    fineGridVertex.getRefinementControl()==Vertex::Records::Unrefined
    &&
    fineGridVertex.isInside() // it is not a boundary point
  ) {
    VertexOperations::writeIndex(fineGridVertex,_unknownCounter);
    _unknownCounter++;
  }
  else {
    VertexOperations::writeIndex(fineGridVertex,-1);
  }

  logTraceOutWith1Argument( "touchVertexFirstTime(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::touchVertexLastTime(
      petsc::Vertex&         fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      petsc::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      petsc::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "touchVertexLastTime(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );
  // @todo Insert your code here
  logTraceOutWith1Argument( "touchVertexLastTime(...)", fineGridVertex );
}


void petsc::mappings::Enumerate::enterCell(
      petsc::Cell&                 fineGridCell,
      petsc::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      petsc::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      petsc::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  logTraceInWith4Arguments( "enterCell(...)", fineGridCell, fineGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfCell );
  // @todo Insert your code here
  logTraceOutWith1Argument( "enterCell(...)", fineGridCell );
}


void petsc::mappings::Enumerate::leaveCell(
      petsc::Cell&           fineGridCell,
      petsc::Vertex * const  fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      petsc::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      petsc::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfCell
) {
  logTraceInWith4Arguments( "leaveCell(...)", fineGridCell, fineGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfCell );
  // @todo Insert your code here
  logTraceOutWith1Argument( "leaveCell(...)", fineGridCell );
}


void petsc::mappings::Enumerate::beginIteration(
  petsc::State&  solverState
) {
  logTraceInWith1Argument( "beginIteration(State)", solverState );

  _unknownCounter = 0;

  logTraceOutWith1Argument( "beginIteration(State)", solverState);
}


void petsc::mappings::Enumerate::endIteration(
  petsc::State&  solverState
) {
  logTraceInWith1Argument( "endIteration(State)", solverState );

  const PetscInt numberOfUnknowns = _unknownCounter;

  PetscErrorCode errorU = VecCreate(tarch::parallel::Node::getInstance().getCommunicator(), &u);
  PetscErrorCode errorF = VecCreate(tarch::parallel::Node::getInstance().getCommunicator(), &f);

  if (errorU!=0) {
    PetscError( tarch::parallel::Node::getInstance().getCommunicator(),
      __LINE__,  __FUNCT__,  __FILE__, errorU,  PETSC_ERROR_INITIAL,
      "creating global solution vector failed" );
  }
  if (errorF!=0) {
    PetscError( tarch::parallel::Node::getInstance().getCommunicator(),
      __LINE__,  __FUNCT__,  __FILE__, errorF,  PETSC_ERROR_INITIAL,
      "creating global rhs vector failed" );
  }

  VecSetSizes(u, PETSC_DECIDE,numberOfUnknowns);
  VecSetSizes(f, PETSC_DECIDE,numberOfUnknowns);
  VecSetType(u, VECMPI);
  VecSetType(f, VECMPI);
  PetscObjectSetName((PetscObject)u, "Solution");
  PetscObjectSetName((PetscObject)f, "Right-hand side");

  PetscErrorCode errorA = MatCreateSeqAIJ(
    tarch::parallel::Node::getInstance().getCommunicator(),
    numberOfUnknowns,numberOfUnknowns,
    THREE_POWER_D, NULL,
    &A
  );
  if (errorA!=0) {
    PetscError( tarch::parallel::Node::getInstance().getCommunicator(),
      __LINE__,  __FUNCT__,  __FILE__, errorA,  PETSC_ERROR_INITIAL,
      "creating system matrix failed" );
  }

  logTraceOutWith1Argument( "endIteration(State)", solverState);
}



void petsc::mappings::Enumerate::descend(
  petsc::Cell * const          fineGridCells,
  petsc::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  petsc::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  petsc::Cell&                 coarseGridCell
) {
  logTraceInWith2Arguments( "descend(...)", coarseGridCell.toString(), coarseGridVerticesEnumerator.toString() );
  // @todo Insert your code here
  logTraceOut( "descend(...)" );
}


void petsc::mappings::Enumerate::ascend(
  petsc::Cell * const    fineGridCells,
  petsc::Vertex * const  fineGridVertices,
  const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
  petsc::Vertex * const  coarseGridVertices,
  const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
  petsc::Cell&           coarseGridCell
) {
  logTraceInWith2Arguments( "ascend(...)", coarseGridCell.toString(), coarseGridVerticesEnumerator.toString() );
  // @todo Insert your code here
  logTraceOut( "ascend(...)" );
}
