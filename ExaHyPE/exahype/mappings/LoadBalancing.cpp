#include "exahype/mappings/LoadBalancing.h"
#include "mpibalancing/HotspotBalancing.h"
#include "peano/utils/UserInterface.h"

#include "tarch/la/ScalarOperations.h"

exahype::mappings::LoadBalancing::LoadBalancingAnalysis  exahype::mappings::LoadBalancing::_loadBalancingAnalysis;


exahype::mappings::LoadBalancing::LoadBalancing()
  : _numberOfLocalCells(0) {
  // do nothing
}

#ifdef Parallel
int exahype::mappings::LoadBalancing::LastLevelToPopulateUniformly  = -1;

/**
 * Compute the number of ranks required to populate the given mesh level
 */
int exahype::mappings::LoadBalancing::determineLastLevelToPopulateUniformly() {
  if ( exahype::solvers::Solver::allSolversPerformOnlyUniformRefinement() ) {
    return std::numeric_limits<int>::max();
  } else {
    tarch::la::Vector<DIMENSIONS,double> domain = exahype::solvers::Solver::getDomainSize();
    const int uniformMeshLevel                  = exahype::solvers::Solver::getCoarsestMeshLevelOfAllSolvers(); // TODO(Dominic): Multisolver: Maybe consider to use the finest mesh level instead?
    const double uniformMeshSize                = exahype::solvers::Solver::getCoarsestMeshSizeOfAllSolvers();
    
    const int numberOfAvailableRanks = tarch::parallel::Node::getInstance().getNumberOfNodes();

    int level     = 1;
    int usedRanks = 1; // global master rank
    while( level < uniformMeshLevel && usedRanks <= numberOfAvailableRanks ) {
      const int levelDelta = uniformMeshLevel - level;
      
      int ranksToDeployOnCurrentLevel = 1;
      for (int d=0; d<DIMENSIONS; d++) {
        const int numberOfCellsOnUniformGrid = static_cast<int>( std::round ( domain[d] / uniformMeshSize ) );
        ranksToDeployOnCurrentLevel *= numberOfCellsOnUniformGrid / tarch::la::aPowI(levelDelta,3);
      }
      usedRanks += ranksToDeployOnCurrentLevel;
      level++;
    }
    int maxLevel =  std::min(uniformMeshLevel-1, std::max(2,level-2)); // -1 since the while loop went one further, -1 since we do not touch the actual uniform grid with the load balancing
    return maxLevel;
  }
}
#endif

void exahype::mappings::LoadBalancing::setLoadBalancingAnalysis(LoadBalancingAnalysis loadBalancingAnalysis) {
  _loadBalancingAnalysis = loadBalancingAnalysis;
}


exahype::mappings::LoadBalancing::LoadBalancingAnalysis exahype::mappings::LoadBalancing::getLoadBalancingAnalysis() {
  return _loadBalancingAnalysis;
}


peano::CommunicationSpecification   exahype::mappings::LoadBalancing::communicationSpecification() const {
  return peano::CommunicationSpecification(
      peano::CommunicationSpecification::ExchangeMasterWorkerData::MaskOutMasterWorkerDataAndStateExchange,
      peano::CommunicationSpecification::ExchangeWorkerMasterData::SendDataAndStateAfterProcessingOfLocalSubtree,
      true
  );
}


/**
 * All empty
 */
peano::MappingSpecification   exahype::mappings::LoadBalancing::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
}
peano::MappingSpecification   exahype::mappings::LoadBalancing::touchVertexFirstTimeSpecification(int level) const { 
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
}
peano::MappingSpecification   exahype::mappings::LoadBalancing::ascendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidCoarseGridRaces,false);
}
peano::MappingSpecification   exahype::mappings::LoadBalancing::descendSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::AvoidCoarseGridRaces,false);
}

/**
 * All is done cell-wisely.
 */
peano::MappingSpecification   exahype::mappings::LoadBalancing::enterCellSpecification(int level) const {
  #ifdef Parallel
  return peano::MappingSpecification(peano::MappingSpecification::WholeTree,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // count cells on coarse grid, too
  #else
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
  #endif
}

peano::MappingSpecification   exahype::mappings::LoadBalancing::leaveCellSpecification(int level) const {
  return peano::MappingSpecification(peano::MappingSpecification::Nop,peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}


tarch::logging::Log                exahype::mappings::LoadBalancing::_log( "exahype::mappings::LoadBalancing" ); 





#if defined(SharedMemoryParallelisation)
exahype::mappings::LoadBalancing::LoadBalancing(const LoadBalancing&  masterThread) {
  _numberOfLocalCells = 0;
}


void exahype::mappings::LoadBalancing::mergeWithWorkerThread(const LoadBalancing& workerThread) {
  _numberOfLocalCells += workerThread._numberOfLocalCells;
}
#endif

void exahype::mappings::LoadBalancing::beginIteration(
  exahype::State&  solverState
) {
  #ifdef Parallel
  LastLevelToPopulateUniformly  = 
      determineLastLevelToPopulateUniformly();
  _numberOfLocalCells = 0;
  solverState.setReduceStateAndCell(true);
  #endif
}

void exahype::mappings::LoadBalancing::enterCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  #ifdef Parallel
  if ( 
    fineGridVerticesEnumerator.getLevel() <= LastLevelToPopulateUniformly
  ) {
    // The user can use this call back to give a hint on how the final mesh will look like
    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
       auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
       _numberOfLocalCells += solver->computeGeometricLoadBalancingWeight(fineGridVerticesEnumerator.getCellCenter(),fineGridVerticesEnumerator.getCellSize());
    }
  } else if ( fineGridVerticesEnumerator.getLevel() == 2 ) {
    // do not compute any weights on level 2 if it does not belong to the coarse grid. 
    // It does not make sense to distribute it then.
  } else {
    _numberOfLocalCells += exahype::solvers::ADERDGSolver::computeWeight(fineGridCell.getCellDescriptionsIndex());
    _numberOfLocalCells += exahype::solvers::FiniteVolumesSolver::computeWeight(fineGridCell.getCellDescriptionsIndex());
  }
  #endif
}


void exahype::mappings::LoadBalancing::leaveCell(
      exahype::Cell&           fineGridCell,
      exahype::Vertex * const  fineGridVertices,
      const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfCell
) {
}


#ifdef Parallel
bool exahype::mappings::LoadBalancing::prepareSendToWorker(
  exahype::Cell&                 fineGridCell,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell,
  int                                                                  worker
) {
  return true; // we want to perform a reduction for this worker
}

void exahype::mappings::LoadBalancing::mergeWithMaster(
  const exahype::Cell&                       workerGridCell,
  exahype::Vertex * const                    workerGridVertices,
  const peano::grid::VertexEnumerator&       workerEnumerator,
  exahype::Cell&                             fineGridCell,
  exahype::Vertex * const                    fineGridVertices,
  const peano::grid::VertexEnumerator&       fineGridVerticesEnumerator,
  exahype::Vertex * const                    coarseGridVertices,
  const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
  exahype::Cell&                             coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell,
  int                                        worker,
  const exahype::State&                      workerState,
  exahype::State&                            masterState
) {
  logTraceIn( "mergeWithMaster(...)" );

  if (_loadBalancingAnalysis==LoadBalancingAnalysis::Hotspot) {
    mpibalancing::HotspotBalancing::mergeWithMaster(
      worker,
      workerState.getCouldNotEraseDueToDecompositionFlag(),
      coarseGridVerticesEnumerator.getLevel()+1
    );
  }

  logTraceOut( "mergeWithMaster(...)" );
}

void exahype::mappings::LoadBalancing::prepareSendToMaster(
  exahype::Cell&                       localCell,
  exahype::Vertex *                    vertices,
  const peano::grid::VertexEnumerator&       verticesEnumerator,
  const exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&       coarseGridVerticesEnumerator,
  const exahype::Cell&                 coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&   fineGridPositionOfCell
) {
  if ( // do not count the root cell
    verticesEnumerator.getLevel() <= LastLevelToPopulateUniformly
  ) {
    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      _numberOfLocalCells -= solver->computeGeometricLoadBalancingWeight(verticesEnumerator.getCellCenter(),verticesEnumerator.getCellSize());
    }
  } else {
    _numberOfLocalCells -= exahype::solvers::ADERDGSolver::computeWeight(localCell.getCellDescriptionsIndex());
    _numberOfLocalCells -= exahype::solvers::FiniteVolumesSolver::computeWeight(localCell.getCellDescriptionsIndex());
  }

  if (_loadBalancingAnalysis==LoadBalancingAnalysis::Hotspot) {
    mpibalancing::HotspotBalancing::setLocalWeightAndNotifyMaster( _numberOfLocalCells );
  }
}
#endif

//
//   NOP
// =======
//

void exahype::mappings::LoadBalancing::endIteration(exahype::State&  solverState) {}

exahype::mappings::LoadBalancing::~LoadBalancing() {
  // do nothing
}

void exahype::mappings::LoadBalancing::createHangingVertex(
      exahype::Vertex&     fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                fineGridH,
      exahype::Vertex * const   coarseGridVertices,
      const peano::grid::VertexEnumerator&      coarseGridVerticesEnumerator,
      exahype::Cell&       coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                   fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::destroyHangingVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::createInnerVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::createBoundaryVertex(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::destroyVertex(
      const exahype::Vertex&   fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::createCell(
      exahype::Cell&                 fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::destroyCell(
      const exahype::Cell&           fineGridCell,
      exahype::Vertex * const        fineGridVertices,
      const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfCell
) {
  // do nothing
}

#ifdef Parallel
void exahype::mappings::LoadBalancing::mergeWithWorker(
  exahype::Cell&           localCell,
  const exahype::Cell&     receivedMasterCell,
  const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
  const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
  int                                          level
) {
}

void exahype::mappings::LoadBalancing::mergeWithNeighbour(
  exahype::Vertex&  vertex,
  const exahype::Vertex&  neighbour,
  int                                           fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&   fineGridH,
  int                                           level
) {
  // do nothing
}

void exahype::mappings::LoadBalancing::prepareSendToNeighbour(
  exahype::Vertex&  vertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
  // do nothing
}

void exahype::mappings::LoadBalancing::prepareCopyToRemoteNode(
  exahype::Vertex&  localVertex,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   x,
      const tarch::la::Vector<DIMENSIONS,double>&   h,
      int                                           level
) {
  // do nothing
}

void exahype::mappings::LoadBalancing::prepareCopyToRemoteNode(
  exahype::Cell&  localCell,
      int                                           toRank,
      const tarch::la::Vector<DIMENSIONS,double>&   cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>&   cellSize,
      int                                           level
) {
  // do nothing
}

void exahype::mappings::LoadBalancing::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Vertex&  localVertex,
  const exahype::Vertex&  masterOrWorkerVertex,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  int                                       level
) {
  // do nothing
}

void exahype::mappings::LoadBalancing::mergeWithRemoteDataDueToForkOrJoin(
  exahype::Cell&  localCell,
  const exahype::Cell&  masterOrWorkerCell,
  int                                       fromRank,
  const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
  const tarch::la::Vector<DIMENSIONS,double>&  cellSize,
  int                                       level
) {
  // do nothing
}

void exahype::mappings::LoadBalancing::receiveDataFromMaster(
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
  // do nothing
}


void exahype::mappings::LoadBalancing::mergeWithWorker(
  exahype::Vertex&        localVertex,
  const exahype::Vertex&  receivedMasterVertex,
  const tarch::la::Vector<DIMENSIONS,double>&   x,
  const tarch::la::Vector<DIMENSIONS,double>&   h,
  int                                           level
) {
  // do nothing
}
#endif

void exahype::mappings::LoadBalancing::touchVertexFirstTime(
      exahype::Vertex&               fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                          fineGridH,
      exahype::Vertex * const        coarseGridVertices,
      const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
      exahype::Cell&                 coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                             fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::touchVertexLastTime(
      exahype::Vertex&         fineGridVertex,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridX,
      const tarch::la::Vector<DIMENSIONS,double>&                    fineGridH,
      exahype::Vertex * const  coarseGridVertices,
      const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
      exahype::Cell&           coarseGridCell,
      const tarch::la::Vector<DIMENSIONS,int>&                       fineGridPositionOfVertex
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::descend(
  exahype::Cell * const          fineGridCells,
  exahype::Vertex * const        fineGridVertices,
  const peano::grid::VertexEnumerator&                fineGridVerticesEnumerator,
  exahype::Vertex * const        coarseGridVertices,
  const peano::grid::VertexEnumerator&                coarseGridVerticesEnumerator,
  exahype::Cell&                 coarseGridCell
) {
  // do nothing
}


void exahype::mappings::LoadBalancing::ascend(
  exahype::Cell * const    fineGridCells,
  exahype::Vertex * const  fineGridVertices,
  const peano::grid::VertexEnumerator&          fineGridVerticesEnumerator,
  exahype::Vertex * const  coarseGridVertices,
  const peano::grid::VertexEnumerator&          coarseGridVerticesEnumerator,
  exahype::Cell&           coarseGridCell
) {
  // do nothing
}
