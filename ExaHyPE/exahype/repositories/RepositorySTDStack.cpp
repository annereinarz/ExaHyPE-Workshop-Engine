#include "exahype/repositories/RepositorySTDStack.h"

#include "tarch/Assertions.h"
#include "tarch/timing/Watch.h"

#include "tarch/compiler/CompilerSpecificSettings.h"

#include "tarch/parallel/Node.h"
#include "tarch/parallel/NodePool.h"

#ifdef Parallel
#include "peano/parallel/SendReceiveBufferPool.h"
#include "peano/parallel/loadbalancing/Oracle.h"
#endif

#include "peano/datatraversal/autotuning/Oracle.h"

#include "tarch/compiler/CompilerSpecificSettings.h"

#include "peano/performanceanalysis/ScorePMacros.h"

#if !defined(CompilerICC)
#include "peano/grid/Grid.cpph"
#endif

#ifdef USE_ITAC
#include "VT.h"
#endif

tarch::logging::Log exahype::repositories::RepositorySTDStack::_log( "exahype::repositories::RepositorySTDStack" );


exahype::repositories::RepositorySTDStack::RepositorySTDStack(
  peano::geometry::Geometry&                   geometry,
  const tarch::la::Vector<DIMENSIONS,double>&  domainSize,
  const tarch::la::Vector<DIMENSIONS,double>&  computationalDomainOffset
):
  _geometry(geometry),
  _cellStack(),
  _vertexStack(),
  _solverState(),
  _gridWithMeshRefinement(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithMeshRefinementAndPlotTree(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithFinaliseMeshRefinement(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithFinaliseMeshRefinementOrLocalRollback(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithInitialPrediction(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithFusedTimeStep(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithPredictionRerun(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithBroadcastAndDropNeighbourMessages(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithRefinementStatusSpreading(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithPredictionOrLocalRecomputation(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithMergeNeighbours(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithUpdateAndReduce(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithPrediction(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithCorrection(_vertexStack,_cellStack,_geometry,_solverState,domainSize,computationalDomainOffset,_regularGridContainer,_traversalOrderOnTopLevel),

  _repositoryState() {
  logTraceIn( "RepositorySTDStack(...)" );
  _repositoryState.setAction( exahype::records::RepositoryState::Terminate );

  peano::datatraversal::autotuning::Oracle::getInstance().setNumberOfOracles(exahype::records::RepositoryState::NumberOfAdapters);
  #ifdef Parallel
  peano::parallel::loadbalancing::Oracle::getInstance().setNumberOfOracles(exahype::records::RepositoryState::NumberOfAdapters);
  #endif
  
  logTraceOut( "RepositorySTDStack(...)" );
}



exahype::repositories::RepositorySTDStack::RepositorySTDStack(
  peano::geometry::Geometry&                   geometry
):
  _geometry(geometry),
  _cellStack(),
  _vertexStack(),
  _solverState(),
  _gridWithMeshRefinement(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithMeshRefinementAndPlotTree(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithFinaliseMeshRefinement(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithFinaliseMeshRefinementOrLocalRollback(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithInitialPrediction(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithFusedTimeStep(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithPredictionRerun(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithBroadcastAndDropNeighbourMessages(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithRefinementStatusSpreading(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithPredictionOrLocalRecomputation(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithMergeNeighbours(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithUpdateAndReduce(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithPrediction(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),
  _gridWithCorrection(_vertexStack,_cellStack,_geometry,_solverState,_regularGridContainer,_traversalOrderOnTopLevel),

  _repositoryState() {
  logTraceIn( "RepositorySTDStack(Geometry&)" );

  _repositoryState.setAction( exahype::records::RepositoryState::Terminate );
  
  peano::datatraversal::autotuning::Oracle::getInstance().setNumberOfOracles(exahype::records::RepositoryState::NumberOfAdapters);
  #ifdef Parallel
  peano::parallel::loadbalancing::Oracle::getInstance().setNumberOfOracles(exahype::records::RepositoryState::NumberOfAdapters);
  #endif
  
  logTraceOut( "RepositorySTDStack(Geometry&)" );
}
    
   
exahype::repositories::RepositorySTDStack::~RepositorySTDStack() {
  assertionMsg( _repositoryState.getAction() == exahype::records::RepositoryState::Terminate, "terminate() must be called before destroying repository." );
}


void exahype::repositories::RepositorySTDStack::restart(
  const tarch::la::Vector<DIMENSIONS,double>&  domainSize,
  const tarch::la::Vector<DIMENSIONS,double>&  domainOffset,
  int                                          domainLevel,
  const tarch::la::Vector<DIMENSIONS,int>&     positionOfCentralElementWithRespectToCoarserRemoteLevel
) {
  logTraceInWith4Arguments( "restart(...)", domainSize, domainOffset, domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel );
  #ifdef Parallel
  assertion( !tarch::parallel::Node::getInstance().isGlobalMaster());
  #endif
  
  logInfo( "restart(...)", "start node for subdomain " << domainOffset << "x" << domainSize << " on level " << domainLevel << " with master " << tarch::parallel::NodePool::getInstance().getMasterRank() );
  
  assertion( _repositoryState.getAction() == exahype::records::RepositoryState::Terminate );

  _vertexStack.clear();
  _cellStack.clear();

  _gridWithMeshRefinement.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithMeshRefinementAndPlotTree.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithFinaliseMeshRefinement.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithFinaliseMeshRefinementOrLocalRollback.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithInitialPrediction.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithFusedTimeStep.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithPredictionRerun.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithBroadcastAndDropNeighbourMessages.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithRefinementStatusSpreading.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithPredictionOrLocalRecomputation.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithMergeNeighbours.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithUpdateAndReduce.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithPrediction.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);
  _gridWithCorrection.restart(domainSize,domainOffset,domainLevel, positionOfCentralElementWithRespectToCoarserRemoteLevel);


  _solverState.restart();

  logTraceOut( "restart(...)" );
}


void exahype::repositories::RepositorySTDStack::terminate() {
  logTraceIn( "terminate()" );
  
  _repositoryState.setAction( exahype::records::RepositoryState::Terminate );
  
  #ifdef Parallel
  if (tarch::parallel::Node::getInstance().isGlobalMaster()) {
    tarch::parallel::NodePool::getInstance().broadcastToWorkingNodes(
      _repositoryState,
      peano::parallel::SendReceiveBufferPool::getInstance().getIterationManagementTag()
    );
  }
  peano::parallel::SendReceiveBufferPool::getInstance().terminate();
  #endif

  _gridWithMeshRefinement.terminate();
  _gridWithMeshRefinementAndPlotTree.terminate();
  _gridWithFinaliseMeshRefinement.terminate();
  _gridWithFinaliseMeshRefinementOrLocalRollback.terminate();
  _gridWithInitialPrediction.terminate();
  _gridWithFusedTimeStep.terminate();
  _gridWithPredictionRerun.terminate();
  _gridWithBroadcastAndDropNeighbourMessages.terminate();
  _gridWithRefinementStatusSpreading.terminate();
  _gridWithPredictionOrLocalRecomputation.terminate();
  _gridWithMergeNeighbours.terminate();
  _gridWithUpdateAndReduce.terminate();
  _gridWithPrediction.terminate();
  _gridWithCorrection.terminate();


  logTraceOut( "terminate()" );
}


exahype::State& exahype::repositories::RepositorySTDStack::getState() {
  return _solverState;
}


const exahype::State& exahype::repositories::RepositorySTDStack::getState() const {
  return _solverState;
}


void exahype::repositories::RepositorySTDStack::iterate(int numberOfIterations, bool exchangeBoundaryVertices) {
  SCOREP_USER_REGION( (std::string("exahype::repositories::RepositorySTDStack::iterate() - ") + _repositoryState.toString( _repositoryState.getAction() )).c_str(), SCOREP_USER_REGION_TYPE_FUNCTION)

  tarch::timing::Watch watch( "exahype::repositories::RepositorySTDStack", "iterate(bool)", false);
 
  #ifdef Parallel
  if (tarch::parallel::Node::getInstance().isGlobalMaster()) {
    _repositoryState.setNumberOfIterations(numberOfIterations);
    _repositoryState.setExchangeBoundaryVertices(exchangeBoundaryVertices);
    tarch::parallel::NodePool::getInstance().broadcastToWorkingNodes(
      _repositoryState,
      peano::parallel::SendReceiveBufferPool::getInstance().getIterationManagementTag()
    );
  }
  else {
    assertionEquals( numberOfIterations, 1 );
    numberOfIterations = _repositoryState.getNumberOfIterations();
  }
  
  peano::parallel::SendReceiveBufferPool::getInstance().exchangeBoundaryVertices(_repositoryState.getExchangeBoundaryVertices());

  if ( numberOfIterations > 1 && _solverState.isInvolvedInJoinOrFork() ) {
    logWarning( "iterate()", "iterate invoked for multiple traversals though load balancing still does redistribute data" );
  }
  bool switchedLoadBalancingTemporarilyOff = false;
  if ( numberOfIterations > 1 && peano::parallel::loadbalancing::Oracle::getInstance().isLoadBalancingActivated() ) {
    switchedLoadBalancingTemporarilyOff = true;
    peano::parallel::loadbalancing::Oracle::getInstance().activateLoadBalancing(false);
  }

  peano::datatraversal::autotuning::Oracle::getInstance().switchToOracle(_repositoryState.getAction());

  peano::parallel::loadbalancing::Oracle::getInstance().switchToOracle(_repositoryState.getAction());
  #else
  _repositoryState.setNumberOfIterations(numberOfIterations);
  peano::datatraversal::autotuning::Oracle::getInstance().switchToOracle(_repositoryState.getAction());
  #endif
  for (int i=0; i<numberOfIterations; i++) {
    _solverState.setBatchState(numberOfIterations, i );
 
    if (
        tarch::parallel::Node::getInstance().isGlobalMaster() &&
        tarch::parallel::Node::getInstance().getNumberOfNodes()>1
    ) {
      logInfo("iterate(...)","start iteration on global master (includes time spent in broadcast).");
    }

    // NOT GENERATED. Manual modification. Be careful when you rerun the PDT.
    exahype::State::kickOffIteration(_repositoryState,_solverState,i);

    #ifdef USE_ITAC 
    static int handle = 0;
    if ( handle == 0 ) {
      int ierr=VT_funcdef("RepositorySTDStack::iterateAdapter", VT_NOCLASS, &handle ); assertion(ierr==0);
    }
    VT_begin(handle);
    #endif

    switch ( _repositoryState.getAction()) {
      case exahype::records::RepositoryState::UseAdapterMeshRefinement: watch.startTimer(); _gridWithMeshRefinement.iterate(); watch.stopTimer(); _measureMeshRefinementCPUTime.setValue( watch.getCPUTime() ); _measureMeshRefinementCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterMeshRefinementAndPlotTree: watch.startTimer(); _gridWithMeshRefinementAndPlotTree.iterate(); watch.stopTimer(); _measureMeshRefinementAndPlotTreeCPUTime.setValue( watch.getCPUTime() ); _measureMeshRefinementAndPlotTreeCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterFinaliseMeshRefinement: watch.startTimer(); _gridWithFinaliseMeshRefinement.iterate(); watch.stopTimer(); _measureFinaliseMeshRefinementCPUTime.setValue( watch.getCPUTime() ); _measureFinaliseMeshRefinementCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterFinaliseMeshRefinementOrLocalRollback: watch.startTimer(); _gridWithFinaliseMeshRefinementOrLocalRollback.iterate(); watch.stopTimer(); _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.setValue( watch.getCPUTime() ); _measureFinaliseMeshRefinementOrLocalRollbackCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterInitialPrediction: watch.startTimer(); _gridWithInitialPrediction.iterate(); watch.stopTimer(); _measureInitialPredictionCPUTime.setValue( watch.getCPUTime() ); _measureInitialPredictionCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterFusedTimeStep: watch.startTimer(); _gridWithFusedTimeStep.iterate(); watch.stopTimer(); _measureFusedTimeStepCPUTime.setValue( watch.getCPUTime() ); _measureFusedTimeStepCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterPredictionRerun: watch.startTimer(); _gridWithPredictionRerun.iterate(); watch.stopTimer(); _measurePredictionRerunCPUTime.setValue( watch.getCPUTime() ); _measurePredictionRerunCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterBroadcastAndDropNeighbourMessages: watch.startTimer(); _gridWithBroadcastAndDropNeighbourMessages.iterate(); watch.stopTimer(); _measureBroadcastAndDropNeighbourMessagesCPUTime.setValue( watch.getCPUTime() ); _measureBroadcastAndDropNeighbourMessagesCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterRefinementStatusSpreading: watch.startTimer(); _gridWithRefinementStatusSpreading.iterate(); watch.stopTimer(); _measureRefinementStatusSpreadingCPUTime.setValue( watch.getCPUTime() ); _measureRefinementStatusSpreadingCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterPredictionOrLocalRecomputation: watch.startTimer(); _gridWithPredictionOrLocalRecomputation.iterate(); watch.stopTimer(); _measurePredictionOrLocalRecomputationCPUTime.setValue( watch.getCPUTime() ); _measurePredictionOrLocalRecomputationCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterMergeNeighbours: watch.startTimer(); _gridWithMergeNeighbours.iterate(); watch.stopTimer(); _measureMergeNeighboursCPUTime.setValue( watch.getCPUTime() ); _measureMergeNeighboursCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterUpdateAndReduce: watch.startTimer(); _gridWithUpdateAndReduce.iterate(); watch.stopTimer(); _measureUpdateAndReduceCPUTime.setValue( watch.getCPUTime() ); _measureUpdateAndReduceCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterPrediction: watch.startTimer(); _gridWithPrediction.iterate(); watch.stopTimer(); _measurePredictionCPUTime.setValue( watch.getCPUTime() ); _measurePredictionCalendarTime.setValue( watch.getCalendarTime() ); break;
      case exahype::records::RepositoryState::UseAdapterCorrection: watch.startTimer(); _gridWithCorrection.iterate(); watch.stopTimer(); _measureCorrectionCPUTime.setValue( watch.getCPUTime() ); _measureCorrectionCalendarTime.setValue( watch.getCalendarTime() ); break;

      case exahype::records::RepositoryState::Terminate:
        assertionMsg( false, "this branch/state should never be reached" ); 
        break;
      case exahype::records::RepositoryState::NumberOfAdapters:
        assertionMsg( false, "this branch/state should never be reached" ); 
        break;
      case exahype::records::RepositoryState::RunOnAllNodes:
        assertionMsg( false, "this branch/state should never be reached" ); 
        break;
      case exahype::records::RepositoryState::ReadCheckpoint:
        assertionMsg( false, "not implemented yet" );
        break;
      case exahype::records::RepositoryState::WriteCheckpoint:
        assertionMsg( false, "not implemented yet" );
        break;
    }

    #ifdef USE_ITAC 
    VT_end(handle);
    #endif

    if (
        tarch::parallel::Node::getInstance().isGlobalMaster() &&
        tarch::parallel::Node::getInstance().getNumberOfNodes()>1
    ) {
      logInfo("iterate(...)","finish iteration on global master (includes time spent performing reduction).");
      logInfo("iterate(...)","local vertices on global master="<<_cellStack.sizeOfInputStack()  );
      logInfo("iterate(...)","local cells on global master   ="<<_vertexStack.sizeOfInputStack());
    }

    #ifdef Parallel
    if ( switchedLoadBalancingTemporarilyOff && i==numberOfIterations-1) {
      peano::parallel::loadbalancing::Oracle::getInstance().activateLoadBalancing(true);
    }
    #endif
  }
  
  #ifdef Parallel
  if (_solverState.isJoiningWithMaster()) {
    _repositoryState.setAction( exahype::records::RepositoryState::Terminate );
  }
  #endif
}

 void exahype::repositories::RepositorySTDStack::switchToMeshRefinement() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterMeshRefinement); }
 void exahype::repositories::RepositorySTDStack::switchToMeshRefinementAndPlotTree() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterMeshRefinementAndPlotTree); }
 void exahype::repositories::RepositorySTDStack::switchToFinaliseMeshRefinement() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterFinaliseMeshRefinement); }
 void exahype::repositories::RepositorySTDStack::switchToFinaliseMeshRefinementOrLocalRollback() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterFinaliseMeshRefinementOrLocalRollback); }
 void exahype::repositories::RepositorySTDStack::switchToInitialPrediction() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterInitialPrediction); }
 void exahype::repositories::RepositorySTDStack::switchToFusedTimeStep() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterFusedTimeStep); }
 void exahype::repositories::RepositorySTDStack::switchToPredictionRerun() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterPredictionRerun); }
 void exahype::repositories::RepositorySTDStack::switchToBroadcastAndDropNeighbourMessages() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterBroadcastAndDropNeighbourMessages); }
 void exahype::repositories::RepositorySTDStack::switchToRefinementStatusSpreading() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterRefinementStatusSpreading); }
 void exahype::repositories::RepositorySTDStack::switchToPredictionOrLocalRecomputation() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterPredictionOrLocalRecomputation); }
 void exahype::repositories::RepositorySTDStack::switchToMergeNeighbours() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterMergeNeighbours); }
 void exahype::repositories::RepositorySTDStack::switchToUpdateAndReduce() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterUpdateAndReduce); }
 void exahype::repositories::RepositorySTDStack::switchToPrediction() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterPrediction); }
 void exahype::repositories::RepositorySTDStack::switchToCorrection() { _repositoryState.setAction(exahype::records::RepositoryState::UseAdapterCorrection); }



 bool exahype::repositories::RepositorySTDStack::isActiveAdapterMeshRefinement() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterMeshRefinement; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterMeshRefinementAndPlotTree() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterMeshRefinementAndPlotTree; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterFinaliseMeshRefinement() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterFinaliseMeshRefinement; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterFinaliseMeshRefinementOrLocalRollback() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterFinaliseMeshRefinementOrLocalRollback; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterInitialPrediction() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterInitialPrediction; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterFusedTimeStep() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterFusedTimeStep; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterPredictionRerun() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterPredictionRerun; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterBroadcastAndDropNeighbourMessages() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterBroadcastAndDropNeighbourMessages; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterRefinementStatusSpreading() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterRefinementStatusSpreading; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterPredictionOrLocalRecomputation() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterPredictionOrLocalRecomputation; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterMergeNeighbours() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterMergeNeighbours; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterUpdateAndReduce() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterUpdateAndReduce; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterPrediction() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterPrediction; }
 bool exahype::repositories::RepositorySTDStack::isActiveAdapterCorrection() const { return _repositoryState.getAction() == exahype::records::RepositoryState::UseAdapterCorrection; }



peano::grid::Checkpoint<exahype::Vertex, exahype::Cell>* exahype::repositories::RepositorySTDStack::createEmptyCheckpoint() {
  return new peano::grid::Checkpoint<exahype::Vertex, exahype::Cell>();
} 


void exahype::repositories::RepositorySTDStack::writeCheckpoint(peano::grid::Checkpoint<exahype::Vertex, exahype::Cell> * const checkpoint) {
  _solverState.writeToCheckpoint( *checkpoint );
  _vertexStack.writeToCheckpoint( *checkpoint );
  _cellStack.writeToCheckpoint( *checkpoint );
} 


void exahype::repositories::RepositorySTDStack::readCheckpoint( peano::grid::Checkpoint<exahype::Vertex, exahype::Cell> const * const checkpoint ) {
  assertionMsg( checkpoint->isValid(), "checkpoint has to be valid if you call this operation" );

  _solverState.readFromCheckpoint( *checkpoint );
  _vertexStack.readFromCheckpoint( *checkpoint );
  _cellStack.readFromCheckpoint( *checkpoint );
}


void exahype::repositories::RepositorySTDStack::setMaximumMemoryFootprintForTemporaryRegularGrids(double value) {
  _regularGridContainer.setMaximumMemoryFootprintForTemporaryRegularGrids(value);
}


#ifdef Parallel
void exahype::repositories::RepositorySTDStack::runGlobalStep() {
  assertion(tarch::parallel::Node::getInstance().isGlobalMaster());

  exahype::records::RepositoryState intermediateStateForWorkingNodes;
  intermediateStateForWorkingNodes.setAction( exahype::records::RepositoryState::RunOnAllNodes );
  
  tarch::parallel::NodePool::getInstance().broadcastToWorkingNodes(
    intermediateStateForWorkingNodes,
    peano::parallel::SendReceiveBufferPool::getInstance().getIterationManagementTag()
  );
  tarch::parallel::NodePool::getInstance().activateIdleNodes(); // Deadlock with non-blocking communication?
}


exahype::repositories::RepositorySTDStack::ContinueCommand exahype::repositories::RepositorySTDStack::continueToIterate() {
  logTraceIn( "continueToIterate()" );

  assertion( !tarch::parallel::Node::getInstance().isGlobalMaster());

  ContinueCommand result;
  if ( _solverState.hasJoinedWithMaster() ) {
    result = Terminate;
  }
  else {
    int masterNode = tarch::parallel::Node::getInstance().getGlobalMasterRank();
    assertion( masterNode != -1 );

    _repositoryState.receive( masterNode, peano::parallel::SendReceiveBufferPool::getInstance().getIterationManagementTag(), true, records::RepositoryState::ExchangeMode::NonblockingWithPollingLoopOverTests);

    result = Continue;
    if (_repositoryState.getAction()==exahype::records::RepositoryState::Terminate) {
      result = Terminate;
    } 
    if (_repositoryState.getAction()==exahype::records::RepositoryState::RunOnAllNodes) {
      result = RunGlobalStep;
    } 
  }
   
  logTraceOutWith1Argument( "continueToIterate()", result );
  return result;
}
#endif


void exahype::repositories::RepositorySTDStack::logIterationStatistics(bool logAllAdapters) const {
  logInfo( "logIterationStatistics()", "|| adapter name \t || iterations \t || total CPU time [t]=s \t || average CPU time/grid sweep [t]=s \t || total real time [t]=s \t || average real time/grid sweep [t]=s  || CPU time properties  || real time properties " );  
   if (logAllAdapters || _measureMeshRefinementCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| MeshRefinement \t |  " << _measureMeshRefinementCPUTime.getNumberOfMeasurements() << " \t |  " << _measureMeshRefinementCPUTime.getAccumulatedValue() << " \t |  " << _measureMeshRefinementCPUTime.getValue()  << " \t |  " << _measureMeshRefinementCalendarTime.getAccumulatedValue() << " \t |  " << _measureMeshRefinementCalendarTime.getValue() << " \t |  " << _measureMeshRefinementCPUTime.toString() << " \t |  " << _measureMeshRefinementCalendarTime.toString() );
   if (logAllAdapters || _measureMeshRefinementAndPlotTreeCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| MeshRefinementAndPlotTree \t |  " << _measureMeshRefinementAndPlotTreeCPUTime.getNumberOfMeasurements() << " \t |  " << _measureMeshRefinementAndPlotTreeCPUTime.getAccumulatedValue() << " \t |  " << _measureMeshRefinementAndPlotTreeCPUTime.getValue()  << " \t |  " << _measureMeshRefinementAndPlotTreeCalendarTime.getAccumulatedValue() << " \t |  " << _measureMeshRefinementAndPlotTreeCalendarTime.getValue() << " \t |  " << _measureMeshRefinementAndPlotTreeCPUTime.toString() << " \t |  " << _measureMeshRefinementAndPlotTreeCalendarTime.toString() );
   if (logAllAdapters || _measureFinaliseMeshRefinementCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| FinaliseMeshRefinement \t |  " << _measureFinaliseMeshRefinementCPUTime.getNumberOfMeasurements() << " \t |  " << _measureFinaliseMeshRefinementCPUTime.getAccumulatedValue() << " \t |  " << _measureFinaliseMeshRefinementCPUTime.getValue()  << " \t |  " << _measureFinaliseMeshRefinementCalendarTime.getAccumulatedValue() << " \t |  " << _measureFinaliseMeshRefinementCalendarTime.getValue() << " \t |  " << _measureFinaliseMeshRefinementCPUTime.toString() << " \t |  " << _measureFinaliseMeshRefinementCalendarTime.toString() );
   if (logAllAdapters || _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| FinaliseMeshRefinementOrLocalRollback \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.getNumberOfMeasurements() << " \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.getAccumulatedValue() << " \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.getValue()  << " \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCalendarTime.getAccumulatedValue() << " \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCalendarTime.getValue() << " \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.toString() << " \t |  " << _measureFinaliseMeshRefinementOrLocalRollbackCalendarTime.toString() );
   if (logAllAdapters || _measureInitialPredictionCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| InitialPrediction \t |  " << _measureInitialPredictionCPUTime.getNumberOfMeasurements() << " \t |  " << _measureInitialPredictionCPUTime.getAccumulatedValue() << " \t |  " << _measureInitialPredictionCPUTime.getValue()  << " \t |  " << _measureInitialPredictionCalendarTime.getAccumulatedValue() << " \t |  " << _measureInitialPredictionCalendarTime.getValue() << " \t |  " << _measureInitialPredictionCPUTime.toString() << " \t |  " << _measureInitialPredictionCalendarTime.toString() );
   if (logAllAdapters || _measureFusedTimeStepCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| FusedTimeStep \t |  " << _measureFusedTimeStepCPUTime.getNumberOfMeasurements() << " \t |  " << _measureFusedTimeStepCPUTime.getAccumulatedValue() << " \t |  " << _measureFusedTimeStepCPUTime.getValue()  << " \t |  " << _measureFusedTimeStepCalendarTime.getAccumulatedValue() << " \t |  " << _measureFusedTimeStepCalendarTime.getValue() << " \t |  " << _measureFusedTimeStepCPUTime.toString() << " \t |  " << _measureFusedTimeStepCalendarTime.toString() );
   if (logAllAdapters || _measurePredictionRerunCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| PredictionRerun \t |  " << _measurePredictionRerunCPUTime.getNumberOfMeasurements() << " \t |  " << _measurePredictionRerunCPUTime.getAccumulatedValue() << " \t |  " << _measurePredictionRerunCPUTime.getValue()  << " \t |  " << _measurePredictionRerunCalendarTime.getAccumulatedValue() << " \t |  " << _measurePredictionRerunCalendarTime.getValue() << " \t |  " << _measurePredictionRerunCPUTime.toString() << " \t |  " << _measurePredictionRerunCalendarTime.toString() );
   if (logAllAdapters || _measureBroadcastAndDropNeighbourMessagesCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| BroadcastAndDropNeighbourMessages \t |  " << _measureBroadcastAndDropNeighbourMessagesCPUTime.getNumberOfMeasurements() << " \t |  " << _measureBroadcastAndDropNeighbourMessagesCPUTime.getAccumulatedValue() << " \t |  " << _measureBroadcastAndDropNeighbourMessagesCPUTime.getValue()  << " \t |  " << _measureBroadcastAndDropNeighbourMessagesCalendarTime.getAccumulatedValue() << " \t |  " << _measureBroadcastAndDropNeighbourMessagesCalendarTime.getValue() << " \t |  " << _measureBroadcastAndDropNeighbourMessagesCPUTime.toString() << " \t |  " << _measureBroadcastAndDropNeighbourMessagesCalendarTime.toString() );
   if (logAllAdapters || _measureRefinementStatusSpreadingCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| RefinementStatusSpreading \t |  " << _measureRefinementStatusSpreadingCPUTime.getNumberOfMeasurements() << " \t |  " << _measureRefinementStatusSpreadingCPUTime.getAccumulatedValue() << " \t |  " << _measureRefinementStatusSpreadingCPUTime.getValue()  << " \t |  " << _measureRefinementStatusSpreadingCalendarTime.getAccumulatedValue() << " \t |  " << _measureRefinementStatusSpreadingCalendarTime.getValue() << " \t |  " << _measureRefinementStatusSpreadingCPUTime.toString() << " \t |  " << _measureRefinementStatusSpreadingCalendarTime.toString() );
   if (logAllAdapters || _measurePredictionOrLocalRecomputationCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| PredictionOrLocalRecomputation \t |  " << _measurePredictionOrLocalRecomputationCPUTime.getNumberOfMeasurements() << " \t |  " << _measurePredictionOrLocalRecomputationCPUTime.getAccumulatedValue() << " \t |  " << _measurePredictionOrLocalRecomputationCPUTime.getValue()  << " \t |  " << _measurePredictionOrLocalRecomputationCalendarTime.getAccumulatedValue() << " \t |  " << _measurePredictionOrLocalRecomputationCalendarTime.getValue() << " \t |  " << _measurePredictionOrLocalRecomputationCPUTime.toString() << " \t |  " << _measurePredictionOrLocalRecomputationCalendarTime.toString() );
   if (logAllAdapters || _measureMergeNeighboursCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| MergeNeighbours \t |  " << _measureMergeNeighboursCPUTime.getNumberOfMeasurements() << " \t |  " << _measureMergeNeighboursCPUTime.getAccumulatedValue() << " \t |  " << _measureMergeNeighboursCPUTime.getValue()  << " \t |  " << _measureMergeNeighboursCalendarTime.getAccumulatedValue() << " \t |  " << _measureMergeNeighboursCalendarTime.getValue() << " \t |  " << _measureMergeNeighboursCPUTime.toString() << " \t |  " << _measureMergeNeighboursCalendarTime.toString() );
   if (logAllAdapters || _measureUpdateAndReduceCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| UpdateAndReduce \t |  " << _measureUpdateAndReduceCPUTime.getNumberOfMeasurements() << " \t |  " << _measureUpdateAndReduceCPUTime.getAccumulatedValue() << " \t |  " << _measureUpdateAndReduceCPUTime.getValue()  << " \t |  " << _measureUpdateAndReduceCalendarTime.getAccumulatedValue() << " \t |  " << _measureUpdateAndReduceCalendarTime.getValue() << " \t |  " << _measureUpdateAndReduceCPUTime.toString() << " \t |  " << _measureUpdateAndReduceCalendarTime.toString() );
   if (logAllAdapters || _measurePredictionCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| Prediction \t |  " << _measurePredictionCPUTime.getNumberOfMeasurements() << " \t |  " << _measurePredictionCPUTime.getAccumulatedValue() << " \t |  " << _measurePredictionCPUTime.getValue()  << " \t |  " << _measurePredictionCalendarTime.getAccumulatedValue() << " \t |  " << _measurePredictionCalendarTime.getValue() << " \t |  " << _measurePredictionCPUTime.toString() << " \t |  " << _measurePredictionCalendarTime.toString() );
   if (logAllAdapters || _measureCorrectionCPUTime.getNumberOfMeasurements()>0) logInfo( "logIterationStatistics()", "| Correction \t |  " << _measureCorrectionCPUTime.getNumberOfMeasurements() << " \t |  " << _measureCorrectionCPUTime.getAccumulatedValue() << " \t |  " << _measureCorrectionCPUTime.getValue()  << " \t |  " << _measureCorrectionCalendarTime.getAccumulatedValue() << " \t |  " << _measureCorrectionCalendarTime.getValue() << " \t |  " << _measureCorrectionCPUTime.toString() << " \t |  " << _measureCorrectionCalendarTime.toString() );

}


void exahype::repositories::RepositorySTDStack::clearIterationStatistics() {
   _measureMeshRefinementCPUTime.erase();
   _measureMeshRefinementAndPlotTreeCPUTime.erase();
   _measureFinaliseMeshRefinementCPUTime.erase();
   _measureFinaliseMeshRefinementOrLocalRollbackCPUTime.erase();
   _measureInitialPredictionCPUTime.erase();
   _measureFusedTimeStepCPUTime.erase();
   _measurePredictionRerunCPUTime.erase();
   _measureBroadcastAndDropNeighbourMessagesCPUTime.erase();
   _measureRefinementStatusSpreadingCPUTime.erase();
   _measurePredictionOrLocalRecomputationCPUTime.erase();
   _measureMergeNeighboursCPUTime.erase();
   _measureUpdateAndReduceCPUTime.erase();
   _measurePredictionCPUTime.erase();
   _measureCorrectionCPUTime.erase();

   _measureMeshRefinementCalendarTime.erase();
   _measureMeshRefinementAndPlotTreeCalendarTime.erase();
   _measureFinaliseMeshRefinementCalendarTime.erase();
   _measureFinaliseMeshRefinementOrLocalRollbackCalendarTime.erase();
   _measureInitialPredictionCalendarTime.erase();
   _measureFusedTimeStepCalendarTime.erase();
   _measurePredictionRerunCalendarTime.erase();
   _measureBroadcastAndDropNeighbourMessagesCalendarTime.erase();
   _measureRefinementStatusSpreadingCalendarTime.erase();
   _measurePredictionOrLocalRecomputationCalendarTime.erase();
   _measureMergeNeighboursCalendarTime.erase();
   _measureUpdateAndReduceCalendarTime.erase();
   _measurePredictionCalendarTime.erase();
   _measureCorrectionCalendarTime.erase();

}
