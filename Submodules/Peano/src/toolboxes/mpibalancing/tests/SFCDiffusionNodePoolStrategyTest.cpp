#include "mpibalancing/tests/SFCDiffusionNodePoolStrategyTest.h"


#include "tarch/tests/TestCaseFactory.h"
registerTest(mpibalancing::tests::SFCDiffusionNodePoolStrategyTest)


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",off)
#endif



tarch::logging::Log mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::_log( "mpibalancing::tests::SFCDiffusionNodePoolStrategyTest" );


mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::SFCDiffusionNodePoolStrategyTest():
  tarch::tests::TestCase( "mpibalancing::tests::SFCDiffusionNodePoolStrategyTest" ) {
}


mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::~SFCDiffusionNodePoolStrategyTest() {
}


void mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::run() {
  logTraceIn( "run() ");

  testMethod( testFirstWorkerDelivery );
  testMethod( testPrimaryNodeDeliveryWith4Nodes );
  testMethod( testSecondaryNodeDeliveryWith4Nodes );

  logTraceOut( "run() ");
}


mpibalancing::SFCDiffusionNodePoolStrategy mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::createSetupWith4Nodes() const {
  SFCDiffusionNodePoolStrategy myStrategy(8,4);

  myStrategy._nodes.resize(32);

  for (int i=0; i<8; i++) {
    myStrategy._nodes[i] = SFCDiffusionNodePoolStrategy::NodePoolListEntry( "node-0", myStrategy.isPrimaryMPIRank(i) );
  }
  for (int i=8; i<16; i++) {
    myStrategy._nodes[i] = SFCDiffusionNodePoolStrategy::NodePoolListEntry( "node-1", myStrategy.isPrimaryMPIRank(i) );
  }
  for (int i=16; i<24; i++) {
    myStrategy._nodes[i] = SFCDiffusionNodePoolStrategy::NodePoolListEntry( "node-2", myStrategy.isPrimaryMPIRank(i) );
  }
  for (int i=24; i<32; i++) {
    myStrategy._nodes[i] = SFCDiffusionNodePoolStrategy::NodePoolListEntry( "node-3", myStrategy.isPrimaryMPIRank(i) );
  }

  for (int i=1; i<32; i++) {
    myStrategy.setNodeIdle(i);
  }

  return myStrategy;
}


mpibalancing::SFCDiffusionNodePoolStrategy::RequestQueue mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::createQueueTriggeredByWorkersOfFirstWorkerWith4Nodes() const {
  mpibalancing::SFCDiffusionNodePoolStrategy::RequestQueue result;

  #ifdef Parallel

  tarch::parallel::messages::WorkerRequestMessage message;
  message.setNumberOfRequestedWorkers(8);

  message._senderDestinationRank = 4;
  result.push_back(message);

  message._senderDestinationRank = 6;
  result.push_back(message);

  message._senderDestinationRank = 10;
  result.push_back(message);

  message._senderDestinationRank = 12;
  result.push_back(message);

  message._senderDestinationRank = 16;
  result.push_back(message);

  message._senderDestinationRank = 18;
  result.push_back(message);

  message._senderDestinationRank = 24;
  result.push_back(message);

  message._senderDestinationRank = 26;
  result.push_back(message);

  message._senderDestinationRank = 28;
  result.push_back(message);

  #endif

  return result;
}


void mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::testFirstWorkerDelivery() {
  SFCDiffusionNodePoolStrategy myStrategy = createSetupWith4Nodes();

  validateEquals( myStrategy.getNumberOfIdlePrimaryRanks(), 15 );

  myStrategy.configureForPrimaryRanksDelivery(1);

  validateEqualsWithParams1( myStrategy._numberOfNodesToSkipPerRequestPlusOne,                4, myStrategy._numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed );
  validateEqualsWithParams1( myStrategy._numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed, 1, myStrategy._numberOfNodesToSkipPerRequestPlusOne );

  // will deliver Rank 2
}


void mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::testPrimaryNodeDeliveryWith4Nodes() {
  SFCDiffusionNodePoolStrategy myStrategy = createSetupWith4Nodes();

  validateEquals( myStrategy.getNumberOfIdlePrimaryRanks(), 15 );

  myStrategy.configureForPrimaryRanksDelivery(1);
  myStrategy._nodes[2].activate();

  validateEquals( myStrategy.getNumberOfIdlePrimaryRanks(), 14 );

  myStrategy.configureForPrimaryRanksDelivery(9);

  validateEqualsWithParams1( myStrategy._numberOfNodesToSkipPerRequestPlusOne,                1, myStrategy._numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed );
  validateEqualsWithParams1( myStrategy._numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed, 2, myStrategy._numberOfNodesToSkipPerRequestPlusOne );


  myStrategy._totalNumberOfRanks = 4*8;

  int newWorker = -1;

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,4, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,6, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,10, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,12, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,16, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,18, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,24, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,26, myStrategy.toString() );

  newWorker = myStrategy.reserveNode(2);
  validateEqualsWithParams1( newWorker,28, myStrategy.toString() );


  validateEquals( myStrategy.getNumberOfIdlePrimaryRanks(), 16-1-1-9 );
}


void mpibalancing::tests::SFCDiffusionNodePoolStrategyTest::testSecondaryNodeDeliveryWith4Nodes() {
  SFCDiffusionNodePoolStrategy myStrategy = createSetupWith4Nodes();

  validateEquals( myStrategy.getNumberOfIdlePrimaryRanks(), 15 );

  myStrategy.configureForPrimaryRanksDelivery(1);
  myStrategy._nodes[2].activate();

  myStrategy.configureForPrimaryRanksDelivery(9);
  myStrategy._nodes[ 4].activate();
  myStrategy._nodes[ 6].activate();
  myStrategy._nodes[10].activate();
  myStrategy._nodes[12].activate();
  myStrategy._nodes[16].activate();
  myStrategy._nodes[18].activate();
  myStrategy._nodes[24].activate();
  myStrategy._nodes[26].activate();
  myStrategy._nodes[28].activate();

  myStrategy._totalNumberOfRanks = 4*8;

  validateEquals( myStrategy.getNumberOfIdlePrimaryRanks(), 16-1-1-9 );

  #ifdef Parallel
/*

  SFCDiffusionNodePoolStrategy::RequestQueue originalRequestQueue = createQueueTriggeredByWorkersOfFirstWorkerWith4Nodes();

  myStrategy._nodePoolState = mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolState::DeployingAlsoSecondaryRanksFirstSweep;

  myStrategy.buildUpPriorityMap(originalRequestQueue);
  SFCDiffusionNodePoolStrategy::RequestQueue sortedRequestQueue = myStrategy.sortRequestQueue( originalRequestQueue );

  int selectedMaster = -1;

  selectedMaster = 4;
  validateEqualsWithParams1( sortedRequestQueue.begin()->getSenderRank(),selectedMaster, myStrategy.toString() );
  sortedRequestQueue.pop_front();
  validateEqualsWithParams1( myStrategy.reserveNode(selectedMaster),-1, myStrategy.toString() );

  selectedMaster = 26;
  validateEqualsWithParams1( sortedRequestQueue.begin()->getSenderRank(),selectedMaster, myStrategy.toString() );
  sortedRequestQueue.pop_front();
  validateEqualsWithParams1( myStrategy.reserveNode(selectedMaster),-1, myStrategy.toString() );

  selectedMaster = 28;
  validateEqualsWithParams1( sortedRequestQueue.begin()->getSenderRank(),selectedMaster, myStrategy.toString() );
  sortedRequestQueue.pop_front();
  validateEqualsWithParams1( myStrategy.reserveNode(selectedMaster),-1, myStrategy.toString() );

  selectedMaster = 24;
  validateEqualsWithParams1( sortedRequestQueue.begin()->getSenderRank(),selectedMaster, myStrategy.toString() );
  sortedRequestQueue.pop_front();

  int reservedNode = -1;
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,23, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,22, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,21, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );

  // evtl. neu sortieren?
  sortedRequestQueue = myStrategy.sortRequestQueue( sortedRequestQueue );

  selectedMaster = 16;
  validateEqualsWithParams1( sortedRequestQueue.begin()->getSenderRank(),selectedMaster, myStrategy.toString() );
  sortedRequestQueue.pop_front();

  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,15, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,14, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,13, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
  reservedNode = myStrategy.reserveNode(selectedMaster);
  validateEqualsWithParams1( reservedNode,-1, myStrategy.toString() );
*/
  #endif
}


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",on)
#endif
