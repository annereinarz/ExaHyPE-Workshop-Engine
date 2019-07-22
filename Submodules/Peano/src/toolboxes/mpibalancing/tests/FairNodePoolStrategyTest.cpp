#include "mpibalancing/tests/FairNodePoolStrategyTest.h"
#include "mpibalancing/FairNodePoolStrategy.h"


#include "tarch/tests/TestCaseFactory.h"
registerTest(mpibalancing::tests::FairNodePoolStrategyTest)


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",off)
#endif



tarch::logging::Log mpibalancing::tests::FairNodePoolStrategyTest::_log( "mpibalancing::tests::FairNodePoolStrategyTest" );


mpibalancing::tests::FairNodePoolStrategyTest::FairNodePoolStrategyTest():
  tarch::tests::TestCase( "mpibalancing::tests::FairNodePoolStrategyTest" ) {
}


mpibalancing::tests::FairNodePoolStrategyTest::~FairNodePoolStrategyTest() {
}


void mpibalancing::tests::FairNodePoolStrategyTest::run() {
  logTraceIn( "run() ");
  testMethod( testDefaultConfiguration );
  testMethod( testSandyBridgeConfiguration );
  logTraceOut( "run() ");
}


void mpibalancing::tests::FairNodePoolStrategyTest::testDefaultConfiguration() {
  #ifdef Parallel
/*
  mpibalancing::FairNodePoolStrategy testStrategy;

  tarch::parallel::messages::RegisterAtNodePoolMessage registrationMessage;

  for (int i=1; i<5*6; i++) {
    registrationMessage._senderDestinationRank = i;
    testStrategy.addNode( registrationMessage );
    testStrategy.setNodeIdle(i);
  }

  for (int i=1; i<5*6; i++) {
    validateWithParams1( testStrategy.hasIdleNode(0), testStrategy.toString() );
    validate( testStrategy.hasIdleNode(tarch::parallel::NodePoolStrategy::AnyMaster) );
    validateEquals( testStrategy.reserveNode(0), i );
  }

  validate( !testStrategy.hasIdleNode(0) );
  validate( !testStrategy.hasIdleNode(tarch::parallel::NodePoolStrategy::AnyMaster) );
*/
  #endif
}


void mpibalancing::tests::FairNodePoolStrategyTest::testSandyBridgeConfiguration() {
  #ifdef Parallel
/*
  mpibalancing::FairNodePoolStrategy testStrategy(6);

  tarch::parallel::messages::RegisterAtNodePoolMessage registrationMessage;

  for (int i=1; i<5*6; i++) {
    registrationMessage._senderDestinationRank = i;
    testStrategy.addNode( registrationMessage );
    testStrategy.setNodeIdle(i);
  }

  const int validResults[] = {
     6, 12, 18, 24,
     7, 13, 19, 25,
     8, 14, 20, 26,
     9, 15, 21, 27,
    10, 16, 22, 28,
    11, 17, 23, 29
  };

  for (int i=0; i<24; i++) {
    validateWithParams1( testStrategy.hasIdleNode(0), testStrategy.toString() );
    const int newRank = testStrategy.reserveNode(0);
    validateEqualsWithParams2( newRank, validResults[i], i, testStrategy.toString() );
  }
  validateWithParams1( !testStrategy.hasIdleNode(0), testStrategy.toString() );
  validateWithParams1(  testStrategy.hasIdleNode(tarch::parallel::NodePoolStrategy::AnyMaster), testStrategy.toString() );
*/
  #endif
}

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",on)
#endif
