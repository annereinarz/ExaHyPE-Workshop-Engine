#include "multiscalelinkedcell/tests/HangingVertexBookkeeperTest.h"
#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

#include "tarch/la/Vector.h"
#include "peano/utils/Globals.h"

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "tarch/tests/TestCaseFactory.h"
registerTest(multiscalelinkedcell::tests::HangingVertexBookkeeperTest)


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",off)
#endif

 
multiscalelinkedcell::tests::HangingVertexBookkeeperTest::HangingVertexBookkeeperTest():
  tarch::tests::TestCase( "multiscalelinkedcell::HangingVertexBookkeeperTest" ) {
}


multiscalelinkedcell::tests::HangingVertexBookkeeperTest::~HangingVertexBookkeeperTest() {
}


void multiscalelinkedcell::tests::HangingVertexBookkeeperTest::run() {
  testMethod( test0 );
}


void multiscalelinkedcell::tests::HangingVertexBookkeeperTest::test0() {
  #ifdef Dim2
  tarch::la::Vector<DIMENSIONS,double>                  x;
  tarch::la::Vector<DIMENSIONS,int>                     fineGridPositionOfVertex;
  tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>  adjacencyEntries;
  tarch::la::Vector<TWO_POWER_D,int>                    result;

  tarch::la::assignList(adjacencyEntries)         =
    593,  594, 595,  596,
     -3,   -3, 596, 1385,
    595,  596, 597,  598,
    596, 1385, 598, 1385;

  tarch::la::assignList(x)                        = 0.1, 0.0/3.0;
  tarch::la::assignList(fineGridPositionOfVertex) =   3,       0;

  result = HangingVertexBookkeeper::getInstance().createHangingVertex(x,4,fineGridPositionOfVertex,adjacencyEntries);

  validateEquals( result(0),   -3 );
  validateEquals( result(1),   -3 );
  validateEquals( result(2),  596 );
  validateEquals( result(3), 1385 );

  tarch::la::assignList(x)                        = 0.1, 1.0/3.0;
  tarch::la::assignList(fineGridPositionOfVertex) =   3,       1;

  result = HangingVertexBookkeeper::getInstance().createHangingVertex(x,4,fineGridPositionOfVertex,adjacencyEntries);

  validateEquals( result(0),  596 );
  validateEquals( result(1), 1385 );
  validateEquals( result(2),  596 );
  validateEquals( result(3), 1385 );

  tarch::la::assignList(x)                        = 0.1, 2.0/3.0;
  tarch::la::assignList(fineGridPositionOfVertex) =   3,       2;

  result = HangingVertexBookkeeper::getInstance().createHangingVertex(x,4,fineGridPositionOfVertex,adjacencyEntries);

  validateEquals( result(0),  596 );
  validateEquals( result(1), 1385 );
  validateEquals( result(2),  596 );
  validateEquals( result(3), 1385 );

  tarch::la::assignList(x)                        = 0.1, 3.0/3.0;
  tarch::la::assignList(fineGridPositionOfVertex) =   3,       3;

  result = HangingVertexBookkeeper::getInstance().createHangingVertex(x,4,fineGridPositionOfVertex,adjacencyEntries);

  validateEquals( result(0),  596 );
  validateEquals( result(1), 1385 );
  validateEquals( result(2),  598 );
  validateEquals( result(3), 1385 );
  #endif
}



#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",on)
#endif
