#include "multiscalelinkedcell/tests/SAMRToolsTest.h"
#include "multiscalelinkedcell/SAMRTools.h"

#include "tarch/la/Vector.h"
#include "peano/utils/Globals.h"

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "tarch/tests/TestCaseFactory.h"
registerTest(multiscalelinkedcell::tests::SAMRToolsTest)


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",off)
#endif

 
multiscalelinkedcell::tests::SAMRToolsTest::SAMRToolsTest():
  tarch::tests::TestCase( "multiscalelinkedcell::SAMRToolsTest" ) {
}


multiscalelinkedcell::tests::SAMRToolsTest::~SAMRToolsTest() {
}


void multiscalelinkedcell::tests::SAMRToolsTest::run() {
  testMethod( testComputeIterationRangeA0A1 );
  testMethod( testComputeIterationRangeA1A0 );
  testMethod( testComputeIterationRangeB1B0 );
  testMethod( testComputeIterationRangeC0C1 );
  testMethod( testComputeIterationRangeD0D1 );
  testMethod( testComputeIterationRangeB1C1 );
}


void multiscalelinkedcell::tests::SAMRToolsTest::testComputeIterationRangeA0A1() {
  #if Dim2
  tarch::la::Vector<DIMENSIONS, int>  cellDescriptionSize( 6 );
  tarch::la::Vector<DIMENSIONS, int>  haloSize( 1 );

  tarch::la::Vector<DIMENSIONS, int>  relativePositionOfSrcCellToDestCall;

  tarch::la::Vector<DIMENSIONS, int>  cellOffset;
  tarch::la::Vector<DIMENSIONS, int>  range;

  tarch::la::assignList( relativePositionOfSrcCellToDestCall ) = 0,1;
  multiscalelinkedcell::SAMRTools::computeIterationRangeFromRelativePosition(
    relativePositionOfSrcCellToDestCall,
    cellDescriptionSize,
    haloSize,
    cellOffset,
    range
  );
  validateEqualsWithParams2( cellOffset(0), 0, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 1, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 6, cellOffset, range );


  tarch::la::Vector<DIMENSIONS, double>  dxdy( 1.0/3.0/3.0/6.0 );
  tarch::la::Vector<DIMENSIONS, double>  offset;
  tarch::la::Vector<DIMENSIONS, double>  leftBottom;
  tarch::la::Vector<DIMENSIONS, double>  rightTop;

  tarch::la::assignList( offset ) = 0.0, 0.0;

  tarch::la::assignList( leftBottom )  = 1.0/9.0 - 1.0/9.0/6.0, 0.0;
  tarch::la::assignList( rightTop   )  = 1.0/9.0,               1.0/9.0;

  multiscalelinkedcell::SAMRTools::computeIterationRangeFromCellDescriptionOverlap(
    offset,  dxdy,  1, leftBottom,  rightTop,  cellOffset,  range
  );
  validateEqualsWithParams2( cellOffset(0), 6, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 1, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 6, cellOffset, range );
  #endif
}


void multiscalelinkedcell::tests::SAMRToolsTest::testComputeIterationRangeA1A0() {
  #if Dim2
  tarch::la::Vector<DIMENSIONS, int>  cellDescriptionSize( 6 );
  tarch::la::Vector<DIMENSIONS, int>  haloSize( 1 );

  tarch::la::Vector<DIMENSIONS, int>  relativePositionOfSrcCellToDestCall;

  tarch::la::Vector<DIMENSIONS, int>  cellOffset;
  tarch::la::Vector<DIMENSIONS, int>  range;

  tarch::la::assignList( relativePositionOfSrcCellToDestCall ) = 2,1;
  multiscalelinkedcell::SAMRTools::computeIterationRangeFromRelativePosition(
    relativePositionOfSrcCellToDestCall,
    cellDescriptionSize,
    haloSize,
    cellOffset,
    range
  );
  validateEqualsWithParams2( cellOffset(0), 7, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 1, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 6, cellOffset, range );


  tarch::la::Vector<DIMENSIONS, double>  dxdy( 1.0/3.0/3.0/6.0 );
  tarch::la::Vector<DIMENSIONS, double>  offset;
  tarch::la::Vector<DIMENSIONS, double>  leftBottom;
  tarch::la::Vector<DIMENSIONS, double>  rightTop;

  tarch::la::assignList( offset )      = 1.0/9.0, 0.0;

  tarch::la::assignList( leftBottom )  = 1.0/9.0,              0.0;
  tarch::la::assignList( rightTop   )  = 1.0/9.0 +1.0/9.0/6.0, 1.0/9.0;

  multiscalelinkedcell::SAMRTools::computeIterationRangeFromCellDescriptionOverlap(
    offset,  dxdy,  1, leftBottom,  rightTop,  cellOffset,  range
  );
  validateEqualsWithParams2( cellOffset(0), 1, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 1, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 6, cellOffset, range );
  #endif
}


void multiscalelinkedcell::tests::SAMRToolsTest::testComputeIterationRangeB1B0() {
  #if Dim2
  tarch::la::Vector<DIMENSIONS, int>  cellDescriptionSize( 6 );
  tarch::la::Vector<DIMENSIONS, int>  haloSize( 1 );

  tarch::la::Vector<DIMENSIONS, int>  relativePositionOfSrcCellToDestCall;

  tarch::la::Vector<DIMENSIONS, int>  cellOffset;
  tarch::la::Vector<DIMENSIONS, int>  range;

  tarch::la::assignList( relativePositionOfSrcCellToDestCall ) = 2,1;
  multiscalelinkedcell::SAMRTools::computeIterationRangeFromRelativePosition(
    relativePositionOfSrcCellToDestCall,
    cellDescriptionSize,
    haloSize,
    cellOffset,
    range
  );
  validateEqualsWithParams2( cellOffset(0), 7, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 1, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 6, cellOffset, range );


  tarch::la::Vector<DIMENSIONS, double>  dxdy( 1.0/3.0/6.0 );
  tarch::la::Vector<DIMENSIONS, double>  offset;
  tarch::la::Vector<DIMENSIONS, double>  leftBottom;
  tarch::la::Vector<DIMENSIONS, double>  rightTop;

  tarch::la::assignList( offset )      = 1.0/3.0, 0.0;

  tarch::la::assignList( leftBottom )  = 1.0/3.0,              0.0;
  tarch::la::assignList( rightTop   )  = 1.0/3.0 +1.0/9.0/6.0, 1.0/9.0;

  multiscalelinkedcell::SAMRTools::computeIterationRangeFromCellDescriptionOverlap(
    offset,  dxdy,  1, leftBottom,  rightTop,  cellOffset,  range
  );
  validateEqualsWithParams2( cellOffset(0), 1, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 1, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 2, cellOffset, range );
  #endif
}


void multiscalelinkedcell::tests::SAMRToolsTest::testComputeIterationRangeC0C1() {
  #if Dim2
  tarch::la::Vector<DIMENSIONS, int>  cellDescriptionSize( 6 );
  tarch::la::Vector<DIMENSIONS, int>  haloSize( 1 );

  tarch::la::Vector<DIMENSIONS, int>  relativePositionOfSrcCellToDestCall;

  tarch::la::Vector<DIMENSIONS, int>  cellOffset;
  tarch::la::Vector<DIMENSIONS, int>  range;

  tarch::la::assignList( relativePositionOfSrcCellToDestCall ) = 1,0;
  multiscalelinkedcell::SAMRTools::computeIterationRangeFromRelativePosition(
    relativePositionOfSrcCellToDestCall,
    cellDescriptionSize,
    haloSize,
    cellOffset,
    range
  );
  validateEqualsWithParams2( cellOffset(0), 1, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 0, cellOffset, range );
  validateEqualsWithParams2( range(0), 6, cellOffset, range );
  validateEqualsWithParams2( range(1), 1, cellOffset, range );


  tarch::la::Vector<DIMENSIONS, double>  dxdy( 1.0/3.0/6.0 );
  tarch::la::Vector<DIMENSIONS, double>  offset;
  tarch::la::Vector<DIMENSIONS, double>  leftBottom;
  tarch::la::Vector<DIMENSIONS, double>  rightTop;

  tarch::la::assignList( offset )      = 2.0/3.0, 0.0;

  tarch::la::assignList( leftBottom )  = 2.0/3.0, 1.0/3.0 - 1.0/27.0/6.0;
  tarch::la::assignList( rightTop   )  = 2.0/3.0 + 1.0/27.0, 1.0/3.0;

  multiscalelinkedcell::SAMRTools::computeIterationRangeFromCellDescriptionOverlap(
    offset,  dxdy,  1, leftBottom,  rightTop,  cellOffset,  range
  );
  validateEqualsWithParams2( cellOffset(0), 1, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 6, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 1, cellOffset, range );



  tarch::la::assignList( relativePositionOfSrcCellToDestCall ) = 2,0;
  multiscalelinkedcell::SAMRTools::computeIterationRangeFromRelativePosition(
    relativePositionOfSrcCellToDestCall,
    cellDescriptionSize,
    haloSize,
    cellOffset,
    range
  );
  validateEqualsWithParams2( cellOffset(0), 7, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 0, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 1, cellOffset, range );


  tarch::la::assignList( offset )      = 2.0/3.0, 0.0;

  tarch::la::assignList( leftBottom )  = 2.0/3.0 + 1.0/27.0, 1.0/3.0 - 1.0/27.0/6.0;
  tarch::la::assignList( rightTop   )  = 2.0/3.0 + 1.0/27.0 + 1.0/27.0/6.0, 1.0/3.0;

  multiscalelinkedcell::SAMRTools::computeIterationRangeFromCellDescriptionOverlap(
    offset,  dxdy,  1, leftBottom,  rightTop,  cellOffset,  range
  );
  validateEqualsWithParams2( cellOffset(0), 1, cellOffset, range );
  validateEqualsWithParams2( cellOffset(1), 6, cellOffset, range );
  validateEqualsWithParams2( range(0), 1, cellOffset, range );
  validateEqualsWithParams2( range(1), 1, cellOffset, range );
  #endif
}


void multiscalelinkedcell::tests::SAMRToolsTest::testComputeIterationRangeD0D1() {

}


void multiscalelinkedcell::tests::SAMRToolsTest::testComputeIterationRangeB1C1() {

}


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",on)
#endif
