#include "matrixfree/solver/Multigrid.h"
#include "MultigridTest.h"

registerTest(matrixfree::solver::tests::MultigridTest)


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",off)
#endif


matrixfree::solver::tests::MultigridTest::MultigridTest():
  tarch::tests::TestCase("matrixfree::solver::tests::MultigridTest"){
}


matrixfree::solver::tests::MultigridTest::~MultigridTest(){
}


void matrixfree::solver::tests::MultigridTest::run(){
  testMethod(testCalculateCellInterGridTransferOperator);
  testMethod(testCalculatePetrovGalerkinCoarseGridOperatorForBilinearInterGridTransferOperators);
  testMethod(testReconstructStencilFragments);
  testMethod(testReconstruction0);
}


void matrixfree::solver::tests::MultigridTest::testCalculateCellInterGridTransferOperator(){
  #ifdef Dim2
  tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>  coarseGridVerticesP;
  coarseGridVerticesP =
       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       3.0/9.0, 6.0/9.0, 9.0/9.0, 6.0/9.0, 3.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,

       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       3.0/9.0, 6.0/9.0, 9.0/9.0, 6.0/9.0, 3.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,

       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       3.0/9.0, 6.0/9.0, 9.0/9.0, 6.0/9.0, 3.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,

       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       3.0/9.0, 6.0/9.0, 9.0/9.0, 6.0/9.0, 3.0/9.0,
       2.0/9.0, 4.0/9.0, 6.0/9.0, 4.0/9.0, 2.0/9.0,
       1.0/9.0, 2.0/9.0, 3.0/9.0, 2.0/9.0, 1.0/9.0;

  tarch::la::Vector<DIMENSIONS,int> fineGridPositionOfCell;
  fineGridPositionOfCell = 2, 2;

  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> cellP;

  Multigrid  multigrid;
  cellP = multigrid.calculateCellInterGridTransferOperator(coarseGridVerticesP, fineGridPositionOfCell);

  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> result;
  tarch::la::assignList(result) =
     1.0/9.0, 2.0/9.0, 2.0/9.0, 4.0/9.0,
     0.0, 3.0/9.0, 0.0, 6.0/9.0,
     0.0, 0.0, 3.0/9.0, 6.0/9.0,
     0.0, 0.0, 0.0, 9.0/9.0;

  validateNumericalVectorEquals(cellP, result);
  #endif
}


void matrixfree::solver::tests::MultigridTest::testCalculatePetrovGalerkinCoarseGridOperatorForBilinearInterGridTransferOperators() {
  #ifdef Dim2
  Multigrid  multigrid;

  matrixfree::stencil::ElementWiseAssemblyMatrix elementAssemblyMatrix;
  elementAssemblyMatrix =
     2.0/3.0, -0.5/3.0, -0.5/3.0, -1.0/3.0,
    -0.5/3.0,  2.0/3.0, -1.0/3.0, -0.5/3.0,
    -0.5/3.0, -1.0/3.0,  2.0/3.0, -0.5/3.0,
    -1.0/3.0, -0.5/3.0, -0.5/3.0,  2.0/3.0;

  tarch::la::Vector<DIMENSIONS,int> fineGridPositionOfCell;
  fineGridPositionOfCell = 2, 2;

  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double>  galerkinOperator =
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );

  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> result;
  result =         0.008230452674897,   0.010288065843621,   0.010288065843621,  -0.028806584362140,
                   0.010288065843621,   0.082304526748971,  -0.028806584362140,  -0.063786008230453,
                   0.010288065843621,  -0.028806584362140,   0.082304526748971,  -0.063786008230453,
                  -0.028806584362140,  -0.063786008230453,  -0.063786008230453,   0.156378600823045;

  validateNumericalVectorEquals(galerkinOperator, result);


  fineGridPositionOfCell = 0,0;
  galerkinOperator =
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 1,0;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 2,0;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 0,1;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 1,1;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 2,1;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 0,2;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 1,2;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  fineGridPositionOfCell = 2,2;
  galerkinOperator = galerkinOperator+
    multigrid.calculatePetrovGalerkinCoarseGridOperator(
      fineGridPositionOfCell,
      elementAssemblyMatrix
    );
  validateNumericalVectorEquals(galerkinOperator, elementAssemblyMatrix);

  #endif
}


void matrixfree::solver::tests::MultigridTest::testReconstructStencilFragments() {
  #ifdef Dim2
  matrixfree::stencil::ElementWiseAssemblyMatrix coarseGridOperator;
  coarseGridOperator =
     2.0/3.0, -0.5/3.0, -0.5/3.0, -1.0/3.0,
    -0.5/3.0,  2.0/3.0, -1.0/3.0, -0.5/3.0,
    -0.5/3.0, -1.0/3.0,  2.0/3.0, -0.5/3.0,
    -1.0/3.0, -0.5/3.0, -0.5/3.0,  2.0/3.0;

  const tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D, double> coarseGridStencilFragments =
    matrixfree::stencil::reconstructStencilFragments(coarseGridOperator);

  tarch::la::Vector<THREE_POWER_D,double> result;

  result = 0.0, 0.0, 0.0, 0.0, 2.0/3.0, -0.5/3.0, 0.0, -0.5/3.0, -1.0/3.0;
  validateNumericalVectorEquals( tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 0*THREE_POWER_D), result  );

  result = 0.0, 0.0, 0.0, -0.5/3.0, 2.0/3.0, 0.0, -1.0/3.0, -0.5/3.0, 0.0;
  validateNumericalVectorEquals( tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 1*THREE_POWER_D), result  );

  result = 0.0, -0.5/3.0, -1.0/3.0, 0.0, 2.0/3.0, -0.5/3.0, 0.0, 0.0, 0.0;
  validateNumericalVectorEquals( tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 2*THREE_POWER_D), result  );

  result = -1.0/3.0, -0.5/3.0, 0.0, -0.5/3.0, 2.0/3.0, 0.0, 0.0, 0.0, 0.0;
  validateNumericalVectorEquals( tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 3*THREE_POWER_D), result  );
  #endif
}


void matrixfree::solver::tests::MultigridTest::testReconstruction0() {
  #ifdef Dim2
  matrixfree::stencil::ElementWiseAssemblyMatrix  A;
  A =
     2.0/3.0, -0.5/3.0, -0.5/3.0, -1.0/3.0,
    -0.5/3.0,  2.0/3.0, -1.0/3.0, -0.5/3.0,
    -0.5/3.0, -1.0/3.0,  2.0/3.0, -0.5/3.0,
    -1.0/3.0, -0.5/3.0, -0.5/3.0,  2.0/3.0;

  tarch::la::Vector<DIMENSIONS,int> fineGridPositionOfCell;
  fineGridPositionOfCell = 1, 0;

  Multigrid  multigrid;
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> coarseGridOperator =
    multigrid.calculatePetrovGalerkinCoarseGridOperator( fineGridPositionOfCell, A );
  tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D, double> coarseGridStencilFragments =
    matrixfree::stencil::reconstructStencilFragments(coarseGridOperator);

  tarch::la::Vector<THREE_POWER_D,double> result;
  tarch::la::Vector<THREE_POWER_D,double> valid;

  result = tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 1*THREE_POWER_D);
  valid  = 0,0,0,-0.0514403,0.106996,0,-0.0411523,-0.0144033,0;
  validateWithParams3( tarch::la::equals(result, valid,1e-4), result, valid, coarseGridStencilFragments );


  result = 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0;


  dfor3(fineGridPositionOfCell)
    coarseGridOperator         = multigrid.calculatePetrovGalerkinCoarseGridOperator( fineGridPositionOfCell, A );
    coarseGridStencilFragments = matrixfree::stencil::reconstructStencilFragments(coarseGridOperator);
    result += tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 0*THREE_POWER_D);
    result += tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 1*THREE_POWER_D);
    result += tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 2*THREE_POWER_D);
    result += tarch::la::slice<THREE_POWER_D>( coarseGridStencilFragments, 3*THREE_POWER_D);
  enddforx

  valid  = -0.333333,-0.333333,-0.333333,-0.333333,2.66667,-0.333333,-0.333333,-0.333333,-0.333333;
  validateWithParams3( tarch::la::equals(result, valid,1e-4), result, valid, coarseGridStencilFragments );
  #endif
}


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",on)
#endif
