#include "matrixfree/solver/BoxMG.h"
#include "matrixfree/solver/tests/BoxMGTest.h"
#include "matrixfree/stencil/StencilFactory.h"
#include "peano/utils/Loop.h"


registerTest(matrixfree::solver::tests::BoxMGTest)


#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",off)
#endif


namespace {
  tarch::la::Vector<2, double> legacySolve2x2System(
    tarch::la::Vector<3, double> stencil1,
    tarch::la::Vector<3, double> stencil2
  ) {
    tarch::la::Vector<2, double> result;

    double invdet = 1.0/(stencil1(1)*stencil2(1) - stencil1(2)*stencil2(0));

          // Test for nan
          assertion2(invdet != std::numeric_limits<double>::infinity(), stencil1, stencil2);
          assertion2(invdet == invdet, stencil1, stencil2);

          result(0) = invdet*(stencil2(1)*(-stencil1(0)) + (-stencil1(2)*(-stencil2(2))));

          result(1) = invdet*((-stencil2(0))*(-stencil1(0)) + stencil1(1)*(-stencil2(2)));

          return result;

  }

  tarch::la::Vector<4, double> legacySolve4x4System(
    tarch::la::Matrix<4, 4, double> a,
    tarch::la::Vector<4, double> rhs
  ) {
    tarch::la::Vector<4, double> result;

    tarch::la::Matrix<4, 4, double> q;
    tarch::la::Matrix<4, 4, double> r;

    //QR decomposition for solving the system (A = Q*R)
    modifiedGramSchmidt(a, q, r);

    tarch::la::Vector<4, double> tempRhs;

    //tempRhs = Q^{t}*rhs
    tempRhs = transpose(q)*rhs;

    // back substitution (x = R^{-1}*tempRhs)
    for(int k = 3; k >= 0; k--){
      for(int i = k+1; i <= 3; i++){
        tempRhs(k) -= r(k, i)*result(i);
      }
      result(k) = tempRhs(k)/r(k, k);
    }

    return result;
  }



  tarch::la::Vector<16, double> legacySolveGammaSystems(
    tarch::la::Vector<3, double> stencil1,
    tarch::la::Vector<3, double> stencil2,
    tarch::la::Vector<3, double> stencil3,
    tarch::la::Vector<3, double> stencil4,
    tarch::la::Vector<3, double> stencil5,
    tarch::la::Vector<3, double> stencil6,
    tarch::la::Vector<3, double> stencil7,
    tarch::la::Vector<3, double> stencil8
  ) {
    tarch::la::Vector<16, double> result;
    tarch::la::Vector<2, double> rtemp;
    tarch::la::Vector<3,double> g1;
    tarch::la::Vector<3,double> g2;

    //bottom edge from c0
    g1 = stencil1;
    g2 = stencil2;
    g2(2) = 0.0;
    rtemp = legacySolve2x2System(g1, g2);
    result(0) = rtemp(0);
    result(1) = rtemp(1);

    //bottom edge from c1
    g1(0) = 0.0;
    g2 = stencil2;
    rtemp = legacySolve2x2System(g1, g2);
    result(2) = rtemp(0);
    result(3) = rtemp(1);

    //left edge from c0
    g1 = stencil3;
    g2 = stencil5;
    g2(2) = 0.0;
    rtemp = legacySolve2x2System(g1, g2);
    result(4) = rtemp(0);
    result(8) = rtemp(1);

    //left edge from c2
    g1(0) = 0.0;
    g2 = stencil5;
    rtemp = legacySolve2x2System(g1, g2);
    result(6) = rtemp(0);
    result(10) = rtemp(1);

    //right edge from c1
    g1 = stencil4;
    g2 = stencil6;
    g2(2) = 0.0;
    rtemp = legacySolve2x2System(g1, g2);
    result(5) = rtemp(0);
    result(9) = rtemp(1);

    //right edge from c3
    g1(0) = 0.0;
    g2 = stencil6;
    rtemp = legacySolve2x2System(g1, g2);
    result(7) = rtemp(0);
    result(11) = rtemp(1);

    //top edge from c2
    g1 = stencil7;
    g2 = stencil8;
    g2(2) = 0.0;
    rtemp = legacySolve2x2System(g1, g2);
    result(12) = rtemp(0);
    result(13) = rtemp(1);

    //top edge from c3
    g1(0) = 0.0;
    g2 = stencil8;
    rtemp = legacySolve2x2System(g1, g2);
    result(14) = rtemp(0);
    result(15) = rtemp(1);

    return result;

  }

  tarch::la::Vector<16, double> legacySolveIotaSystem(
    tarch::la::Vector<9, double> stencil1,
    tarch::la::Vector<9, double> stencil2,
    tarch::la::Vector<9, double> stencil3,
    tarch::la::Vector<9, double> stencil4,
    tarch::la::Vector<16, double> gammas
  ) {
    tarch::la::Vector<16, double> result;
    tarch::la::Vector<4, double> tempresult;

    tarch::la::Matrix<4, 4, double> a;
    tarch::la::Vector<4, double> rhs;

    //from c0
    a(0, 0) = stencil1(4);
    a(1, 0) = stencil1(5);
    a(2, 0) = stencil1(7);
    a(3, 0) = stencil1(8);
    rhs(0) = -(stencil1(0)*1.0 + stencil1(1)*gammas(0) + stencil1(2)*gammas(1) + stencil1(3)* gammas(4) + stencil1(6)*gammas(8));

    a(0, 1) = stencil2(3);
    a(1, 1) = stencil2(4);
    a(2, 1) = stencil2(6);
    a(3, 1) = stencil2(7);
    rhs(1) = -(stencil2(0)*gammas(2) + stencil2(1)*gammas(3) + stencil2(2)*0.0 + stencil2(5)*0.0 + stencil2(8)*0.0);

    a(0, 2) = stencil3(1);
    a(1, 2) = stencil3(2);
    a(2, 2) = stencil3(4);
    a(3, 2) = stencil3(5);
    rhs(2) = -(stencil3(0)*gammas(6) + stencil3(3)*gammas(10) + stencil3(6)*0.0 + stencil3(7)*0.0 + stencil3(8)*0.0);

    a(0, 3) = stencil4(0);
    a(1, 3) = stencil4(1);
    a(2, 3) = stencil4(3);
    a(3, 3) = stencil4(4);
    rhs(3) = -(stencil4(2)*0.0 + stencil4(5)*0.0 + stencil4(6)*0.0 + stencil4(7)*0.0 + stencil4(8)*0.0);

    tempresult = legacySolve4x4System(a, rhs);
    result(0) = tempresult(0);
    result(1) = tempresult(1);
    result(2) = tempresult(2);
    result(3) = tempresult(3);

    //from c1
  //  a(0, 0) = stencil1(4);
  //  a(1, 0) = stencil1(5);
  //  a(2, 0) = stencil1(7);
  //  a(3, 0) = stencil1(8);
    rhs(0) = -(stencil1(0)*0.0 + stencil1(1)*gammas(2) + stencil1(2)*gammas(3) + stencil1(3)*0.0 + stencil1(6)*0.0);

  //  a(0, 1) = stencil2(3);
  //  a(1, 1) = stencil2(4);
  //  a(2, 1) = stencil2(6);
  //  a(3, 1) = stencil2(7);
    rhs(1) = -(stencil2(0)*gammas(2) + stencil2(1)*gammas(3) + stencil2(2)*1.0 + stencil2(5)*gammas(5) + stencil2(8)*gammas(9));

  //  a(0, 2) = stencil3(1);
  //  a(1, 2) = stencil3(2);
  //  a(2, 2) = stencil3(4);
  //  a(3, 2) = stencil3(5);
    rhs(2) = -(stencil3(0)*0.0 + stencil3(3)*0.0 + stencil3(6)*0.0 + stencil3(7)*0.0 + stencil3(8)*0.0);

  //  a(0, 3) = stencil4(0);
  //  a(1, 3) = stencil4(1);
  //  a(2, 3) = stencil4(3);
  //  a(3, 3) = stencil4(4);
    rhs(3) = -(stencil4(2)*gammas(7) + stencil4(5)*gammas(11) + stencil4(6)*0.0 + stencil4(7)*0.0 + stencil4(8)*0.0);

    tempresult = legacySolve4x4System(a, rhs);
    result(4) = tempresult(0);
    result(5) = tempresult(1);
    result(6) = tempresult(2);
    result(7) = tempresult(3);

    //from c2
  //  a(0, 0) = stencil1(4);
  //  a(1, 0) = stencil1(5);
  //  a(2, 0) = stencil1(7);
  //  a(3, 0) = stencil1(8);
    rhs(0) = -(stencil1(0)*0.0 + stencil1(1)*0.0 + stencil1(2)*0.0 + stencil1(3)*gammas(6) + stencil1(6)*gammas(10));

  //  a(0, 1) = stencil2(3);
  //  a(1, 1) = stencil2(4);
  //  a(2, 1) = stencil2(6);
  //  a(3, 1) = stencil2(7);
    rhs(1) = -(stencil2(0)*0.0 + stencil2(1)*0.0 + stencil2(2)*0.0 + stencil2(5)*0.0 + stencil2(8)*0.0);

  //  a(0, 2) = stencil3(1);
  //  a(1, 2) = stencil3(2);
  //  a(2, 2) = stencil3(4);
  //  a(3, 2) = stencil3(5);
    rhs(2) = -(stencil3(0)*gammas(6) + stencil3(3)*gammas(10) + stencil3(6)*1.0 + stencil3(7)*gammas(12) + stencil3(8)*gammas(13));

  //  a(0, 3) = stencil4(0);
  //  a(1, 3) = stencil4(1);
  //  a(2, 3) = stencil4(3);
  //  a(3, 3) = stencil4(4);
    rhs(3) = -(stencil4(2)*0.0 + stencil4(5)*0.0 + stencil4(6)*gammas(12) + stencil4(7)*gammas(13) + stencil4(8)*0.0);

    tempresult = legacySolve4x4System(a, rhs);
    result(8) = tempresult(0);
    result(9) = tempresult(1);
    result(10) = tempresult(2);
    result(11) = tempresult(3);

    //from c3
  //  a(0, 0) = stencil1(4);
  //  a(1, 0) = stencil1(5);
  //  a(2, 0) = stencil1(7);
  //  a(3, 0) = stencil1(8);
    rhs(0) = -(stencil1(0)*0.0 + stencil1(1)*0.0 + stencil1(2)*0.0 + stencil1(3)*0.0 + stencil1(6)*0.0);

  //  a(0, 1) = stencil2(3);
  //  a(1, 1) = stencil2(4);
  //  a(2, 1) = stencil2(6);
  //  a(3, 1) = stencil2(7);
    rhs(1) = -(stencil2(0)*0.0 + stencil2(1)*0.0 + stencil2(2)*0.0 + stencil2(5)*gammas(7) + stencil2(8)*gammas(11));

  //  a(0, 2) = stencil3(1);
  //  a(1, 2) = stencil3(2);
  //  a(2, 2) = stencil3(4);
  //  a(3, 2) = stencil3(5);
    rhs(2) = -(stencil3(0)*0.0 + stencil3(3)*0.0 + stencil3(6)*0.0 + stencil3(7)*gammas(14) + stencil3(8)*gammas(15));

  //  a(0, 3) = stencil4(0);
  //  a(1, 3) = stencil4(1);
  //  a(2, 3) = stencil4(3);
  //  a(3, 3) = stencil4(4);
    rhs(3) = -(stencil4(2)*gammas(7) + stencil4(5)*gammas(11) + stencil4(6)*gammas(14) + stencil4(7)*gammas(15) + stencil4(8)*1.0);

    tempresult = legacySolve4x4System(a, rhs);
    result(12) = tempresult(0);
    result(13) = tempresult(1);
    result(14) = tempresult(2);
    result(15) = tempresult(3);

    return result;

  }


  tarch::la::Vector<9, int> legacyGetPositionsInIntergridTransferOperator(
    const int coarseVertexNumber
  ){
    tarch::la::Vector<9, int> result;

    switch(coarseVertexNumber){
      case 0:
        tarch::la::assignList(result) = 12, 13, 14, 17, 18, 19, 22, 23, 24;
        break;
      case 1:
        tarch::la::assignList(result) = 10, 11, 12, 15, 16, 17, 20, 21, 22;
        break;
      case 2:
        tarch::la::assignList(result) = 2, 3, 4, 7, 8, 9, 12, 13, 14;
        break;
      case 3:
        tarch::la::assignList(result) = 0, 1, 2, 5, 6, 7, 10, 11, 12;
        break;
    }

    return result;
  }


  /**
   * Old routine that maps the 3x3 operators that are computed by BoxMG into
   * the 5x5 operators. In the new implementation, this behaviour is built in.
   */
  tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double> legacyFillInIntergridTransferOperators(
    const tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D,double>&  operators3x3,
    const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&  operators5x5
  ) {
    tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double> result = operators5x5;

    dfor2(c) //Run over coarse vertices of the cell
      tarch::la::Vector<THREE_POWER_D, int> positionsInOperator = legacyGetPositionsInIntergridTransferOperator(cScalar);
      for(int i = 0; i<THREE_POWER_D; i++){
        int vertexIndex = cScalar*FIVE_POWER_D + positionsInOperator(i);
        double value = operators3x3(cScalar*THREE_POWER_D + i);
        result(vertexIndex) = value;
      }
    enddforx

    return result;
  }


  void legacyComputeBoxMG2dPatch(
    tarch::la::Vector<144,double>  stencils,
    tarch::la::Vector<16, double>& iotas,
    tarch::la::Vector<16, double>& gammas
  ) {
    //Compute gamma points (using fine-grid vertex numbering for iota and gamma, and coarse for c)
    // c2 g13 g14 c3
    // g8 i9  i10 i11
    // g4 i5  i6  g7
    // c0 g1  g2  c1

                  //bottom edge (coarse grid points 0 and 1) -> sum over columns
                   tarch::la::Vector<3,double> g1;
                   tarch::la::assignList(g1) = stencils(1*9 + 0) + stencils(1*9 + 3) + stencils(1*9 + 6),
                                               stencils(1*9 + 1) + stencils(1*9 + 4) + stencils(1*9 + 7),
                                               stencils(1*9 + 2) + stencils(1*9 + 5) + stencils(1*9 + 8);

                   tarch::la::Vector<3,double> g2;
                   tarch::la::assignList(g2)= stencils(2*9 + 0) + stencils(2*9 + 3) + stencils(2*9 + 6),
                                              stencils(2*9 + 1) + stencils(2*9 + 4) + stencils(2*9 + 7),
                                              stencils(2*9 + 2) + stencils(2*9 + 5) + stencils(2*9 + 8);

                   //left edge (coarse grid points 0 and 2) -> sum over rows
                   tarch::la::Vector<3,double> g4;
                   tarch::la::assignList(g4)= stencils(4*9 + 0) + stencils(4*9 + 1) + stencils(4*9 + 2),
                                              stencils(4*9 + 3) + stencils(4*9 + 4) + stencils(4*9 + 5),
                                              stencils(4*9 + 6) + stencils(4*9 + 7) + stencils(4*9 + 8);

                   tarch::la::Vector<3,double> g8;
                   tarch::la::assignList(g8)= stencils(8*9 + 0) + stencils(8*9 + 1) + stencils(8*9 + 2),
                                              stencils(8*9 + 3) + stencils(8*9 + 4) + stencils(8*9 + 5),
                                              stencils(8*9 + 6) + stencils(8*9 + 7) + stencils(8*9 + 8);

                   //right edge (coarse grid points 1 and 3) -> sum over rows
                   tarch::la::Vector<3,double> g7;
                   tarch::la::assignList(g7)= stencils(7*9 + 0) + stencils(7*9 + 1) + stencils(7*9 + 2),
                                              stencils(7*9 + 3) + stencils(7*9 + 4) + stencils(7*9 + 5),
                                              stencils(7*9 + 6) + stencils(7*9 + 7) + stencils(7*9 + 8);

                   tarch::la::Vector<3,double> g11;
                   tarch::la::assignList(g11)= stencils(11*9 + 0) + stencils(11*9 + 1) + stencils(11*9 + 2),
                                               stencils(11*9 + 3) + stencils(11*9 + 4) + stencils(11*9 + 5),
                                               stencils(11*9 + 6) + stencils(11*9 + 7) + stencils(11*9 + 8);

            //top edge (coarse grid points 2 and 3) -> sum over columns
             tarch::la::Vector<3,double> g13;
             tarch::la::assignList(g13)= stencils(13*9 + 0) + stencils(13*9 + 3) + stencils(13*9 + 6),
                                         stencils(13*9 + 1) + stencils(13*9 + 4) + stencils(13*9 + 7),
                                         stencils(13*9 + 2) + stencils(13*9 + 5) + stencils(13*9 + 8);

             tarch::la::Vector<3,double> g14;
             tarch::la::assignList(g14)= stencils(14*9 + 0) + stencils(14*9 + 3) + stencils(14*9 + 6),
                                         stencils(14*9 + 1) + stencils(14*9 + 4) + stencils(14*9 + 7),
                                         stencils(14*9 + 2) + stencils(14*9 + 5) + stencils(14*9 + 8);

             //Solve the four equation systems (2 equations with 2 unknowns for every c point)
             gammas = legacySolveGammaSystems(g1, g2, g4, g7, g8, g11, g13, g14);

          //Compute iota points
             tarch::la::Vector<9,double> i5;
             tarch::la::assignList(i5) = stencils(5*9 + 0), stencils(5*9 + 1), stencils(5*9 + 2),
                                                                     stencils(5*9 + 3), stencils(5*9 + 4), stencils(5*9 + 5),
                                                                     stencils(5*9 + 6), stencils(5*9 + 7), stencils(5*9 + 8);

             tarch::la::Vector<9,double> i6;
             tarch::la::assignList(i6) = stencils(6*9 + 0), stencils(6*9 + 1), stencils(6*9 + 2),
                                                                     stencils(6*9 + 3), stencils(6*9 + 4), stencils(6*9 + 5),
                                                                     stencils(6*9 + 6), stencils(6*9 + 7), stencils(6*9 + 8);

             tarch::la::Vector<9,double> i9;
             tarch::la::assignList(i9) = stencils(9*9 + 0), stencils(9*9 + 1), stencils(9*9 + 2),
                                                                     stencils(9*9 + 3), stencils(9*9 + 4), stencils(9*9 + 5),
                                                                     stencils(9*9 + 6), stencils(9*9 + 7), stencils(9*9 + 8);

             tarch::la::Vector<9,double> i10;
             tarch::la::assignList(i10) = stencils(10*9 + 0), stencils(10*9 + 1), stencils(10*9 + 2),
                                                                      stencils(10*9 + 3), stencils(10*9 + 4), stencils(10*9 + 5),
                                                                      stencils(10*9 + 6), stencils(10*9 + 7), stencils(10*9 + 8);

             // Solve equation system (4 equations with 4 unknowns for every c point)
             iotas = legacySolveIotaSystem(i5, i6, i9, i10, gammas);
  }



  /**
   * Marion's original implementation from her thesis that follows directly the Dendy paper
   */
  tarch::la::Vector<4*9,double> legacyComputeBoxMGIntergridTransferOperator(tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double> stencils){

           // For each of the 2^D coarse grid vertices, the 3^D overlap
           // of the 5^D prolongation stencil with the cell is computed
           // from the 3^D stencils of the 4^D fine grid vertices
           tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D,double> result;

           tarch::la::Vector<16, double> iotas;
           tarch::la::Vector<16, double> gammas;

           legacyComputeBoxMG2dPatch(stencils, iotas, gammas);

           // Initialise and set weights to 1.0 for c points
           // We have four c points, and for each a 3x3 overlap of the 5x5 prolongation stencil with the cell
           tarch::la::assignList(result) = 1.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0,

                                           0.0, 0.0, 1.0,
                                           0.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0,

                                           0.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0,
                                           1.0, 0.0, 0.0,

                                           0.0, 0.0, 0.0,
                                           0.0, 0.0, 0.0,
                                           0.0, 0.0, 1.0;


          // Fill in results to result vector

     //bottom edge (from coarse grid point 0 and 1)
     result(0*9 + 1) = gammas(0);
     result(1*9 + 0) = gammas(2);

     result(0*9 + 2) = gammas(1);
     result(1*9 + 1) = gammas(3);

     //left edge (from coarse grid point 0 and 2)
     result(0*9 + 3) = gammas(4);
     result(2*9 + 0) = gammas(6);

     result(0*9 + 6) = gammas(8);
     result(2*9 + 3) = gammas(10);

     //right edge (from coarse grid point 1 and 3)
     result(1*9 + 5) = gammas(5);
     result(3*9 + 2) = gammas(7);

     result(1*9 + 8) = gammas(9);
     result(3*9 + 5) = gammas(11);

     //top edge (from coarse grid point 2 and 3)
     result(2*9 + 7) = gammas(12);
     result(3*9 + 6) = gammas(14);

     result(2*9 + 8) = gammas(13);
     result(3*9 + 7) = gammas(15);

     //from coarse grid point 0
     result(0*9 + 4) = iotas(0);
     result(0*9 + 5) = iotas(1);
     result(0*9 + 7) = iotas(2);
     result(0*9 + 8) = iotas(3);

     //from coarse grid point 1
     result(1*9 + 3) = iotas(4);
     result(1*9 + 4) = iotas(5);
     result(1*9 + 6) = iotas(6);
     result(1*9 + 7) = iotas(7);

     //from coarse grid point 2
     result(2*9 + 1) = iotas(8);
     result(2*9 + 2) = iotas(9);
     result(2*9 + 4) = iotas(10);
     result(2*9 + 5) = iotas(11);

     //from coarse grid point 3
     result(3*9 + 0) = iotas(12);
     result(3*9 + 1) = iotas(13);
     result(3*9 + 3) = iotas(14);
     result(3*9 + 4) = iotas(15);

     // Test for valid values
     for(int i = 0; i<TWO_POWER_D_TIMES_THREE_POWER_D; i++){
       assertion2(result(i) != std::numeric_limits<double>::infinity(), i, result(i));
       assertion2(result(i) == result(i), i, result(i));
     //  std::cout << result(i) << " at  "<< i << "\n";
     }

     return result;
  }
}


matrixfree::solver::tests::BoxMGTest::BoxMGTest():
  tarch::tests::TestCase("matrixfree::solver::tests::BoxMGTest"){
}


matrixfree::solver::tests::BoxMGTest::~BoxMGTest(){
}


void matrixfree::solver::tests::BoxMGTest::run(){
  testMethod(runPoisson);
  testMethod(runPoissonWithSymmetricP);
  testMethod(run2dPoissonComparisonToLegacyCode);
  testMethod(run2dConvectionComparisonToLegacyCode);
  testMethod(run2dConvectionWithSymmetricP);
}


void matrixfree::solver::tests::BoxMGTest::run2dConvectionComparisonToLegacyCode() {
  #ifdef Dim2
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>  stencils;

  int currentEntry = 0;
  for(int i=0; i<FOUR_POWER_D; i++) {
    for (int j=0; j<THREE_POWER_D; j++) {
      stencils(currentEntry) = matrixfree::stencil::getLaplacian(1.0)(j);
      currentEntry++;
    }
    stencils(3) += -1.0;
    stencils(5) +=  1.0;
  }

  tarch::la::Vector<4*9,double>    legacyTmpResult = legacyComputeBoxMGIntergridTransferOperator(stencils);
  tarch::la::Vector<4*25, double>  legacyResult(0.0);
  legacyResult = legacyFillInIntergridTransferOperators(legacyTmpResult, legacyResult );

  dfor2(k)
    tarch::la::Vector<FIVE_POWER_D,double>  prolongationStencil;

    matrixfree::solver::boxmg::computeBoxMGIntergridTransferOperator( stencils, prolongationStencil, k );

    dfor5(j)
      validateNumericalEqualsWithParams3(
        prolongationStencil(jScalar),legacyResult(kScalar* 5*5 + jScalar),
        kScalar,
        prolongationStencil, matrixfree::stencil::getDLinearInterpolation()
      );
    enddforx
  enddforx
  #endif
}


void matrixfree::solver::tests::BoxMGTest::run2dPoissonComparisonToLegacyCode() {
  #ifdef Dim2
/*
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>  stencils;

  int currentEntry = 0;
  for(int i=0; i<FOUR_POWER_D; i++) {
    for (int j=0; j<THREE_POWER_D; j++) {
      stencils(currentEntry) = matrixfree::stencil::getLaplacian(1.0)(j);
      currentEntry++;
    }
  }

  tarch::la::Vector<4*9,double>    legacyTmpResult = legacyComputeBoxMGIntergridTransferOperator(stencils);
  tarch::la::Vector<4*25, double>  legacyResult(0.0);
  legacyResult = legacyFillInIntergridTransferOperators(legacyTmpResult, legacyResult );

  tarch::la::Vector<FIVE_POWER_D,double>  prolongationStencil(0.0);
  dfor2(k)
    matrixfree::solver::boxmg::computeBoxMGIntergridTransferOperator( stencils, prolongationStencil, k );
  enddforx

  dfor2(k)
    dfor5(j)
      validateNumericalEqualsWithParams4(
        prolongationStencil(jScalar),legacyResult(kScalar* 5*5 + jScalar),
        kScalar,
        prolongationStencil, legacyResult,matrixfree::stencil::getDLinearInterpolation()
      );
    enddforx
  enddforx
*/
  #endif
}


void matrixfree::solver::tests::BoxMGTest::runPoisson() {
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>  stencils;

  int currentEntry = 0;
  for(int i=0; i<FOUR_POWER_D; i++) {
    for (int j=0; j<THREE_POWER_D; j++) {
      stencils(currentEntry) = matrixfree::stencil::getLaplacian(1.0)(j);
      currentEntry++;
    }
  }

  tarch::la::Vector<FIVE_POWER_D,double>  prolongationStencil(0.0);

  dfor2(k)
    matrixfree::solver::boxmg::computeBoxMGIntergridTransferOperator( stencils, prolongationStencil, k );
  enddforx

  dfor5(k)
    validateNumericalEqualsWithParams4(
      prolongationStencil(kScalar),matrixfree::stencil::getDLinearInterpolation()(kScalar),
      kScalar, k,
      prolongationStencil, matrixfree::stencil::getDLinearInterpolation()
    );
  enddforx
}


void matrixfree::solver::tests::BoxMGTest::runPoissonWithSymmetricP() {
/*
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>  stencils;

  int currentEntry = 0;
  for(int i=0; i<FOUR_POWER_D; i++) {
    for (int j=0; j<THREE_POWER_D; j++) {
      stencils(currentEntry) = matrixfree::stencil::getLaplacian(1.0)(j);
      currentEntry++;
    }
  }

  tarch::la::Vector<FIVE_POWER_D,double>  prolongationStencil(0.0);

  dfor2(k)
    matrixfree::solver::boxmg::computeSymmetricBoxMGIntergridTransferOperator( stencils, prolongationStencil, k );
  enddforx

  dfor5(k)
    validateNumericalEqualsWithParams3(
      prolongationStencil(kScalar),matrixfree::stencil::getDLinearInterpolation()(kScalar),
      kScalar,
      prolongationStencil, matrixfree::stencil::getDLinearInterpolation()
    );
  enddforx
*/
}


void matrixfree::solver::tests::BoxMGTest::run2dConvectionWithSymmetricP() {
/*
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>  stencils;
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>  symStencils;

  int currentEntry = 0;
  for(int i=0; i<FOUR_POWER_D; i++) {
    for (int j=0; j<THREE_POWER_D; j++) {
      stencils(currentEntry)    = matrixfree::stencil::getLaplacian(1.0)(j);
      symStencils(currentEntry) = matrixfree::stencil::getLaplacian(1.0)(j);
      currentEntry++;
   }
    stencils(3) += -1.0;
    stencils(5) +=  1.0;
  }

  tarch::la::Vector<FIVE_POWER_D,double>  prolongationStencil(0.0);

  dfor2(k)
    matrixfree::solver::boxmg::computeSymmetricBoxMGIntergridTransferOperator( stencils, prolongationStencil, k );
  enddforx

  dfor5(k)
    validateNumericalEqualsWithParams3(
      prolongationStencil(kScalar),matrixfree::stencil::getDLinearInterpolation()(kScalar),
      kScalar,
      prolongationStencil, matrixfree::stencil::getDLinearInterpolation()
    );
  enddforx
*/
}




#ifdef UseTestSpecificCompilerSettings
#pragma optimize("",on)
#endif
