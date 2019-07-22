/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/

#include "exahype/tests/kernels/c/GenericEulerKernelTest.h"

#include <cstring>

#include "../testdata/generic_euler_testdata.h"
#include "kernels/aderdg/generic/Kernels.h"


#if DIMENSIONS == 2

namespace exahype {
namespace tests {
namespace c {

// 2.5D Euler

void GenericEulerKernelTest::flux(const double *Q, double **F) {
  double *f = F[0];
  double *g = F[1];

  const double GAMMA = 1.4;

  const double irho = 1.0 / Q[0];
  const double p =
      (GAMMA - 1) * (Q[4] - 0.5 * (Q[1] * Q[1] + Q[2] * Q[2]) * irho);

  f[0] = Q[1];
  f[1] = irho * Q[1] * Q[1] + p;
  f[2] = irho * Q[1] * Q[2];
  f[3] = irho * Q[1] * Q[3];
  f[4] = irho * Q[1] * (Q[4] + p);

  g[0] = Q[2];
  g[1] = irho * Q[2] * Q[1];
  g[2] = irho * Q[2] * Q[2] + p;
  g[3] = irho * Q[2] * Q[3];
  g[4] = irho * Q[2] * (Q[4] + p);
}

void GenericEulerKernelTest::viscousFlux(const double *Q, double* gradQ, double **F) {}

void GenericEulerKernelTest::algebraicSource(const tarch::la::Vector<DIMENSIONS, double>& x, double t, const double *const Q, double *S) {
  S[0] = 0.0;
  S[1] = 0.0;
  S[2] = 0.0;
  S[3] = 0.0;
  S[4] = 0.0;
}

void GenericEulerKernelTest::eigenvalues(const double *const Q,
                                             const int directionIndex,
                                             double *lambda) {
  const double GAMMA = 1.4;

  double irho = 1.0 / Q[0];
  double p = (GAMMA - 1) * (Q[4] - 0.5 * (Q[1] * Q[1] + Q[2] * Q[2]) * irho);

  double u_n = Q[directionIndex + 1] * irho;
  double c = std::sqrt(GAMMA * p * irho);

  lambda[0] = u_n - c;
  lambda[1] = u_n;
  lambda[2] = u_n;
  lambda[3] = u_n;
  lambda[4] = u_n + c;
}

void GenericEulerKernelTest::nonConservativeProduct(const double *const Q,
                                     const double *const gradQ,
                                     double *BgradQ) {
  // Arbitrary BS

  // Sven: I have no clue what these data shall do, but
  // ensure len(BgradQ)==len(Q).

  BgradQ[0] = 0;
  BgradQ[1] = 0;
  BgradQ[2] = 0;
  BgradQ[3] = 0;
  BgradQ[4] = 0;

  // Q[5]
  // gradQ[2][5]
  // BgradQ[2][5]
  if (!_setNcpAndMatrixBToZero) {
    BgradQ[0] = Q[0];
    BgradQ[1] = Q[3];
    BgradQ[2] = 3.0;
    BgradQ[3] = gradQ[0];
    BgradQ[4] = 0.7;
  }
}  // ncp

void GenericEulerKernelTest::coefficientMatrix(const double *const Q,
                                         const int direction, double *Bn) {
  std::fill_n(Bn, 5*5, 0.0);

//  if (!_setNcpAndMatrixBToZero) {
//    // 3D compressible Euler equations
//    double *B1 = new double[5 * 5];
//    double *B2 = new double[5 * 5];
//
//    std::memset(B1, 0, 5 * 5 * sizeof(double));
//    std::memset(B2, 0, 5 * 5 * sizeof(double));
//    // Bn = B1 if direction == 0
//    //      B2 if direction == 1
//    std::memcpy(Bn, (direction == 0) ? B1 : B2, 5 * 5 * sizeof(double));
//
//    delete[] B1;
//    delete[] B2;
//  }
}  // matrixb

void GenericEulerKernelTest::testPDEFluxes() {
  logInfo( "testPDEFluxes()", "Test PDE-related functions, DIM=2" );

  double Q[5] = {1., 0.1, 0.2, 0.3, 3.5};  // pressure = 1.39
  double f[5], g[5];
  double *F[2] = {f, g};

  flux(Q, F);

  for (int i = 0; i < 5; i++) {
    validateNumericalEqualsWithParams1(
        f[i], ::exahype::tests::testdata::generic_euler::testPDEFluxes::f[i],
        i);
  }

  for (int i = 0; i < 5; i++) {
    validateNumericalEqualsWithParams1(
        g[i], ::exahype::tests::testdata::generic_euler::testPDEFluxes::g[i],
        i);
  }

}  // testPDEFluxes

/**
 * Only luh considers parameters.
 * We thus make the test data strides fit.
 *
 * lduh does not consider parameters.
 */
void GenericEulerKernelTest::testSolutionUpdate() {
  logInfo( "testSolutionUpdate()",  "Test solution update, ORDER=2, DIM=2" );

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 = basisSize*basisSize;

  // inputs:
  double luh[nData*basisSize2] = {0.0};
  for (int i = 0; i < nData*basisSize2; i += nData) {
    luh[i] = 1.0;
    luh[i + 4] = 2.5;
  }

  const double dt = 1.40831757919882352703e-03;
  // ::exahype::tests::testdata::generic_euler::GenericEulerKernelTest::lduh[80]

  kernels::aderdg::generic::c::solutionUpdate<GenericEulerKernelTest>(*this,
      luh, luh, ::exahype::tests::testdata::generic_euler::testSolutionUpdate::lduh,
      dt
      );

  for (int i = 0; i < basisSize2; i++) {
    for (int m = 0; m < nVar; m++) {
      int i_luh          = i*nData + m;
      int i_luh_testdata = i*nVar + m;

      validateNumericalEqualsWithEpsWithParams1(
          luh[i_luh],
          ::exahype::tests::testdata::generic_euler::testSolutionUpdate::luh[i_luh_testdata],
           eps, i);
    }
  }
}  // testSolutionUpdate

void GenericEulerKernelTest::testSurfaceIntegralLinear() {
  logInfo( "testSurfaceIntegralLinear()", "Test surface integral linear, ORDER=2, DIM=2" );

  constexpr int nVar       = NumberOfVariables;
  constexpr int basisSize  = (Order+1);

  {  // test 1
    // inputs:
    const double dx[2] = {0.1, 0.1};  // mesh spacing
    double lFhbnd[4 * nVar * basisSize] = {0.0};    // nVar * nDofY * 4, zero initialized

    double *FLeft  = lFhbnd + 0;
    double *FRight = lFhbnd + 20;
    double *FFront = lFhbnd + 40;
    double *FBack  = lFhbnd + 60;

    for (int i = 0; i < 20; i += 5) {
      // in x orientation 1
      FLeft[i + 1] = 1.;
      FRight[i + 1] = 1.;
      // in y orientation 1
      FFront[i + 2] = 1.;
      FBack[i + 2] = 1.;
    }

    // input:
    // ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in
    double lduh[80] = {0.0};
    std::memcpy(
        lduh,
        ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in,
        80 * sizeof(double));

    // lFhbnd = [ FLeft | FRight | FFront | FBack ]
    kernels::aderdg::generic::c::surfaceIntegralLinear<nVar,basisSize>(
        lduh, lFhbnd, dx[0]
        );

    for (int i = 0; i < 80; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          lduh[i], ::exahype::tests::testdata::generic_euler::
                       testSurfaceIntegralLinear::lduh_out_1[i],
          eps, i);
    }
  }  // test 1

  {  // test 2
    // inputs:
    const double dx[2] = {0.1, 0.1};  // mesh spacing
    // ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lFhbnd_in
    // ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in
    double lduh[80] = {0.0};
    std::memcpy(
        lduh,
        ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in,
        80 * sizeof(double));

    // lFhbnd = [ FLeft | FRight | FFront | FBack ]
    kernels::aderdg::generic::c::surfaceIntegralLinear<nVar, basisSize>(
        lduh, ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::
                  lFhbnd_in,
        dx[0]
        );

    for (int i = 0; i < 80; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          lduh[i], ::exahype::tests::testdata::generic_euler::
                       testSurfaceIntegralLinear::lduh_out_2[i],
          eps, i);
    }
  }  // test 2

}  // testSurfaceIntegralLinear

void GenericEulerKernelTest::testSurfaceIntegralNonlinear() {
  logInfo( "testSurfaceIntegralNonlinear()", "Test surface integral nonlinear, ORDER=2, DIM=2" );

  constexpr int nVar       = NumberOfVariables;
  constexpr int basisSize  = (Order+1);

  {  // test 1
    // inputs:
    const double dx[2] = {0.1, 0.1};  // mesh spacing
    double lFhbnd[4 * nVar*basisSize] = {0.0};
    double *FLeft  = lFhbnd + 0;
    double *FRight = lFhbnd + 20;
    double *FFront = lFhbnd + 40;
    double *FBack  = lFhbnd + 60;

    for (int i = 0; i < 20; i += 5) {
      // in x orientation 1
      FLeft[i + 1] = 1.;
      FRight[i + 1] = 1.;
      // in y orientation 1
      FFront[i + 2] = 1.;
      FBack[i + 2] = 1.;
    }

    // input:
    // ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in
    double lduh[80] = {0.0};
    std::memcpy(
        lduh,
        ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in,
        80 * sizeof(double));

    // lFhbnd = [ FLeft | FRight | FFront | FBack ]
    kernels::aderdg::generic::c::surfaceIntegralNonlinear<nVar, basisSize>(
        lduh, lFhbnd, dx[0]
        );

    for (int i = 0; i < 80; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          lduh[i], ::exahype::tests::testdata::generic_euler::
                       testSurfaceIntegralNonlinear::lduh_1[i],
          eps, i);
    }
  }  // test 1

  {  // test 2
    // inputs:
    const double dx[2] = {0.1, 0.1};  // mesh spacing

    // ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in
    // ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lFhbnd_in

    double lduh[80] = {0.0};
    std::memcpy(
        lduh,
        ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::lduh_in,
        80 * sizeof(double));

    kernels::aderdg::generic::c::surfaceIntegralNonlinear<nVar, basisSize>(
        lduh, ::exahype::tests::testdata::generic_euler::testSurfaceIntegral::
                  lFhbnd_in,
        dx[0]
        );

    for (int i = 0; i < 80; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          lduh[i], ::exahype::tests::testdata::generic_euler::
                       testSurfaceIntegralNonlinear::lduh_2[i],
          eps, i);
    }
  }  // test2
}  // testSurfaceIntegralNonlinear

/**
 * We need to consider material parameters
 * in QL and QR.
 * We don't need to consider material parameters
 * in FL and FR.
 */
 //broken, need new linear PDE signature

void GenericEulerKernelTest::testRiemannSolverLinear() {
  // Rusanov
  logInfo( "testRiemannSolverLinear()", "Test Riemann Solver linear (Rusanov), ORDER=2, DIM=2" );
 /*
  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);

  {
    // test direction = 0
    // output:
    double FL[nVar*basisSize] = {0.0};  // nVar * nDOF(2)
    double FR[nVar*basisSize] = {0.0};  // nVar * nDOF(2)

    // input
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR
    const int direction = 0;
    const double dt = 0.1;

    // Adapt the striding of test data to the parameters
    double QL[nData*basisSize] = {0.0};  // nData * nDOF(2)
    double QR[nData*basisSize] = {0.0};  // nData * nDOF(2)
    for (int i = 0; i < basisSize; i++) {
      for (int m=0; m < nVar; m++) {
        const int i_Qbnd          = i*nData + m;
        const int i_Qbnd_testdata = i*nVar  + m;

        QL[i_Qbnd]=
            ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL[i_Qbnd_testdata];
        QR[i_Qbnd]=
            ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR[i_Qbnd_testdata];
      }
    }

    kernels::aderdg::generic::c::riemannSolverLinear<false,false,false,GenericEulerKernelTest>(
        *this,
        FL, FR,
        QL,
        QR,
        0.0,
        dt,
        direction);

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FL[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverLinear::FL_1[i],
          eps, i);
    }

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FR[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverLinear::FR_1[i],
          eps, i);
    }
  }  // end direction = 0

  { // test direction = 1
    // output:
    double FL[nVar*basisSize] = {0.0};  // nVar * nDOF(2)
    double FR[nVar*basisSize] = {0.0};  // nVar * nDOF(2)

    // input:
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR
    const int direction = 1;
    const double dt = 0.1;

    // Adapt the striding of test data to the parameters
    double QL[nData*basisSize] = {0.0};  // nData * nDOF(2)
    double QR[nData*basisSize] = {0.0};  // nData * nDOF(2)
    for (int i = 0; i < basisSize; i++) {
      for (int m=0; m < nVar; m++) {
        const int i_Qbnd          = i*nData + m;
        const int i_Qbnd_testdata = i*nVar  + m;

        QL[i_Qbnd]=
            ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL[i_Qbnd_testdata];
        QR[i_Qbnd]=
            ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR[i_Qbnd_testdata];
      }
    }

    kernels::aderdg::generic::c::riemannSolverLinear<false,false,false,GenericEulerKernelTest>(
        *this,
        FL, FR,
        QL,
        QR,
        0.0, 
        dt,
        direction
    );

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FL[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverLinear::FL_2[i],
          eps, i);
    }

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FR[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverLinear::FR_2[i],
          eps, i);
    }
  }  // end direction = 1
*/
}  // testRiemannSolverLinear

/**
 * We need to consider material parameters
 * in QL and QR.
 * We don't need to consider material parameters
 * in FL and FR.
 */
void GenericEulerKernelTest::testRiemannSolverNonlinear() {
  // Rusanov
  logInfo( "testRiemannSolverNonlinear()", "Test Riemann Solver nonlinear (Rusanov), ORDER=2, DIM=2" );

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);

  {  // test 1
     // input:
    double QL_raw[nVar*basisSize] = {1., 0., 0., 0., 2.5, 1., 0., 0., 0., 2.5,
                     1., 0., 0., 0., 2.5, 1., 0., 0., 0., 2.5}; // no parameters in this field
    double QR_raw[nVar*basisSize] = {1., 0., 0., 0., 2.5, 1., 0., 0., 0., 2.5,
                     1., 0., 0., 0., 2.5, 1., 0., 0., 0., 2.5};
    double QL[nData*basisSize] = {0.0};  // nData * nDOF(2)
    double QR[nData*basisSize] = {0.0};  // nData * nDOF(2)

    // Update striding according to number of parameters
    for (int i = 0; i < basisSize; i++) {
      for (int m=0; m < nVar; m++) {
        const int i_Qbnd          = i*nData + m;
        const int i_Qbnd_testdata = i*nVar  + m;

        QL[i_Qbnd]= QL_raw[i_Qbnd_testdata];
        QR[i_Qbnd]= QR_raw[i_Qbnd_testdata];
      }
    }

    // inout:
    double FL[nVar*basisSize] = {0.0}; // ~nVar
    double FR[nVar*basisSize] = {0.0};

    kernels::aderdg::generic::c::riemannSolverNonlinear<false,false,GenericEulerKernelTest>(
        *this,
        FL, FR, QL, QR,
        0,   // t
        0.0, // dt
        tarch::la::Vector<DIMENSIONS, double>(0.5, 0.5), // dx
        0    // direction
        );

    // FL == FR, element by element
    for (int i = 0; i < nVar*basisSize; i++) {
      validateEquals(FL[i], FR[i]);
    }
  }  // test 1

  {  // test 2 nnz = 0
     // input:
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR
    double QL[nData*basisSize] = {0.0};  // nData * nDOF(2)
    double QR[nData*basisSize] = {0.0};  // nData * nDOF(2)
    // Update striding according to number of parameters
    for (int i = 0; i < basisSize; i++) {
      for (int m=0; m < nVar; m++) {
        const int i_Qbnd          = i*nData + m;
        const int i_Qbnd_testdata = i*nVar  + m;

        QL[i_Qbnd]=  ::exahype::tests::testdata::generic_euler::
            testRiemannSolverNonlinear::QL_1_in[i_Qbnd_testdata];
        QR[i_Qbnd]= ::exahype::tests::testdata::generic_euler::
            testRiemannSolverNonlinear::QR_1_in[i_Qbnd_testdata];
      }
    }

    // output:
    double FL[nVar*basisSize] = {0.0};
    double FR[nVar*basisSize] = {0.0};
    std::memcpy(FL, ::exahype::tests::testdata::generic_euler::
                        testRiemannSolverNonlinear::FL_1_in,
                20 * sizeof(double));
    std::memcpy(FR, ::exahype::tests::testdata::generic_euler::
                        testRiemannSolverNonlinear::FR_1_in,
                20 * sizeof(double));

    kernels::aderdg::generic::c::riemannSolverNonlinear<false,false,GenericEulerKernelTest>(
        *this,
        FL, FR, QL, QR,
        0,     // t
        0.0,  // dt
        tarch::la::Vector<DIMENSIONS, double>(0.5, 0.5), // dx
        0     // direction
        );

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FL[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverNonlinear::F_1_out[i],
          eps, i);
    }

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FR[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverNonlinear::F_1_out[i],
          eps, i);
    }
  }  // test 2

  {  // test 3 nnz = 1
     // input:
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL
    // ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR
    double QL[nData*basisSize];  // nData * nDOF(2)
    double QR[nData*basisSize];  // nData * nDOF(2)
    // Update striding according to number of parameters
    for (int i = 0; i < basisSize; i++) {
      for (int m=0; m < nVar; m++) {
        const int i_Qbnd          = i*nData + m;
        const int i_Qbnd_testdata = i*nVar  + m;

        QL[i_Qbnd]=  ::exahype::tests::testdata::generic_euler::
            testRiemannSolverNonlinear::QL_2_in[i_Qbnd_testdata];
        QR[i_Qbnd]= ::exahype::tests::testdata::generic_euler::
            testRiemannSolverNonlinear::QR_2_in[i_Qbnd_testdata];
      }
    }

    // output:
    double FL[nVar*basisSize] = {0.0};
    double FR[nVar*basisSize] = {0.0};
    std::memcpy(FL, ::exahype::tests::testdata::generic_euler::
                        testRiemannSolverNonlinear::FL_2_in,
                20 * sizeof(double));
    std::memcpy(FR, ::exahype::tests::testdata::generic_euler::
                        testRiemannSolverNonlinear::FR_2_in,
                20 * sizeof(double));

    kernels::aderdg::generic::c::riemannSolverNonlinear<false,false,GenericEulerKernelTest>(
        *this,
        FL, FR, QL, QR,
        0,   // t
        0.0, // dt
        tarch::la::Vector<DIMENSIONS, double>(0.5, 0.5), // dx
        1    // direction
        );

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FL[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverNonlinear::F_2_out[i],
          eps, i);
    }

    for (int i = 0; i < nVar*basisSize; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          FR[i], ::exahype::tests::testdata::generic_euler::
                     testRiemannSolverNonlinear::F_2_out[i],
          eps, i);
    }
  }  // test3

}  // testRiemannSolverNonlinear

void GenericEulerKernelTest::testVolumeIntegralLinear() {
  logInfo( "testVolumeIntegralLinear()", "Test volume integral linear, ORDER=2, DIM=2" );

  constexpr int nVar       = NumberOfVariables;
  constexpr int basisSize  = (Order+1);

  {  // first test
    // output:
    double lduh[80];

    // input:
    double dx[2] = {3.70370370370370349811e-02,
                    3.70370370370370349811e-02};  // mesh spacing
    // ::exahype::tests::testdata::generic_euler::testVolumeIntegral::lFhi[160]

    kernels::aderdg::generic::c::volumeIntegralLinear<false,false,nVar,basisSize>(
        lduh,
        ::exahype::tests::testdata::generic_euler::testVolumeIntegral::lFhi,
        dx[0]
        );

    for (int i = 0; i < 80; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          lduh[i], ::exahype::tests::testdata::generic_euler::
                       testVolumeIntegralLinear::lduh_1[i],
          eps, i);
    }
  }  // scope limiter first test

  {  // second test, analogous to 3d seed

    // input:
    double dx[2] = {0.05, 0.05};     // mesh spacing
    double lFhi[160] = {0.0};  // nVar * dim * nDOFx * nDOFy
    // lFhi = [ lFhi_x | lFhi_y ]
    double *lFhi_x = lFhi + 0;
    double *lFhi_y = lFhi + 80;

    // seed direction
    for (int i = 0; i < 80; i += 5) {
      lFhi_x[i + 1] = 1.;
      lFhi_y[i + 2] = 1.;
    }

    // output:
    double lduh[80];  // intentionally left uninitialised

    kernels::aderdg::generic::c::volumeIntegralLinear<false,false,5,4>(lduh, lFhi, dx[0]);

    for (int i = 0; i < 80; i++) {
      validateNumericalEqualsWithEpsWithParams1(
          lduh[i], ::exahype::tests::testdata::generic_euler::
                       testVolumeIntegralLinear::lduh_2[i],
          eps, i);
    }
  }  // scope second test
}  // testVolumeIntegralLinear

void GenericEulerKernelTest::testVolumeIntegralNonlinear() {
  logInfo( "testVolumeIntegralNonlinear()", "Test volume integral nonlinear, ORDER=2, DIM=2" );

//  constexpr int nVar       = NumberOfVariables;
//  constexpr int basisSize  = (Order+1);

  {  // first test

    // output:
//    double lduh[80];

    // input:
//    double dx[2] = {3.70370370370370349811e-02,
//                    3.70370370370370349811e-02};  // mesh spacing
    // ::exahype::tests::testdata::generic_euler::testVolumeIntegral::lFhi[240]

    logWarning( "testVolumeIntegralNonlinear()", "Test is currently disabled since input data is not suitable." );
//    kernels::aderdg::generic::c::volumeIntegralNonlinear<true, true, nVar, basisSize>(
//        lduh,
//        ::exahype::tests::testdata::generic_euler::testVolumeIntegral::lFhi,
//        dx[0]
//        );
//
//    for (int i = 0; i < 80; i++) {
//      validateNumericalEqualsWithEpsWithParams1(
//          lduh[i], ::exahype::tests::testdata::generic_euler::
//                       testVolumeIntegralNonlinear::lduh_1[i],
//          eps, i);
//    }

  }  // scope limiter first test

  {  // second test, analogous to 3d seed

    // input:
//    double dx[2] = {0.05, 0.05};     // mesh spacing
    double lFhi[240] = {0.0};  // nVar * (dim+1) * nDOFx * nDOFy
    // lFhi = [ lFhi_x | lFhi_y | lShi]
    double *lFhi_x = lFhi + 0;
    double *lFhi_y = lFhi + 80;
    double *lShi   = lFhi + 160;

    // seed direction
    for (int i = 0; i < 80; i += 5) {
      lFhi_x[i + 1] = 1.;
      lFhi_y[i + 2] = 1.;
    }

    std::fill_n(lShi, 80, 0.0);

    // output:
//    double lduh[80];  // intentionally left uninitialised

//    kernels::aderdg::generic::c::volumeIntegralNonlinear<true, true, nVar, basisSize>(lduh, lFhi, dx[0]);
//
//    for (int i = 0; i < 80; i++) {
//      validateNumericalEqualsWithEpsWithParams1(
//          lduh[i], ::exahype::tests::testdata::generic_euler::
//                       testVolumeIntegralNonlinear::lduh_2[i],
//          eps, i);
//    }
  }  // scope limiter second test

}  // testVolumeIntegralNonlinear

//broken, need new linear PDE signature

void GenericEulerKernelTest::testSpaceTimePredictorLinear() {
  logInfo( "testSpaceTimePredictorLinear()", "Test space time predictor linear, ORDER=3, DIM=2" );
/*
  // inputs:
  // exahype::tests::testdata::generic_euler::testSpaceTimePredictor::luh[80 =
  // nVar * nDOFx * nDOFy]

  const tarch::la::Vector<DIMENSIONS, double> dx(0.5, 0.5);
  const double dt = 1.267423918681417E-002;

   // These values are only used if the source depends on x or t.
  // Hence, the actual values do not matter here.
  const tarch::la::Vector<DIMENSIONS, double> x(0.0, 0.0);
  const double t = 0.0;

  // Inputs:
  double lQi[400];  // lQi; nVar * nDOFx * nDOFy * (nDOFt+1); nDOF+1 only here
  double PSi[400];  // point sources
  double PSderivatives[400];
  double tmp_PSderivatives[400];
  double lFi[640+320]; // lFi; (dim+1) * nVar * nDOFx * nDOFy * nDOFt
  double gradQ[640];   // gradQ; nDim * nVar * nDOFx * nDOFy * nDOFt

  // Outputs:
  double lQhi[80];     // lQhi; nVar * nDOFx * nDOFz
  double lFhi[160+80]; // lFh,nVar * nDOFx * nDOFy * (dim+1)

  double lQhbnd[80];  // nVar * nDOFy * 4
  double lFhbnd[80];  // nVar * nDOFy * 4

  _setNcpAndMatrixBToZero = true;
  kernels::aderdg::generic::c::spaceTimePredictorLinear<false,false,false,false,false,GenericEulerKernelTest>(
      *this,
      lQhbnd, lFhbnd,
      lQi,lFi,gradQ,PSi,PSderivatives,tmp_PSderivatives,lQhi,lFhi,
      ::exahype::tests::testdata::generic_euler::testSpaceTimePredictorLinear::luh,
      x, dx, t, dt);

  _setNcpAndMatrixBToZero = false;

  for (int i = 0; i < 80; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lQhi[i], ::exahype::tests::testdata::generic_euler::
                     testSpaceTimePredictorLinear::lQhi[i],
        eps, i);
  }

  for (int i = 0; i < 160; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lFhi[i], ::exahype::tests::testdata::generic_euler::
                     testSpaceTimePredictorLinear::lFhi[i],
        eps, i);
  }

  for (int i = 0; i < 80; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lQhbnd[i], ::exahype::tests::testdata::generic_euler::
                      testSpaceTimePredictorLinear::lQbnd[i],
        eps, i);
  }

  // TODO: The "fixed" kernel from the coding week doesn't compute lFbnd.

  // for (int i = 0; i < 80; i++) {
    // validateNumericalEqualsWithEpsWithParams1(lFbnd[i], 0.0, eps, i);
  // }
  */
}  // testSpaceTimePredictorLinear

/*
 *  We have to consider that we store parameters in
 *  luh, lQi, lQhi, lQhbnd and have to size
 *  the arrays accordingly
 */
void GenericEulerKernelTest::testSpaceTimePredictorNonlinear() {
  logInfo( "testSpaceTimePredictorNonlinear()", "Test space time predictor nonlinear, ORDER=3, DIM=2" );

  const tarch::la::Vector<DIMENSIONS, double> dx(5e-02, 5e-02);
  const double dt = 1.686854344081342E-003;

  // These values are only used if the source depends on x or t.
  // Hence, the actual values do not matter here.
  const tarch::la::Vector<DIMENSIONS, double> x(0.0, 0.0);
  const double t = 0.0;

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 = basisSize *basisSize;
  constexpr int basisSize3 = basisSize2*basisSize;

  // Inputs:
  double lQi[nData*basisSize3];
  double rhs[nData*basisSize3];
  double lFi[(2 + 1)*nVar*basisSize3]; // also stores the source
  double gradQ[2*nVar*basisSize3];

  double luh[nData*basisSize2];
  for (int i = 0; i < basisSize2; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_luh          = i*nData + m;
      const int i_luh_testdata = i*nVar  + m;

      luh[i_luh] = ::exahype::tests::testdata::generic_euler::
                    testSpaceTimePredictorNonlinear::luh[i_luh_testdata];
    }
    for (int m=nVar; m < nData; m++) {
      const int i_luh = i*nData + m;
      luh[i_luh] = 0;
    }
  }

  // Outputs:
  double lQhi[nData*basisSize2];      // lQh; nData * nDOFx * nDOFy
  double lFhi[(2+1)*nVar*basisSize2]; // lFh+source; nVar * nDOFx * nDOFy * (dim + 1)

  double lQhbnd[4 * nData*basisSize];  // nData * nDOFy * 4
  double lFhbnd[4 * nData*basisSize];  // nData * nDOFy * 4

  _setNcpAndMatrixBToZero = true;
  kernels::aderdg::generic::c::spaceTimePredictorNonlinear<true,true,false,true,false,GenericEulerKernelTest>(
      *this,
      lQhbnd, nullptr, lFhbnd,
      lQi, rhs, lFi, gradQ, lQhi, lFhi,
      luh,
      x,
      tarch::la::invertEntries(dx),
      t,
      dt);

  _setNcpAndMatrixBToZero = false;

  for (int i = 0; i < basisSize3; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_lQi          = i*nData + m;
      const int i_lQi_testdata = i*nVar  + m;

      validateNumericalEqualsWithEpsWithParams1(
          lQi[i_lQi], ::exahype::tests::testdata::generic_euler::
          testSpaceTimePredictorNonlinear::lQi[i_lQi_testdata],
          eps, i);
    }
  }

  kernels::idx5 idx_lFi(basisSize, basisSize, basisSize, (DIMENSIONS + 1), nVar, __LINE__);
  kernels::idx5 idx_lFi_expected(basisSize, basisSize, basisSize, DIMENSIONS, nVar, __LINE__);

  for (int i = 0; i < basisSize; i++) {
    for (int k = 0; k < basisSize; k++) {
      for (int l = 0; l < basisSize; l++) {
        for (int m = 0; m < 2; m++) {  // skip 2 ( = source)
          for (int n = 0; n < nVar; n++) {
            validateNumericalEqualsWithEpsWithParams1(
                lFi[idx_lFi(i, k, l, m, n)],
                ::exahype::tests::testdata::generic_euler::
                    testSpaceTimePredictorNonlinear::lFi[idx_lFi_expected(
                        i, k, l, m, n)],
                eps, idx_lFi(i, k, l, m, n));
          }
        }
      }
    }
  }

  for (int i = 0; i < basisSize2; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_lQhi          = i*nData + m;
      const int i_lQi_testdata = i*nVar  + m;

      validateNumericalEqualsWithEpsWithParams1(
          lQhi[i_lQhi], ::exahype::tests::testdata::generic_euler::
          testSpaceTimePredictorNonlinear::lQhi[i_lQi_testdata],
          eps, i);
    }
  }

  for (int i = 0; i < 2*nVar*basisSize2; i++) {  // skip 160 - 239 (source)
    validateNumericalEqualsWithEpsWithParams1(
        lFhi[i], ::exahype::tests::testdata::generic_euler::
                     testSpaceTimePredictorNonlinear::lFhi[i],
        eps, i);
  }

  // lQhbnd
  for (int i = 0; i < 4*basisSize; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_lQhbnd          = i*nData + m;
      const int i_lQhbnd_testdata = i*nVar  + m;

      validateNumericalEqualsWithEpsWithParams1(
          lQhbnd[i_lQhbnd], ::exahype::tests::testdata::generic_euler::
          testSpaceTimePredictorNonlinear::lQhbnd[i_lQhbnd_testdata],
          eps, i);
    }
  }

  // lFhbnd
  for (int i = 0; i < 4*nVar*basisSize; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lFhbnd[i], ::exahype::tests::testdata::generic_euler::
                       testSpaceTimePredictorNonlinear::lFhbnd[i],
        eps, i);
  }
}  // testSpaceTimePredictorNonlinear

void GenericEulerKernelTest::testFaceUnknownsProjection() {
  logInfo( "testFaceUnknownsProjection()", "Test face unknowns projection operators, ORDER=2, DIM=2" );

  constexpr int nVar  = 1;
  constexpr int nPar  = 1;
  constexpr int nData = nVar+nPar;
  constexpr int basisSize = 4;

  // in/out
  double lQhbndCoarseOut[basisSize * nData];
  double lFhbndCoarseOut[basisSize * nVar];
  double lQhbndFineOut[basisSize * nData];
  double lFhbndFineOut[basisSize * nVar];

  // in:
  double lQhbndCoarseIn[basisSize * nData];
  double lFhbndCoarseIn[basisSize * nVar];
  double lQhbndFineIn[basisSize * nData];
  double lFhbndFineIn[basisSize * nVar];

  // Initialise to constant value.
  for (int i = 0; i < basisSize * nData; ++i) {
    lQhbndCoarseIn[i] = 1.0;
    lQhbndFineIn[i]   = 1.0;
  }
  for (int i = 0; i < basisSize * nVar; ++i) {
    lFhbndCoarseIn[i] = 1.0;
    lFhbndFineIn[i]   = 1.0;
  }

  // Test the prolongation operator.
  for (int levelCoarse = 0; levelCoarse < 3; ++levelCoarse) {
    for (int levelDelta = 1; levelDelta < 3; ++levelDelta) {
      // todo For a levelDelta >= 4, assertionNumericalEquals
      // fails since the restriction result is not precise enough anymore.
      const int numberOfSubIntervals = tarch::la::aPowI(levelDelta, 3);

      // Test the restriction operator.
      std::fill_n(lQhbndCoarseOut, nData * basisSize, 0.0);
      std::fill_n(lFhbndCoarseOut, nVar * basisSize, 0.0);
      for (int i1 = 0; i1 < numberOfSubIntervals; ++i1) {
        // Prolongate.
        tarch::la::Vector<DIMENSIONS - 1, int> subfaceIndex(i1);
        kernels::aderdg::generic::c::faceUnknownsProlongation<nVar, nPar, basisSize>(
            lQhbndFineOut, lFhbndFineOut, lQhbndCoarseIn, lFhbndCoarseIn,
            levelCoarse, levelCoarse + levelDelta, subfaceIndex);

        // Test prolongated values.
        for (int m = 0; m < basisSize * nVar; ++m) {
          assertionNumericalEquals(lQhbndFineOut[m], lQhbndFineIn[m]);
          assertionNumericalEquals(lFhbndFineOut[m], lFhbndFineIn[m]);
        }

        // Restrict.
        kernels::aderdg::generic::c::faceUnknownsRestriction<nVar, nPar, basisSize>(
            lQhbndCoarseOut, lFhbndCoarseOut, lQhbndFineIn, lFhbndFineIn,
            levelCoarse, levelCoarse + levelDelta, subfaceIndex);
      }
      // Test restricted values.
      for (int m = 0; m < basisSize * nData; ++m) {
        assertionNumericalEquals(lQhbndCoarseOut[m], lQhbndCoarseIn[m]);
      }
      for (int m = 0; m < basisSize * nVar; ++m) {
        assertionNumericalEquals(lFhbndCoarseOut[m], lFhbndCoarseIn[m]);
      }
    }
  }
}

void GenericEulerKernelTest::testVolumeUnknownsProjection() {
  logInfo( "testVolumeUnknownsProjection()", "Test volume unknowns projection operators, ORDER=2, DIM=2" );

  constexpr int nVar  = 1;
  constexpr int nPar  = 1;
  constexpr int nData = nVar+nPar;
  constexpr int basisSize  = 4;
  constexpr int basisSize2 = basisSize*basisSize;

  // in/out
  double luhCoarseOut[basisSize2 * nData];
  double luhFineOut  [basisSize2 * nData];

  // in:
  double luhCoarseIn[basisSize2 * nData];
  double luhFineIn  [basisSize2 * nData];

  // Initialise to constant value.
  for (int i = 0; i < basisSize2 * nData; ++i) {
    luhCoarseIn[i] = 1.0;
    luhFineIn[i]   = 1.0;
  }

  // Test the prolongation operator.
  for (int levelCoarse = 0; levelCoarse < 3; ++levelCoarse) {
    for (int levelDelta = 1; levelDelta < 2; ++levelDelta) {
      // todo For a levelDelta >= 2, assertionNumericalEquals
      // fails since the prolongation result is not precise enough anymore.
      const int numberOfSubIntervals = tarch::la::aPowI(levelDelta, 3);

      // Test the restriction operator.
      tarch::la::Vector<DIMENSIONS, int> subcellIndex(0);
      std::fill_n(luhCoarseOut, basisSize2 * nData, 0.0);

      for (int i2 = 0; i2 < numberOfSubIntervals; ++i2) {
        for (int i1 = 0; i1 < numberOfSubIntervals; ++i1) {
          subcellIndex[0] = i1;
          subcellIndex[1] = i2;

          // Prolongate.
          kernels::aderdg::generic::c::volumeUnknownsProlongation<nVar, nPar, basisSize>(
              luhFineOut, luhCoarseIn, levelCoarse, levelCoarse + levelDelta,
              subcellIndex);

          // Test prolongated values.
          for (int m = 0; m < basisSize2 * nData; ++m) {
            assertionNumericalEquals5(luhFineOut[m], luhFineIn[m], m,
                                      levelCoarse, levelDelta, i1, i2);
          }

          // Restrict.
          kernels::aderdg::generic::c::volumeUnknownsRestriction<nVar, nPar, basisSize>(
              luhCoarseOut, luhFineIn, levelCoarse, levelCoarse + levelDelta,
              subcellIndex);
        }
      }
      // Test restricted values.
      for (int m = 0; m < basisSize2 * nData; ++m) {
        assertionNumericalEquals3(luhCoarseOut[m], luhCoarseIn[m], m,
                                  levelCoarse, levelDelta);
      }
    }
  }
}

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // DIMENSIONS==2
