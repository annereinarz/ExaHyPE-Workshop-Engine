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

#include "../testdata/generic_euler_testdata.h"
#include "kernels/KernelUtils.h"
#include "kernels/aderdg/generic/Kernels.h"

#if DIMENSIONS == 3

namespace exahype {
namespace tests {
namespace c {



  
void GenericEulerKernelTest::flux(const double *const Q, double **F) {
  double *f = F[0];
  double *g = F[1];
  double *h = F[2];

  const double GAMMA = 1.4;

  const double irho = 1.0 / Q[0];
  const double p =
      (GAMMA - 1) *
      (Q[4] - 0.5 * (Q[1] * Q[1] + Q[2] * Q[2] + Q[3] * Q[3]) * irho);

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

  h[0] = Q[3];
  h[1] = irho * Q[3] * Q[1];
  h[2] = irho * Q[3] * Q[2];
  h[3] = irho * Q[3] * Q[3] + p;
  h[4] = irho * Q[3] * (Q[4] + p);
}

void GenericEulerKernelTest::algebraicSource(const tarch::la::Vector<DIMENSIONS, double>& cellCenter, double t, const double *const Q, double *S) {
  S[0] = 0.0;
  S[1] = 0.0;
  S[2] = 0.0;
  S[3] = 0.0;
  S[4] = 0.0;
}

void GenericEulerKernelTest::eigenvalues(const double *const Q,
    const int normalNonZeroIndex,
    double *lambda) {
  const double GAMMA = 1.4;

  double irho = 1.0 / Q[0];
  double p = (GAMMA - 1) *
      (Q[4] - 0.5 * (Q[1] * Q[1] + Q[2] * Q[2] + Q[3] * Q[3]) * irho);

  double u_n = Q[normalNonZeroIndex + 1] * irho;
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
    const int normalNonZero, double *Bn) {
  std::fill_n(Bn, 5 * 5, 0);

//  if (!_setNcpAndMatrixBToZero) {
//    // 3D compressible Euler equations
//    double *B1 = new double[5 * 5];
//    double *B2 = new double[5 * 5];
//    double *B3 = new double[5 * 5];
//
//    std::memset(B1, 0, 5 * 5 * sizeof(double));
//    std::memset(B2, 0, 5 * 5 * sizeof(double));
//    std::memset(B3, 0, 5 * 5 * sizeof(double));
//
//    // Bn = B1 if normalNonZero == 0
//    //      B2 if normalNonZero == 1
//    //      B3 if normalNonZero == 2
//    std::memcpy(Bn, (normalNonZero == 0) ? B1 : (normalNonZero == 1) ? B2 : B3,
//        5 * 5 * sizeof(double));
//
//    delete[] B1;
//    delete[] B2;
//    delete[] B3;
//  }
}  // matrixb

void GenericEulerKernelTest::testPDEFluxes() {
  logInfo("testPDEFluxes()", "Test PDE-related functions, DIM=3");

  double Q[5] = {1., 0.1, 0.2, 0.3, 3.5};  // pressure = 1.372
  double f[5], g[5], h[5];
  double *F[3] = {f, g, h};

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

  for (int i = 0; i < 5; i++) {
    validateNumericalEqualsWithParams1(
        h[i], ::exahype::tests::testdata::generic_euler::testPDEFluxes::h[i],
        i);
  }
}  // testPDEFluxes

void GenericEulerKernelTest::testVolumeIntegralLinear() {
  logInfo("testVolumeIntegralLinear()",
      "Test volume integral linear, ORDER=3, DIM=3");

  // output:
  double lduh[320];  // intentionally left uninitialised

  // input:
  const tarch::la::Vector<DIMENSIONS, double> dx(0.5, 0.5,
      0.5);  // mesh spacing
  double lFhi[960] = {0.0};  // nVar * nDOFx * nDOFy * nDOFz * dim
  // lFhi = [ lFhi_x  | lFhi_y | lFhi_z ], 320 entries each
  double *lFhi_x = &lFhi[0];
  double *lFhi_y = &lFhi[320];
  double *lFhi_z = &lFhi[640];

  // seed direction
  for (int i = 0; i < 320; i += 5) {
    lFhi_x[i + 1] = 1.;
    lFhi_y[i + 2] = 2.;
    lFhi_z[i + 3] = 3.;
  }

  kernels::aderdg::generic::c::volumeIntegralLinear<false,false,NumberOfVariables,Order+1>(
      lduh, lFhi, dx);

  for (int i = 0; i < 320; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lduh[i], ::exahype::tests::testdata::generic_euler::
        testVolumeIntegralLinear::lduh[i],
        eps, i);
  }
}  // testVolumeIntegralLinear

void GenericEulerKernelTest::testVolumeIntegralNonlinear() {
  logInfo("testVolumeIntegralNonlinear()",
      "Test volume integral nonlinear, ORDER=3, DIM=3");

  logWarning("testVolumeIntegralNonlinear()",
      "Test is currently disabled since input data is not suitable.");

  // input:
  double lFhi[1280] = {0.0};  // nVar * nDOFx * nDOFy * nDOFz * (dim + 1)
  // lFhi = [ lFhi_x  | lFhi_y | lFhi_z | lShi], 320 entries each
  double *lFhi_x = lFhi;
  double *lFhi_y = lFhi+320;
  double *lFhi_z = lFhi+640;
  double *lShi =   lFhi+960;

  // seed direction
  for (int i = 0; i < 320; i += 5) {
    lFhi_x[i + 1] = 1.;
    lFhi_y[i + 2] = 1.;
    lFhi_z[i + 3] = 1.;
  }
  std::fill_n(lShi, 320, 0.0);

//  kernels::aderdg::generic::c::volumeIntegralNonlinear<true,true,NumberOfVariables,Order+1>(
//      lduh, lFhi, dx[0]);
//
//  for (int i = 0; i < 320; i++) {
//    validateNumericalEqualsWithEpsWithParams1(
//        lduh[i], ::exahype::tests::testdata::generic_euler::
//        testVolumeIntegralNonlinear::lduh[i],
//        eps, i);
//  }
}  // testVolumeIntegralNonlinear

void GenericEulerKernelTest::testSurfaceIntegralLinear() {
  logInfo("testSurfaceIntegralLinear()",
      "Test surface integral linear, ORDER=3, DIM=3");

  // inputs:
  const double dx[3] = {0.1, 0.1, 0.1};  // mesh spacing
  double lFhbnd[6 * 80] = {0.0};   // 480
  double *FLeft   = lFhbnd;
  double *FRight  = lFhbnd + 80;
  double *FFront  = lFhbnd + 160;
  double *FBack   = lFhbnd + 240;
  double *FBottom = lFhbnd + 320;
  double *FTop    = lFhbnd + 400;

  for (int i = 0; i < 80; i += 5) {
    // in x orientation 1
    FLeft[i + 1] = 1.;
    FRight[i + 1] = 1.;
    // in y orientation 1
    FFront[i + 2] = 1.;
    FBack[i + 2] = 1.;
    // in z direction 1
    FBottom[i + 3] = 1.;
    FTop[i + 3] = 1.;
  }

  // in/out:
  double lduh[320];
  for (int i = 0; i < 320; i++) {
    lduh[i] = i / 10.;
  }

  // lFhbnd = [ FLeft | FRight | FFront | FBack | FBottom | FTop ]
  kernels::aderdg::generic::c::surfaceIntegralLinear<NumberOfVariables,Order+1>(
      lduh, lFhbnd, dx[0]);

  for (int i = 0; i < 320; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lduh[i], ::exahype::tests::testdata::generic_euler::
        testSurfaceIntegralLinear::lduh[i],
        eps, i);
  }
}  // testSurfaceIntegralLinear

void GenericEulerKernelTest::testSurfaceIntegralNonlinear() {
  logInfo("testSurfaceIntegralNonlinear()",
      "Test surface integral nonlinear, ORDER=3, DIM=3");

  // inputs:
  const double dx[3] = {0.1, 0.1, 0.1};  // mesh spacing
  double lFhbnd[6 * 80] = {0.0};   // 480
  double *FLeft         = lFhbnd + 0;
  double *FRight        = lFhbnd + 80;
  double *FFront        = lFhbnd + 160;
  double *FBack         = lFhbnd + 240;
  double *FBottom       = lFhbnd + 320;
  double *FTop          = lFhbnd + 400;

  for (int i = 0; i < 80; i += 5) {
    // in x orientation 1
    FLeft[i + 1] = 1.;
    FRight[i + 1] = 1.;
    // in y orientation 1
    FFront[i + 2] = 1.;
    FBack[i + 2] = 1.;
    // in z direction 1
    FBottom[i + 3] = 1.;
    FTop[i + 3] = 1.;
  }

  // in/out:
  double lduh[320];
  for (int i = 0; i < 320; i++) {
    lduh[i] = i / 10.;
  }

  // lFhbnd = [ FLeft | FRight | FFront | FBack | FBottom | FTop ]
  kernels::aderdg::generic::c::surfaceIntegralNonlinear<NumberOfVariables,Order+1>(
      lduh, lFhbnd, dx[0]);

  for (int i = 0; i < 320; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lduh[i], ::exahype::tests::testdata::generic_euler::
        testSurfaceIntegralNonlinear::lduh[i],
        eps, i);
  }
}  // testSurfaceIntegralNonlinear

/**
 * We need to consider material parameters
 * in QL,QR,
 * We don't need to consider material parameters
 * in FL and FR.
 */
 //broken, need new linear ncp method

void GenericEulerKernelTest::testRiemannSolverLinear() {
  // Rusanov
  logInfo("testRiemannSolverLinear()",
      "Test Riemann solver linear (Rusanov), ORDER=3, DIM=3");
 /*
  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 =  basisSize*basisSize;

  // output (intentionally left uninitialised):
  double FL[nVar*basisSize2] = {0.0};  // nDOF(3) * nDOF(2) * nVar
  double FR[nVar*basisSize2] = {0.0};  // nDOF(3) * nDOF(2) * nVar

  // inputs:
  // exahype::tests::testdata::generic_euler::testRiemannSolver::QL[80 =
  // nVar * nDOF * nDOF]
  // exahype::tests::testdata::generic_euler::testRiemannSolver::QR[80 =
  // nVar * nDOF * nDOF]
  const double t = 0.0;
  const double dt = 1.40831757919882352703e-03;
  const int normalNonZero=1; // normalNonZero (only changes result of eigenvalues, matrixb) 

  // Adapt the striding of test data to the parameters
  double QL[nData*basisSize2] = {0.0};  // nData * nDOF(2)
  double QR[nData*basisSize2] = {0.0};  // nData * nDOF(2)
  for (int i = 0; i < basisSize2; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_Qbnd          = i*nData + m;
      const int i_Qbnd_testdata = i*nVar  + m;

      QL[i_Qbnd]=
          exahype::tests::testdata::generic_euler::testRiemannSolver::QL[i_Qbnd_testdata];
      QR[i_Qbnd]=
          exahype::tests::testdata::generic_euler::testRiemannSolver::QR[i_Qbnd_testdata];
    }
  }

  kernels::aderdg::generic::c::riemannSolverLinear<false,false,false,GenericEulerKernelTest>(
      *this,
      FL, FR, QL, QR,
      t,
      dt,
      normalNonZero
  );

  for (int i = 0; i < nVar*basisSize2; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        FL[i], ::exahype::tests::testdata::generic_euler::
        testRiemannSolverLinear::FL[i],
        eps, i);
  }

  for (int i = 0; i < nVar*basisSize2; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        FR[i], ::exahype::tests::testdata::generic_euler::
        testRiemannSolverLinear::FR[i],
        eps, i);
  }
  */ 
}  // testRiemannSolverLinear

/**
 * We need to consider material parameters
 * in QL and QR.
 * We don't need to consider material parameters
 * in FL and FR.
 */
void GenericEulerKernelTest::testRiemannSolverNonlinear() {
  logInfo("testRiemannSolverNonlinear()",
      "Test Riemann solver nonlinear (Rusanov), ORDER=3, DIM=3");

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 =  basisSize*basisSize;

  // inout:
  double FL[nVar*basisSize2] = {0.0};  // nDOF(3) * nDOF(2) * nVar
  double FR[nVar*basisSize2] = {0.0};  // nDOF(3) * nDOF(2) * nVar
  for (int i = 0; i < nVar*basisSize2; i++) {
    // arbitrary values
    FL[i] = static_cast<double>(i + 1);
    FR[i] = static_cast<double>(i - 1);
  }
  // inputs:
  // exahype::tests::testdata::generic_euler::testRiemannSolver::QL[80 =
  // nVar * nVar * nDOF]
  // exahype::tests::testdata::generic_euler::testRiemannSolver::QR[80 =
  // nVar * nVar * nDOF]
  const double t  = 0.0;
  const double dt = 1.40831757919882352703e-03;

  double QL[nData*basisSize2] = {0.0};  // nData * nDOF(2)
  double QR[nData*basisSize2] = {0.0};  // nData * nDOF(2)
  // Update striding according to number of parameters
  for (int i = 0; i < basisSize2; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_Qbnd          = i*nData + m;
      const int i_Qbnd_testdata = i*nVar  + m;

      QL[i_Qbnd]= ::exahype::tests::testdata::generic_euler::testRiemannSolver::QL[i_Qbnd_testdata];
      QR[i_Qbnd]= ::exahype::tests::testdata::generic_euler::testRiemannSolver::QR[i_Qbnd_testdata];
    }
  }
  kernels::aderdg::generic::c::riemannSolverNonlinear<false,false,GenericEulerKernelTest>(
      *this,
      FL, FR, QL, QR,
      t,
      dt,
      tarch::la::Vector<DIMENSIONS, double>(0.5, 0.5), // dx
      1  // normalNonZero
  );

  for (int i = 0; i < nVar*basisSize2; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        FL[i], ::exahype::tests::testdata::generic_euler::
        testRiemannSolverNonlinear::FL[i],
        eps, i);
  }

  for (int i = 0; i < nVar*basisSize2; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        FR[i], ::exahype::tests::testdata::generic_euler::
        testRiemannSolverNonlinear::FR[i],
        eps, i);
  };
}  // testRiemannSolverNonlinear


/**
 * Only luh considers parameters.
 * We thus make the test data strides fit.
 *
 * lduh does not consider parameters.
 */
void GenericEulerKernelTest::testSolutionUpdate() {
  logInfo("testSolutionUpdate()", "Test solution update, ORDER=3, DIM=3");

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 = basisSize*basisSize;
  constexpr int basisSize3 = basisSize2*basisSize;

  // in/out:
  double luh[nData*basisSize3] = {0.0};
  for (int i = 0; i < nData*basisSize3; i += nData) {
    luh[i] = 1.0;
    luh[i + 4] = 2.5;
  }

  // inputs:
  const double dt = 1.40831757919882352703e-03;

  double lduh[nVar*basisSize3] = {0.0};
  for (int i = 0; i < nVar*basisSize3; i++) {
    lduh[i] = i;
  }

  kernels::aderdg::generic::c::solutionUpdate<GenericEulerKernelTest>(*this,luh, luh, lduh, dt);

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

//broken need new linear method

void GenericEulerKernelTest::testSpaceTimePredictorLinear() {
  logInfo("testSpaceTimePredictorLinear()",
      "Test space time predictor linear, ORDER=3, DIM=3");
/*
  // inputs:
  // exahype::tests::testdata::generic_euler::testSpaceTimePredictor::luh[320 =
  // nVar * nDOFx * nDOFy * nDOFz]

  // Actual values for cellCenter/t do not matter, they are only used for
  // source terms that depend on time/space.
  const tarch::la::Vector<DIMENSIONS, double> cellCenter(0.0, 0.0, 0.0);
  const double t = 0.0;

  const tarch::la::Vector<DIMENSIONS, double> dx(0.5, 0.5, 0.5);
  const double dt = 1.267423918681417E-002;

  // Inputs:
  double lQi[1600];  // lQi; nVar * nDOFx * nDOFy * nDOFz * (nDOFt+1); nDOF+1 only here
  double PSi[1600];  // pointSources
  double PSderivatives[1600];
  double tmp_PSderivatives[1600];
  double lFi[3840+1280];           // lFi+source; nVar * nDOFx * nDOFy * nDOFt * (dim+1)
  double gradQ[3840];              // lQi; nVar * nDOFx * nDOFy * nDOFt * dim

  // Outputs:
  double lQhi[320];     // lQh; nVar * nDOFx * nDOFy * nDOFz
  double lFhi[960+320]; // lFh+source; nVar * nDOFx * nDOFy * nDOFz * (dm+1) *

  double lQhbnd[480] = {0.0};  // nVar * nDOFy * nDOF_z * 6
  double lFhbnd[480] = {0.0};  // nVar * nDOFy * nDOF_z * 6

  kernels::aderdg::generic::c::spaceTimePredictorLinear<false,false,false,false,false,GenericEulerKernelTest>(
      *this,
      lQhbnd, lFhbnd,
      lQi,lFi,gradQ,PSi,PSderivatives,tmp_PSderivatives,lQhi,lFhi,
      ::exahype::tests::testdata::generic_euler::
       testSpaceTimePredictor::luh, // TODO(Dominic): Rename namespace to testSpaceTimePredictorLinear?
       cellCenter, tarch::la::invertEntries(dx),
       t, dt
  );

  for (int i = 0; i < 320; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lQhi[i], ::exahype::tests::testdata::generic_euler::
        testSpaceTimePredictorLinear::lQhi[i],
        eps, i);
  }

  for (int i = 0; i < 960; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lFhi[i], ::exahype::tests::testdata::generic_euler::
        testSpaceTimePredictorLinear::lFhi[i],
        eps, i);
  }

  for (int i = 0; i < 480; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lQhbnd[i], ::exahype::tests::testdata::generic_euler::
        testSpaceTimePredictorLinear::lQhbnd[i],
        eps, i);
  }

  for (int i = 0; i < 480; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lFhbnd[i], ::exahype::tests::testdata::generic_euler::
        testSpaceTimePredictorLinear::lFhbnd[i],
        eps, i);
  }
  */
}  // testSpaceTimePredictorLinear

/*
 *  We have to consider that we store parameters in
 *  luh, lQi, lQhi, lQhbnd and have to size
 *  the arrays accordingly
 */
void GenericEulerKernelTest::testSpaceTimePredictorNonlinear() {
  logInfo("testSpaceTimePredictorNonlinear()",
      "Test space time predictor nonlinear, ORDER=3, DIM=3");

  // inputs:
  // exahype::tests::testdata::generic_euler::testSpaceTimePredictor::luh[320 =
  // nVar * nDOFx * nDOFy * nDOFz]

  const tarch::la::Vector<DIMENSIONS, double> dx(0.05, 0.05, 0.05);
  const double timeStepSize = 1.083937460199773E-003;

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
  constexpr int basisSize4 = basisSize2*basisSize2;

  // Inputs:
  double lQi[nData*basisSize4];
  double rhs[nData*basisSize4];
  double lFi[(3 + 1)*nVar*basisSize4]; // also stores the source
  double gradQ[3*nVar*basisSize4];

  double luh[nData*basisSize3] = {0.0};
  for (int i = 0; i < basisSize3; i++) {
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
  // spatial unknowns,flux unknowns
  double lQhi[nData*basisSize3]     = {0.0};       // nData * nDOFx * nDOFy * nDOFz; // intentionally left uninitialised;
  double lFhi[(3+1)*nVar*basisSize3] = {0.0};  // nData * nDOFx * nDOFy * nDOFz * (dim + 1);

  double lQhbnd[6 * nData*basisSize2] = {0.0};  // nData * nDOFy * nDOF_z * 6
  double lFhbnd[6 * nData*basisSize2] = {0.0};  // nData * nDOFy * nDOF_z * 6

  _setNcpAndMatrixBToZero = true;
  kernels::aderdg::generic::c::spaceTimePredictorNonlinear<true,true,false, true,false,GenericEulerKernelTest>(
      *this,
      lQhbnd, nullptr, lFhbnd,
      lQi, rhs, lFi, gradQ, lQhi, lFhi,
      luh,
      x,
      tarch::la::invertEntries(dx),
      t, timeStepSize
  );
  _setNcpAndMatrixBToZero = false;

  for (int i = 0; i < basisSize4; i++) {
    for (int m=0; m < nVar; m++) {
      const int i_lQi          = i*nData + m;
      const int i_lQi_testdata = i*nVar  + m;

      validateNumericalEqualsWithEpsWithParams1(
          lQi[i_lQi], ::exahype::tests::testdata::generic_euler::
          testSpaceTimePredictorNonlinear::lQi[i_lQi_testdata],
          eps, i);
    }
  }

  kernels::idx6 idx_lFi(basisSize, basisSize, basisSize, basisSize, (DIMENSIONS + 1),
      nVar, __LINE__);
  kernels::idx6 idx_lFi_expected(basisSize, basisSize, basisSize, basisSize, DIMENSIONS,
      nVar, __LINE__);

  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < basisSize; k++) {
        for (int l = 0; l < basisSize; l++) {
          for (int m = 0; m < 3; m++) {  // skip 3 ( = source)
            for (int n = 0; n < nVar; n++) {
              validateNumericalEqualsWithEpsWithParams1(
                  lFi[idx_lFi(i, j, k, l, m, n)],
                  ::exahype::tests::testdata::generic_euler::
                   testSpaceTimePredictorNonlinear::lFi[idx_lFi_expected(
                       i, j, k, l, m, n)],
                       eps, idx_lFi(i, j, k, l, m, n));
            }
          }
        }
      }
    }
  }

  // lQhbnd
  for (int i = 0; i < 6*basisSize2; i++) {
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
  for (int i = 0; i < 6*nVar*basisSize2; i++) {
    validateNumericalEqualsWithEpsWithParams1(
        lFhbnd[i], ::exahype::tests::testdata::generic_euler::
        testSpaceTimePredictorNonlinear::lFhbnd[i],
        eps, i);
  }
}  // testSpaceTimePredictorNonlinear

void GenericEulerKernelTest::testFaceUnknownsProjection() {
  logInfo("testFaceUnknownsProjection()",
      "Test face unknowns projection operators, ORDER=3, DIM=3");

  const int nVar  = 1;
  const int nPar  = 1;
  const int nData = nVar+nPar;
  const int basisSize  = 4;
  const int basisSize2 = basisSize*basisSize;

  // in/out
  double lQhbndCoarseOut[basisSize2 * nData] = {0.0};
  double lFhbndCoarseOut[basisSize2 * nVar]  = {0.0};
  double lQhbndFineOut  [basisSize2 * nData] = {0.0};
  double lFhbndFineOut  [basisSize2 * nVar]  = {0.0};

  // in:
  double lQhbndCoarseIn[basisSize2 * nData];
  double lFhbndCoarseIn[basisSize2 * nVar];
  double lQhbndFineIn  [basisSize2 * nData];
  double lFhbndFineIn  [basisSize2 * nVar];

  // Initialise to constant value.
  std::fill_n(lQhbndCoarseIn, basisSize2*nData, 1.0); // No, you cannot merge this with the initialisation!
  std::fill_n(lQhbndFineIn,   basisSize2*nData, 1.0);
  std::fill_n(lFhbndCoarseIn, basisSize2*nVar, 1.0);
  std::fill_n(lFhbndFineIn,   basisSize2*nVar, 1.0);

  // Test the prolongation operator.
  for (int levelCoarse = 0; levelCoarse < 3; ++levelCoarse) {
    for (int levelDelta = 1; levelDelta < 1; ++levelDelta) {
      // todo For a levelDelta >= 2, assertionNumericalEquals
      // fails since the restriction result is not precise enough anymore.
      const int numberOfSubIntervals = tarch::la::aPowI(levelDelta, 3);

      tarch::la::Vector<DIMENSIONS - 1, int> subfaceIndex(0);

      // Test the restriction operator.
      for (int i2 = 0; i2 < numberOfSubIntervals; ++i2) {
        for (int i1 = 0; i1 < numberOfSubIntervals; ++i1) {
          subfaceIndex[0] = i1;
          subfaceIndex[1] = i2;

          // Prolongate.
          kernels::aderdg::generic::c::faceUnknownsProlongation<NumberOfVariables,NumberOfParameters,Order+1>(
              lQhbndFineOut, lFhbndFineOut, lQhbndCoarseIn, lFhbndCoarseIn,
              levelCoarse, levelCoarse + levelDelta, subfaceIndex);

          // Test prolongated values.
          for (int m = 0; m < basisSize * basisSize * nVar; ++m) {
            assertionNumericalEquals5(lQhbndFineOut[m], lQhbndFineIn[m], m,
                levelCoarse, levelDelta, i1, i2);
            assertionNumericalEquals5(lFhbndFineOut[m], lFhbndFineIn[m], m,
                levelCoarse, levelDelta, i1, i2);
          }

          // Restrict.
          kernels::aderdg::generic::c::faceUnknownsRestriction<NumberOfVariables,NumberOfParameters,Order+1>(
              lQhbndCoarseOut, lFhbndCoarseOut, lQhbndFineIn, lFhbndFineIn,
              levelCoarse, levelCoarse + levelDelta, subfaceIndex);
        }
        // Test restricted values.
        for (int m = 0; m < basisSize2 * nData; ++m) {
          assertionNumericalEquals3(lQhbndCoarseOut[m], lQhbndCoarseIn[m], m,
              levelCoarse, levelDelta);
        }
        for (int m = 0; m < basisSize2 * nData; ++m) {
          assertionNumericalEquals3(lFhbndCoarseOut[m], lFhbndCoarseIn[m], m,
                      levelCoarse, levelDelta);
        }
      }
    }
  }
}

void GenericEulerKernelTest::testVolumeUnknownsProjection() {
  logInfo("testVolumeUnknownsProjection()",
      "Test volume unknowns projection operators, ORDER=3, DIM=3");

  constexpr int nVar       = 1;
  constexpr int nPar       = 1;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = 4;
  constexpr int basisSize3 = basisSize*basisSize*basisSize;

  // in/out
  double luhCoarseOut[basisSize3 * nData];
  double luhFineOut  [basisSize3 * nData];

  // in:
  double luhCoarseIn[basisSize3 * nData];
  double luhFineIn  [basisSize3 * nData];

  // Initialise to constant value.
  for (int i = 0; i < basisSize3 * nData; ++i) {
    luhCoarseIn[i] = 1.0;
    luhFineIn[i] = 1.0;
  }

  // Test the prolongation operator.
  for (int levelCoarse = 0; levelCoarse < 3; ++levelCoarse) {
    for (int levelDelta = 1; levelDelta < 2; ++levelDelta) {
      // todo For a levelDelta >= 2, assertionNumericalEquals
      // fails since the prolongation result is not precise enough anymore.
      const int numberOfSubIntervals = tarch::la::aPowI(levelDelta, 3);

      // Test the restriction operator.
      tarch::la::Vector<DIMENSIONS, int> subcellIndex(0);
      std::fill_n(luhCoarseOut, basisSize3 * nData, 0.0);

      for (int i3 = 0; i3 < numberOfSubIntervals; ++i3) {
        for (int i2 = 0; i2 < numberOfSubIntervals; ++i2) {
          for (int i1 = 0; i1 < numberOfSubIntervals; ++i1) {
            subcellIndex[0] = i1;
            subcellIndex[1] = i2;
            subcellIndex[2] = i3;

            // Prolongate.
            kernels::aderdg::generic::c::volumeUnknownsProlongation<nVar,nPar,basisSize>(
                luhFineOut, luhCoarseIn, levelCoarse, levelCoarse + levelDelta,
                subcellIndex);

            // Test prolongated values.
            for (int m = 0; m < basisSize3 * nData; ++m) {
              assertion7(tarch::la::equals(luhFineOut[m], luhFineIn[m], 1e-9), luhFineOut[m], luhFineIn[m], m,
                                        levelCoarse, levelDelta, i1, i2);
            }


            // Restrict.
            kernels::aderdg::generic::c::volumeUnknownsRestriction<nVar,nPar,basisSize>(
                luhCoarseOut, luhFineIn, levelCoarse, levelCoarse + levelDelta,
                subcellIndex);
          }
        }
      }
      // Test restricted values.
      for (int m = 0; m < basisSize3 * nData; ++m) {
        assertionNumericalEquals3(luhCoarseOut[m], luhCoarseIn[m], m,
            levelCoarse, levelDelta);
      }
    }
  }
}

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // DIMENSIONS==3
