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

#include "exahype/tests/kernels/c/ElasticityKernelTest.h"

#include <algorithm>
#include <cstring>
#include <limits>
#include <numeric>

#include "../testdata/elasticity_testdata.h"
#include "kernels/KernelUtils.h"
#include "kernels/aderdg/generic/Kernels.h"

#if DIMENSIONS == 2

namespace exahype {
namespace tests {
namespace c {

/*
 * Q stores parameters, F doesn't.
 */
void ElasticityKernelTest::flux(const double *Q, double **F) {
  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;

  for (int i = 0; i < nData; i++) {
    assertion2(std::isfinite(Q[i]), Q[i], i);
  }

  double *f = F[0]; // ~nVar
  double *g = F[1]; // ~nVar
  // double* h = F[2];

  std::fill_n (f, nVar, 0.0);
  std::fill_n (g, nVar, 0.0);
//  std::fill_n (h, nVar, 0.0);

  double lam = Q[9];          // par(2)
  double mu = Q[10];          // par(2)
  double irho = 1.0 / Q[11];  // 1.0 / par(3)

  f[1 - 1] = -(lam + 2 * mu) * Q[7 - 1];
  f[2 - 1] = -lam * Q[7 - 1];
  f[3 - 1] = -lam * Q[7 - 1];
  f[4 - 1] = -mu * Q[8 - 1];
  f[5 - 1] = 0.0;
  f[6 - 1] = -mu * Q[9 - 1];
  f[7 - 1] = -irho * Q[1 - 1];
  f[8 - 1] = -irho * Q[4 - 1];
  f[9 - 1] = -irho * Q[6 - 1];

  g[1 - 1] = -lam * Q[8 - 1];
  g[2 - 1] = -(lam + 2 * mu) * Q[8 - 1];
  g[3 - 1] = -lam * Q[8 - 1];
  g[4 - 1] = -mu * Q[7 - 1];
  g[5 - 1] = -mu * Q[9 - 1];
  g[6 - 1] = 0.0;
  g[7 - 1] = -irho * Q[4 - 1];
  g[8 - 1] = -irho * Q[2 - 1];
  g[9 - 1] = -irho * Q[5 - 1];

  //  h[1 - 1] = - lam*Q[9 - 1];
  //  h[2 - 1] = - lam*Q[9 - 1];
  //  h[3 - 1] = - (lam+2*mu)*Q[9 - 1];
  //  h[4 - 1] = 0.0;
  //  h[5 - 1] = - mu *Q[8 - 1];
  //  h[6 - 1] = - mu *Q[7 - 1];
  //  h[7 - 1] = - irho *Q[4 - 1];
  //  h[8 - 1] = - irho *Q[2 - 1];
  //  h[9 - 1] = - irho *Q[5 - 1];

  for (int i = 0; i < DIMENSIONS; i++) {
    for (int j = 0; j < nVar; j++) {
      assertion3(std::isfinite(F[i][j]), F[i][j], i, j);
    }
  }
}

/*
 * Q stores parameters, S doesn't.
 */
void ElasticityKernelTest::algebraicSource(const double *Q, double *S) {
  constexpr int nVar       = NumberOfVariables;

  std::fill_n (S, nVar, 0.0);
}


void ElasticityKernelTest::multiplyMaterialParameterMatrix(const double *Q, double **rhs) {
  return;
}


void ElasticityKernelTest::eigenvalues(const double *const Q,
                                           const int normalNonZeroIndex,
                                           double *lambda) {
  constexpr int nVar  = NumberOfVariables;

  std::fill_n (lambda, nVar, 0.0); // We can ignore the parameters here

  double lam = Q[9];    // par(1)
  double mu = Q[10];    // par(2)
  double rho0 = Q[11];  // par(3)
  double cp = std::sqrt((lam + 2 * mu) / rho0);
  double cs = std::sqrt(mu / rho0);

  assert(std::isfinite(cp));
  assert(std::isfinite(cs));

  lambda[1 - 1] = -cp;
  lambda[2 - 1] = -cs;
  lambda[3 - 1] = -cs;
  lambda[4 - 1] = 0.0;
  lambda[5 - 1] = 0.0;
  lambda[6 - 1] = 0.0;
  lambda[7 - 1] = +cs;
  lambda[8 - 1] = +cs;
  lambda[9 - 1] = +cp;
}

/*
 * Q stores parameters, gradQ and BgradQ doesn't.
 */
void ElasticityKernelTest::nonConservativeProduct(const double *const Q,
                                   const double *const * const gradQ, double ** BgradQ) {
  constexpr int nVar  = NumberOfVariables;

  std::fill_n (BgradQ[0], nVar, 0.0);
  std::fill_n (BgradQ[1], nVar, 0.0);

  double lam  = Q[NumberOfVariables];           // par(1)
  double mu   = Q[NumberOfVariables + 1];       // par(2)
  double irho = 1.0 / Q[NumberOfVariables + 2]; // 1.0 / par(3)

  assert(std::isfinite(irho));

  const double *gradQx = gradQ[0];
  const double *gradQy = gradQ[1];
  //  const double *gradQz = gradQ + 2 * kNumberOfVariables;

  double *BgradQx = BgradQ[0];
  double *BgradQy = BgradQ[1];
  //  double *BgradQz = BgradQ + 2 * kNumberOfVariables;

  BgradQx[1 - 1] = -(lam + 2 * mu) * gradQx[7 - 1];
  BgradQx[2 - 1] = -lam * gradQx[7 - 1];
  BgradQx[3 - 1] = -lam * gradQx[7 - 1];
  BgradQx[4 - 1] = -mu * gradQx[8 - 1];
  BgradQx[5 - 1] = 0.0;
  BgradQx[6 - 1] = -mu * gradQx[9 - 1];
  BgradQx[7 - 1] = -irho * gradQx[1 - 1];
  BgradQx[8 - 1] = -irho * gradQx[4 - 1];
  BgradQx[9 - 1] = -irho * gradQx[6 - 1];

  BgradQy[1 - 1] = -lam * gradQy[8 - 1];
  BgradQy[2 - 1] = -(lam + 2 * mu) * gradQy[8 - 1];
  BgradQy[3 - 1] = -lam * gradQy[8 - 1];
  BgradQy[4 - 1] = -mu * gradQy[7 - 1];
  BgradQy[5 - 1] = -mu * gradQy[9 - 1];
  BgradQy[6 - 1] = 0.0;
  BgradQy[7 - 1] = -irho * gradQy[4 - 1];
  BgradQy[8 - 1] = -irho * gradQy[2 - 1];
  BgradQy[9 - 1] = -irho * gradQy[5 - 1];

  //  BgradQy[1 - 1] = -lam * gradQz[9 - 1];
  //  BgradQy[2 - 1] = -lam * gradQz[9 - 1];
  //  BgradQy[3 - 1] = -(lam + 2 * mu) * gradQz[9 - 1];
  //  BgradQy[4 - 1] = 0.0;
  //  BgradQy[5 - 1] = -mu * gradQz[8 - 1];
  //  BgradQy[6 - 1] = -mu * gradQz[7 - 1];
  //  BgradQy[7 - 1] = -irho * gradQz[6 - 1];
  //  BgradQy[8 - 1] = -irho * gradQz[5 - 1];
  //  BgradQy[9 - 1] = -irho * gradQz[3 - 1];

}  // ncp

void ElasticityKernelTest::coefficientMatrix(const double *const Q, const int normalNonZero, double *Bn) {
  constexpr int nVar       = NumberOfVariables;
  constexpr int nVar2      = nVar*nVar;

  std::fill_n (Bn, nVar2, 0.0);

  kernels::idx2 idx_Bn(nVar, nVar);

  double lam = Q[9];          // par(1)
  double mu = Q[10];          // par(2)
  double irho = 1.0 / Q[11];  // 1./par(3)

  assert(std::isfinite(irho));

  switch (normalNonZero) {
    case 0:
      Bn[idx_Bn(7 - 1, 1 - 1)] = -(lam + 2 * mu);
      Bn[idx_Bn(7 - 1, 2 - 1)] = -lam;
      Bn[idx_Bn(7 - 1, 3 - 1)] = -lam;
      Bn[idx_Bn(8 - 1, 4 - 1)] = -mu;
      Bn[idx_Bn(9 - 1, 6 - 1)] = -mu;
      Bn[idx_Bn(1 - 1, 7 - 1)] = -irho;
      Bn[idx_Bn(4 - 1, 8 - 1)] = -irho;
      Bn[idx_Bn(6 - 1, 9 - 1)] = -irho;
      break;
    case 1:
      Bn[idx_Bn(8 - 1, 1 - 1)] = -lam;
      Bn[idx_Bn(8 - 1, 2 - 1)] = -(lam + 2 * mu);
      Bn[idx_Bn(8 - 1, 3 - 1)] = -lam;
      Bn[idx_Bn(7 - 1, 4 - 1)] = -mu;
      Bn[idx_Bn(9 - 1, 5 - 1)] = -mu;
      Bn[idx_Bn(4 - 1, 7 - 1)] = -irho;
      Bn[idx_Bn(2 - 1, 8 - 1)] = -irho;
      Bn[idx_Bn(5 - 1, 9 - 1)] = -irho;
      break;
    //    case 2:
    //      Bn[idx_Bn(9 - 1, 1 - 1)] = -lam;
    //      Bn[idx_Bn(9 - 1, 2 - 1)] = -lam;
    //      Bn[idx_Bn(9 - 1, 3 - 1)] = -(lam + 2 * mu);
    //      Bn[idx_Bn(8 - 1, 5 - 1)] = -mu;
    //      Bn[idx_Bn(7 - 1, 6 - 1)] = -mu;
    //      Bn[idx_Bn(6 - 1, 7 - 1)] = -irho;
    //      Bn[idx_Bn(5 - 1, 8 - 1)] = -irho;
    //      Bn[idx_Bn(3 - 1, 9 - 1)] = -irho;
    //      break;
    default:
      assert(false);
      break;
  }
}  // matrixb

void ElasticityKernelTest::testRiemannSolverLinear() {
  validate(false);

  logInfo("ElasticityKernelTest::testRiemannSolverLinear()",
          "Test Riemann solver linear, ORDER=4, DIM=2");

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);

  double qL[basisSize * nData];
  double qR[basisSize * nData];

  kernels::idx2 idx_q(basisSize, nData);

  kernels::idx2 idx_q_in(basisSize, nVar);

  kernels::idx2 idx_param_in(basisSize, nPar);

  for (int i = 0; i < basisSize; i++) {
    // copy variables
    std::copy_n (exahype::tests::testdata::elasticity::testRiemannSolverLinear::qL_IN + idx_q_in(i, 0),
                 nVar, qL + idx_q(i, 0));
    std::copy_n (exahype::tests::testdata::elasticity::testRiemannSolverLinear::qR_IN + idx_q_in(i, 0),
                 nVar, qR + idx_q(i, 0));

    // append copied parameters
    std::copy_n (exahype::tests::testdata::elasticity::testRiemannSolverLinear::paramL_IN + idx_param_in(i, 0),
                 nPar, qL + idx_q(i, nVar));
    std::copy_n (exahype::tests::testdata::elasticity::testRiemannSolverLinear::paramR_IN + idx_param_in(i, 0),
                 nPar, qR + idx_q(i, nVar));
  }
  double FL[basisSize * nVar]; // no params here
  double FR[basisSize * nVar]; // no params here

  const double dt = 1.916666666666667E-004;

  // TODO(Dominic): Fix test
  kernels::aderdg::generic::c::riemannSolverLinear<true,false,false,ElasticityKernelTest>(*this,
      FL, FR, qL, qR,
      0.0 /*t*/, dt, 1 /* normalNonZero */);

  kernels::idx2 idx_F(basisSize, nVar);

  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < nVar; j++) {
      validateNumericalEqualsWithEpsWithParams1(
          FL[idx_F(i, j)], exahype::tests::testdata::elasticity::
                               testRiemannSolverLinear::FL_OUT[idx_F(i, j)],
          eps, idx_F(i, j));
    }
  }

  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < nVar; j++) {
      validateNumericalEqualsWithEpsWithParams1(
          FR[idx_F(i, j)], exahype::tests::testdata::elasticity::
                               testRiemannSolverLinear::FR_OUT[idx_F(i, j)],
          eps, idx_F(i, j));
    }
  }
}

void ElasticityKernelTest::testSpaceTimePredictorLinear() {
  validate(false);
  logInfo("ElasticityKernelTest::testSpaceTimePredictorLinear()",
          "Test SpaceTimePredictor linear, ORDER=4, DIM=2");

  constexpr int nVar       = NumberOfVariables;
  constexpr int nPar       = NumberOfParameters;
  constexpr int nData      = nVar+nPar;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 = basisSize*basisSize;
  constexpr int basisSize3 = basisSize2*basisSize;

  double luh[nData * basisSize2];
  kernels::idx3 idx_luh(basisSize, basisSize, nData);
  kernels::idx3 idx_luh_IN(basisSize, basisSize, nVar);
  kernels::idx3 idx_param_IN(basisSize, basisSize, nPar);

  // Assemble luh = concatenate dofs and parameters
  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < basisSize; j++) {
      std::copy_n (exahype::tests::testdata::elasticity::testSpaceTimePredictorLinear::luh_IN + idx_luh_IN(i, j, 0),
                   nVar, luh + idx_luh(i, j, 0));
      std::copy_n (exahype::tests::testdata::elasticity::testSpaceTimePredictorLinear::param_IN + idx_param_IN(i, j, 0),
                   nPar, luh + idx_luh(i, j, nVar));
    }
  }

  // Inputs:
  double lQi[nData*basisSize2*(basisSize+1)];  // lQi; nVar * nDOFx * nDOFy * (nDOFt+1); nDOF+1 only here
  double PSi[nData*basisSize3];  // point sources
  double PSderivatives[nData*basisSize3];
  double tmp_PSderivatives[nData*basisSize3];
  double lFi[(2+1)*nVar*basisSize3]; // lFi; (dim+1) * nVar * nDOFx * nDOFy * nDOFt
  double gradQ[2*nVar*basisSize3];   // gradQ; nDim * nVar * nDOFx * nDOFy * nDOFt

  // Outputs:
  double lQhi[nData*basisSize2];                // lQhi; nVar * nDOFx * nDOFz
  double lFhi[(DIMENSIONS+1)*nVar*basisSize2]; // lFh+source,nVar * nDOFx * nDOFy * (dim+1)

  double lQbnd[nData * basisSize * 2 * DIMENSIONS];
  double lFbnd[nVar  * basisSize * 2 * DIMENSIONS];

  kernels::idx3 idx_lQhi(basisSize, basisSize, nData);
  kernels::idx4 idx_lFhi(DIMENSIONS, basisSize, basisSize, nVar);
  kernels::idx3 idx_lQbnd(2 * DIMENSIONS, basisSize, nData);
  kernels::idx3 idx_lFbnd(2 * DIMENSIONS, basisSize, nVar);

  const tarch::la::Vector<DIMENSIONS, double> dx(38.4615384615385,
                                                 35.7142857142857);
  const double dt = 0.813172798364530;

  // Execute kernel
  // TODO(Dominic): Fix test
  return;
  kernels::aderdg::generic::c::spaceTimePredictorLinear<false,false,false,true,false,ElasticityKernelTest>(
      *this,
      lQbnd, lFbnd,
      lQi,lFi,gradQ,PSi,PSderivatives,tmp_PSderivatives,lQhi,lFhi,
      luh, dx, dt);

  for (int i = 0; i < DIMENSIONS; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < basisSize; k++) {
        for (int l = 0; l < nVar; l++) {
          validateNumericalEqualsWithEpsWithParams1(
              lFhi[idx_lFhi(i, j, k, l)],
              exahype::tests::testdata::elasticity::
                  testSpaceTimePredictorLinear::lFhi_OUT[idx_lFhi(i, j, k,
                                                                      l)],
              eps, idx_lFhi(i, j, k, l));
        }
      }
    }
  }

  // Check result
  kernels::idx3 idx_lQhi_OUT(basisSize, basisSize, nVar);
  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < nVar; k++) {
        validateNumericalEqualsWithEpsWithParams1(
            lQhi[idx_lQhi(i, j, k)],
            exahype::tests::testdata::elasticity::testSpaceTimePredictorLinear::
            lQhi_OUT[idx_lQhi_OUT(i, j, k)],
            eps, idx_lQhi_OUT(i, j, k));
      }
    }
  }

  kernels::idx3 idx_lQbnd_OUT(2 * DIMENSIONS, basisSize, nVar, __LINE__);
  for (int i = 0; i < 2 * DIMENSIONS; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < nVar; k++) {
        validateNumericalEqualsWithEpsWithParams1(
            lQbnd[idx_lQbnd(i, j, k)],
            exahype::tests::testdata::elasticity::testSpaceTimePredictorLinear::
                lQbnd_OUT[idx_lQbnd_OUT(i, j, k)],
            eps, idx_lQbnd_OUT(i, j, k));
      }
    }
  }

  for (int i = 0; i < 2 * DIMENSIONS; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < nVar; k++) {
        validateNumericalEqualsWithEpsWithParams1(
            lFbnd[idx_lFbnd(i, j, k)],
            exahype::tests::testdata::elasticity::testSpaceTimePredictorLinear::
                lFbnd_OUT[idx_lFbnd(i, j, k)],
            eps, idx_lFbnd(i, j, k));
      }
    }
  }
}

void ElasticityKernelTest::testVolumeIntegralLinear() {
  validate(false);
  logInfo("ElasticityKernelTest::testVolumeIntegralLinear()",
          "Test VolumeIntegral linear, ORDER=4, DIM=2");

  constexpr int nVar       = NumberOfVariables;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 = basisSize*basisSize;

  double lFhi[(DIMENSIONS + 1) * basisSize2 * nVar];
  std::fill(
      lFhi,
      lFhi + (DIMENSIONS + 1) * basisSize * basisSize * nVar,
      std::numeric_limits<double>::quiet_NaN());
  kernels::idx4 idx_lFhi(DIMENSIONS + 1, basisSize, basisSize, nVar);
  kernels::idx4 idx_lFhi_IN(DIMENSIONS, basisSize, basisSize, nVar);

  for (int i = 0; i < DIMENSIONS; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < basisSize; k++) {
        for (int l = 0; l < nVar; l++) {
          lFhi[idx_lFhi(i, j, k, l)] = exahype::tests::testdata::elasticity::
              testVolumeIntegralLinear::lFhi_IN[idx_lFhi_IN(i, j, k, l)];
        }
      }
    }
  }

  double lduh[basisSize * basisSize * nVar];
  kernels::idx3 idx_lduh(basisSize, basisSize, nVar);
  kernels::idx3 idx_lduh_OUT(basisSize, basisSize, nVar);

  const tarch::la::Vector<DIMENSIONS, double> dx(38.4615384615385,
                                                 35.7142857142857);

  // Execute kernel
  kernels::aderdg::generic::c::volumeIntegralLinear<true,false,nVar, basisSize>(lduh, lFhi, dx);

  // Compare
  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < nVar; k++) {
        validateNumericalEqualsWithEpsWithParams1(
            lduh[idx_lduh(i, j, k)],
            exahype::tests::testdata::elasticity::testVolumeIntegralLinear::
                lduh_OUT[idx_lduh_OUT(i, j, k)],
            eps, idx_lduh_OUT(i, j, k));
      }
    }
  }
}

void ElasticityKernelTest::testSurfaceIntegralLinear() {
  validate(false);
  logInfo("ElasticityKernelTest::testSurfaceIntegralLinear()",
          "Test SurfaceIntegral linear, ORDER=4, DIM=2");

  constexpr int nVar       = NumberOfVariables;
  constexpr int basisSize  = (Order+1);
  constexpr int basisSize2 = basisSize*basisSize;

  double lFbnd[2 * DIMENSIONS * basisSize * nVar];
  std::copy_n (exahype::tests::testdata::elasticity::testSurfaceIntegralLinear::lFbnd_IN,
               2 * DIMENSIONS * basisSize * nVar, lFbnd);

  double lduh[basisSize2 * nVar];
  std::copy_n (exahype::tests::testdata::elasticity::testSurfaceIntegralLinear::lduh_IN,
               basisSize2 * nVar, lduh);

  const tarch::la::Vector<DIMENSIONS, double> dx(38.4615384615385,
                                                 35.7142857142857);
  // Execute kernel
  kernels::aderdg::generic::c::surfaceIntegralLinear<nVar, basisSize>(lduh, lFbnd, dx);

  // Compare
  kernels::idx3 idx_lduh(basisSize, basisSize, nVar);
  kernels::idx3 idx_lFbnd(2 * DIMENSIONS, basisSize, nVar);

  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < basisSize; j++) {
      for (int k = 0; k < nVar; k++) {
        validateNumericalEqualsWithEpsWithParams1(
            lduh[idx_lduh(i, j, k)],
            exahype::tests::testdata::elasticity::testSurfaceIntegralLinear::
                lduh_OUT[idx_lduh(i, j, k)],
            eps, idx_lduh(i, j, k));
      }
    }
  }
}


}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // DIMENSIONS==2
