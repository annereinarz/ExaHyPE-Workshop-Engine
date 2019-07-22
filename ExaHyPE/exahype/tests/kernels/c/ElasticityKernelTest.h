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

// @todo ExaHyPE Lizenz
#ifndef _EXAHYPE_TESTS_ELASTICITY_KERNEL_TEST_H_
#define _EXAHYPE_TESTS_ELASTICITY_KERNEL_TEST_H_

#include "peano/utils/Globals.h"
#include "tarch/logging/Log.h"
#include "tarch/tests/TestCase.h"

namespace exahype {
namespace tests {
namespace c {

class ElasticityKernelTest : public tarch::tests::TestCase {
 public:
  ElasticityKernelTest();
  virtual ~ElasticityKernelTest();

  void run() override;

 private:
  static tarch::logging::Log _log;

  //  void testPDEFluxes();
  void testSpaceTimePredictorLinear();
  //  void testSpaceTimePredictorNonlinear();
  void testVolumeIntegralLinear();
  //  void testVolumeIntegralNonlinear();
  void testRiemannSolverLinear();
  //  void testRiemannSolverNonlinear();
  void testSurfaceIntegralLinear();
  //  void testSurfaceIntegralNonlinear();
  //  void testSolutionUpdate();
  //  void testVolumeUnknownsProjection();
  //  void testFaceUnknownsProjection();
  //  void testEquidistantGridProjection();

 public:
  static constexpr int NumberOfVariables  = 9;
  static constexpr int NumberOfParameters = 3; // !!! Must be chosen '3' see ncp(...) definition
  static constexpr int Order              = 4;
  static constexpr int MaxPicardIterations     = -1;
  static constexpr bool UseMaxPicardIterations = false;
  static constexpr double CFL             = 0.9;

  void flux(const double* const Q, double** F);

  void algebraicSource(const double* Q, double* S);

  void multiplyMaterialParameterMatrix(const double *Q, double **rhs);

  void eigenvalues(const double* const Q,const int normalNonZeroIndex, double* lambda);

  /**
   * BgradQ is a matrix for the linear kernels.
   */
  void nonConservativeProduct(const double* const Q, const double* const *const gradQ, double** BgradQ);

  void coefficientMatrix(const double* const Q, const int normalNonZero, double* Bn);

  const double eps = 1.0e-9;  // for quick adaption of the test cases (say,
                              // switch to single precision)
};

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_ELASTICITY_KERNEL_TEST_H_
