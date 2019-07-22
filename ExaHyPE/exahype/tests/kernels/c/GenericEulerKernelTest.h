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
#ifndef _EXAHYPE_TESTS_GENERIC_EULER_KERNEL_TEST_H_
#define _EXAHYPE_TESTS_GENERIC_EULER_KERNEL_TEST_H_

#include "peano/utils/Globals.h"
#include "tarch/logging/Log.h"
#include "tarch/tests/TestCase.h"
#include "tarch/la/Vector.h"

namespace exahype {
namespace tests {
namespace c {

class GenericEulerKernelTest : public tarch::tests::TestCase {
	
 public:
  static constexpr int NumberOfVariables       = 5;
  static constexpr int NumberOfParameters      = 1;
  static constexpr int Order                   = 3;
  static constexpr int MaxPicardIterations     = -1;
  static constexpr bool UseMaxPicardIterations = false;

  static constexpr double CFL             = 0.9;

  GenericEulerKernelTest();
  virtual ~GenericEulerKernelTest();

  void run() override;

 private:
  static tarch::logging::Log _log;

  static bool _setNcpAndMatrixBToZero;

  void testPDEFluxes();
  void testSpaceTimePredictorLinear();
  void testSpaceTimePredictorNonlinear();
  void testVolumeIntegralLinear();
  void testVolumeIntegralNonlinear();
  void testRiemannSolverLinear();
  void testRiemannSolverNonlinear();
  void testSurfaceIntegralLinear();
  void testSurfaceIntegralNonlinear();
  void testSolutionUpdate();
  void testVolumeUnknownsProjection();
  void testFaceUnknownsProjection();
  void testEquidistantGridProjection();

 public:
  static void flux(const double* const Q, double** F);

  static void flux(const double* const Q, const double* const gradQ, double** F);

  static void algebraicSource(const tarch::la::Vector<DIMENSIONS, double>& x, double t, const double *const Q,
          double *S);

  static void eigenvalues(const double* const Q, const int normalNonZeroIndex, double* lambda);
  static void viscousEigenvalues(const double* const Q, const int normalNonZeroIndex, double* lambda);

  static void nonConservativeProduct(const double* const Q, const double* const gradQ, double* BgradQ);

  static void coefficientMatrix(const double* const Q, const int normalNonZero, double* Bn);

  static void viscousFlux(const double* const Q, double* gradQ, double** F);

  static void multiplyMaterialParameterMatrix(const double *Q, double **rhs){return;}

  const double eps = 1.0e-10;  // for quick adaption of the test cases (say,
                               // switch to single precision)

  bool useAlgebraicSource() {return false;}
  bool useNonConservativeProduct() {return false;}
};


}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_GENERIC_EULER_KERNEL_TEST_H_
