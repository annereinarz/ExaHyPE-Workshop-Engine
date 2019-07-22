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
#ifndef _EXAHYPE_TESTS_KERNELS_FINITEVOLUMES_RIEMANNSOLVERS_C_RUSANOV_TEST_H_
#define _EXAHYPE_TESTS_KERNELS_FINITEVOLUMES_RIEMANNSOLVERS_C_RUSANOV_TEST_H_

#include "peano/utils/Globals.h"
#include "tarch/logging/Log.h"
#include "tarch/tests/TestCase.h"

namespace exahype {
namespace tests {
namespace kernels {
namespace finitevolumes {
namespace riemannsolvers {
namespace c {

/**
 * This class yields the following left and right fluxes:
 *
 * fL[i] = 0.5 * 1 * (qL[i] - qR[i]);
 * fR[i] = fL[i];
 */
class ZeroFluxZeroNCP {
public:
  static constexpr int NumberOfVariables  = 4;
  static constexpr int NumberOfParameters = 3;

  ZeroFluxZeroNCP();
  virtual ~ZeroFluxZeroNCP();

  void flux           (const double* const Q, double** F);
  void viscousFlux    (const double* const Q, const double* const gradQ, double** F);
  void eigenvalues    (const double* const Q,const int normalNonZeroIndex, double* lambda);
  void viscousEigenvalues (const double* const Q,const int normalNonZeroIndex, double* lambda);

  bool useNonConservativeProduct();
  void nonConservativeProduct(const double* const Q, const double* const gradQ, double* BgradQ);
};

class RusanovTest : public tarch::tests::TestCase {
 public:
  RusanovTest();
  virtual ~RusanovTest();

  void run() override;

 private:
  static tarch::logging::Log _log;

 public:
  const double eps = 1.0e-9;  // for quick adaption of the test cases (say,
                              // switch to single precision)
};

}  // namespace c
}  // namespace riemannsolvers
}  // namespace finitevolumes
}  // namespace kernels
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_KERNELS_FINITEVOLUMES_RIEMANNSOLVERS_C_RUSANOV_TEST_H_
