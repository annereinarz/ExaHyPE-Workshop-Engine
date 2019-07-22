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

#ifndef _EXAHYPE_TESTS_FINITEVOLUMES_MUSCL_TEST_H_
#define _EXAHYPE_TESTS_FINITEVOLUMES_MUSCL_TEST_H_

#include "peano/utils/Globals.h"
#include "tarch/logging/Log.h"
#include "tarch/tests/TestCase.h"

namespace exahype {
namespace tests {
namespace c {

class FinitevolumesMusclTest : public tarch::tests::TestCase {
 public:
  FinitevolumesMusclTest();
  virtual ~FinitevolumesMusclTest();

  void run() override;

 private:
  static tarch::logging::Log _log;
  void testSolutionUpdate();

  static void testFlux(const double* const Q, double** F);
  static void testSource(const double* const Q, double* S);
  static void testEigenvalues(const double* const Q, const int normalNonZero,
                              double* lambda);
  // TODO(guera): Extend
  // static void testSource();

  const double eps = 1.0e-10;
};

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_FINITEVOLUMES_MUSCL_TEST_H_
