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

#ifndef _EXAHYPE_TESTS_LIMITER_KERNEL_TEST_H_
#define _EXAHYPE_TESTS_LIMITER_KERNEL_TEST_H_

#include "peano/utils/Globals.h"
#include "peano/utils/Loop.h"
#include "tarch/logging/Log.h"
#include "tarch/tests/TestCaseFactory.h"
#include "tarch/tests/TestCase.h"
#include "tarch/compiler/CompilerSpecificSettings.h"

#include "kernels/limiter/generic/Limiter.h"
#include "../testdata/limiter_testdata.h"

namespace exahype {
namespace tests {
namespace c {

class LimiterKernelTest : public tarch::tests::TestCase {
 public:
  LimiterKernelTest();
  virtual ~LimiterKernelTest();

  void run() override;

 private:
  static tarch::logging::Log _log;
  static const double eps;  // for quick adaption of the test cases
  static const int numberOfVariables;
  static const int basisSize;
  static const int basisSizeLim;
  static const std::string dim; // for log

  void testGetGaussLobattoData();
  void testGetFVMData();
  void testUpdateSubcellWithLimiterData();
  void testFindCellLocallocalMinlocalMax();
  void testIsTroubledCell();
};

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_LIMITER_KERNEL_TEST_H_
