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

#include "exahype/tests/kernels/c/FinitevolumesMusclTest.h"

#include "tarch/tests/TestCaseFactory.h"

// clang-format off
#ifndef ALIGNMENT
registerTest(exahype::tests::c::FinitevolumesMusclTest)
#endif

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", off)
#endif

tarch::logging::Log exahype::tests::c::FinitevolumesMusclTest::_log( "exahype::tests::c::FinitevolumesMusclTest" );


namespace exahype {
namespace tests {
namespace c {

FinitevolumesMusclTest::FinitevolumesMusclTest()
    : tarch::tests::TestCase("exahype::tests::c::FinitevolumesMusclTest") {}

FinitevolumesMusclTest::~FinitevolumesMusclTest() {}

void FinitevolumesMusclTest::run() {
  testMethod(testSolutionUpdate);
}

}  // namepsace c
}  // namespace tests
}  // namespace exahype

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", on)
#endif

// clang-format on
