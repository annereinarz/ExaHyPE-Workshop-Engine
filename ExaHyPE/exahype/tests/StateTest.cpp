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
 
#include "exahype/tests/StateTest.h"

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "tarch/tests/TestCaseFactory.h"
registerTest(exahype::tests::StateTest)
#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", off)
#endif

    exahype::tests::StateTest::StateTest()
    : tarch::tests::TestCase("exahype::tests::StateTest") {
}

exahype::tests::StateTest::~StateTest() {}

void exahype::tests::StateTest::run() {
  // @todo If you have further tests, add them here
  testMethod(testState);
}

void exahype::tests::StateTest::testState() {
  // @todo Add your test here
  validateEquals(1, 1);
}

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", on)
#endif
