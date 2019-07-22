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
 
#ifndef _EXAHYPE_TESTS_TEST_CASE_H_
#define _EXAHYPE_TESTS_TEST_CASE_H_

#include "tarch/tests/TestCase.h"

namespace exahype {
namespace tests {
class StateTest;
}
}

/**
 * This is just a default test case that demonstrated how to write unit tests
 * in Peano. Feel free to rename, remove, or duplicate it.
 */
class exahype::tests::StateTest : public tarch::tests::TestCase {
 private:
  /**
   * Tests the properties of a Solve
   * under various conditions.
   */
  void testState();

 public:
  StateTest();
  virtual ~StateTest();

  virtual void run();
};

#endif
