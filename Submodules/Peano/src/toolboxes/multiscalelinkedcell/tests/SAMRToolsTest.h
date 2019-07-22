// This file is part of the Peano project. For conditions of distribution and 
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MULTISCALE_LINKED_CELL_TESTS_SAMR_TOOLS_TEST_H_
#define _MULTISCALE_LINKED_CELL_TESTS_SAMR_TOOLS_TEST_H_
 

#include "tarch/tests/TestCase.h"


namespace multiscalelinkedcell {
  namespace tests {
    class SAMRToolsTest;
  }
}
 

/**
 * @todo scan
 */ 
class multiscalelinkedcell::tests::SAMRToolsTest: public tarch::tests::TestCase {
  private:
    void testComputeIterationRangeA0A1();
    void testComputeIterationRangeA1A0();
    void testComputeIterationRangeB1B0();
    void testComputeIterationRangeC0C1();
    void testComputeIterationRangeD0D1();
    void testComputeIterationRangeB1C1();

  public: 
    SAMRToolsTest();
    virtual ~SAMRToolsTest();
     
    virtual void run();
};


#endif
