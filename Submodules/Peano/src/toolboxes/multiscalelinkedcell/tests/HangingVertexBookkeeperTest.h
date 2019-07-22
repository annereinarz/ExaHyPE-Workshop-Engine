// This file is part of the Peano project. For conditions of distribution and 
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MULTISCALE_LINKED_CELL_TESTS_HANGING_VERTEX_BOOKKEEPER_TEST_H_
#define _MULTISCALE_LINKED_CELL_TESTS_HANGING_VERTEX_BOOKKEEPER_TEST_H_
 

#include "tarch/tests/TestCase.h"


namespace multiscalelinkedcell {
  namespace tests {
    class HangingVertexBookkeeperTest;
  }
}
 

/**
 * @todo scan
 */ 
class multiscalelinkedcell::tests::HangingVertexBookkeeperTest: public tarch::tests::TestCase {
  private:
    void test0();

  public: 
    HangingVertexBookkeeperTest();
    virtual ~HangingVertexBookkeeperTest();
     
    virtual void run();
};


#endif
