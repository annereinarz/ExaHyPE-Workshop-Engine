// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MPIBALANCING_FAIR_NODE_POOL_STRATEGY_TEST_H_
#define _MPIBALANCING_FAIR_NODE_POOL_STRATEGY_TEST_H_


#include "tarch/tests/TestCase.h"
#include "peano/grid/tests/records/TestCell.h"
#include "peano/grid/Cell.h"


namespace mpibalancing {
  namespace tests {
    class FairNodePoolStrategyTest;
  }
}


/**
 *
 * @author Tobias Weinzierl
 */
class mpibalancing::tests::FairNodePoolStrategyTest: public tarch::tests::TestCase {
  private:
    static tarch::logging::Log _log;

    void testDefaultConfiguration();
    void testSandyBridgeConfiguration();

  public:
    FairNodePoolStrategyTest();

    virtual ~FairNodePoolStrategyTest();

    virtual void run();
};

#endif
