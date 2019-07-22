// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MPIBALANCING_SFC_DIFFUSION_NODE_POOL_STRATEGY_TEST_H_
#define _MPIBALANCING_SFC_DIFFUSION_NODE_POOL_STRATEGY_TEST_H_


#include "tarch/tests/TestCase.h"
#include "peano/grid/tests/records/TestCell.h"
#include "peano/grid/Cell.h"


#include "mpibalancing/SFCDiffusionNodePoolStrategy.h"


namespace mpibalancing {
  namespace tests {
    class SFCDiffusionNodePoolStrategyTest;
  }
}


/**
 *
 * @author Tobias Weinzierl
 */
class mpibalancing::tests::SFCDiffusionNodePoolStrategyTest: public tarch::tests::TestCase {
  private:
    static tarch::logging::Log _log;

    /**
     * We create a setup mimicing four nodes with eight ranks per node. The
     * total rank count thus is 4*8=32. All of them are set idle besides
     * rank 0 which is already busy.
     */
    SFCDiffusionNodePoolStrategy createSetupWith4Nodes() const;

    /**
     * @see testPrimaryNodeDeliveryWith4Nodes() for sequence of requests
     */
    SFCDiffusionNodePoolStrategy::RequestQueue createQueueTriggeredByWorkersOfFirstWorkerWith4Nodes() const;

    void testFirstWorkerDelivery();

    /**
     * We start from the configuration of createSetupWith4Nodes() and assume
     * that the global rank is forked already. Therefore, rank 2 is working.
     * We then continue with a 2d setup, i.e. the strategy receives one request
     * asking for nine workers. The routine hands out the ranks
     *
     * 4,6,10,12,16,18,24,26,28.
     */
    void testPrimaryNodeDeliveryWith4Nodes();

    /**
     * We recreate the situation described in
     * testPrimaryNodeDeliveryWith4Nodes(). The test ensures that the first
     * requests deployed stem from
     *
     * 4,26 and 28.
     *
     * Those are answered with -1 as they are not a head or tail of the SFC.
     * Next, we have to answer the request of 24 as the underlying node has
     * issued the most requests (there are four nodes in total, see
     * createSetupWith4Nodes() and the ranks 24,26,28 all are deployed to the
     * forth node). Though 24 wants to have eight new workers, only three
     * requests in total are served, i.e. we return
     *
     * 23,22,21.
     *
     * Next, the request by rank 16 is handled. Rank 16 is given
     *
     * 15,14,13.
     *
     * The remaining 8-3=5 requests are all answered with -1, i.e. no rank
     * available.
     */
    void testSecondaryNodeDeliveryWith4Nodes();

  public:
    SFCDiffusionNodePoolStrategyTest();

    virtual ~SFCDiffusionNodePoolStrategyTest();

    virtual void run();
};

#endif
