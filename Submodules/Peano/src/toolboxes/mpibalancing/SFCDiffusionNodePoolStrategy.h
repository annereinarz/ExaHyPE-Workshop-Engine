// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MPIBALANCING_SFC_DIFFUSION_NODE_POOL_STRATEGY_H_
#define _MPIBALANCING_SFC_DIFFUSION_NODE_POOL_STRATEGY_H_


#ifdef Parallel
#include <mpi.h>
#endif

#include "tarch/parallel/NodePoolStrategy.h"
#include "tarch/logging/Log.h"

#include <vector>
#include <set>


namespace mpibalancing {
  class SFCDiffusionNodePoolStrategy;

  namespace tests {
    /**
     * Forward declaration.
     */
    class SFCDiffusionNodePoolStrategyTest;
  }
}


/**
 * SFC Diffusion Node Pool Strategy
 *
 * <h2> User pitfalls </h2>
 * The strategy tries to book solely the primary MPI nodes. Therefore,
 * statements alike
 * <pre>
  assertion( tarch::parallel::NodePool::getInstance().getNumberOfIdleNodes()==0 );
   </pre>
 * are doomed to fail.
 *
 *
 * <h2>Number of idle nodes</h2>
 *
 * Please consult the getNumberOfIdleNodes() routine for details. The point is
 * that the handling of idle nodes is slightly inconsistent here, as there are
 * idle nodes (secondary) that are never to be deployed in the standard case
 * even though they have nothing to do.
 *
 *
 * <h2>Typical configurations</h2>
 *
 * - It seems to be reasonable to have twice as many ranks as primary ranks.
 *
 *
 * @author Tobias Weinzierl
 * @version $Revision: 1.5 $
 */
class mpibalancing::SFCDiffusionNodePoolStrategy: public tarch::parallel::NodePoolStrategy {
  private:
    friend class mpibalancing::tests::SFCDiffusionNodePoolStrategyTest;

    /**
     * Copy from FCFS but enriched by counter how many rank have already
     * requested an update.
     *
     * @author Tobias Weinzierl
     * @version $Revision: 1.5 $
     */
    class NodePoolListEntry {
      public:
        /**
         * Represents the state of the worker, i.e. whether it is idle or busy.
         */
        enum class State {
          Undef,
          IdlePrimaryRank,
          IdleSecondaryRank,
          WorkingPrimaryRank,
          WorkingSecondaryRank
        };

      private:
        /**
         * Holds the state of the process.
         */
        State             _state;

        /**
         * Machine name
         *
         * Should be const but then using the operator= becomes a mess.
         */
        std::string       _name;

      public:
        /**
         * Construct one entry. By default this entry corresponds to an idle worker.
         */
        NodePoolListEntry( const std::string& name, bool isPrimaryNode );

        /**
         * I need a default constructor for some resorting, but it is not
         * available from outside.
         */
        NodePoolListEntry();

        virtual ~NodePoolListEntry();

        /**
         * Activates the node. Precondition: Node is idle. Thus, the local min level
         * is overwritten by the argument level and the state is set to working.
         */
        void activate();

        /**
         * The state is switched to idle.
         */
        void deActivate();

        bool isRegistered() const;

        /**
         * @return Rank of process.
         */
        int getRank() const;

        /**
         * @return Name of the node the process is running on.
         */
        std::string getNodeName() const;

        /**
         * Create string representation.
         */
        void toString(std::ostream& out) const;

        /**
         * Return string representation.
         */
        std::string toString() const;

        bool isIdlePrimaryRank() const;
        bool isIdleSecondaryRank() const;
    };

    enum class NodePoolState {
      /**
       * Standard mode. As soon as we run out of primary nodes, we switch it to
       * DeployingAlsoSecondaryRanks.
       */
      DeployingIdlePrimaryRanks=6,
      /**
       * If this flag is set, we also deploy secondary nodes for a fixed number
       * of sweeps. Afterwards (or as soon as absolutely no entries are left 
       * anymore), we switch into NoNodesLeft.
       */
      DeployingAlsoSecondaryRanksFirstSweep=5,
      DeployingAlsoSecondaryRanksLastSweep=1,
      /**
       * We do not hand out any nodes anymore.
       */
      NoNodesLeft=0
    };


    /**
     * Is ordered along ranks.
     */
    typedef std::vector<NodePoolListEntry>   NodeContainer;

    /**
     * Logging Device
     */
    static tarch::logging::Log _log;

    std::string nodePoolStateToString() const;

    /**
     * Tag on which the node pool works
     */
    int _tag;

    /**
     * This field equals tarch::parallel::Node::getInstance().getNumberOfNodes()
     * usually, so we could always analyse it on-the-fly. However, I wanna be
     * able to run unit tests on a single node, so I make it a variable
     * initialised once that I can override in the unit tests.
     */
    int _totalNumberOfRanks;

    /**
     * The list the list of active nodes. Every entry corresponds to one node.
     * If the entry is set, the node is working already and the server is not
     * allowed to deploy another job on this node. If the entry isn't set, there
     * is a job request message in the queue and the server is allowed to send
     * a job. Therefore, in the beginning, all the entries are set. For the very
     * first entry, corresponding to the server node, the invariant holds, that
     * this entry is set always.
     */
    NodeContainer _nodes;

    double        _waitTimeOut;

    /**
     * is not used at the moment, but should be used in first mode.
     */
    const int     _mpiRanksPerNode;

    const int     _primaryMPIRanksPerNode;

    int           _numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed;
    int           _numberOfNodesToSkipPerRequestPlusOne;

    NodePoolState _nodePoolState;

    /**
     * If a node has got rid of too much work, noone else should have the 
     * opportunity to reassign it more work again
     */ 
    std::set<int> _rankBlackList;

    /**
     * If secondary ranks from a particular node have been activated, requests
     * from this node should have higher priority.
     */
    std::set<int> _rankPriorityList;

    int getNumberOfIdlePrimaryRanks() const;

    /**
     * Computes the total number of real nodes
     *
     * For this, we take the number of ranks and divide it by the passed
     * argument ranks-per-node. To facilitate debugging (where fewer ranks
     * might be used than are usually deployed to one node), we combine the
     * result with a max(...,1).
     */
    int getNumberOfPhysicalNodes() const;

    bool hasCompleteIdleNode() const;

    bool isPrimaryMPIRank(int rank) const;

    void configureForPrimaryRanksDelivery(int numberOfRequestedRanks);

    /**
     * Called in diffusion phase. Just ensure that noone grabs now ranks from 
     * the master's node. That would be an oscillation.
     */
    void haveReservedSecondaryRank(int masterRank, int workerRank);

    int deployIdlePrimaryRank(int forMaster);
    int deployIdleSecondaryRank(int forMaster);
    
    /**
     * If no ranks are available or we are still giving out primary ranks, then 
     * this operation becomes nop. If we are however already deploying 
     * secondary ranks, then this routine decrements the internal state such 
     * that it eventually becomes "nothing left".
     */
    void updateStrategyState();
  public:
    /**
     * Constructor
     *
     * @param mpiRanksPerNode       Number of ranks per node.
     * @param waitTimeOutSec        How long shall the node wait for more
     *   messages dropping in before it starts to answer them.
     */
    SFCDiffusionNodePoolStrategy(int mpiRanksPerNode, int primaryMPIRanksPerNode, double waitTimeOutSec = 1e-2);
    virtual ~SFCDiffusionNodePoolStrategy();

    void setNodePoolTag(int tag) override;

    /**
     * Here's the magic to get the balancing right.
     */
    tarch::parallel::messages::WorkerRequestMessage extractElementFromRequestQueue(RequestQueue& queue) override;
    
    /**
     * Wait strategy for DeployingIdlePrimaryRanks:
     * 
     * In this mode, hasIdleNodes returns true iff there are still primary 
     * nodes available. Compared to the fair strategy, we thus may not stop
     * immediately if we have more requests than primary nodes. We might 
     * skip the waits too early - way too early.
     */
    void fillWorkerRequestQueue(RequestQueue& queue) override;
    void addNode(const tarch::parallel::messages::RegisterAtNodePoolMessage& node ) override;
    void removeNode( int rank ) override;

    /**
     * This routine is used by the NodePool to compare to the total number of
     * ranks. The calling operation therefrom derives whether all nodes have
     * successfully registered. So we may not only return the number of primary
     * ranks, but we have to return the total number of ranks.
     *
     * Once the node pool's status changes into NoNodesLeft, i.e. ranks have been
     * deployed successfully, this operation returns 0.
     */
    int getNumberOfIdleNodes() const override;
    void setNodeIdle( int rank ) override;
    int reserveNode(int forMaster) override;
    void reserveParticularNode(int rank) override;
    bool isRegisteredNode(int rank) const override;
    bool isIdleNode(int rank) const override;
    int getNumberOfRegisteredNodes() const override;
    std::string toString() const override;
    bool hasIdleNode(int forMaster) const override;
};

#endif
