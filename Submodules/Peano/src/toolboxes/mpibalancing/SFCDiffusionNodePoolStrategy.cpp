#include "mpibalancing/SFCDiffusionNodePoolStrategy.h"

#include "tarch/parallel/Node.h"
#include "tarch/compiler/CompilerSpecificSettings.h"
#include "tarch/parallel/NodePool.h"
#include "peano/utils/PeanoOptimisations.h"

#include "peano/utils/Globals.h"

#include <sstream>
#include <limits>
#include <map>


tarch::logging::Log mpibalancing::SFCDiffusionNodePoolStrategy::_log( "mpibalancing::SFCDiffusionNodePoolStrategy" );


mpibalancing::SFCDiffusionNodePoolStrategy::SFCDiffusionNodePoolStrategy(int mpiRanksPerNode, int primaryMPIRanksPerNode, double waitTimeOutSec):
  NodePoolStrategy(),
  _tag(-1),
  _nodes(),
  _waitTimeOut(waitTimeOutSec),
  _mpiRanksPerNode(mpiRanksPerNode),
  _primaryMPIRanksPerNode(primaryMPIRanksPerNode),
  _numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed(_primaryMPIRanksPerNode),
  _numberOfNodesToSkipPerRequestPlusOne(1),
  _nodePoolState(NodePoolState::DeployingIdlePrimaryRanks),
  _rankBlackList(),
  _rankPriorityList() {

  assertion2( mpiRanksPerNode>0, mpiRanksPerNode, primaryMPIRanksPerNode );
  assertion2( primaryMPIRanksPerNode<=mpiRanksPerNode, mpiRanksPerNode, primaryMPIRanksPerNode );
  assertion2( (mpiRanksPerNode%primaryMPIRanksPerNode==0), mpiRanksPerNode, primaryMPIRanksPerNode );

  _totalNumberOfRanks = tarch::parallel::Node::getInstance().getNumberOfNodes();
}


mpibalancing::SFCDiffusionNodePoolStrategy::~SFCDiffusionNodePoolStrategy() {
}


void mpibalancing::SFCDiffusionNodePoolStrategy::fillWorkerRequestQueue(RequestQueue& queue) {
  switch (_nodePoolState) {
    case NodePoolState::NoNodesLeft:
    {
      #ifdef Parallel
      // get the messages out of the system
      while (tarch::parallel::messages::WorkerRequestMessage::isMessageInQueue(_tag, true)) {
        tarch::parallel::messages::WorkerRequestMessage message;
        message.receive(MPI_ANY_SOURCE,_tag, true, tarch::parallel::messages::WorkerRequestMessage::ExchangeMode::Blocking);
        queue.push_back( message );
      }
      #endif
    }
    break;
    case NodePoolState::DeployingIdlePrimaryRanks:
    {
      #ifdef Parallel
      assertion( _tag >= 0 );
      std::clock_t waitTimeoutTimeStamp = clock() + static_cast<std::clock_t>(std::floor(_waitTimeOut * CLOCKS_PER_SEC));
      bool continueToWait = true;
      while ( continueToWait ) {
        while (tarch::parallel::messages::WorkerRequestMessage::isMessageInQueue(_tag, true)) {
          tarch::parallel::messages::WorkerRequestMessage message;
          message.receive(MPI_ANY_SOURCE,_tag, true, tarch::parallel::messages::WorkerRequestMessage::ExchangeMode::Blocking );
          queue.push_back( message );
          waitTimeoutTimeStamp = clock() + static_cast<std::clock_t>(std::floor(_waitTimeOut * CLOCKS_PER_SEC));
        }

       continueToWait =
         (static_cast<int>(queue.size()) < getNumberOfRegisteredNodes()-getNumberOfIdleNodes()) &&
         (clock() < waitTimeoutTimeStamp);
      }

      int totalNumberOfRequestedWorkers = 0;
      for (auto m: queue) {
        totalNumberOfRequestedWorkers += m.getNumberOfRequestedWorkers();
      }
      if (
        (totalNumberOfRequestedWorkers>getNumberOfIdlePrimaryRanks())
        &&
        !hasCompleteIdleNode()
      ) {
        _nodePoolState = NodePoolState::DeployingAlsoSecondaryRanksFirstSweep;
        logInfo(
          "fillWorkerRequestQueue(RequestQueue)",
          "have " << totalNumberOfRequestedWorkers <<
          " worker requests but only " << getNumberOfIdlePrimaryRanks() <<
          " primary node(s), i.e. code is running out of idle nodes. Start to deploy secondary nodes"
        );
      }
      else if (totalNumberOfRequestedWorkers>0) {
        configureForPrimaryRanksDelivery(totalNumberOfRequestedWorkers);
      }
      #endif
    }
    break;
    default: // we give out secondary notes
    {
      #ifdef Parallel
      // get the messages out of the system
      if (tarch::parallel::messages::WorkerRequestMessage::isMessageInQueue(_tag, true)) {
        while (tarch::parallel::messages::WorkerRequestMessage::isMessageInQueue(_tag, true)) {
          tarch::parallel::messages::WorkerRequestMessage message;
          message.receive(MPI_ANY_SOURCE,_tag, true, tarch::parallel::messages::WorkerRequestMessage::ExchangeMode::Blocking);
          queue.push_back( message );
        }
      }
      else {
        updateStrategyState();
      }
      #endif
    }
    break;
  }
}


void mpibalancing::SFCDiffusionNodePoolStrategy::configureForPrimaryRanksDelivery(int totalNumberOfRequestedWorkers) {
  assertion(totalNumberOfRequestedWorkers>0);

  _numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed = std::max(1,totalNumberOfRequestedWorkers / getNumberOfPhysicalNodes());
  _numberOfNodesToSkipPerRequestPlusOne   = std::max(1,getNumberOfPhysicalNodes() / totalNumberOfRequestedWorkers);
}



tarch::parallel::messages::WorkerRequestMessage mpibalancing::SFCDiffusionNodePoolStrategy::extractElementFromRequestQueue(RequestQueue& queue) {
  assertion( !queue.empty() );

  #ifdef Parallel
  queue.sort(
    [&]( const tarch::parallel::messages::WorkerRequestMessage& l, const tarch::parallel::messages::WorkerRequestMessage& r ) {
      return (l.getNumberOfRequestedWorkers() > r.getNumberOfRequestedWorkers())
          || (
              (l.getNumberOfRequestedWorkers() == r.getNumberOfRequestedWorkers()) 
              &&
              (_rankPriorityList.count(l.getSenderRank())>0) 
              && 
              (_rankPriorityList.count(r.getSenderRank())==0)
             );
    }
  );
  #endif

  tarch::parallel::messages::WorkerRequestMessage result = *queue.begin();
  queue.erase(queue.begin());

  #ifdef Parallel
  logInfo(
    "extractElementFromRequestQueue(RequestQueue)",
    "return " << result.toString() << " from sender rank " << result.getSenderRank() << " with " << queue.size() << " element(s) remaining in queue"
  );
  #endif

  return result;
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::isPrimaryMPIRank(int rank) const {
  const int NumberOfRanksPerPrimaryRank = _mpiRanksPerNode / _primaryMPIRanksPerNode;

  return rank % NumberOfRanksPerPrimaryRank==0;
}


void mpibalancing::SFCDiffusionNodePoolStrategy::addNode(const tarch::parallel::messages::RegisterAtNodePoolMessage& node) {
  #ifdef Parallel
  logTraceInWith1Argument( "addNode(...)", node.getSenderRank() );

  while (static_cast<int>(_nodes.size())<=node.getSenderRank()) {
    _nodes.push_back( NodePoolListEntry() );
  }

  const std::string name = tarch::parallel::StringTools::convert(node.getNodeName());
  assertion(_primaryMPIRanksPerNode!=0);
  _nodes[node.getSenderRank()] = NodePoolListEntry(
    name,
    isPrimaryMPIRank(node.getSenderRank())
  );

  logInfo( 
    "addNode(...)", 
    "added " << _nodes[node.getSenderRank()].toString() << 
    " as is-primary-rank=" << isPrimaryMPIRank(node.getSenderRank()) <<
    ". Primary MPI ranks per node=" << _primaryMPIRanksPerNode <<
    ", MPI ranks per node=" << _mpiRanksPerNode
  );

  logTraceOutWith1Argument( "addNode(...)", _nodes[node.getSenderRank()].toString() );
  #endif
}


void mpibalancing::SFCDiffusionNodePoolStrategy::removeNode( int rank ) {
  assertion( isRegisteredNode(rank) );
  _nodes[rank] = NodePoolListEntry(); // overwrite with invalidated entry
}


int mpibalancing::SFCDiffusionNodePoolStrategy::getNumberOfIdlePrimaryRanks() const {
  int result = 0;
  for (auto node: _nodes) {
    if (node.isIdlePrimaryRank()) {
      result++;
    }
  }
  return result;
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::hasIdleNode(int forMaster) const {
  switch (_nodePoolState) {
    case NodePoolState::DeployingIdlePrimaryRanks:
      {
        for (auto node: _nodes) {
          if (node.isIdlePrimaryRank()) {
            logDebug( "hasIdleNode(int)", "I have idle notes in mode DeployingIdlePrimaryRanks" );
            return true;
          }
        }
      }
      break;
    case NodePoolState::NoNodesLeft:
      break;
    default:
    {
      for (auto node: _nodes) {
        if (node.isIdlePrimaryRank() || node.isIdleSecondaryRank()) {
          logDebug( "hasIdleNode(int)", "I have idle notes in mode NoNodesLeft" );
          return true;
        }
      }
    }
    break;
  }
  logDebug( "hasIdleNode(int)", "No idle nodes left" );
  return false;
}


int mpibalancing::SFCDiffusionNodePoolStrategy::getNumberOfIdleNodes() const {
  if (_nodePoolState==NodePoolState::NoNodesLeft) {
    return 0;
  }
  else {
    int result = 0;
    for (auto node: _nodes) {
      if (node.isIdlePrimaryRank() || node.isIdleSecondaryRank()) {
        result++;
      }
    }
    return result;
  }
}


void mpibalancing::SFCDiffusionNodePoolStrategy::setNodeIdle( int rank ) {
  assertion( isRegisteredNode(rank) );
  _nodes[rank].deActivate();
  if (_nodePoolState!=NodePoolState::DeployingIdlePrimaryRanks) {
    logInfo( "setNodeIdle(int)", "reset node pool state to DeployingIdlePrimaryRanks as rank " << rank << " registered as idle" );
    _nodePoolState = NodePoolState::DeployingIdlePrimaryRanks;
  }
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::isRegisteredNode(int rank) const {
  return static_cast<int>(_nodes.size())>rank
      && _nodes[rank].isRegistered();
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::isIdleNode(int rank) const {
  return (static_cast<int>(_nodes.size())>rank)
      && (_nodes[rank].isIdlePrimaryRank() || _nodes[rank].isIdleSecondaryRank());
}


int mpibalancing::SFCDiffusionNodePoolStrategy::getNumberOfRegisteredNodes() const {
  int result = 0;
  for (auto node: _nodes) {
    if (node.isRegistered()) {
      result++;
    }
  }
  return result;
}

std::string mpibalancing::SFCDiffusionNodePoolStrategy::nodePoolStateToString() const {
  std::ostringstream result;

  switch (_nodePoolState) {
    case NodePoolState::DeployingIdlePrimaryRanks:
      result << "deploying-idle-ranks";
      break;
    case NodePoolState::NoNodesLeft:
      result << "no-nodes-left";
      break;
    default:
      result << "deploying-also-secondary-ranks";
      break;
  }

  return result.str();
}


std::string mpibalancing::SFCDiffusionNodePoolStrategy::toString() const {
  std::ostringstream result;
  result << "(node-pool-state:" << nodePoolStateToString();
  for (
    NodeContainer::const_iterator p = _nodes.begin();
    p != _nodes.end();
    p++
  ) {
    result << ",";
    p->toString(result);
  }
  result << "(";
  return result.str();
}


void mpibalancing::SFCDiffusionNodePoolStrategy::reserveParticularNode(int rank) {
  assertion( isRegisteredNode(rank) );
  assertion( _nodes[rank].isIdlePrimaryRank() || _nodes[rank].isIdleSecondaryRank() );

  _nodes[rank].activate();
}


int mpibalancing::SFCDiffusionNodePoolStrategy::getNumberOfPhysicalNodes() const {
  return std::max(static_cast<int>(_nodes.size()) / _mpiRanksPerNode,1);
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::hasCompleteIdleNode() const {
  for (int node=0; node<getNumberOfPhysicalNodes(); node++) {
    bool allPrimaryRanksIdle = true;
    for (int i=0; i<_mpiRanksPerNode; i++) {
      const int rank = node*_mpiRanksPerNode + i;
      if (isPrimaryMPIRank(rank)) {
        allPrimaryRanksIdle &= isIdleNode(rank);
      }
    }
    if (allPrimaryRanksIdle) {
      return true;
    }
  }

  return false;
}


int mpibalancing::SFCDiffusionNodePoolStrategy::deployIdlePrimaryRank(int forMaster) {
  assertion(_primaryMPIRanksPerNode>0);

  const int NumberOfRanksPerPrimaryRank = _mpiRanksPerNode / _primaryMPIRanksPerNode;

  const int FirstNodeToLookAt = _numberOfNodesToSkipPerRequestPlusOne==1 ? forMaster/_mpiRanksPerNode : forMaster/_mpiRanksPerNode+1;

  int firstLoopRank = 0;
  int searchOffset  = forMaster+NumberOfRanksPerPrimaryRank;

  for (int j=0; j<getNumberOfPhysicalNodes(); j+=_numberOfNodesToSkipPerRequestPlusOne) {
    for (int i=0; i<_numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed;i++) {
      firstLoopRank = ( (FirstNodeToLookAt+j)*_mpiRanksPerNode+i*NumberOfRanksPerPrimaryRank+searchOffset) % _totalNumberOfRanks;
      if (_nodes[firstLoopRank].isIdlePrimaryRank()) {
        _nodes[firstLoopRank].activate();
        return firstLoopRank;
      }
    }
    searchOffset = searchOffset>=NumberOfRanksPerPrimaryRank ? searchOffset-NumberOfRanksPerPrimaryRank : searchOffset;
  }

  logInfo(
    "deployIdlePrimaryRank(int)",
    "can't serve request from rank " << forMaster << " with the constraint of " <<
    _numberOfPrimaryRanksPerNodeThatAreCurrentlyDeployed << " primary ranks per node that should currently be deployed. Fallback to all primary ranks"
  );

  // Fallback
  for (int i=0; i<static_cast<int>(_nodes.size()); i++) {
    const int rank = (firstLoopRank + i) % _totalNumberOfRanks;
    if (_nodes[rank].isIdlePrimaryRank()) {
      _nodes[rank].activate();
      return rank;
    }
  }

  assertionMsg(false, "should never be hit");
  return tarch::parallel::NodePool::NoFreeNodesMessage;
}


int mpibalancing::SFCDiffusionNodePoolStrategy::deployIdleSecondaryRank(int forMaster) {
  assertion1( !_nodes[forMaster].isIdlePrimaryRank(), forMaster );
  assertion1( !_nodes[forMaster].isIdleSecondaryRank(), forMaster );

  const int firstRankOnMastersNode = (forMaster / _mpiRanksPerNode) * _mpiRanksPerNode;
  const int searchStart = (forMaster - firstRankOnMastersNode < _mpiRanksPerNode/2) ? firstRankOnMastersNode - _mpiRanksPerNode : firstRankOnMastersNode + _mpiRanksPerNode;
  if (
    searchStart < 0
    ||  
    searchStart > _totalNumberOfRanks-_mpiRanksPerNode
  ) {
    logInfo(
      "deployIdleSecondaryRank(int)",
      "can't serve " << forMaster << " as it is tail or head along SFC"
    );

    return tarch::parallel::NodePool::NoFreeNodesMessage;
  }
  

  for (int rank=searchStart; rank<searchStart+_mpiRanksPerNode; rank++) {
    if (
      (_nodes[rank].isIdlePrimaryRank() || _nodes[rank].isIdleSecondaryRank())
      &&
      _rankBlackList.count(rank)==0
    ) {
      logInfo(
        "deployIdleSecondaryRank(int)",
        "seems that rank " << rank << " is suitable and available. Searched a range starting from " << searchStart 
      );

      _nodes[rank].activate();
      _nodePoolState = NodePoolState::DeployingAlsoSecondaryRanksFirstSweep;
      haveReservedSecondaryRank(forMaster,rank);
      return rank;
    }
  }
  logInfo(
    "deployIdleSecondaryRank(int)",
    "can't serve " << forMaster << " as no free nodes found. Searched a range starting from " << searchStart << " without success"
  );

  return tarch::parallel::NodePool::NoFreeNodesMessage;
}


void mpibalancing::SFCDiffusionNodePoolStrategy::updateStrategyState() {
  if (
    static_cast<int>(_nodePoolState)<=static_cast<int>(NodePoolState::DeployingAlsoSecondaryRanksFirstSweep)
    &&
    static_cast<int>(_nodePoolState) >static_cast<int>(NodePoolState::DeployingAlsoSecondaryRanksLastSweep)
  ) {
    _nodePoolState = static_cast<NodePoolState>( static_cast<int>(_nodePoolState)-1 );
    logInfo("updateStrategyState()",
      "reduce internal state to " << static_cast<int>(_nodePoolState) <<
      " with DeployingAlsoSecondaryRanksFirstSweep=" << static_cast<int>(NodePoolState::DeployingAlsoSecondaryRanksFirstSweep) <<
      " and DeployingAlsoSecondaryRanksLastSweep=" << static_cast<int>(NodePoolState::DeployingAlsoSecondaryRanksLastSweep)
    );
    if (getNumberOfIdleNodes()==0) {
      logInfo(
        "updateStrategyState()",
        "running out of secondary ranks, too. Stop to deliver MPI ranks"
      );
      _nodePoolState = NodePoolState::NoNodesLeft;
    }
  }
}


int mpibalancing::SFCDiffusionNodePoolStrategy::reserveNode(int forMaster) {
  switch (_nodePoolState) {
    case NodePoolState::DeployingIdlePrimaryRanks:
      return deployIdlePrimaryRank(forMaster);
      break;
    case NodePoolState::NoNodesLeft:
      logInfo(
        "reserveNode(int)",
        "deny request from " << forMaster << " as no more ranks left"
      );
      return tarch::parallel::NodePool::NoFreeNodesMessage;
      break;
    case NodePoolState::DeployingAlsoSecondaryRanksLastSweep:
      _nodePoolState = NodePoolState::NoNodesLeft;
      logInfo(
        "reserveNode(int)",
        "switch to NoNodesLeft state"
      );
      return deployIdleSecondaryRank(forMaster);
    default:
      return deployIdleSecondaryRank(forMaster);
      break;
  }

  assertion1(false,toString());
  return tarch::parallel::NodePool::NoFreeNodesMessage;
}


void mpibalancing::SFCDiffusionNodePoolStrategy::haveReservedSecondaryRank(int masterRank, int workerRank) {
  const int baseRankOfMasterNode = (masterRank / _mpiRanksPerNode) * _mpiRanksPerNode;
  const int lastRankOnMasterNode = baseRankOfMasterNode + _mpiRanksPerNode;
  logInfo(
    "haveReservedSecondaryRank(int)",
    "blacklist ranks " << baseRankOfMasterNode << "-" << (lastRankOnMasterNode-1) <<
    " as master " << masterRank << " has aquired new worker through diffusion"
  );

  for (int i=baseRankOfMasterNode; i<lastRankOnMasterNode; i++) {
    _rankBlackList.insert(i);
    if (_rankPriorityList.find(i) != _rankPriorityList.end()) {
      _rankPriorityList.erase(i);
    }
  }

  const int baseRankOfWorkerNode = (workerRank / _mpiRanksPerNode) * _mpiRanksPerNode;
  const int lastRankOnWorkerNode = baseRankOfWorkerNode + _mpiRanksPerNode;

  for (int i=baseRankOfWorkerNode; i<lastRankOnWorkerNode; i++) {
    _rankPriorityList.insert(i);
  }
}


void mpibalancing::SFCDiffusionNodePoolStrategy::setNodePoolTag(int tag) {
  _tag = tag;
}


mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::NodePoolListEntry():
  _state(State::Undef),
  _name("undef") {
}


mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::NodePoolListEntry(
  const std::string& name,
  bool isPrimaryNode
  ):
  _state( isPrimaryNode ? State::WorkingPrimaryRank : State::WorkingSecondaryRank ),
  _name(name) {
}


mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::~NodePoolListEntry() {
}


std::string mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::getNodeName() const {
  return _name;
}


std::string mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::toString() const {
  std::ostringstream out;
  toString(out);
  return out.str();
}


void mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::toString(std::ostream& out) const {
  out << "(state:";
  switch (_state) {
    case State::Undef:                 out << "undef";                   break;
    case State::WorkingPrimaryRank:    out << "working-primary-rank";    break;
    case State::WorkingSecondaryRank:  out << "working-secondary-rank";  break;
    case State::IdlePrimaryRank:       out << "idle-primary-rank";       break;
    case State::IdleSecondaryRank:     out << "idle-secondary-rank";     break;
  }
  out << ",name:" << _name << ")";
}


void mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::activate() {
  assertion1( _state==State::IdlePrimaryRank || _state==State::IdleSecondaryRank, toString() );
  _state = _state==State::IdlePrimaryRank ? State::WorkingPrimaryRank : State::WorkingSecondaryRank;
}


void mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::deActivate() {
  assertion( _state==State::WorkingPrimaryRank || _state==State::WorkingSecondaryRank );
  _state = _state==State::WorkingPrimaryRank ? State::IdlePrimaryRank : State::IdleSecondaryRank;
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::isRegistered() const {
  return _state!=State::Undef;
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::isIdlePrimaryRank() const {
  return _state==State::IdlePrimaryRank;
}


bool mpibalancing::SFCDiffusionNodePoolStrategy::NodePoolListEntry::isIdleSecondaryRank() const {
  return _state==State::IdleSecondaryRank;
}
