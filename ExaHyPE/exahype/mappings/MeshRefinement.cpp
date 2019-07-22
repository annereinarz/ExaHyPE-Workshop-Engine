/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 All rights reserved.
 *
 * The project has received funding from the European Union's Horizon 
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/
 
#include "exahype/mappings/MeshRefinement.h"

#include "peano/utils/Globals.h"
#include "peano/utils/Loop.h"

#include "peano/datatraversal/autotuning/Oracle.h"
#include "peano/datatraversal/TaskSet.h"

#include "peano/grid/aspects/VertexStateAnalysis.h"

#include "peano/parallel/loadbalancing/Oracle.h"

#include "tarch/la/VectorScalarOperations.h"

#include "tarch/multicore/Lock.h"

#include "multiscalelinkedcell/HangingVertexBookkeeper.h"


#include "exahype/VertexOperations.h"

#include "exahype/solvers/LimitingADERDGSolver.h"

#include "exahype/mappings/RefinementStatusSpreading.h"

#include <sstream>

bool exahype::mappings::MeshRefinement::DynamicLoadBalancing    = false;

bool exahype::mappings::MeshRefinement::IsFirstIteration        = true;
bool exahype::mappings::MeshRefinement::IsInitialMeshRefinement = true;

tarch::logging::Log exahype::mappings::MeshRefinement::_log("exahype::mappings::MeshRefinement");

tarch::multicore::BooleanSemaphore exahype::mappings::MeshRefinement::BoundarySemaphore;

/**
 * @todo Please tailor the parameters to your mapping's properties.
 */
peano::CommunicationSpecification
exahype::mappings::MeshRefinement::communicationSpecification() const {
  return peano::CommunicationSpecification(
      peano::CommunicationSpecification::ExchangeMasterWorkerData::
          SendDataAndStateBeforeFirstTouchVertexFirstTime,
      peano::CommunicationSpecification::ExchangeWorkerMasterData::
          SendDataAndStateAfterLastTouchVertexLastTime,
      true);
}

peano::MappingSpecification
exahype::mappings::MeshRefinement::touchVertexFirstTimeSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::WholeTree,
      peano::MappingSpecification::AvoidFineGridRaces,true);
}

peano::MappingSpecification
exahype::mappings::MeshRefinement::touchVertexLastTimeSpecification(int level) const {
  return Vertex::getNeighbourMergeSpecification(level);
}

peano::MappingSpecification
exahype::mappings::MeshRefinement::enterCellSpecification(int level) const {
  if ( exahype::solvers::Solver::SpawnAMRBackgroundJobs ) {
    return peano::MappingSpecification(
        peano::MappingSpecification::WholeTree,
        peano::MappingSpecification::Serial,true);
  } else {
    return peano::MappingSpecification(
        peano::MappingSpecification::WholeTree,
        peano::MappingSpecification::AvoidFineGridRaces,true);
  }
}

peano::MappingSpecification
exahype::mappings::MeshRefinement::leaveCellSpecification(int level) const {
  if ( exahype::solvers::Solver::SpawnAMRBackgroundJobs ) {
    return peano::MappingSpecification(
        peano::MappingSpecification::WholeTree,
        peano::MappingSpecification::Serial,true);
  } else {
    return peano::MappingSpecification(
        peano::MappingSpecification::WholeTree,
        peano::MappingSpecification::AvoidFineGridRaces,true);
  }
}

// Nop.
peano::MappingSpecification
exahype::mappings::MeshRefinement::ascendSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::AvoidCoarseGridRaces,false);
}
peano::MappingSpecification
exahype::mappings::MeshRefinement::descendSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::AvoidCoarseGridRaces,false);
}

#if defined(SharedMemoryParallelisation)
exahype::mappings::MeshRefinement::MeshRefinement(const MeshRefinement& masterThread):
  _allSolversAttainedStableState(masterThread._allSolversAttainedStableState),
  _stateCopy(masterThread._stateCopy) {
  // do nothing
}
#endif

#if defined(SharedMemoryParallelisation)
void exahype::mappings::MeshRefinement::mergeWithWorkerThread(
    const MeshRefinement& workerThread) {
  _allSolversAttainedStableState &= workerThread._allSolversAttainedStableState;
}
#endif

void exahype::mappings::MeshRefinement::beginIteration( exahype::State& solverState ) {
  _stateCopy = solverState;

  tarch::multicore::jobs::Job::setMaxNumberOfRunningBackgroundThreads(0); // during the traversal only have zero/one consumer thread running

  if ( IsInitialMeshRefinement || DynamicLoadBalancing ) {
    peano::parallel::loadbalancing::Oracle::getInstance().activateLoadBalancing(true);
  }

  // mesh refinement convergence check
  _allSolversAttainedStableState =
      !exahype::mappings::MeshRefinement::IsFirstIteration &&
      !solverState.isInvolvedInJoinOrFork();

  // reset
  _verticalExchangeOfSolverDataRequired = false;

  #ifdef Parallel
  if (! MetadataHeap::getInstance().validateThatIncomingJoinBuffersAreEmpty() ) {
      exit(-1);
  }
  #endif
}

void exahype::mappings::MeshRefinement::endIteration(exahype::State& solverState) {
  logTraceInWith1Argument("endIteration(State)", solverState);

  // update the solver state
  solverState.setAllSolversAttainedStableState(_allSolversAttainedStableState);  // merge the local values
  solverState.setVerticalExchangeOfSolverDataRequired(_verticalExchangeOfSolverDataRequired);

  if ( tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    #ifndef TrackGridStatistics
    #error Compiler flag TrackGridStatistics must be defined!
    #endif
    solverState.setAllSolversAttainedStableState( // check if we actually reached the solver's grids
        solverState.getAllSolversAttainedStableState() &&
        solverState.getMaxLevel()>=exahype::solvers::Solver::getFinestUniformMeshLevelOfAllSolvers()); // max level only available in endIteration(..>)
  }

  exahype::mappings::MeshRefinement::IsFirstIteration=false;

  // background threads
  exahype::solvers::Solver::ensureAllJobsHaveTerminated(exahype::solvers::Solver::JobType::AMRJob);

  logTraceOutWith1Argument("endIteration(State)", solverState);
}

void exahype::mappings::MeshRefinement::refineSafely(
    exahype::Vertex&                              fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>&  fineGridH,
    int                                           fineGridLevel,
    bool                                          isCalledByCreationalEvent) const {
  if ( fineGridVertex.getRefinementControl()==Vertex::Records::Unrefined ) {
    switch ( _stateCopy.mayRefine(isCalledByCreationalEvent,fineGridLevel) ) {
    case State::RefinementAnswer::DontRefineYet:
      break;
    case State::RefinementAnswer::Refine:
      fineGridVertex.refine();
      break;
    case State::RefinementAnswer::EnforceRefinement:
      #ifdef Parallel // TODO hotfix
      fineGridVertex.refine();
      #else
      fineGridVertex.enforceRefine();
      #endif
      break;
    }
  }
}

void exahype::mappings::MeshRefinement::touchVertexLastTime(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  exahype::solvers::Solver::RefinementControl refinementControl =
      fineGridVertex.evaluateRefinementCriterion(
      fineGridX,
      coarseGridVerticesEnumerator.getLevel()+1,
      fineGridH,
      !_stateCopy.getMeshRefinementIsInRefiningMode());

  if ( refinementControl==exahype::solvers::Solver::RefinementControl::Refine ) {
    refineSafely(fineGridVertex,fineGridH,coarseGridVerticesEnumerator.getLevel()+1,false);
  } else if (
      refinementControl==exahype::solvers::Solver::RefinementControl::Erase
      &&
      !fineGridVertex.isHangingNode()
      &&
      fineGridVertex.isInside()
      && // otherwise, we compete with ensureRegularityAlongBoundary
      fineGridVertex.getRefinementControl()==
          Vertex::Records::RefinementControl::Refined
  ) {
  // TODO  fineGridVertex.erase(); // TODO(Dominic): vertex erasing is not well understood yet
  }
}


void exahype::mappings::MeshRefinement::createBoundaryVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  logTraceInWith6Arguments("createBoundaryVertex(...)", fineGridVertex,
                           fineGridX, fineGridH,
                           coarseGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfVertex);

  if (
      fineGridVertex.evaluateRefinementCriterion(
      fineGridX,
      coarseGridVerticesEnumerator.getLevel()+1,
      fineGridH,
      !_stateCopy.getMeshRefinementIsInRefiningMode())
      == exahype::solvers::Solver::RefinementControl::Refine
  ) {
    refineSafely(fineGridVertex,fineGridH,coarseGridVerticesEnumerator.getLevel()+1,true);
  }

  logTraceOutWith1Argument("createBoundaryVertex(...)", fineGridVertex);
}


void exahype::mappings::MeshRefinement::createInnerVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  logTraceInWith6Arguments("createInnerVertex(...)", fineGridVertex, fineGridX,
                           fineGridH, coarseGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfVertex);
  if (
      fineGridVertex.evaluateRefinementCriterion(
          fineGridX,
          coarseGridVerticesEnumerator.getLevel()+1,
          fineGridH,
          !_stateCopy.getMeshRefinementIsInRefiningMode())
      == exahype::solvers::Solver::RefinementControl::Refine
  ) {
    refineSafely(fineGridVertex,fineGridH,coarseGridVerticesEnumerator.getLevel()+1,true);
  }

  logTraceOutWith1Argument("createInnerVertex(...)", fineGridVertex);
}


void exahype::mappings::MeshRefinement::createCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith4Arguments("createCell(...)", fineGridCell,
                           fineGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfCell);

  // do nothing
  fineGridCell.setCellDescriptionsIndex(
      multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);

  logTraceOutWith1Argument("createCell(...)", fineGridCell);
}

void exahype::mappings::MeshRefinement::createHangingVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::MeshRefinement::touchVertexFirstTime(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  logTraceInWith6Arguments("touchVertexFirstTime(...)", fineGridVertex,
                           fineGridX, fineGridH,
                           coarseGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfVertex);

  fineGridVertex.mergeOnlyNeighboursMetadata(
      exahype::State::AlgorithmSection::MeshRefinement,fineGridX,fineGridH,true);

  logTraceOutWith1Argument("touchVertexFirstTime(...)", fineGridVertex);
}

void exahype::mappings::MeshRefinement::ensureRegularityAlongBoundary(
    exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator) const {
  if (
      !_stateCopy.getMeshRefinementIsInRefiningMode()
      &&
      peano::grid::aspects::VertexStateAnalysis::isOneVertexBoundary(
          fineGridVertices,fineGridVerticesEnumerator)
  ) {
    bool oneInnerVertexIsRefined = false;
    bool noInnerVertexIsRefined  = true;
    dfor2(v)
      oneInnerVertexIsRefined |=
          fineGridVertices[fineGridVerticesEnumerator(v)].isInside() &&
          fineGridVertices[fineGridVerticesEnumerator(v)].getRefinementControl()
          ==exahype::Vertex::Records::RefinementControl::Refined;
      noInnerVertexIsRefined &=
          !fineGridVertices[fineGridVerticesEnumerator(v)].isInside() ||
          fineGridVertices[fineGridVerticesEnumerator(v)].getRefinementControl()
          ==exahype::Vertex::Records::RefinementControl::Unrefined;
    enddforx

	// @todo
    if (oneInnerVertexIsRefined) {
      dfor2(v)
        tarch::multicore::Lock lock(BoundarySemaphore);
        if (
            fineGridVertices[fineGridVerticesEnumerator(v)].isBoundary()
            &&
            fineGridVertices[fineGridVerticesEnumerator(v)].getRefinementControl()==
                exahype::Vertex::Records::RefinementControl::Unrefined
            &&
            fineGridVertices[fineGridVerticesEnumerator(v)].evaluateRefinementCriterion(
                fineGridVerticesEnumerator.getVertexPosition(vScalar),
                fineGridVerticesEnumerator.getLevel(),
                fineGridVerticesEnumerator.getCellSize(),
                false)
            !=exahype::solvers::Solver::RefinementControl::Erase
			&&
        	_stateCopy.mayRefine(true,fineGridVerticesEnumerator.getLevel())
        ) {
          fineGridVertices[fineGridVerticesEnumerator(v)].refine();
        }
        lock.free();
      enddforx
    } else if (
        noInnerVertexIsRefined
    ) {
      dfor2(v)
        tarch::multicore::Lock lock(BoundarySemaphore);
        if (
            fineGridVertices[fineGridVerticesEnumerator(v)].isBoundary()
            &&
            fineGridVertices[fineGridVerticesEnumerator(v)].getRefinementControl()==
                exahype::Vertex::Records::RefinementControl::Refined
            &&
            fineGridVertices[fineGridVerticesEnumerator(v)].evaluateRefinementCriterion(
              fineGridVerticesEnumerator.getVertexPosition(vScalar),
              fineGridVerticesEnumerator.getLevel(),
              fineGridVerticesEnumerator.getCellSize(),
              false)
            ==exahype::solvers::Solver::RefinementControl::Erase

        ) {
          // TODO
          // fineGridVertices[fineGridVerticesEnumerator(v)].erase(); // TODO(Dominic): vertex erasing is not well understood yet
        }
        lock.free();
      enddforx
    }
  }
}

void exahype::mappings::MeshRefinement::enterCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith4Arguments("enterCell(...)", fineGridCell,
                           fineGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfCell);

  assertion(fineGridCell.isInside());

  const bool firstMeshRefinementIteration =
      #ifdef Parallel
      !exahype::State::isNewWorkerDueToForkOfExistingDomain() &&
      #endif
      exahype::mappings::MeshRefinement::IsFirstIteration;     // It has to be the first overall iteration

  for (unsigned int solverNumber=0; solverNumber<exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
    auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
    if ( solver->hasRequestedAnyMeshRefinement() ) {
      const bool newComputeCell =
          !firstMeshRefinementIteration && // skip in first iteration
          solver->progressMeshRefinementInEnterCell(
              fineGridCell,
              fineGridVertices,
              fineGridVerticesEnumerator,
              coarseGridCell,
              coarseGridVerticesEnumerator,
              solverNumber,
              _stateCopy.getMeshRefinementIsInRefiningMode());

      // Synchronise time stepping, adjust the solution, evaluate refinement criterion if required
      if (
          (firstMeshRefinementIteration && fineGridCell.isInitialised()) ||
          newComputeCell
      ) {
        solvers::Solver::CellInfo cellInfo = fineGridCell.createCellInfo();
        solver->adjustSolutionDuringMeshRefinement(solverNumber,cellInfo); // TODO(Dominic): Consider to merge this into the last loop
      }
    }
  }

  if ( fineGridCell.isInitialised() ) {
    solvers::Solver::CellInfo cellInfo = fineGridCell.createCellInfo();
    Cell::resetNeighbourMergePerformedFlags(cellInfo,fineGridVertices,fineGridVerticesEnumerator);
    // shutdown metadata for empty cells (no cell descriptions)
    if ( fineGridCell.isEmpty() ) {
      fineGridCell.shutdownMetaDataAndResetCellDescriptionsIndex();
    }
  }

  logTraceOutWith1Argument("enterCell(...)", fineGridCell);
}

void exahype::mappings::MeshRefinement::leaveCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith4Arguments("leaveCell(...)", fineGridCell,
                           fineGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfCell);
 
  for (unsigned int solverNumber=0; solverNumber<exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
    auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
    if ( solver->hasRequestedAnyMeshRefinement() ) {
      solver->progressMeshRefinementInLeaveCell(
          fineGridCell,
          fineGridVertices,
          fineGridVerticesEnumerator,
          coarseGridCell,
          fineGridPositionOfCell,
          solverNumber,
          _stateCopy.getMeshRefinementIsInRefiningMode());

      _allSolversAttainedStableState &=
          solver->attainedStableState(
              fineGridCell,
              fineGridVertices,
              fineGridVerticesEnumerator,
              solverNumber,
              _stateCopy.getMeshRefinementIsInRefiningMode());
    }
  }

  ensureRegularityAlongBoundary(fineGridVertices,fineGridVerticesEnumerator);

  logTraceOutWith1Argument("leaveCell(...)", fineGridCell);
}

void exahype::mappings::MeshRefinement::destroyCell(
    const exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  if ( fineGridCell.isInitialised() ) {
    fineGridCell.shutdownMetaData();
  }
}

#ifdef Parallel
void exahype::mappings::MeshRefinement::mergeWithNeighbour(
    exahype::Vertex& vertex, const exahype::Vertex& neighbour, int fromRank,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH, int level) {
  logTraceInWith6Arguments("mergeWithNeighbour(...)", vertex, neighbour,
                           fromRank, fineGridX, fineGridH, level);

  if ( !IsFirstIteration ) {
    vertex.mergeOnlyWithNeighbourMetadata(fromRank,fineGridX,fineGridH,level,exahype::State::AlgorithmSection::MeshRefinement,true);
  }

  logTraceOut("mergeWithNeighbour(...)");
}

void exahype::mappings::MeshRefinement::prepareSendToNeighbour(
    exahype::Vertex& vertex, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  logTraceInWith5Arguments("prepareSendToNeighbour(...)", vertex,
                           toRank, x, h, level);

  vertex.sendOnlyMetadataToNeighbour(toRank,x,h,level,true);

  logTraceOut("prepareSendToNeighbour(...)");
}

bool exahype::mappings::MeshRefinement::prepareSendToWorker(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
    int worker) {
  logTraceIn( "prepareSendToWorker(...)" );

  if (
      !exahype::State::isForkingRank(worker) &&
      fineGridCell.hasToCommunicate(fineGridVerticesEnumerator.getLevel() )
  ) {
    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      solver->progressMeshRefinementInPrepareSendToWorker(
          worker, fineGridCell, fineGridVertices,fineGridVerticesEnumerator,
          coarseGridCell, coarseGridVerticesEnumerator,
          solverNumber);
    }

    // send out the cell descriptions
    const int cellDescriptionsIndex = fineGridCell.getCellDescriptionsIndex();
    _verticalExchangeOfSolverDataRequired |=
        exahype::solvers::ADERDGSolver::sendCellDescriptions(worker,cellDescriptionsIndex,
            false/* !(send data from worker side) */,
            peano::heap::MessageType::MasterWorkerCommunication,
            fineGridVerticesEnumerator.getCellCenter(),fineGridVerticesEnumerator.getLevel());
    exahype::solvers::FiniteVolumesSolver::sendCellDescriptions(worker,cellDescriptionsIndex,
        peano::heap::MessageType::MasterWorkerCommunication,
        fineGridVerticesEnumerator.getCellCenter(),fineGridVerticesEnumerator.getLevel());

    // possibly send out data
    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      const int element = solver->tryGetElement(cellDescriptionsIndex,solverNumber);
      if ( element!=exahype::solvers::Solver::NotFound ) {
        solver->sendDataToWorkerIfProlongating(
            worker, 
            cellDescriptionsIndex,element,
            fineGridVerticesEnumerator.getCellCenter(),
            fineGridVerticesEnumerator.getLevel());
      }
    }
  }

  logTraceOutWith1Argument( "prepareSendToWorker(...)", true );
  return true;
}

// TODO(Dominic): Add to docu: see documentation in peano/pdt/stdtemplates/MappingHeader.template
// on function receiveDataFromMaster
void exahype::mappings::MeshRefinement::receiveDataFromMaster(
    exahype::Cell& receivedCell, exahype::Vertex* receivedVertices,
    const peano::grid::VertexEnumerator& receivedVerticesEnumerator,
    exahype::Vertex* const receivedCoarseGridVertices,
    const peano::grid::VertexEnumerator& receivedCoarseGridVerticesEnumerator,
    exahype::Cell& receivedCoarseGridCell,
    exahype::Vertex* const workersCoarseGridVertices,
    const peano::grid::VertexEnumerator& workersCoarseGridVerticesEnumerator,
    exahype::Cell& workersCoarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceIn( "receiveDataFromMaster(...)" );

  receivedCell.setCellDescriptionsIndex(
      multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
  if (
      !exahype::State::isNewWorkerDueToForkOfExistingDomain() &&
      receivedCell.hasToCommunicate( receivedVerticesEnumerator.getLevel())
  ) {
    receivedCell.setupMetaData();
    exahype::solvers::ADERDGSolver::receiveCellDescriptions(
        tarch::parallel::NodePool::getInstance().getMasterRank(),
        receivedCell,
        peano::heap::MessageType::MasterWorkerCommunication,
        receivedVerticesEnumerator.getCellCenter(),
        receivedVerticesEnumerator.getLevel());
    exahype::solvers::FiniteVolumesSolver::receiveCellDescriptions(
        tarch::parallel::NodePool::getInstance().getMasterRank(),
        receivedCell, // Two step approach
        peano::heap::MessageType::MasterWorkerCommunication,
        receivedVerticesEnumerator.getCellCenter(),
        receivedVerticesEnumerator.getLevel());

    const int receivedCellDescriptionsIndex = receivedCell.getCellDescriptionsIndex();
    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      const int receivedElement = solver->tryGetElement(receivedCellDescriptionsIndex,solverNumber);
      if ( receivedElement!=exahype::solvers::Solver::NotFound ) {
        solver->receiveDataFromMasterIfProlongating(
            tarch::parallel::NodePool::getInstance().getMasterRank(),
            receivedCellDescriptionsIndex,receivedElement,
            receivedVerticesEnumerator.getCellCenter(),
            receivedVerticesEnumerator.getLevel());
      }
    }
  }

  logTraceOut( "receiveDataFromMaster(...)" );
}

void exahype::mappings::MeshRefinement::mergeWithWorker(
    exahype::Cell& localCell, const exahype::Cell& receivedMasterCell,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  logTraceInWith2Arguments( "mergeWithWorker(...)", localCell.toString(), receivedMasterCell.toString() );

  if ( receivedMasterCell.isInitialised() ) { // we do not receive anything here
    // Do not merge anything if our worker is on a newly forked part of the mesh
    if ( !exahype::State::isNewWorkerDueToForkOfExistingDomain() ) {
      if ( !localCell.isInitialised() ) { // simply copy the index
        localCell.setupMetaData();
      }

      const int receivedCellDescriptionsIndex = receivedMasterCell.getCellDescriptionsIndex();
      for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
        auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
        const int receivedElement = solver->tryGetElement(receivedCellDescriptionsIndex,solverNumber);
        if ( receivedElement!=exahype::solvers::Solver::NotFound  ) {
          bool newComputeCell =
              solver->progressMeshRefinementInMergeWithWorker(
                  localCell.getCellDescriptionsIndex(),
                  receivedCellDescriptionsIndex,receivedElement);

          if ( newComputeCell ) {
            solvers::Solver::CellInfo cellInfo = localCell.createCellInfo();
            solver->adjustSolutionDuringMeshRefinement(solverNumber,cellInfo);
          }
        }
      }
      if ( localCell.isInitialised() && localCell.isEmpty() ) {
        localCell.shutdownMetaDataAndResetCellDescriptionsIndex();
      }
    }
    receivedMasterCell.shutdownMetaData();
  }

  logTraceOutWith1Argument( "mergeWithWorker(...)", localCell.toString() );
}



void exahype::mappings::MeshRefinement::prepareSendToMaster(
    exahype::Cell& localCell, exahype::Vertex* vertices,
    const peano::grid::VertexEnumerator& verticesEnumerator,
    const exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    const exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith2Arguments( "prepareSendToMaster(...)", localCell, verticesEnumerator.toString() );

  if ( localCell.hasToCommunicate( verticesEnumerator.getLevel() ) ) {
    exahype::solvers::ADERDGSolver::sendCellDescriptions(
        tarch::parallel::NodePool::getInstance().getMasterRank(),
        localCell.getCellDescriptionsIndex(),true/* send data from worker side*/,
        peano::heap::MessageType::MasterWorkerCommunication,
        verticesEnumerator.getCellCenter(),
        verticesEnumerator.getLevel());
    exahype::solvers::FiniteVolumesSolver::sendCellDescriptions(
        tarch::parallel::NodePool::getInstance().getMasterRank(),
        localCell.getCellDescriptionsIndex(),
        peano::heap::MessageType::MasterWorkerCommunication,
        verticesEnumerator.getCellCenter(),
        verticesEnumerator.getLevel()); // make collective

    for (unsigned int solverNumber = 0; solverNumber < exahype::solvers::RegisteredSolvers.size(); ++solverNumber) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      const int element = solver->tryGetElement(localCell.getCellDescriptionsIndex(),solverNumber);
      if ( element!=exahype::solvers::Solver::NotFound ) {
        solver->progressMeshRefinementInPrepareSendToMaster(
            tarch::parallel::NodePool::getInstance().getMasterRank(),
            localCell.getCellDescriptionsIndex(),element,
            verticesEnumerator.getCellCenter(),verticesEnumerator.getLevel());
      }
    }
  }
  
  logTraceOut( "prepareSendToMaster(...)" );
}

void exahype::mappings::MeshRefinement::mergeWithMaster(
    const exahype::Cell& workerGridCell,
    exahype::Vertex* const workerGridVertices,
    const peano::grid::VertexEnumerator& workerEnumerator,
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
    int worker, const exahype::State& workerState,
    exahype::State& masterState) {
  logTraceIn( "mergeWithMaster(...)" );

  // Merge global solver states
  _allSolversAttainedStableState        &= workerState.getAllSolversAttainedStableState();
  _verticalExchangeOfSolverDataRequired |= workerState.getVerticalExchangeOfSolverDataRequired();

  if ( fineGridCell.hasToCommunicate( fineGridVerticesEnumerator.getLevel() ) ) {
    if ( fineGridCell.isInitialised() ) {
      exahype::solvers::ADERDGSolver::eraseCellDescriptions(fineGridCell.getCellDescriptionsIndex());
      exahype::solvers::FiniteVolumesSolver::eraseCellDescriptions(fineGridCell.getCellDescriptionsIndex());
    } else {
      fineGridCell.setupMetaData();
    }

    exahype::solvers::ADERDGSolver::receiveCellDescriptions(
        worker,fineGridCell,
        peano::heap::MessageType::MasterWorkerCommunication,
        fineGridVerticesEnumerator.getCellCenter(),
        fineGridVerticesEnumerator.getLevel());
    exahype::solvers::FiniteVolumesSolver::receiveCellDescriptions(
        worker,fineGridCell,
        peano::heap::MessageType::MasterWorkerCommunication,
        fineGridVerticesEnumerator.getCellCenter(),
        fineGridVerticesEnumerator.getLevel());

    const int cellDescriptionsIndex = fineGridCell.getCellDescriptionsIndex();
    for (unsigned int solverNumber = 0; solverNumber < exahype::solvers::RegisteredSolvers.size(); ++solverNumber) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      const int element = solver->tryGetElement(cellDescriptionsIndex,solverNumber);
      if ( element!=exahype::solvers::Solver::NotFound ) {
        _verticalExchangeOfSolverDataRequired |=
            solver->progressMeshRefinementInMergeWithMaster(
                worker, cellDescriptionsIndex, element,
                coarseGridCell.getCellDescriptionsIndex(),
                fineGridVerticesEnumerator.getCellCenter(),
                fineGridVerticesEnumerator.getLevel(),
                _stateCopy.getMeshRefinementIsInRefiningMode());
      }
    }

    if ( fineGridCell.isEmpty() ) {
      fineGridCell.shutdownMetaDataAndResetCellDescriptionsIndex();  
    }
  }

  logTraceOut( "mergeWithMaster(...)" );
}


//////////////////
// FORK and JOIN//
//////////////////

void exahype::mappings::MeshRefinement::prepareCopyToRemoteNode(
    exahype::Cell& localCell, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  logTraceInWith5Arguments( "prepareCopyToRemoteNode(...)", localCell, toRank, cellCentre, cellSize, level );

  _allSolversAttainedStableState = false;

  if ( 
      localCell.hasToCommunicate(level) &&
      localCell.getRankOfRemoteNode()==toRank 
  ) { // isAsignedToRemoteRank does not work, remember the halo sends
    const int cellDescriptionsIndex = localCell.getCellDescriptionsIndex();

    exahype::solvers::ADERDGSolver::sendCellDescriptions(toRank,cellDescriptionsIndex,
        exahype::State::isJoiningWithMaster()/* send out data from worker side */,
        peano::heap::MessageType::ForkOrJoinCommunication,cellCentre,level);
    exahype::solvers::FiniteVolumesSolver::sendCellDescriptions(toRank,cellDescriptionsIndex,
        peano::heap::MessageType::ForkOrJoinCommunication,cellCentre,level);

    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      const int element = solver->tryGetElement(cellDescriptionsIndex,solverNumber);
      if( element!=exahype::solvers::Solver::NotFound ) {
        solver->sendDataToWorkerOrMasterDueToForkOrJoin(toRank,cellDescriptionsIndex,element,
            peano::heap::MessageType::ForkOrJoinCommunication,cellCentre,level);
      }
    }

    if ( localCell.isInitialised() ) { 
      localCell.shutdownMetaDataAndResetCellDescriptionsIndex();
    } 
  }

  logTraceOut( "prepareCopyToRemoteNode(...)" );
}

void exahype::mappings::MeshRefinement::mergeWithRemoteDataDueToForkOrJoin(
    exahype::Vertex& localVertex, const exahype::Vertex& masterOrWorkerVertex,
    int fromRank, const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  logTraceInWith6Arguments( "mergeWithRemoteDataDueToForkOrJoin(...)", localVertex, masterOrWorkerVertex, fromRank, x, h, level );

  if ( exahype::State::isNewWorkerDueToForkOfExistingDomain() ) {
    exahype::VertexOperations::writeCellDescriptionsIndex(
        localVertex,multiscalelinkedcell::HangingVertexBookkeeper::getInstance().createVertexLinkMapForNewVertex());
  }

  logTraceOut( "mergeWithRemoteDataDueToForkOrJoin(...)" );
}

void exahype::mappings::MeshRefinement::mergeWithRemoteDataDueToForkOrJoin(
        exahype::Cell& localCell, const exahype::Cell& masterOrWorkerCell,
        int fromRank, const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
        const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  logTraceInWith3Arguments( "mergeWithRemoteDataDueToForkOrJoin(...)", localCell, masterOrWorkerCell, fromRank );

  if ( 
      localCell.hasToCommunicate(level) &&
      masterOrWorkerCell.getRankOfRemoteNode()==tarch::parallel::Node::getInstance().getRank()
  ) {
    if ( exahype::State::isNewWorkerDueToForkOfExistingDomain() ) {
      localCell.setCellDescriptionsIndex(
          multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
      localCell.setupMetaData();
    } else if ( exahype::State::isJoiningWithWorker() ) {
      exahype::solvers::ADERDGSolver::eraseCellDescriptions(localCell.getCellDescriptionsIndex());
      exahype::solvers::FiniteVolumesSolver::eraseCellDescriptions(localCell.getCellDescriptionsIndex());
    }

    exahype::solvers::ADERDGSolver::receiveCellDescriptions(
        fromRank,localCell,
        peano::heap::MessageType::ForkOrJoinCommunication,
        cellCentre,level);
    exahype::solvers::FiniteVolumesSolver::receiveCellDescriptions(
        fromRank,localCell,
        peano::heap::MessageType::ForkOrJoinCommunication,
        cellCentre,level);

    // receive accompanying data
    for (unsigned int solverNumber=0; solverNumber < exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      const int element = solver->tryGetElement(localCell.getCellDescriptionsIndex(),solverNumber);
      if ( element!=exahype::solvers::Solver::NotFound ) {
        solver->mergeWithWorkerOrMasterDataDueToForkOrJoin(fromRank,
            localCell.getCellDescriptionsIndex(),element,
            peano::heap::MessageType::ForkOrJoinCommunication,cellCentre,level);
      }
    }

    // if no solver was found or the cell does belong to the master,
    // shut down the metadata again
    if (
        localCell.isInitialised() &&
        localCell.isEmpty()
    ) {
      localCell.shutdownMetaDataAndResetCellDescriptionsIndex();
    }
  }

  logTraceOut( "mergeWithRemoteDataDueToForkOrJoin(...)" );
}

//
// All methods below are nop,
//
// ==================================



void exahype::mappings::MeshRefinement::prepareCopyToRemoteNode(
    exahype::Vertex& localVertex, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}

void exahype::mappings::MeshRefinement::mergeWithWorker(
    exahype::Vertex& localVertex, const exahype::Vertex& receivedMasterVertex,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}
#endif

exahype::mappings::MeshRefinement::MeshRefinement() {
  // do nothing
}

exahype::mappings::MeshRefinement::~MeshRefinement() {
  // do nothing
}

void exahype::mappings::MeshRefinement::destroyHangingVertex(
    const exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::MeshRefinement::destroyVertex(
    const exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::MeshRefinement::descend(
    exahype::Cell* const fineGridCells, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell) {
  // do nothing
}

void exahype::mappings::MeshRefinement::ascend(
    exahype::Cell* const fineGridCells, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell) {
  // do nothing
}
