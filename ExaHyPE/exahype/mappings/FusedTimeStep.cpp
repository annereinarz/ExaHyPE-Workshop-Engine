/*tru
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
 
#include "exahype/mappings/FusedTimeStep.h"

#include "tarch/multicore/Loop.h"

#include "peano/utils/Globals.h"

#include "peano/datatraversal/autotuning/Oracle.h"
#include "peano/datatraversal/TaskSet.h"

#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

#include "exahype/VertexOperations.h"

#include "exahype/solvers/LimitingADERDGSolver.h"

#include "exahype/mappings/Prediction.h"

tarch::logging::Log exahype::mappings::FusedTimeStep::_log("exahype::mappings::FusedTimeStep");

bool exahype::mappings::FusedTimeStep::issuePredictionJobsInThisIteration() {
  return
      exahype::solvers::Solver::PredictionSweeps==1 ||
      exahype::State::isEvenBatchIteration();
}

bool exahype::mappings::FusedTimeStep::sendOutRiemannDataInThisIteration() {
  return
      exahype::solvers::Solver::PredictionSweeps==1     ||
      exahype::State::isLastIterationOfBatchOrNoBatch() || // covers the NoBatch case
      !exahype::State::isEvenBatchIteration();
}

peano::CommunicationSpecification
exahype::mappings::FusedTimeStep::communicationSpecification() const {
  return peano::CommunicationSpecification(
      peano::CommunicationSpecification::ExchangeMasterWorkerData::MaskOutMasterWorkerDataAndStateExchange,
      peano::CommunicationSpecification::ExchangeWorkerMasterData::SendDataAndStateAfterLastTouchVertexLastTime,true);
}

peano::MappingSpecification
exahype::mappings::FusedTimeStep::enterCellSpecification(int level) const {
  #ifdef Parallel
  return peano::MappingSpecification(
      peano::MappingSpecification::WholeTree,
      peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // counter
  #else
  
  const int coarsestSolverLevel = solvers::Solver::getCoarsestMeshLevelOfAllSolvers();
  if ( std::abs(level)>=coarsestSolverLevel && sendOutRiemannDataInThisIteration() ) {
    return peano::MappingSpecification(
          peano::MappingSpecification::WholeTree,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // counter
  } else {
    return peano::MappingSpecification(
          peano::MappingSpecification::Nop,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
  }
  #endif
}

peano::MappingSpecification
exahype::mappings::FusedTimeStep::leaveCellSpecification(int level) const {
  #ifdef Parallel
  return peano::MappingSpecification(
      peano::MappingSpecification::WholeTree,
      peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // counter
  #else
  
  const int coarsestSolverLevel = solvers::Solver::getCoarsestMeshLevelOfAllSolvers();
  if ( std::abs(level)>=coarsestSolverLevel && issuePredictionJobsInThisIteration() ) {
    return peano::MappingSpecification(
          peano::MappingSpecification::WholeTree,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // performs reductions
  } else {
    return peano::MappingSpecification(
          peano::MappingSpecification::Nop,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
  }
  #endif
}

peano::MappingSpecification
exahype::mappings::FusedTimeStep::touchVertexFirstTimeSpecification(int level) const {
  #ifdef Parallel
  return peano::MappingSpecification(
      peano::MappingSpecification::WholeTree,
      peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // counter
  #else

  const int coarsestSolverLevel = solvers::Solver::getCoarsestMeshLevelOfAllSolvers();
  if ( std::abs(level)>=coarsestSolverLevel && issuePredictionJobsInThisIteration() ) {
    return peano::MappingSpecification(
          peano::MappingSpecification::WholeTree,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,true); // counter
  } else {
    return peano::MappingSpecification(
          peano::MappingSpecification::Nop,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
  }
  #endif
}

/**
 * Nop.
 */
peano::MappingSpecification
exahype::mappings::FusedTimeStep::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
}
peano::MappingSpecification
exahype::mappings::FusedTimeStep::ascendSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::AvoidCoarseGridRaces,false);
}
peano::MappingSpecification
exahype::mappings::FusedTimeStep::descendSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::AvoidCoarseGridRaces,false);
}

exahype::mappings::FusedTimeStep::FusedTimeStep() {
}

void exahype::mappings::FusedTimeStep::beginIteration(
    exahype::State& solverState) {
  logTraceInWith1Argument("beginIteration(State)", solverState);

  if (
      tarch::parallel::Node::getInstance().isGlobalMaster() &&
      tarch::parallel::Node::getInstance().getNumberOfNodes()>1
  ) {
    logInfo("beginIteration(...)","start traversal on global master (after broadcast).");
  }

  if ( exahype::State::isFirstIterationOfBatchOrNoBatch() ) {
    exahype::plotters::startPlottingIfAPlotterIsActive(
        solvers::Solver::getMinTimeStampOfAllSolvers());
  }

  #ifdef Parallel
  if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
    // ensure reductions are inititated from worker side
    solverState.setReduceStateAndCell(true);
  }
  #endif

  logTraceOutWith1Argument("beginIteration(State)", solverState);
}

void exahype::mappings::FusedTimeStep::endIteration(
    exahype::State& state) {
  logTraceInWith1Argument("endIteration(State)", state);

  if ( sendOutRiemannDataInThisIteration() ) {
    exahype::plotters::finishedPlotting();

    if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
      // background threads
      exahype::solvers::Solver::ensureAllJobsHaveTerminated(exahype::solvers::Solver::JobType::ReductionJob);
    }

    if ( tarch::parallel::Node::getInstance().isGlobalMaster() ) {
      const bool endOfFirstFusedTimeStepInBatch =
          ( exahype::solvers::Solver::PredictionSweeps == 1 ) ?
              state.isFirstIterationOfBatchOrNoBatch() :
              state.isSecondIterationOfBatchOrNoBatch();
      for (auto* solver : solvers::RegisteredSolvers) {
        solver->wrapUpTimeStep(endOfFirstFusedTimeStepInBatch,state.isLastIterationOfBatchOrNoBatch());
      }
    }
  }
  logTraceOutWith1Argument("endIteration(State)", state);
}

void exahype::mappings::FusedTimeStep::touchVertexFirstTime(
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

  if ( issuePredictionJobsInThisIteration() ) {
    fineGridVertex.mergeNeighbours(fineGridX,fineGridH);
  }

  logTraceOutWith1Argument("touchVertexFirstTime(...)", fineGridVertex);
}

void exahype::mappings::FusedTimeStep::enterCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith4Arguments("enterCell(...)", fineGridCell,fineGridVerticesEnumerator.toString(),coarseGridCell, fineGridPositionOfCell);

  if (
      fineGridCell.isInitialised() &&
      sendOutRiemannDataInThisIteration()
   ) {
     solvers::Solver::CellInfo cellInfo = fineGridCell.createCellInfo();
     const bool isAtRemoteBoundary = exahype::Cell::isAtRemoteBoundary(fineGridVertices,fineGridVerticesEnumerator);

     for (int solverNumber=0; solverNumber<static_cast<int>(solvers::RegisteredSolvers.size()); solverNumber++) {
       auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
       switch ( solver->getType() ) {
         case solvers::Solver::Type::ADERDG:
           static_cast<solvers::ADERDGSolver*>(solver)->prolongateFaceData(solverNumber,cellInfo,isAtRemoteBoundary);
           break;
         case solvers::Solver::Type::LimitingADERDG:
           static_cast<solvers::LimitingADERDGSolver*>(solver)->getSolver()->prolongateFaceData(solverNumber,cellInfo,isAtRemoteBoundary);
           break;
         case solvers::Solver::Type::FiniteVolumes:
           // insert code here
           break;
         default:
           assertionMsg(false,"Unrecognised solver type: "<<solvers::Solver::toString(solver->getType()));
           logError("mergeWithBoundaryDataIfNotDoneYet(...)","Unrecognised solver type: "<<solvers::Solver::toString(solver->getType()));
           std::abort();
           break;
       }
     }
  }

  logTraceOutWith1Argument("enterCell(...)", fineGridCell);
}



void exahype::mappings::FusedTimeStep::leaveCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith4Arguments("leaveCell(...)", fineGridCell,fineGridVerticesEnumerator.toString(),coarseGridCell, fineGridPositionOfCell);

  if (
      issuePredictionJobsInThisIteration() &&
      fineGridCell.isInitialised()
  ) {
    solvers::Solver::CellInfo cellInfo = fineGridCell.createCellInfo();
    const bool isAtRemoteBoundary = exahype::Cell::isAtRemoteBoundary(fineGridVertices,fineGridVerticesEnumerator);
    const int isLastTimeStep =
        ( exahype::solvers::Solver::PredictionSweeps==1 ) ?
            exahype::State::isLastIterationOfBatchOrNoBatch() :
            exahype::State::isSecondToLastIterationOfBatchOrNoBatch(); // PredictionSweeps==2

    for (int solverNumber=0; solverNumber<static_cast<int>(solvers::RegisteredSolvers.size()); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];

      // this operates only on compute cells
      plotters::plotPatchIfAPlotterIsActive(solverNumber,cellInfo); // TODO(Dominic) potential for IO overlap?

      solvers::Solver::UpdateResult result;
      switch ( solver->getType() ) {
        case solvers::Solver::Type::ADERDG:
          result = static_cast<solvers::ADERDGSolver*>(solver)->fusedTimeStepOrRestrict(
              solverNumber,cellInfo,exahype::State::isFirstIterationOfBatchOrNoBatch(),isLastTimeStep,isAtRemoteBoundary);
          break;
        case solvers::Solver::Type::LimitingADERDG:
          result = static_cast<solvers::LimitingADERDGSolver*>(solver)->fusedTimeStepOrRestrict(
              solverNumber,cellInfo,exahype::State::isFirstIterationOfBatchOrNoBatch(),isLastTimeStep,isAtRemoteBoundary);
          break;
        case solvers::Solver::Type::FiniteVolumes:
          result = static_cast<solvers::FiniteVolumesSolver*>(solver)->fusedTimeStepOrRestrict(
              solverNumber,cellInfo,exahype::State::isFirstIterationOfBatchOrNoBatch(),isLastTimeStep,isAtRemoteBoundary);
          break;
        default:
          assertionMsg(false,"Unrecognised solver type: "<<solvers::Solver::toString(solver->getType()));
          logError("mergeWithBoundaryDataIfNotDoneYet(...)","Unrecognised solver type: "<<solvers::Solver::toString(solver->getType()));
          std::abort();
          break;
      }

      // mesh refinement events, cell sizes (for AMR), time
      if ( isLastTimeStep && !exahype::solvers::Solver::SpawnUpdateAsBackgroundJob ) {
        solver->updateMeshUpdateEvent(result._meshUpdateEvent);
        solver->updateAdmissibleTimeStepSize(result._timeStepSize);
      }
      if (isLastTimeStep) {
	solver->reduceGlobalObservables(solver->getGlobalObservables(), cellInfo, solverNumber);
      }
    }

    // Must be performed for all cell descriptions
    Cell::resetNeighbourMergePerformedFlags(cellInfo,fineGridVertices,fineGridVerticesEnumerator);
  }

  logTraceOutWith1Argument("leaveCell(...)", fineGridCell);
}

#ifdef Parallel
void exahype::mappings::FusedTimeStep::mergeWithNeighbour(
    exahype::Vertex& vertex, const exahype::Vertex& neighbour, int fromRank,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH, int level) {
  logTraceInWith6Arguments( "mergeWithNeighbour(...)", vertex, neighbour, fromRank, fineGridX, fineGridH, level );

  if ( issuePredictionJobsInThisIteration() ) {
    vertex.receiveNeighbourData(
        fromRank, true/*merge with data*/,exahype::State::isFirstIterationOfBatchOrNoBatch(),
        fineGridX,fineGridH,level);
  }

  logTraceOut( "mergeWithNeighbour(...)" );
}

void exahype::mappings::FusedTimeStep::prepareSendToNeighbour(
    exahype::Vertex& vertex, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  logTraceInWith5Arguments( "prepareSendToNeighbour(...)", vertex, toRank, x, h, level );

  if ( sendOutRiemannDataInThisIteration() ) {
    vertex.sendToNeighbour(toRank,exahype::State::isLastIterationOfBatchOrNoBatch(),x,h,level);
  }

  logTraceOut( "prepareSendToNeighbour(...)" );
}

// WORKER->MASTER
bool exahype::mappings::FusedTimeStep::prepareSendToWorker(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
    int worker) {
  // master has to be notified about planned reduction of worker
  // has to notify worker too via message
  return exahype::State::isLastIterationOfBatchOrNoBatch();
}

void exahype::mappings::FusedTimeStep::prepareSendToMaster(
    exahype::Cell& localCell, exahype::Vertex* vertices,
    const peano::grid::VertexEnumerator& verticesEnumerator,
    const exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    const exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
    const int masterRank = tarch::parallel::NodePool::getInstance().getMasterRank();
    exahype::State::reduceGlobalDataToMaster(masterRank,0.0,0);
  }
}

void exahype::mappings::FusedTimeStep::mergeWithMaster(
    const exahype::Cell& workerGridCell,
    exahype::Vertex* const workerGridVertices,
    const peano::grid::VertexEnumerator& workerEnumerator,
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
    int workerRank, const exahype::State& workerState,
    exahype::State& masterState) {
  if (
      tarch::parallel::Node::getInstance().isGlobalMaster() &&
      tarch::parallel::Node::getInstance().getNumberOfNodes()>1
  ) {
    logInfo("mergeWithMaster(...)","end traversal on global master (before reduction).");
  }

  if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
    exahype::State::mergeWithGlobalDataFromWorker(workerRank,0.0,0);
  }
}

//
// Below all methods are nop.
//
//=====================================

void exahype::mappings::FusedTimeStep::receiveDataFromMaster(
    exahype::Cell& receivedCell, exahype::Vertex* receivedVertices,
    const peano::grid::VertexEnumerator& receivedVerticesEnumerator,
    exahype::Vertex* const receivedCoarseGridVertices,
    const peano::grid::VertexEnumerator& receivedCoarseGridVerticesEnumerator,
    exahype::Cell& receivedCoarseGridCell,
    exahype::Vertex* const workersCoarseGridVertices,
    const peano::grid::VertexEnumerator& workersCoarseGridVerticesEnumerator,
    exahype::Cell& workersCoarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::mergeWithWorker(
    exahype::Cell& localCell, const exahype::Cell& receivedMasterCell,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::prepareCopyToRemoteNode(
    exahype::Vertex& localVertex, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::prepareCopyToRemoteNode(
    exahype::Cell& localCell, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::mergeWithRemoteDataDueToForkOrJoin(
    exahype::Vertex& localVertex, const exahype::Vertex& masterOrWorkerVertex,
    int fromRank, const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::mergeWithRemoteDataDueToForkOrJoin(
    exahype::Cell& localCell, const exahype::Cell& masterOrWorkerCell,
    int fromRank, const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::mergeWithWorker(
    exahype::Vertex& localVertex, const exahype::Vertex& receivedMasterVertex,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}
#endif

exahype::mappings::FusedTimeStep::~FusedTimeStep() {
  // do nothing
}

#if defined(SharedMemoryParallelisation)
exahype::mappings::FusedTimeStep::FusedTimeStep(
    const FusedTimeStep& masterThread) {
  // do nothing
}
void exahype::mappings::FusedTimeStep::mergeWithWorkerThread(
    const FusedTimeStep& workerThread) {
  // do nothing
}
#endif

void exahype::mappings::FusedTimeStep::createHangingVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::destroyHangingVertex(
    const exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::createInnerVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::createBoundaryVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::destroyVertex(
    const exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::createCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::destroyCell(
    const exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::touchVertexLastTime(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::descend(
    exahype::Cell* const fineGridCells, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell) {
  // do nothing
}

void exahype::mappings::FusedTimeStep::ascend(
    exahype::Cell* const fineGridCells, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell) {
  // do nothing
}
