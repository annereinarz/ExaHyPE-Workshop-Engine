/**os
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
 
#include "exahype/mappings/PredictionOrLocalRecomputation.h"

#include <algorithm>

#include "tarch/multicore/Loop.h"

#include "peano/utils/Globals.h"
#include "peano/utils/Loop.h"

#include "peano/datatraversal/autotuning/Oracle.h"
#include "peano/datatraversal/TaskSet.h"

#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

#include "exahype/solvers/LimitingADERDGSolver.h"

#include "exahype/mappings/Prediction.h"

tarch::logging::Log exahype::mappings::PredictionOrLocalRecomputation::_log(
    "exahype::mappings::PredictionOrLocalRecomputation");

bool exahype::mappings::PredictionOrLocalRecomputation::OneSolverRequestedLocalRecomputation = false;

peano::CommunicationSpecification
exahype::mappings::PredictionOrLocalRecomputation::communicationSpecification() const {
  return peano::CommunicationSpecification(
      peano::CommunicationSpecification::ExchangeMasterWorkerData::MaskOutMasterWorkerDataAndStateExchange,
      peano::CommunicationSpecification::ExchangeWorkerMasterData::SendDataAndStateAfterLastTouchVertexLastTime,true);
}

peano::MappingSpecification exahype::mappings::PredictionOrLocalRecomputation::enterCellSpecification(int level) const {
  const int coarsestSolverLevel = solvers::Solver::getCoarsestMeshLevelOfAllSolvers();
  if ( std::abs(level)>=coarsestSolverLevel ) {
    return peano::MappingSpecification(
           peano::MappingSpecification::WholeTree,
           peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);  // performs reduction
  } else {
    return peano::MappingSpecification(
          peano::MappingSpecification::Nop,
          peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
  }
}
peano::MappingSpecification
exahype::mappings::PredictionOrLocalRecomputation::touchVertexFirstTimeSpecification(int level) const {
  return Vertex::getNeighbourMergeSpecification(level);
}

// Below specs are all nop
peano::MappingSpecification
exahype::mappings::PredictionOrLocalRecomputation::leaveCellSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::RunConcurrentlyOnFineGrid,false);
}

peano::MappingSpecification
exahype::mappings::PredictionOrLocalRecomputation::touchVertexLastTimeSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::RunConcurrentlyOnFineGrid,true);
}
peano::MappingSpecification
exahype::mappings::PredictionOrLocalRecomputation::ascendSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::AvoidCoarseGridRaces,true);
}
peano::MappingSpecification
exahype::mappings::PredictionOrLocalRecomputation::descendSpecification(int level) const {
  return peano::MappingSpecification(
      peano::MappingSpecification::Nop,
      peano::MappingSpecification::AvoidCoarseGridRaces,true);
}

void exahype::mappings::PredictionOrLocalRecomputation::beginIteration(
    exahype::State& solverState) {
  logTraceInWith1Argument("beginIteration(State)", solverState);

  #ifdef Parallel
  if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
    // ensure reductions are inititated from worker side
    solverState.setReduceStateAndCell(true);
  }
  #endif

  if (
      exahype::State::isFirstIterationOfBatchOrNoBatch()
  ) {
    OneSolverRequestedLocalRecomputation = exahype::solvers::Solver::oneSolverRequestedLocalRecomputation();
  }

  if (
      exahype::solvers::Solver::SpawnPredictionAsBackgroundJob &&
      exahype::State::isLastIterationOfBatchOrNoBatch()
  ) {
    peano::datatraversal::TaskSet::startToProcessBackgroundJobs(); // TODO(Dominic): Still necessary?
  }

  logTraceOutWith1Argument("beginIteration(State)", solverState);
}

bool exahype::mappings::PredictionOrLocalRecomputation::performLocalRecomputation(
    exahype::solvers::Solver* solver) {
  return
      solver->getMeshUpdateEvent()==exahype::solvers::Solver::MeshUpdateEvent::IrregularLimiterDomainChange;
}

bool exahype::mappings::PredictionOrLocalRecomputation::performPrediction(
    exahype::solvers::Solver* solver) {
  return exahype::solvers::Solver::FuseAllADERDGPhases &&
         solver->hasRequestedAnyMeshRefinement();
}

void exahype::mappings::PredictionOrLocalRecomputation::endIteration(
    exahype::State& state) {
  logTraceInWith1Argument("endIteration(State)", state);

  if ( state.isLastIterationOfBatchOrNoBatch() ) {
    // background threads
    exahype::solvers::Solver::ensureAllJobsHaveTerminated(exahype::solvers::Solver::JobType::ReductionJob);
  }

  logTraceOutWith1Argument("endIteration(State)", state);
}

void exahype::mappings::PredictionOrLocalRecomputation::enterCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  logTraceInWith4Arguments("enterCell(...)", fineGridCell,
                           fineGridVerticesEnumerator.toString(),
                           coarseGridCell, fineGridPositionOfCell);

  if ( fineGridCell.isInitialised() ) {
    solvers::Solver::CellInfo cellInfo = fineGridCell.createCellInfo();
    const bool isAtRemoteBoundary = exahype::Cell::isAtRemoteBoundary(fineGridVertices,fineGridVerticesEnumerator);

    for (unsigned int solverNumber=0; solverNumber<exahype::solvers::RegisteredSolvers.size(); solverNumber++) {
      auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
      if ( performLocalRecomputation( solver ) && exahype::State::isFirstIterationOfBatchOrNoBatch() ) {
        auto* limitingADERDG = static_cast<exahype::solvers::LimitingADERDGSolver*>(solver);
        double admissibleTimeStepSize =
            limitingADERDG->localRecomputation(solverNumber,cellInfo,isAtRemoteBoundary);
        solver->updateAdmissibleTimeStepSize(admissibleTimeStepSize);
      }
      else if ( performPrediction(solver) && exahype::State::isFirstIterationOfBatchOrNoBatch() ) {
        switch ( solver->getType() ) {
          case solvers::Solver::Type::ADERDG:
            static_cast<solvers::ADERDGSolver*>(solver)->predictionAndVolumeIntegral(solverNumber,cellInfo,isAtRemoteBoundary);
            break;
          case solvers::Solver::Type::LimitingADERDG:
            static_cast<solvers::LimitingADERDGSolver*>(solver)->predictionAndVolumeIntegral(solverNumber,cellInfo,isAtRemoteBoundary);
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
      else if ( performPrediction(solver) && exahype::State::isLastIterationOfBatchOrNoBatch() ) {
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
    if ( exahype::State::isFirstIterationOfBatchOrNoBatch() ) {
      Cell::resetNeighbourMergePerformedFlags(cellInfo,fineGridVertices,fineGridVerticesEnumerator);
    }
  }
  logTraceOutWith1Argument("enterCell(...)", fineGridCell);
}


void exahype::mappings::PredictionOrLocalRecomputation::leaveCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::mergeNeighboursDataDuringLocalRecomputationLoopBody(
    const int                                    pos1Scalar,
    const int                                    pos2Scalar,
    const exahype::Vertex&                       vertex,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h) {
  tarch::la::Vector<DIMENSIONS,int> pos1 = Vertex::delineariseIndex2(pos1Scalar);
  tarch::la::Vector<DIMENSIONS,int> pos2 = Vertex::delineariseIndex2(pos2Scalar);
  assertion(tarch::la::countEqualEntries(pos1,pos2)==(DIMENSIONS-1));

  const int  cellDescriptionsIndex1 = vertex.getCellDescriptionsIndex(pos1Scalar);
  const int  cellDescriptionsIndex2 = vertex.getCellDescriptionsIndex(pos2Scalar);
  const bool validIndex1 = cellDescriptionsIndex1 >= 0;
  const bool validIndex2 = cellDescriptionsIndex2 >= 0;

  if ( validIndex1 && validIndex2 ) {
    solvers::Solver::CellInfo cellInfo1 = vertex.createCellInfo(pos1Scalar);
    solvers::Solver::CellInfo cellInfo2 = vertex.createCellInfo(pos2Scalar);
    solvers::Solver::InterfaceInfo face(pos1,pos2);

    const bool mergeWithCell1 = Vertex::hasToMergeAtFace(cellInfo1,face._faceIndex1,false/*prefetchADERDGFace*/);
    const bool mergeWithCell2 = Vertex::hasToMergeAtFace(cellInfo2,face._faceIndex2,false/*prefetchADERDGFace*/);
    assertion2(mergeWithCell1==mergeWithCell2,mergeWithCell1,mergeWithCell2);
    if ( mergeWithCell1 && mergeWithCell2 ) {
      for (int solverNumber=0; solverNumber<static_cast<int>(solvers::RegisteredSolvers.size()); solverNumber++) {
        auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
        if ( performLocalRecomputation(solver) ) {
          static_cast<exahype::solvers::LimitingADERDGSolver*>(solver)->
              mergeNeighboursDataDuringLocalRecomputation(solverNumber,cellInfo1,cellInfo2,pos1,pos2);
        }
      }
    }
  } else if (
      ((validIndex1 && !validIndex2 &&
        cellDescriptionsIndex2==multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex)
        ||
        (!validIndex1 && validIndex2 &&
        cellDescriptionsIndex1==multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex))
  ) {
    int                               posCellScalar = pos1Scalar;
    tarch::la::Vector<DIMENSIONS,int> posCell       = pos1;
    tarch::la::Vector<DIMENSIONS,int> posBoundary   = pos2;
    if ( cellDescriptionsIndex2 >= 0 ) {
      posCellScalar         = pos2Scalar;
      posCell               = pos2;
      posBoundary           = pos1;
    }
    solvers::Solver::CellInfo         cellInfo = vertex.createCellInfo(posCellScalar);
    solvers::Solver::BoundaryFaceInfo face(posCell,posBoundary);

    if ( !cellInfo.empty() && Vertex::hasToMergeAtFace(cellInfo,face._faceIndex,false/*prefetchADERDGFace*/) ) {
      for (int solverNumber=0; solverNumber<static_cast<int>(solvers::RegisteredSolvers.size()); solverNumber++) {
        auto* solver = exahype::solvers::RegisteredSolvers[solverNumber];
        if ( performLocalRecomputation(solver) ) {
          static_cast<exahype::solvers::LimitingADERDGSolver*>(solver)->
              mergeWithBoundaryDataDuringLocalRecomputation(solverNumber,cellInfo,posCell,posBoundary);
        }
      }
    }
  }
}

void exahype::mappings::PredictionOrLocalRecomputation::touchVertexFirstTime(
  exahype::Vertex& vertex,
  const tarch::la::Vector<DIMENSIONS, double>& x,
  const tarch::la::Vector<DIMENSIONS, double>& h,
  exahype::Vertex* const coarseGridVertices,
  const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
  exahype::Cell& coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>& fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "touchVertexFirstTime(...)", vertex, x, h, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );

  if (
      exahype::State::isFirstIterationOfBatchOrNoBatch() &&
      OneSolverRequestedLocalRecomputation
  ) {
    mergeNeighboursDataDuringLocalRecomputationLoopBody(0,1,vertex,x,h);
    mergeNeighboursDataDuringLocalRecomputationLoopBody(0,2,vertex,x,h);
    #if DIMENSIONS==3
    mergeNeighboursDataDuringLocalRecomputationLoopBody(0,4,vertex,x,h);
    #endif
  }

  logTraceOutWith1Argument( "touchVertexFirstTime(...)", vertex );
}


#ifdef Parallel
///////////////////////////////////////
// NEIGHBOUR
///////////////////////////////////////
void exahype::mappings::PredictionOrLocalRecomputation::mergeWithNeighbour(
    exahype::Vertex& vertex, const exahype::Vertex& neighbour, int fromRank,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH, int level) {
  logTraceInWith6Arguments( "mergeWithNeighbour(...)", vertex, neighbour, fromRank, fineGridX, fineGridH, level );
  
  if (
      exahype::State::isFirstIterationOfBatchOrNoBatch() &&
      OneSolverRequestedLocalRecomputation &&
      vertex.hasToCommunicate(level)
  ) {
    const tarch::la::Vector<DIMENSIONS,int> lowerLeft(0);
    #if DIMENSIONS==3
    receiveNeighbourDataLoopBody(fromRank,4,0,vertex,fineGridX,level);
    receiveNeighbourDataLoopBody(fromRank,0,4,vertex,fineGridX,level);
    #endif
    receiveNeighbourDataLoopBody(fromRank,2,0,vertex,fineGridX,level);
    receiveNeighbourDataLoopBody(fromRank,0,2,vertex,fineGridX,level);
    receiveNeighbourDataLoopBody(fromRank,1,0,vertex,fineGridX,level);
    receiveNeighbourDataLoopBody(fromRank,0,1,vertex,fineGridX,level);
  }
  logTraceOut( "mergeWithNeighbour(...)" );
}

void exahype::mappings::PredictionOrLocalRecomputation::receiveNeighbourDataLoopBody(
    const int                                    fromRank,
    const int                                    srcScalar,
    const int                                    destScalar,
    const exahype::Vertex&                       vertex,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) {
  if ( vertex.hasToReceiveMetadata(fromRank,srcScalar,destScalar,vertex.getAdjacentRanks()) ) {
    const int destCellDescriptionsIndex = vertex.getCellDescriptionsIndex(destScalar);
    bool validIndex = destCellDescriptionsIndex >= 0;
    assertion( !validIndex || exahype::solvers::ADERDGSolver::Heap::getInstance().isValidIndex(destCellDescriptionsIndex));

    if ( validIndex ) {
      const tarch::la::Vector<DIMENSIONS,int> src = Vertex::delineariseIndex2(srcScalar);
      const tarch::la::Vector<DIMENSIONS,int> dest = Vertex::delineariseIndex2(destScalar);
      solvers::Solver::CellInfo         cellInfo = vertex.createCellInfo(destScalar);
      solvers::Solver::BoundaryFaceInfo face(dest,src); // dest and src are swapped

      if ( vertex.hasToMergeAtFace(cellInfo,face._faceIndex,false/*prefetchADERDGFaceData*/) ) {
        for(unsigned int solverNumber = solvers::RegisteredSolvers.size(); solverNumber-- > 0;) {
          auto* solver = solvers::RegisteredSolvers[solverNumber];
          if ( performLocalRecomputation( solver ) ) {
            assertion1( solver->getType()==solvers::Solver::Type::LimitingADERDG, solver->toString() );
            static_cast<solvers::LimitingADERDGSolver*>(solver)->
                mergeWithNeighbourDataDuringLocalRecomputation(fromRank,solverNumber,cellInfo,src,dest,x,level);
          }
        }
      }
    }
  }
}

void exahype::mappings::PredictionOrLocalRecomputation::prepareSendToNeighbour(
    exahype::Vertex& vertex, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  logTraceInWith3Arguments( "prepareSendToNeighbour(...)", vertex, toRank, level );

  if (
      exahype::State::isLastIterationOfBatchOrNoBatch() &&
      exahype::solvers::Solver::FuseAllADERDGPhases
  ) {
   vertex.sendToNeighbour(toRank,true,x,h,level);
  }

  logTraceOut( "prepareSendToNeighbour(...)" );
}

bool exahype::mappings::PredictionOrLocalRecomputation::prepareSendToWorker(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
    int worker) {
  return exahype::State::isLastIterationOfBatchOrNoBatch(); // notifies master this worker wants to perform reduction
}

void exahype::mappings::PredictionOrLocalRecomputation::prepareSendToMaster(
    exahype::Cell& localCell, exahype::Vertex* vertices,
    const peano::grid::VertexEnumerator& verticesEnumerator,
    const exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    const exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
    const int masterRank = tarch::parallel::NodePool::getInstance().getMasterRank();
    for (auto* solver : exahype::solvers::RegisteredSolvers) {
      if ( solver->getMeshUpdateEvent()==exahype::solvers::Solver::MeshUpdateEvent::IrregularLimiterDomainChange ) {
        solver->sendDataToMaster(masterRank,0.0,0);
      }
    }
  }
 }

void exahype::mappings::PredictionOrLocalRecomputation::mergeWithMaster(
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
  if ( exahype::State::isLastIterationOfBatchOrNoBatch() ) {
    for (auto* solver : exahype::solvers::RegisteredSolvers) {
      if ( solver->getMeshUpdateEvent()==exahype::solvers::Solver::MeshUpdateEvent::IrregularLimiterDomainChange ) {
        solver->mergeWithWorkerData(workerRank,0.0,0);
      }
    }
  }
}

//
// Below all methods are nop.
//
//=====================================

void exahype::mappings::PredictionOrLocalRecomputation::receiveDataFromMaster(
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

void exahype::mappings::PredictionOrLocalRecomputation::prepareCopyToRemoteNode(
    exahype::Vertex& localVertex, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::prepareCopyToRemoteNode(
    exahype::Cell& localCell, int toRank,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::mergeWithRemoteDataDueToForkOrJoin(
    exahype::Vertex& localVertex, const exahype::Vertex& masterOrWorkerVertex,
    int fromRank, const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::mergeWithRemoteDataDueToForkOrJoin(
    exahype::Cell& localCell, const exahype::Cell& masterOrWorkerCell,
    int fromRank, const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::mergeWithWorker(
    exahype::Cell& localCell, const exahype::Cell& receivedMasterCell,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize, int level) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::mergeWithWorker(
    exahype::Vertex& localVertex, const exahype::Vertex& receivedMasterVertex,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h, int level) {
  // do nothing
}
#endif

exahype::mappings::PredictionOrLocalRecomputation::PredictionOrLocalRecomputation() {
  // do nothing
}

exahype::mappings::PredictionOrLocalRecomputation::~PredictionOrLocalRecomputation() {
  // do nothing
}

#if defined(SharedMemoryParallelisation)
exahype::mappings::PredictionOrLocalRecomputation::PredictionOrLocalRecomputation(
    const PredictionOrLocalRecomputation& masterThread) {
  // do nothing
}
// Merge over threads
void exahype::mappings::PredictionOrLocalRecomputation::mergeWithWorkerThread(
    const PredictionOrLocalRecomputation& workerThread) {
  // do nothing
}
#endif

void exahype::mappings::PredictionOrLocalRecomputation::createHangingVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::destroyHangingVertex(
    const exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::createInnerVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::createBoundaryVertex(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::destroyVertex(
    const exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::createCell(
    exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::destroyCell(
    const exahype::Cell& fineGridCell, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::touchVertexLastTime(
    exahype::Vertex& fineGridVertex,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridX,
    const tarch::la::Vector<DIMENSIONS, double>& fineGridH,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfVertex) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::descend(
    exahype::Cell* const fineGridCells, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell) {
  // do nothing
}

void exahype::mappings::PredictionOrLocalRecomputation::ascend(
    exahype::Cell* const fineGridCells, exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Vertex* const coarseGridVertices,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    exahype::Cell& coarseGridCell) {
  // do nothing
}
