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
 *
 * \author Dominic E. Charrier, Tobias Weinzierl
 **/
 
#include "exahype/solvers/Solver.h"

#include "exahype/Cell.h"

#include "tarch/multicore/Lock.h"
#include "tarch/la/Scalar.h"

#include "peano/heap/CompressedFloatingPointNumbers.h"

#include <algorithm>
#include <mm_malloc.h> //g++
#include <cstring> //memset

#include "../../../Peano/tarch/multicore/Jobs.h"
#include "LimitingADERDGSolver.h"
#include "ADERDGSolver.h"
#include "FiniteVolumesSolver.h"

#ifdef USE_ITAC
int exahype::solvers::Solver::waitUntilCompletedLastStepHandle  = 0;
int exahype::solvers::Solver::ensureAllJobsHaveTerminatedHandle = 0;
#endif

std::vector<exahype::solvers::Solver*> exahype::solvers::RegisteredSolvers;

#ifdef Parallel
exahype::DataHeap::HeapEntries exahype::EmptyDataHeapMessage(0);
#endif

tarch::multicore::BooleanSemaphore exahype::ReductionSemaphore;

tarch::multicore::BooleanSemaphore exahype::HeapSemaphore;

exahype::DataHeap::HeapEntries& exahype::getDataHeapEntries(const int index) {
  assertion1(DataHeap::getInstance().isValidIndex(index),index);
  return DataHeap::getInstance().getData(index);
}

const exahype::DataHeap::HeapEntries& exahype::getDataHeapEntriesForReadOnlyAccess(const int index) {
  return DataHeap::getInstance().getData(index);
}

void exahype::moveDataHeapEntries(
    const int fromIndex,const int toIndex,bool recycleFromArray) {
  std::copy(
      getDataHeapEntries(fromIndex).begin(),
      getDataHeapEntries(fromIndex).end(),
      getDataHeapEntries(toIndex).begin());
  DataHeap::getInstance().deleteData(fromIndex,recycleFromArray);
}

#ifdef TrackGridStatistics
/**
 * If you enable assertions, we have the option not to remove any entries from
 * any heap but to continue to store all unknown on the standard heap when we
 * compress. This allows us to validate that the data that is compressed in one
 * iteration and uncompressed in the next one does not differ too significantly
 * from the original data. There are however two drawbacks to this approach:
 *
 * - It is costly.
 * - It changes the code semantics - we actually work with other and more heap
 *   entries and thus cannot claim that a code with these assertions equals a
 *   code without any assertions.
 *
 * I thus decided to trigger the comparison of compressed vs. uncompressed data
 * through a special flag.
 */
//#define ValidateCompressedVsUncompressedData

double exahype::solvers::Solver::PipedUncompressedBytes = 0;
double exahype::solvers::Solver::PipedCompressedBytes = 0;
#endif

tarch::logging::Log exahype::solvers::Solver::_log( "exahype::solvers::Solver");

#ifdef Parallel
int exahype::solvers::Solver::MasterWorkerCommunicationTag = MPI_ANY_TAG;
#endif

bool exahype::solvers::Solver::ProfileUpdate = false;

bool exahype::solvers::Solver::OnlyInitialMeshRefinement          = false;
bool exahype::solvers::Solver::OnlyStaticLimiting                 = false;

bool exahype::solvers::Solver::FuseAllADERDGPhases                = false;
double exahype::solvers::Solver::FusedTimeSteppingRerunFactor     = 0.99;
double exahype::solvers::Solver::FusedTimeSteppingDiffusionFactor = 0.99;

bool exahype::solvers::Solver::DisableMetaDataExchangeInBatchedTimeSteps = false;
bool exahype::solvers::Solver::DisablePeanoNeighbourExchangeInTimeSteps = false;

int exahype::solvers::Solver::MaxNumberOfRunningBackgroundJobConsumerTasksDuringTraversal = 0;

bool exahype::solvers::Solver::SpawnPredictionAsBackgroundJob = false;

bool exahype::solvers::Solver::SpawnUpdateAsBackgroundJob = false;

int exahype::solvers::Solver::PredictionSweeps     = 1;

bool exahype::solvers::Solver::SpawnProlongationAsBackgroundJob = false;

bool exahype::solvers::Solver::SpawnAMRBackgroundJobs = false;

double exahype::solvers::Solver::CompressionAccuracy = 0.0;
bool exahype::solvers::Solver::SpawnCompressionAsBackgroundJob = false;

exahype::solvers::Solver::JobSystemWaitBehaviourType exahype::solvers::Solver::JobSystemWaitBehaviour;

std::atomic<int> exahype::solvers::Solver::NumberOfAMRBackgroundJobs(0);
std::atomic<int> exahype::solvers::Solver::NumberOfReductionJobs(0);
std::atomic<int> exahype::solvers::Solver::NumberOfEnclaveJobs(0);
std::atomic<int> exahype::solvers::Solver::NumberOfSkeletonJobs(0);

std::string exahype::solvers::Solver::toString(const JobType& jobType) {
  switch (jobType) {
    case JobType::AMRJob:       return "AMRJob";
    case JobType::ReductionJob: return "ReductionJob";
    case JobType::EnclaveJob:   return "EnclaveJob";
    case JobType::SkeletonJob:  return "SkeletonJob";
    default:
      logError("toString(const JobType&)","Job type not supported.");
      std::abort();
      return 0;
  }
}

int exahype::solvers::Solver::getNumberOfQueuedJobs(const JobType& jobType) {
  switch (jobType) {
    case JobType::AMRJob:       return NumberOfAMRBackgroundJobs.load();
    case JobType::ReductionJob: return NumberOfReductionJobs.load();
    case JobType::EnclaveJob:   return NumberOfEnclaveJobs.load();
    case JobType::SkeletonJob:  return NumberOfSkeletonJobs.load();
    default:
      logError("getNumberOfQueuedJobs(const JobType&)","Job type not supported.");
      std::abort();
      return 0;
  }
}

void exahype::solvers::Solver::ensureAllJobsHaveTerminated(JobType jobType) {
  #ifdef USE_ITAC
  VT_begin(ensureAllJobsHaveTerminatedHandle);
  #endif

  int queuedJobs = getNumberOfQueuedJobs(jobType);
  bool finishedWait = queuedJobs == 0;

  if ( !finishedWait ) {
    #if defined(Asserts)
    logInfo("waitUntilAllBackgroundTasksHaveTerminated()",
      "waiting for " << queuedJobs << " background job(s) to complete (type=" << toString(jobType) << ").");
    #endif
    peano::datatraversal::TaskSet::startToProcessBackgroundJobs();
  }

  const int maxNumberOfJobsAtOnce = 2; // allows prefetching next job's data
  const bool processHighPriorityJobs =
      jobType==JobType::SkeletonJob ||
      jobType==JobType::ReductionJob;
  while ( !finishedWait ) {
    // do some work myself; basically same body as waitUntil... method
    #ifdef Parallel
    {
      tarch::multicore::RecursiveLock lock( tarch::services::Service::receiveDanglingMessagesSemaphore );
      tarch::parallel::Node::getInstance().receiveDanglingMessages();
      lock.free();
    }
    #endif

    switch ( JobSystemWaitBehaviour ) {
    case JobSystemWaitBehaviourType::ProcessJobsWithSamePriority:
      tarch::multicore::jobs::processBackgroundJobs( maxNumberOfJobsAtOnce, getTaskPriority(processHighPriorityJobs) );
      break;
    case JobSystemWaitBehaviourType::ProcessAnyJobs:
      tarch::multicore::jobs::processBackgroundJobs( maxNumberOfJobsAtOnce );
      break;
    default:
      break;
    }
    queuedJobs = getNumberOfQueuedJobs(jobType);
    finishedWait = queuedJobs == 0;
  }

  #ifdef USE_ITAC
  VT_end(ensureAllJobsHaveTerminatedHandle);
  #endif
}

void exahype::solvers::Solver::configurePredictionPhase(const bool usePredictionBackgroundJobs, bool useProlongationBackgroundJobs) {
  exahype::solvers::Solver::SpawnPredictionAsBackgroundJob   = usePredictionBackgroundJobs;
  exahype::solvers::Solver::SpawnProlongationAsBackgroundJob = useProlongationBackgroundJobs;

  #ifdef OnePredictionSweep
  exahype::solvers::Solver::PredictionSweeps = 1;
  #else
  exahype::solvers::Solver::PredictionSweeps = ( 
         !allSolversPerformOnlyUniformRefinement() || // prolongations are done in second sweep
         usePredictionBackgroundJobs ) ? 
         2 : 1;
  #endif

  if ( tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    logInfo("configurePredictionPhase()","prediction sweeps                   ="<<exahype::solvers::Solver::PredictionSweeps);
    logInfo("configurePredictionPhase()","spawn prediction as background job  ="<<exahype::solvers::Solver::SpawnPredictionAsBackgroundJob);
    logInfo("configurePredictionPhase()","spawn prolongation as background job="<<exahype::solvers::Solver::SpawnProlongationAsBackgroundJob);
  }
}


exahype::solvers::Solver::Solver(
  const std::string&                     identifier,
  exahype::solvers::Solver::Type         type,
  int                                    numberOfVariables,
  int                                    numberOfParameters,
  int                                    numberOfGlobalObservables,
  int                                    nodesPerCoordinateAxis,
  double                                 maximumMeshSize,
  int                                    maximumAdaptiveMeshDepth,
  exahype::solvers::Solver::TimeStepping timeStepping,
  std::unique_ptr<profilers::Profiler>   profiler
  ):  _identifier(identifier),
      _type(type),
      _numberOfVariables(numberOfVariables),
      _numberOfParameters(numberOfParameters),
      _numberOfGlobalObservables(numberOfGlobalObservables),
      _nodesPerCoordinateAxis(nodesPerCoordinateAxis),
      _domainOffset(std::numeric_limits<double>::infinity()),
      _domainSize(std::numeric_limits<double>::infinity()),
      _maximumMeshSize(maximumMeshSize),
      _maximumAdaptiveMeshDepth(maximumAdaptiveMeshDepth),
      _timeStepping(timeStepping),
      _coarsestMeshLevel(std::numeric_limits<int>::max()),
      _coarsestMeshSize(std::numeric_limits<double>::infinity()),
      _profiler(std::move(profiler)) {}


std::string exahype::solvers::Solver::getIdentifier() const {
  return _identifier;
}

std::string exahype::solvers::Solver::toString(const exahype::solvers::Solver::Type& param) {
  switch (param) {
    case Type::ADERDG:         return "ADER-DG";
    case Type::FiniteVolumes:  return "Finite Volumes";
    case Type::LimitingADERDG: return "Limiting ADER-DG";
  }
  return "undefined";
}

std::string exahype::solvers::Solver::toString(const exahype::solvers::Solver::TimeStepping& param) {
  switch (param) {
    case TimeStepping::Global:      return "global";
    case TimeStepping::GlobalFixed: return "globalfixed";
  }
  return "undefined";
}

void exahype::solvers::Solver::tearApart(
  int numberOfEntries, int normalHeapIndex, int compressedHeapIndex, int bytesForMantissa) const {
  char exponent;
  long int mantissa;
  char* pMantissa = reinterpret_cast<char*>( &(mantissa) );

  assertion( DataHeap::getInstance().isValidIndex(normalHeapIndex) );
  assertion( CompressedDataHeap::getInstance().isValidIndex(compressedHeapIndex) );
  assertion2( static_cast<int>(getDataHeapEntries(normalHeapIndex).size())==numberOfEntries, getDataHeapEntries(normalHeapIndex).size(), numberOfEntries );
  assertion( CompressedDataHeap::getInstance().getData(compressedHeapIndex).empty() );

  CompressedDataHeap::getInstance().getData( compressedHeapIndex ).clear();

  for (int i=0; i<numberOfEntries; i++) {
	if (tarch::la::equals(DataHeap::getInstance().getData( normalHeapIndex )[i],0.0,CompressionAccuracy)) {
      CompressedDataHeap::getInstance().getData( compressedHeapIndex ).push_back( 0 );
	}
	else {
     peano::heap::decompose(
	  getDataHeapEntries(normalHeapIndex)[i],
		  exponent, mantissa, bytesForMantissa
		);

		CompressedDataHeap::getInstance().getData( compressedHeapIndex ).push_back( exponent );
		for (int j=0; j<bytesForMantissa-1; j++) {
		  CompressedDataHeap::getInstance().getData( compressedHeapIndex ).push_back( pMantissa[j] );
		}
		// ensure that 0 marker is not misused
        if (pMantissa[bytesForMantissa-1]==0) {
  		  CompressedDataHeap::getInstance().getData( compressedHeapIndex ).push_back( 1 );
        }
        else {
  		  CompressedDataHeap::getInstance().getData( compressedHeapIndex ).push_back( pMantissa[bytesForMantissa-1] );
        }

		#ifdef ValidateCompressedVsUncompressedData
		const double reconstructedValue = peano::heap::compose(
		  exponent, mantissa, bytesForMantissa
		);
		assertion9(
		  tarch::la::equals( reconstructedValue, getDataHeapEntries(normalHeapIndex)[i], tarch::la::absoluteWeight(reconstructedValue, getDataHeapEntries(normalHeapIndex)[i], CompressionAccuracy) ),
		  reconstructedValue, getDataHeapEntries(normalHeapIndex)[i],
		  reconstructedValue-DataHeap::getInstance().getData( normalHeapIndex )[i],
		  CompressionAccuracy, bytesForMantissa, numberOfEntries, normalHeapIndex,
		  static_cast<int>(exponent), mantissa
		);
		#endif
	}
  }
}


void exahype::solvers::Solver::glueTogether(
  int numberOfEntries, int normalHeapIndex, int compressedHeapIndex, int bytesForMantissa
) const {
  char exponent  = 0;
  long int mantissa;
  char* pMantissa = reinterpret_cast<char*>( &(mantissa) );

  assertion( DataHeap::getInstance().isValidIndex(normalHeapIndex) );
  assertion( CompressedDataHeap::getInstance().isValidIndex(compressedHeapIndex) );

  #ifdef ValidateCompressedVsUncompressedData
  assertion( static_cast<int>(getDataHeapEntries(normalHeapIndex).size())==numberOfEntries );
  #else
  getDataHeapEntries(normalHeapIndex).resize(numberOfEntries);
  #endif

  for (int i=numberOfEntries-1; i>=0; i--) {
    const char firstEntry = CompressedDataHeap::getInstance().getData( compressedHeapIndex ).back();
	if (firstEntry==0) {
      getDataHeapEntries(normalHeapIndex)[i] = 0.0;
      CompressedDataHeap::getInstance().getData( compressedHeapIndex ).pop_back();
	}
	else {
      mantissa = 0;
      for (int j=bytesForMantissa-1; j>=0; j--) {
        pMantissa[j] = CompressedDataHeap::getInstance().getData( compressedHeapIndex ).back();
        CompressedDataHeap::getInstance().getData( compressedHeapIndex ).pop_back();
      }
      exponent = CompressedDataHeap::getInstance().getData( compressedHeapIndex ).back();
      CompressedDataHeap::getInstance().getData( compressedHeapIndex ).pop_back();
      double reconstructedValue = peano::heap::compose(
        exponent, mantissa, bytesForMantissa
      );
      #ifdef ValidateCompressedVsUncompressedData
      assertion7(
        tarch::la::equals( getDataHeapEntries(normalHeapIndex)[i], reconstructedValue, tarch::la::absoluteWeight(reconstructedValue, getDataHeapEntries(normalHeapIndex)[i], CompressionAccuracy) ),
        getDataHeapEntries(normalHeapIndex)[i], reconstructedValue, getDataHeapEntries(normalHeapIndex)[i] - reconstructedValue,
        CompressionAccuracy, bytesForMantissa, numberOfEntries, normalHeapIndex
      );
      #endif
      getDataHeapEntries(normalHeapIndex)[i] = reconstructedValue;
    }
  }
}


std::pair<double,int> exahype::solvers::Solver::computeCoarsestMeshSizeAndLevel(double meshSize, double domainSize) {
  int    peanoLevel      = 1; // The domain root cell is actually at Peano level 1
  double currenthMax = std::numeric_limits<double>::infinity();
  while ( currenthMax>meshSize ) {
    currenthMax = domainSize / threePowI(peanoLevel-1);
    peanoLevel++;
  }
  peanoLevel--; // currenthMax was computed with peanoLevel-1 and we start to count at 1
  return std::pair<double,int>(currenthMax,peanoLevel);
}

exahype::solvers::Solver::Type exahype::solvers::Solver::getType() const {
  return _type;
}

exahype::solvers::Solver::TimeStepping exahype::solvers::Solver::getTimeStepping() const {
  return _timeStepping;
}

int exahype::solvers::Solver::getNumberOfVariables() const {
  return _numberOfVariables;
}

int exahype::solvers::Solver::getNumberOfParameters() const {
  return _numberOfParameters;
}

int exahype::solvers::Solver::getNumberOfGlobalObservables() const {
  return _numberOfGlobalObservables;
}


int exahype::solvers::Solver::getNodesPerCoordinateAxis() const {
  return _nodesPerCoordinateAxis;
}

double exahype::solvers::Solver::getMaximumMeshSize() const {
  return _maximumMeshSize;
}

int exahype::solvers::Solver::getCoarsestMeshLevel() const {
  return _coarsestMeshLevel;
}

double exahype::solvers::Solver::getCoarsestMeshSize() const {
  return _coarsestMeshSize;
}

int exahype::solvers::Solver::getMaximumAdaptiveMeshDepth() const {
  return _maximumAdaptiveMeshDepth;
}

int exahype::solvers::Solver::getMaximumAdaptiveMeshLevel() const {
  return _coarsestMeshLevel+_maximumAdaptiveMeshDepth;
}

std::vector<double>& exahype::solvers::Solver::getGlobalObservables() {
  return _globalObservables;
}

// TODO(Lukas) Is this still needed?
/*
std::vector<double>& exahype::solvers::Solver::getNextGlobalObservables() {
  return _nextGlobalObservables;
}
*/

bool exahype::solvers::Solver::hasRequestedAnyMeshRefinement() const {
  return getMeshUpdateEvent()==MeshUpdateEvent::RefinementRequested ||
         getMeshUpdateEvent()==MeshUpdateEvent::InitialRefinementRequested;
}

bool exahype::solvers::Solver::oneSolverIsOfType(const Type& type) {
  bool result = false;
  for (auto* solver : RegisteredSolvers) {
    result |= solver->getType()==type;
  }
  return result;
}

double exahype::solvers::Solver::getMinTimeStampOfAllSolvers() {
  double currentMinTimeStamp = std::numeric_limits<double>::infinity();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    currentMinTimeStamp =
        std::min(currentMinTimeStamp, p->getMinTimeStamp());
  }
  return currentMinTimeStamp;
}


double exahype::solvers::Solver::getMaxTimeStampOfAllSolvers() {
  double currentMaxTimeStamp = -std::numeric_limits<double>::infinity(); // "-", min

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    currentMaxTimeStamp =
        std::max(currentMaxTimeStamp, p->getMinTimeStamp());
  }

  return currentMaxTimeStamp;
}

double exahype::solvers::Solver::estimateMinNextSolverTimeStampOfAllSolvers() {
  double currentMinTimeStamp = std::numeric_limits<double>::infinity();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    currentMinTimeStamp =
        std::min(currentMinTimeStamp, p->getMinTimeStamp()+p->getMinTimeStepSize());
  }
  return currentMinTimeStamp;
}

double exahype::solvers::Solver::getMinTimeStepSizeOfAllSolvers() {
  double currentMinTimeStepSize = std::numeric_limits<double>::infinity();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    currentMinTimeStepSize =
        std::min(currentMinTimeStepSize, p->getMinTimeStepSize());
  }

  return currentMinTimeStepSize;
}

double exahype::solvers::Solver::getMaxSolverTimeStepSizeOfAllSolvers() {
  double currentMaxTimeStepSize = -std::numeric_limits<double>::infinity();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    currentMaxTimeStepSize =
        std::max(currentMaxTimeStepSize, p->getMinTimeStepSize());
  }

  return currentMaxTimeStepSize;
}

bool exahype::solvers::Solver::allSolversUseTimeSteppingScheme(solvers::Solver::TimeStepping scheme) {
  bool result = true;

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    result &= p->_timeStepping==scheme;
  }

  return result;
}

double exahype::solvers::Solver::getCoarsestMaximumMeshSizeOfAllSolvers() {
  static double result = std::numeric_limits<double>::infinity();

  if ( result == std::numeric_limits<double>::infinity() ) {
    for (const auto& p : exahype::solvers::RegisteredSolvers) {
      result = std::min( result, p->getMaximumMeshSize() );
    }
  }

  return result;
}

const tarch::la::Vector<DIMENSIONS,double>& exahype::solvers::Solver::getDomainSize() {
  assertion(RegisteredSolvers.size()>0);
  return RegisteredSolvers[0]->_domainSize;
}

const tarch::la::Vector<DIMENSIONS,double>& exahype::solvers::Solver::getDomainOffset() {
  assertion(RegisteredSolvers.size()>0);
  return RegisteredSolvers[0]->_domainOffset;
}

double exahype::solvers::Solver::getFinestMaximumMeshSizeOfAllSolvers() {
  double result = -std::numeric_limits<double>::infinity(); // "-", min

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    result = std::max( result, p->getMaximumMeshSize() );
  }

  return result;
}

int exahype::solvers::Solver::getCoarsestMeshLevelOfAllSolvers() {
  int result = std::numeric_limits<int>::max();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    result = std::min( result, p->getCoarsestMeshLevel() );
  }

  return result;
}

int exahype::solvers::Solver::getFinestUniformMeshLevelOfAllSolvers() {
  int result = -std::numeric_limits<int>::max();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    result = std::max( result, p->getCoarsestMeshLevel() );
  }

  return result;
}

int exahype::solvers::Solver::getMaximumAdaptiveMeshLevelOfAllSolvers() {
  int result = -std::numeric_limits<int>::max(); // "-", min

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    result = std::max( result, p->getMaximumAdaptiveMeshLevel() );
  }

  return result;
}


double exahype::solvers::Solver::getCoarsestMeshSizeOfAllSolvers() {
  double result = std::numeric_limits<double>::infinity();

  for (const auto& p : exahype::solvers::RegisteredSolvers) {
    result = std::min( result, p->getCoarsestMeshSize() );
  }

  return result;
}

bool exahype::solvers::Solver::allSolversPerformOnlyUniformRefinement() {
  bool result = true;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    result &= solver->getMaximumAdaptiveMeshDepth()==0;
  }
  return result;
}

bool exahype::solvers::Solver::oneSolverRequestedMeshRefinement() {
  bool result = false;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    result |= solver->hasRequestedAnyMeshRefinement();
  }
  return result;
}

bool exahype::solvers::Solver::oneSolverRequestedRefinementStatusSpreading() {
  bool result = false;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    result |= solver->getMeshUpdateEvent()==MeshUpdateEvent::IrregularLimiterDomainChange ||
              solver->getMeshUpdateEvent()==MeshUpdateEvent::RefinementRequested;
  }
  return result;
}

bool exahype::solvers::Solver::oneSolverRequestedLocalRecomputation() {
  bool result = false;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    result |= solver->getMeshUpdateEvent()==MeshUpdateEvent::IrregularLimiterDomainChange;
  }
  return result;
}

bool exahype::solvers::Solver::oneSolverRequestedGlobalRecomputation() {
  bool result = false;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    result |= solver->getMeshUpdateEvent()==MeshUpdateEvent::RefinementRequested;
  }
  return result;
}

int exahype::solvers::Solver::getMaxRefinementStatus() {
  int result = 0;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    switch (solver->getType()) {
      case Type::ADERDG:
        result =
            std::max(result,
                static_cast<exahype::solvers::ADERDGSolver*>(solver)->
                getMinRefinementStatusForTroubledCell());
        break;
      case Type::LimitingADERDG:
        result =
            std::max(result,
                static_cast<exahype::solvers::LimitingADERDGSolver*>(solver)->getSolver()->
                getMinRefinementStatusForTroubledCell());
        break;
      default:
        break;
    }
  }
  return result;
}

bool exahype::solvers::Solver::oneSolverViolatedStabilityCondition() {
  bool result = false;
  for (auto* solver : exahype::solvers::RegisteredSolvers) {
    switch (solver->getType()) {
      case Type::ADERDG:
        result |= static_cast<ADERDGSolver*>(solver)->getStabilityConditionWasViolated();
        break;
      case Type::LimitingADERDG:
        result |=
            static_cast<LimitingADERDGSolver*>(solver)->getSolver().get()->
            getStabilityConditionWasViolated();
        break;
      default:
        break;
    }
  }
  return result;
}

std::string exahype::solvers::Solver::toString(const MeshUpdateEvent& meshUpdateEvent) {
  switch (meshUpdateEvent) {
  case MeshUpdateEvent::None:
    return "None";
  case MeshUpdateEvent::IrregularLimiterDomainChange:
    return "IrregularLimiterDomainChange";
  case MeshUpdateEvent::InitialRefinementRequested:
    return "InitialRefinementRequested";
  case MeshUpdateEvent::RefinementRequested:
    return "RefinementRequested";
  default:
    return "undefined";
  }
}

double exahype::solvers::Solver::convertToDouble(const MeshUpdateEvent& meshUpdateEvent) {
  return static_cast<double>(static_cast<int>(meshUpdateEvent));
}

exahype::solvers::Solver::MeshUpdateEvent exahype::solvers::Solver::convertToMeshUpdateEvent(const double value) {
  assertion((int) std::round(value)>=static_cast<int>(MeshUpdateEvent::None));
  assertion((int) std::round(value)<=static_cast<int>(MeshUpdateEvent::InitialRefinementRequested));
  return static_cast<MeshUpdateEvent>((int) std::round(value));
}

exahype::solvers::Solver::MeshUpdateEvent exahype::solvers::Solver::mergeMeshUpdateEvents(
    const MeshUpdateEvent meshUpdateEvent1,
    const MeshUpdateEvent meshUpdateEvent2) {
  return static_cast<MeshUpdateEvent>(
      std::max( static_cast<int>(meshUpdateEvent1), static_cast<int>(meshUpdateEvent2) )
  );
}

std::string exahype::solvers::Solver::toString() const {
  std::ostringstream stringstr;
  toString(stringstr);
  return stringstr.str();
}

void exahype::solvers::Solver::toString(std::ostream& out) const {
  out << "(";
  out << "_identifier:" << _identifier;
  out << ",";
  out << "_type:" << toString(_type);
  out << ",";
  out << "_numberOfVariables:" << _numberOfVariables;
  out << ",";
  out << "_numberOfParameters:" << _numberOfParameters;
  out << ",";
  out << "_nodesPerCoordinateAxis:" << _nodesPerCoordinateAxis;
  out << ",";
  out << "_maximumMeshSize:" << _maximumMeshSize;
  out << ",";
  out << "_timeStepping:" << toString(_timeStepping);
  out <<  ")";
}

#ifdef Parallel

int exahype::solvers::Solver::computeGeometricLoadBalancingWeight(
    const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS,double>& cellSize) {
  if ( tarch::la::equals(getMinTimeStamp(),0) ) {
    return getGeometricLoadBalancingWeight(cellCentre,cellSize);
  } else {
    return 1;
  }
}

// Neighbours TODO(Dominic): Move in exahype::Vertex

exahype::MetadataHeap::HeapEntries exahype::gatherNeighbourCommunicationMetadata(
    const int cellDescriptionsIndex,
    const tarch::la::Vector<DIMENSIONS,int>& src,
    const tarch::la::Vector<DIMENSIONS,int>& dest) {
  const int length = solvers::RegisteredSolvers.size()*exahype::NeighbourCommunicationMetadataPerSolver;
  MetadataHeap::HeapEntries encodedMetaData;
  encodedMetaData.reserve(length);

  for (unsigned int solverNumber = 0; solverNumber < solvers::RegisteredSolvers.size(); ++solverNumber) {
    auto* solver = solvers::RegisteredSolvers[solverNumber];

    solver->appendNeighbourCommunicationMetadata(
        encodedMetaData,src,dest,cellDescriptionsIndex,solverNumber);
  }
  assertion2(static_cast<int>(encodedMetaData.size())==length,encodedMetaData.size(),length);
  return encodedMetaData;
}

void exahype::sendNeighbourCommunicationMetadata(
    const int                                   toRank,
    const int                                   cellDescriptionsIndex,
    const tarch::la::Vector<DIMENSIONS,int>&    src,
    const tarch::la::Vector<DIMENSIONS,int>&    dest,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level) {
  MetadataHeap::HeapEntries metadata =
      gatherNeighbourCommunicationMetadata(cellDescriptionsIndex,src,dest);

  MetadataHeap::getInstance().sendData(
      metadata,toRank,
      x,level,peano::heap::MessageType::NeighbourCommunication);
}

void exahype::sendNeighbourCommunicationMetadataSequenceWithInvalidEntries(
    const int                                   toRank,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level) {
   MetadataHeap::HeapEntries metadata(0);
  #if defined(UsePeanosSymmetricBoundaryExchangerForMetaData)
  // We currently do not send an empty metadata message
  const unsigned int length =
      exahype::NeighbourCommunicationMetadataPerSolver*exahype::solvers::RegisteredSolvers.size();
  metadata.reserve(length);
  metadata.assign(length, InvalidMetadataEntry);
  assertion(metadata.size()==length);
  #endif

  MetadataHeap::getInstance().sendData(
      metadata,toRank,x,level,
      peano::heap::MessageType::NeighbourCommunication);
}

void
exahype::receiveNeighbourCommunicationMetadata(
    MetadataHeap::HeapEntries&                  buffer,
    const int                                   fromRank,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level) {
  const size_t length = NeighbourCommunicationMetadataPerSolver*solvers::RegisteredSolvers.size();
  buffer.reserve(length);
  buffer.clear();
  assertion(buffer.size()==0);
  assertion(buffer.capacity()>=length);

  MetadataHeap::HeapEntries receivedMessage =
      MetadataHeap::getInstance().receiveData(
          fromRank, x, level,peano::heap::MessageType::NeighbourCommunication);
  assertion(receivedMessage.size()==0 || receivedMessage.size()==length);

  buffer.insert(buffer.begin(),receivedMessage.begin(),receivedMessage.end());
  if ( buffer.size()==0 ) {
    buffer.assign(length,InvalidMetadataEntry);
  }
}

// Master<=>Worker

/**
 * Drop metadata sent by rank \p fromRank.
 */
void exahype::dropMetadata(
    const int                                   fromRank,
    const peano::heap::MessageType&             messageType,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level) {
  MetadataHeap::getInstance().receiveData(
      fromRank,x,level,messageType);
}

void exahype::solvers::Solver::sendMeshUpdateEventToMaster(
    const int                                    masterRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) const {
  DataHeap::HeapEntries meshUpdateEvent(0,1); // !!! does not fill the vector
  meshUpdateEvent.push_back(convertToDouble( getMeshUpdateEvent() ));

  assertion1(meshUpdateEvent.size()==1,meshUpdateEvent.size());
  if (tarch::parallel::Node::getInstance().getRank()!=
      tarch::parallel::Node::getInstance().getGlobalMasterRank()) {
/*
    logDebug("sendDataToMaster(...)","sending mesh update event: " <<
             "data[0]=" << toString(convertToMeshUpdateEvent( meshUpdateEvent[0] )) <<
	     ",_meshUpdateEvent=" << toString( getMeshUpdateEvent() ) <<
             ",_nextMeshUpdateEvent=" << toString( getNextMeshUpdateEvent() ));
*/
  }

  // MPI_Send(
  //   meshUpdateEvent.data(), meshUpdateEvent.size(),
  //   MPI_DOUBLE,
  //   masterRank,
  //   MasterWorkerCommunicationTag,
  //   tarch::parallel::Node::getInstance().getCommunicator());

  DataHeap::getInstance().sendData(
      meshUpdateEvent.data(), meshUpdateEvent.size(),
      masterRank,x,level,peano::heap::MessageType::MasterWorkerCommunication);
}

void exahype::solvers::Solver::mergeWithWorkerMeshUpdateEvent(
    const int                                    workerRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) {
  DataHeap::HeapEntries messageFromWorker(1); // !!! fills the vector
  
  // MPI_Recv(
  //   messageFromWorker.data(), messageFromWorker.size(),
  //   MPI_DOUBLE,
  //   workerRank,
  //   MasterWorkerCommunicationTag,
  //   tarch::parallel::Node::getInstance().getCommunicator(),
  //   MPI_STATUS_IGNORE);

  DataHeap::getInstance().receiveData(
      messageFromWorker.data(),messageFromWorker.size(),workerRank, x, level,
      peano::heap::MessageType::MasterWorkerCommunication);

  updateMeshUpdateEvent( convertToMeshUpdateEvent( messageFromWorker[0] ) );
  assertion1(messageFromWorker.size()==1,messageFromWorker.size());

  if (tarch::parallel::Node::getInstance().getRank()==
      tarch::parallel::Node::getInstance().getGlobalMasterRank()) {
/*
    logDebug("mergeWithWorkerData(...)","merged with worker's mesh update event: " <<
             "data[0]=" << toString(convertToMeshUpdateEvent( messageFromWorker[0] )) <<
	     ",_meshUpdateEvent=" << toString( getMeshUpdateEvent() ) <<
             ",_nextMeshUpdateEvent=" << toString( getNextMeshUpdateEvent() ));
*/
  }
}
#endif


