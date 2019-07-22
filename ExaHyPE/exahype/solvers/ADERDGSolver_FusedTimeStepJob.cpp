#include "ADERDGSolver.h"

#if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
#include <immintrin.h>
#endif


exahype::solvers::ADERDGSolver::FusedTimeStepJob::FusedTimeStepJob(
  ADERDGSolver&    solver,
  CellDescription& cellDescription,
  CellInfo&        cellInfo,
  const double     predictionTimeStamp,
  const double     predictionTimeStepSize,
  const bool       isFirstTimeStepOfBatch,
  const bool       isLastTimeStepOfBatch,
  const bool       isSkeletonJob)
  :
  tarch::multicore::jobs::Job(
      tarch::multicore::jobs::JobType::BackgroundTask,0,
      getTaskPriority(isLastTimeStepOfBatch)
  ),
  _solver(solver),
  _cellDescription(cellDescription),
  _cellInfo(cellInfo),
  _neighbourMergePerformed(cellDescription.getNeighbourMergePerformed()),
  _predictionTimeStamp   (predictionTimeStamp   ),
  _predictionTimeStepSize(predictionTimeStepSize),
  _isFirstTimeStepOfBatch(isFirstTimeStepOfBatch),
  _isLastTimeStepOfBatch(isLastTimeStepOfBatch),
  _isSkeletonJob(isSkeletonJob) {
  NumberOfReductionJobs.fetch_add(1);
  if (_isSkeletonJob) {
    NumberOfSkeletonJobs.fetch_add(1);
  } else {
    NumberOfEnclaveJobs.fetch_add(1);
  }
}

bool exahype::solvers::ADERDGSolver::FusedTimeStepJob::run() {
  UpdateResult result =
      _solver.fusedTimeStepBody(
          _cellDescription, _cellInfo, _neighbourMergePerformed,
          _predictionTimeStamp,_predictionTimeStepSize,
          _isFirstTimeStepOfBatch,_isLastTimeStepOfBatch,
          _isSkeletonJob,false/*mustBeDoneImmediately*/);

  if (_isLastTimeStepOfBatch) {
    _solver.updateMeshUpdateEvent(result._meshUpdateEvent);
    _solver.updateAdmissibleTimeStepSize(result._timeStepSize);
  }

  NumberOfReductionJobs.fetch_sub(1);
  assertion( NumberOfReductionJobs.load()>=0 );
  if (_isSkeletonJob) {
    NumberOfSkeletonJobs.fetch_sub(1);
    assertion( NumberOfSkeletonJobs.load()>=0 );
  } else {
    NumberOfEnclaveJobs.fetch_sub(1);
    assertion( NumberOfEnclaveJobs.load()>=0 );
  }
  return false;
}


//
// @see PredictionJob
//
void exahype::solvers::ADERDGSolver::FusedTimeStepJob::prefetchData() {
  #if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
  double* luh  = static_cast<double*>(_cellDescription.getSolution());
  double* lduh = static_cast<double*>(_cellDescription.getUpdate());
  double* lQhbnd = static_cast<double*>(_cellDescription.getExtrapolatedPredictor());
  double* lFhbnd = static_cast<double*>(_cellDescription.getFluctuation());

  _mm_prefetch(luh, _MM_HINT_NTA);
  _mm_prefetch(lduh, _MM_HINT_NTA);
  _mm_prefetch(lQhbnd, _MM_HINT_NTA);
  _mm_prefetch(lFhbnd, _MM_HINT_NTA);
  #endif
}
