#include "ADERDGSolver.h"

#if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
#include <immintrin.h>
#endif

exahype::solvers::ADERDGSolver::PredictionJob::PredictionJob(
    ADERDGSolver&    solver,
    CellDescription& cellDescription,
    const int        cellDescriptionsIndex,
    const int        element,
    const double     predictorTimeStamp,
    const double     predictorTimeStepSize,
    const bool       uncompressBefore,
    const bool       isSkeletonJob,
    const bool       addVolumeIntegralResultToUpdate):
    tarch::multicore::jobs::Job(
        tarch::multicore::jobs::JobType::BackgroundTask,0,
        getTaskPriority(isSkeletonJob)
    ), // ! high priority only if skeleton job
    _solver(solver),
    _cellDescription(cellDescription),
    _cellDescriptionsIndex(cellDescriptionsIndex),
    _element(element),
    _predictorTimeStamp(predictorTimeStamp),
    _predictorTimeStepSize(predictorTimeStepSize),
    _uncompressBefore(uncompressBefore),
    _isSkeletonJob(isSkeletonJob),
    _addVolumeIntegralResultToUpdate(addVolumeIntegralResultToUpdate) {
  if (_isSkeletonJob) {
    NumberOfSkeletonJobs.fetch_add(1);
  } else {
    NumberOfEnclaveJobs.fetch_add(1);
  }
}


bool exahype::solvers::ADERDGSolver::PredictionJob::run() {
  _solver.predictionAndVolumeIntegralBody(
      _cellDescription,_predictorTimeStamp,_predictorTimeStepSize,
      _uncompressBefore,_isSkeletonJob,_addVolumeIntegralResultToUpdate); // ignore return value

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
void exahype::solvers::ADERDGSolver::PredictionJob::prefetchData() {
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

