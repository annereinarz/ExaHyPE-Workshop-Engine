#include "exahype/solvers/FiniteVolumesSolver.h"

exahype::solvers::FiniteVolumesSolver::FusedTimeStepJob::FusedTimeStepJob(
  FiniteVolumesSolver& solver,
  CellDescription&     cellDescription,
  CellInfo&            cellInfo,
  const bool           isFirstTimeStepOfBatch,
  const bool           isLastTimeStepOfBatch,
  const bool           isSkeletonJob)
  :
  tarch::multicore::jobs::Job(
      tarch::multicore::jobs::JobType::BackgroundTask,0,
      getTaskPriority(isLastTimeStepOfBatch)
  ),
  _solver(solver),
  _cellDescription(cellDescription),
  _cellInfo(cellInfo),
  _neighbourMergePerformed(cellDescription.getNeighbourMergePerformed()),
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

bool exahype::solvers::FiniteVolumesSolver::FusedTimeStepJob::run() {
  UpdateResult result =
      _solver.updateBody(
          _cellDescription,_cellInfo,_neighbourMergePerformed,
          _isFirstTimeStepOfBatch,_isLastTimeStepOfBatch,
          _isSkeletonJob,false/*uncompressBefore*/);

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
