#include "exahype/solvers/FiniteVolumesSolver.h"

#if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
#include <immintrin.h>
#endif


exahype::solvers::FiniteVolumesSolver::UpdateJob::UpdateJob(
  FiniteVolumesSolver&    solver,
  CellDescription& cellDescription,
  CellInfo&        cellInfo,
  const bool       isAtRemoteBoundary):
  tarch::multicore::jobs::Job(
      tarch::multicore::jobs::JobType::BackgroundTask,0,
      getHighPriorityTaskPriority()
  ), // ! always high priority
  _solver(solver),
  _cellDescription(cellDescription),
  _cellInfo(cellInfo),
  _neighbourMergePerformed(cellDescription.getNeighbourMergePerformed()),
  _isAtRemoteBoundary(isAtRemoteBoundary) {
  NumberOfReductionJobs.fetch_add(1);
}

bool exahype::solvers::FiniteVolumesSolver::UpdateJob::run() {
  UpdateResult result =
      _solver.updateBody(
          _cellDescription,_cellInfo,_neighbourMergePerformed,
          true,true,_isAtRemoteBoundary,true);
  NumberOfReductionJobs.fetch_sub(1);
  assertion( NumberOfReductionJobs.load()>=0 );

  _solver.updateAdmissibleTimeStepSize(result._timeStepSize);
  return false;
}


//
// @see UpdateJob
//
void exahype::solvers::FiniteVolumesSolver::UpdateJob::prefetchData() {
  #if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
  double* luh    = static_cast<double*>(_cellDescription.getSolution());
  double* luhOld = static_cast<double*>(_cellDescription.getPreviousSolution());

  _mm_prefetch(luh,    _MM_HINT_NTA);
  _mm_prefetch(luhOld, _MM_HINT_NTA);
  #endif
}
