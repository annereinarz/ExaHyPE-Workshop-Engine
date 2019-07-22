#include "exahype/solvers/FiniteVolumesSolver.h"

exahype::solvers::FiniteVolumesSolver::AdjustSolutionDuringMeshRefinementJob::AdjustSolutionDuringMeshRefinementJob(
  FiniteVolumesSolver& solver,
  CellDescription&     cellDescription,
  const bool           isInitialMeshRefinement):
  tarch::multicore::jobs::Job(
      tarch::multicore::jobs::JobType::BackgroundTask,0,
      getDefaultTaskPriority()
  ),
  _solver(solver),
  _cellDescription(cellDescription),
  _isInitialMeshRefinement(isInitialMeshRefinement)
{
  NumberOfAMRBackgroundJobs.fetch_add(1);
}

bool exahype::solvers::FiniteVolumesSolver::AdjustSolutionDuringMeshRefinementJob::run() {
  _solver.adjustSolutionDuringMeshRefinementBody(_cellDescription,_isInitialMeshRefinement);

  NumberOfAMRBackgroundJobs.fetch_sub(1);
  assertion( NumberOfAMRBackgroundJobs>=0 );
  return false;
}
