#include "ADERDGSolver.h"

exahype::solvers::ADERDGSolver::AdjustSolutionDuringMeshRefinementJob::AdjustSolutionDuringMeshRefinementJob(
  ADERDGSolver&    solver,
  CellDescription& cellDescription,
  const bool       isInitialMeshRefinement):
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

bool exahype::solvers::ADERDGSolver::AdjustSolutionDuringMeshRefinementJob::run() {
  _solver.ensureNecessaryMemoryIsAllocated(_cellDescription);
  _solver.adjustSolutionDuringMeshRefinementBody(_cellDescription,_isInitialMeshRefinement);
  NumberOfAMRBackgroundJobs.fetch_sub(1);
  assertion( NumberOfAMRBackgroundJobs.load()>=0 );
  return false;
}
