#include "exahype/solvers/LimitingADERDGSolver.h"

exahype::solvers::LimitingADERDGSolver::AdjustSolutionDuringMeshRefinementJob::AdjustSolutionDuringMeshRefinementJob(
  LimitingADERDGSolver& solver,
  SolverPatch&          solverPatch,
  CellInfo&             cellInfo,
  const bool            isInitialMeshRefinement):
  tarch::multicore::jobs::Job(
      tarch::multicore::jobs::JobType::BackgroundTask,0,
      getDefaultTaskPriority()
  ),
  _solver(solver),
  _solverPatch(solverPatch),
  _cellInfo(cellInfo),
  _isInitialMeshRefinement(isInitialMeshRefinement)
{
  NumberOfAMRBackgroundJobs.fetch_add(1);
}

bool exahype::solvers::LimitingADERDGSolver::AdjustSolutionDuringMeshRefinementJob::run() {
  _solver._solver->ensureNecessaryMemoryIsAllocated(_solverPatch);
  _solver.adjustSolutionDuringMeshRefinementBody(_solverPatch,_cellInfo,_isInitialMeshRefinement);

  NumberOfAMRBackgroundJobs.fetch_sub(1);
  assertion( NumberOfAMRBackgroundJobs.load()>=0 );
  return false;
}
