#include "exahype/solvers/SolverCoupling.h"


std::vector<exahype::solvers::SolverCoupling*>  exahype::solvers::RegisteredSolverCouplings;


exahype::solvers::SolverCoupling::SolverCoupling(Type type, double time, double repeat):
  _type(type),
  _repeat(repeat),
  _nextSnapshot(time) {
}


bool exahype::solvers::SolverCoupling::isActive(double timeStamp) {
  bool result = timeStamp > _nextSnapshot;
  if (result) {
    _nextSnapshot += _repeat;
    return true;
  }
  else return false;
}


exahype::solvers::SolverCoupling::Type exahype::solvers::SolverCoupling::getType() const {
  return _type;
}
