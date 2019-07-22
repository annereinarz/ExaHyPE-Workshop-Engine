#include "exahype/solvers/CellWiseCoupling.h"


exahype::solvers::CellWiseCoupling::CellWiseCoupling(double time, double repeat):
  SolverCoupling( SolverCoupling::Type::CellWise, time, repeat) {
}
