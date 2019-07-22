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
 **/

#include "LimitingADERDG2UserDefined.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"
#include "peano/utils/Loop.h"

#include "exahype/solvers/ADERDGSolver.h"

#include "kernels/DGBasisFunctions.h"


std::string exahype::plotters::LimitingADERDG2UserDefined::getIdentifier() {
  return "user::defined";
}

exahype::plotters::LimitingADERDG2UserDefined::LimitingADERDG2UserDefined():
  Device(nullptr),
  _order(-1),
  _variables(-1),
  _writtenVariables(-1) {
}


void exahype::plotters::LimitingADERDG2UserDefined::init(
  const std::string& filename,
  int                orderPlusOne,
  int                variables,
  int                writtenVariables,
  exahype::parser::ParserView plotterParameters
) {
  _filename         = filename;
  _order            = orderPlusOne-1;
  _variables        = variables;
  _plotterParameters           = plotterParameters;
  _writtenVariables = writtenVariables;
}

exahype::plotters::LimitingADERDG2UserDefined::~LimitingADERDG2UserDefined() {
}

void exahype::plotters::LimitingADERDG2UserDefined::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& solverPatch  = cellInfo._ADERDGCellDescriptions[element];

  if (solverPatch.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    assertion(exahype::solvers::RegisteredSolvers[solverPatch.getSolverNumber()]->getType()==
        exahype::solvers::Solver::Type::LimitingADERDG);
    auto* limitingADERDG =
        static_cast<exahype::solvers::LimitingADERDGSolver*>(exahype::solvers::RegisteredSolvers[solverPatch.getSolverNumber()]);

    if (solverPatch.getRefinementStatus()>=limitingADERDG->getSolver()->getMinRefinementStatusForTroubledCell()-1) {
      auto& limiterPatch = limitingADERDG->getLimiterPatch(solverPatch,cellInfo);

      double* limiterSolution = static_cast<double*>(limiterPatch.getSolution());

      plotFiniteVolumesPatch(
          limiterPatch.getOffset(),
          limiterPatch.getSize(), limiterSolution,
          limiterPatch.getTimeStamp()); // The limiter time stamp might not be valid at the time of the plotting
    } else { // solverPatch.getRefinementStatus()<exahype::solvers::ADERDGSolver::MinimumRefinementStatusForActiveFVPatch
      double* solverSolution = static_cast<double*>(solverPatch.getSolution());

      plotADERDGPatch(
          solverPatch.getOffset(),
          solverPatch.getSize(), solverSolution,
          solverPatch.getTimeStamp());
    }
  }
}
