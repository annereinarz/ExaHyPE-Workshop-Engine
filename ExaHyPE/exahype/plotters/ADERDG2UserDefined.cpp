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

#include "ADERDG2UserDefined.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"
#include "peano/utils/Loop.h"

#include "exahype/solvers/ADERDGSolver.h"

#include "kernels/DGBasisFunctions.h"


std::string exahype::plotters::ADERDG2UserDefined::getIdentifier() {
  return "user::defined";
}

exahype::plotters::ADERDG2UserDefined::ADERDG2UserDefined():
  Device(nullptr),
  _order(-1),
  _variables(-1),
  _writtenVariables(-1) {
}


void exahype::plotters::ADERDG2UserDefined::init(
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

void exahype::plotters::ADERDG2UserDefined::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  auto& aderdgCellDescription  = cellInfo._ADERDGCellDescriptions[element];

  if (aderdgCellDescription.getType()==exahype::solvers::ADERDGSolver::CellDescription::Type::Cell) {
    double* solverSolution = static_cast<double*>(aderdgCellDescription.getSolution());

    plotPatch(
        aderdgCellDescription.getOffset(),
        aderdgCellDescription.getSize(), solverSolution,
        aderdgCellDescription.getTimeStamp());
  }
}
