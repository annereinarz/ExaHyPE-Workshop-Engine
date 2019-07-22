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

#include "FiniteVolumes2UserDefined.h"
#include "tarch/parallel/Node.h"

#include "kernels/DGMatrices.h"
#include "peano/utils/Loop.h"

#include "exahype/solvers/FiniteVolumesSolver.h"

#include "kernels/DGBasisFunctions.h"


std::string exahype::plotters::FiniteVolumes2UserDefined::getIdentifier() {
  return "user::defined";
}

exahype::plotters::FiniteVolumes2UserDefined::FiniteVolumes2UserDefined():
  Device(nullptr),
  _order(-1),
  _variables(-1),
  _writtenVariables(-1) {
}


void exahype::plotters::FiniteVolumes2UserDefined::init(
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

exahype::plotters::FiniteVolumes2UserDefined::~FiniteVolumes2UserDefined() {
}

void exahype::plotters::FiniteVolumes2UserDefined::plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) {
  const int element = cellInfo.indexOfFiniteVolumesCellDescription(solverNumber);
  auto& finiteVolumesCellDescription  = cellInfo._FiniteVolumesCellDescriptions[element];

  if (finiteVolumesCellDescription.getType()==exahype::solvers::FiniteVolumesSolver::CellDescription::Type::Cell) {
    double* solverSolution = static_cast<double*>(finiteVolumesCellDescription.getSolution());

    plotPatch(
        finiteVolumesCellDescription.getOffset(),
        finiteVolumesCellDescription.getSize(), solverSolution,
        finiteVolumesCellDescription.getTimeStamp());
  }
}
