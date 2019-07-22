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
 
#include "kernels/aderdg/generic/Kernels.h"

#include "string.h"

#include "tarch/la/Scalar.h"
#include "tarch/la/ScalarOperations.h"

#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/DGMatrices.h"

using std::endl;
using std::cout;

extern "C" {
void adersurfaceintegralnonlinear_(double* lduh, const double* const lFhi, const double* const dx);
}

void kernels::aderdg::generic::fortran::surfaceIntegralNonlinear(
    double* lduh, const double* const lFbnd,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    const int numberOfVariables, const int basisSize) {

  // This Fortran subroutine does not work any more (assumably because of the
  // lFbnd data ordering)

  adersurfaceintegralnonlinear_(lduh, lFbnd, dx.data());

}
