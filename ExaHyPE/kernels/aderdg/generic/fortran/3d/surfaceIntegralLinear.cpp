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
void adersurfaceintegrallinear_(double* lduh, double* lFhi, double* dx);
}

void kernels::aderdg::generic::fortran::surfaceIntegralLinear(
    double* lduh, const double* const lFbnd,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    const int numberOfVariables, const int basisSize) {
    
  // circumvent 'const double'
  double* lFbndFortran = new double[numberOfVariables * 6 * basisSize * basisSize];
  for (int i = 0; i < numberOfVariables * 6 * basisSize * basisSize; i++) {
    lFbndFortran[i] = lFbnd[i];
  }

  double* dxTemp = new double[3];
  dxTemp[0] = dx[0];
  dxTemp[1] = dx[1];
  dxTemp[2] = dx[2];

  adersurfaceintegrallinear_(lduh, lFbndFortran, dxTemp);

  delete[] lFbndFortran;
  delete[] dxTemp;
}
