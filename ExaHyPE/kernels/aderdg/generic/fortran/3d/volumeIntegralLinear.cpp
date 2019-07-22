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

#include <fstream> 

using std::endl;
using std::cout;



extern "C" 
{
  void adervolumeintegrallinear_(double *lduh, double *lFhi, double *dx);
}
void kernels::aderdg::generic::fortran::volumeIntegralLinear(
    double* lduh, const double* const lFhi,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    const int numberOfVariables, const int numberOfParameters, const int basisSize) {

  // // todo Angelika
  // // Please remove the typedefs in generic kernels again since numberOf(...)Dof is not
  // // a compile time variable anymore

 
  double* lFhiFortran = new double[numberOfVariables*DIMENSIONS*basisSize*basisSize*basisSize];
  for(int i=0; i < numberOfVariables*DIMENSIONS*basisSize*basisSize*basisSize; i++){
    lFhiFortran[i] = lFhi[i];
  }

  
  double* dxTemp = new double[3];
  dxTemp[0]= dx[0];
  dxTemp[1]= dx[1];
  dxTemp[2]= dx[2];
  
  adervolumeintegrallinear_(lduh, lFhiFortran, dxTemp);

  

  delete[] lFhiFortran;
  delete[] dxTemp;
}

