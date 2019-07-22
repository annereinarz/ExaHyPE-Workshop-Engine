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

extern "C" {
void adervolumeintegralnonlinear_(double* lduh, const double* const lFhi_x, const double* const lFhi_y,
                         const double* const lFhi_z, const double* const lShi, const double* const dx);
}

void kernels::aderdg::generic::fortran::volumeIntegralNonlinear(
    double* lduh, const double* const lFhi,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    const int numberOfVariables, const int numberOfParameters, const int basisSize) {

  // lFhiFortran = [ lFhi_x | lFhi_y | lFhi_z ]
  const double* const lFhi_x =
      &lFhi[0 * numberOfVariables * basisSize * basisSize * basisSize];
  const double* const lFhi_y =
      &lFhi[1 * numberOfVariables * basisSize * basisSize * basisSize];
  const double* const lFhi_z =
      &lFhi[2 * numberOfVariables * basisSize * basisSize * basisSize];
  const double* const lShi =
      &lFhi[3 * numberOfVariables * basisSize * basisSize * basisSize];

  // std::ofstream ofs;
  // ofs.open ("boutput_lFhi.txt", std::ofstream::out);
  // for (int ii=0;
  // ii<numberOfVariables*DIMENSIONS*basisSize*basisSize*basisSize; ii++) {
  // ofs << lFhi[ii] << "\n";
  // }
  // ofs.close();

  // ofs.open ("boutput_lFhiFortran.txt", std::ofstream::out);
  // for (int ii=0;
  // ii<numberOfVariables*DIMENSIONS*basisSize*basisSize*basisSize; ii++) {
  // ofs << lFhiFortran[ii] << "\n";
  // }
  // ofs.close();

  // cout << "-------------lFhi in volumeIntegral.cpph------------------" <<
  // "\n";
  // cout << lFhi[0] << "\n";
  // cout << lFhi[1]<< "\n";
  // cout << lFhi[2] << "\n";
  // cout << lFhi[3] << "\n";
  // cout << lFhi[4] << "\n";
  // cout << lFhi[5] << "\n";
  // cout << "-------------lFhi in volumeIntegral.cpph------------------" <<
  // "\n";

  adervolumeintegralnonlinear_(lduh, lFhi_x, lFhi_y, lFhi_z, lShi, dx.data());

}
