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

#include "tarch/la/Scalar.h"
#include "tarch/la/ScalarOperations.h"

#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/DGMatrices.h"

#if DIMENSIONS == 3
void kernels::aderdg::generic::fortran::faceUnknownsProlongation(double* lQhbndFine,
                              double* lFhbndFine,
                              const double* lQhbndCoarse,
                              const double* lFhbndCoarse,
                              const int coarseGridLevel,
                              const int fineGridLevel,
                              const tarch::la::Vector<DIMENSIONS-1, int>& subfaceIndex,
                              const int numberOfVariables, const int basisSize){
  // @todo Please implement!
}

void kernels::aderdg::generic::fortran::faceUnknownsRestriction(double* lQhbndCoarse,
                             double* lFhbndCoarse,
                             const double* lQhbndFine,
                             const double* lFhbndFine,
                             const int coarseGridLevel,
                             const int fineGridLevel,
                             const tarch::la::Vector<DIMENSIONS-1, int>& subfaceIndex,
                             const int numberOfVariables, const int basisSize){
  // @todo Please implement!
}

void kernels::aderdg::generic::fortran::volumeUnknownsProlongation(double* luhFine,
                                const double* luhCoarse,
                                const int coarseGridLevel,
                                const int fineGridLevel,
                                const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
                                const int numberOfVariables, const int basisSize){
  // @todo Please implement!
}

void kernels::aderdg::generic::fortran::volumeUnknownsRestriction(double* luhCoarse,
                               const double* luhFine,
                               const int coarseGridLevel,
                               const int fineGridLevel,
                               const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
                               const int numberOfVariables, const int basisSize){
  // @todo Please implement!
}
#endif
