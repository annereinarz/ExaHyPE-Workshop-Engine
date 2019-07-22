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
#ifndef LIMITERPROJECTIONMATRICES_H_
#define LIMITERPROJECTIONMATRICES_H_

#include <set>

//TODO remove when generated
#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/GaussLobattoQuadrature.h"
#include "kernels/KernelUtils.h"
#include <stdlib.h>

namespace kernels {
/**
 * Initialises the lookup tables \p luh2lim, \p luh2lob
 * and \p lim2luh for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see freeLimiterProjectionMatrices
 */
void initLimiterProjectionMatrices(const std::set<int>& orders);

/**
 * Frees the memory that was allocated for the lookup tables \p luh2lim, \p luh2lob
 * and \p lim2luh for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see initLimiterProjectionMatrices
 */
void freeLimiterProjectionMatrices(const std::set<int>& orders);


extern double** uh2lim;
extern double** uh2lob;
extern double** lim2uh;

//TODO JMG remove when generated
void BaseFunc1D(double* phi, double xi, const int basisSize);
double* matrixInverse(int n, double* a);
}

#endif //LIMITERPROJECTIONMATRICES_H_
