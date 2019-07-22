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
#ifndef GAUSSLEGENDRE_H_
#define GAUSSLEGENDRE_H_

#include <set>

namespace kernels {
/**
 * Initialises the lookup tables \p gaussLegendreWeights
 * and \p gaussLegendreNodes for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see freeGaussLegendreNodesAndWeights
 */
void initGaussLegendreNodesAndWeights(const std::set<int>& orders);

/**
 * Frees the memory that was allocated for the lookup tables \p gaussLegendreWeights
 * and \p gaussLegendreNodes for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see initGaussLegendreNodesAndWeights
 */
void freeGaussLegendreNodesAndWeights(const std::set<int>& orders);

/**
 * The Gauss-Legendre weights mapped onto [0,1]. Array of arrays. The first
 * entry is the order, the second entry the Legendre point.
 */
extern double** gaussLegendreWeights;

/**
 * The Gauss-Legendre nodes mapped onto [0,1]. Array of arrays. The first entry
 * is the order, the second entry the Legendre point.
 */
extern double** gaussLegendreNodes;
}

#endif
