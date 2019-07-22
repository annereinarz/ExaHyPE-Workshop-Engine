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
#ifndef GAUSSLOBATTO_H_
#define GAUSSLOBATTO_H_

#include <set>

namespace kernels {
/**
 * Initialises the lookup tables \p gaussLobattoWeights
 * and \p gaussLobattoNodes for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see freeGaussLobattoNodesAndWeights
 */
void initGaussLobattoNodesAndWeights(const std::set<int>& orders);

/**
 * Frees the memory that was allocated for the lookup tables \p gaussLobattoWeights
 * and \p gaussLobattoNodes for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see initGaussLobattoNodesAndWeights
 */
void freeGaussLobattoNodesAndWeights(const std::set<int>& orders);

/**
 * The Gauss-Lobatto weights mapped onto [0,1]. Array of arrays. The first
 * entry is the order, the second entry the Lobatto point.
 */
extern double** gaussLobattoWeights;

/**
 * The Gauss-Lobatto nodes mapped onto [0,1]. Array of arrays. The first entry
 * is the order, the second entry the Lobatto point.
 */
extern double** gaussLobattoNodes;
}

#endif //GAUSSLOBATTO_H_
