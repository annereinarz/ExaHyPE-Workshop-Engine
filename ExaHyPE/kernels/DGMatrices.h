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
#ifndef EXAHYPE_KERNELS_DGMATRICES_H
#define EXAHYPE_KERNELS_DGMATRICES_H

#include <set>

namespace kernels {
/**
 * Initialises the lookup tables
 * \p Kxi, ... \p fineGridProjector1d
 * for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see freeDGMatrices
 */
void initDGMatrices(const std::set<int>& orders);

/**
 * Frees the memory that was allocated for the lookup tables
 * \p Kxi, ... \p fineGridProjector1d
 * for the specified \p orders.
 *
 * \todo default implementation!
 *
 * \see initGaussLegendreNodesAndWeights
 */
void freeDGMatrices(const std::set<int>& orders);

/**
 * \brief Element stiffness matrix
 */
// todo Dominic Etienne Charrier
// order,row,column
// [order+1][order+1][order+1]
extern double*** Kxi;

/**
 * \brief Discrete derivative operator which projects the derivatives onto the
 * basis
 */
extern double*** dudx;

/**
 * \brief Transposed inverse stiffness matrix
 */
// todo Dominic Etienne Charrier
// order, row, column
// [order+1][order+1][order+1]
extern double*** iK1;

/**
 * \brief Left extrapolation coefficients
 */
// todo Dominic Etienne Charrier
// order, row,
// [order+1][order+1]
extern double** FLCoeff;

/**
 * \brief Right extrapolation coefficients
 */
// todo Dominic Etienne Charrier
// order, row
// [order+1][order+1];
extern double** FRCoeff;

/**
 * \brief Joint extrapolation coefficients
 *
 * FCoeff = [[FLCoeff];[FRCoeff]]
 */
// todo Dominic Etienne Charrier
// order, left/right, row
// [order][2][order+1];
extern double*** FCoeff;

/**
 * Transforms the degrees of freedom located at the non-equidistant
 * Gauss-Legendre nodes to degrees of freedoms located at nodes of an
 * equidistant grid over (0,1).
 * Let us denote by \f$P\f$ the 1-$d$ projection operator. The equidistant DoF
 * are computed according to:
 *
 * The matrix is indexed the following way: [order][DG DoF][equidistant grid
 * DoF].
 */
extern double*** equidistantGridProjector1d;

/**
 * This operator is used to transforms the degrees of freedom (DoF) located on a
 * coarse grid edge nodes to degrees of freedoms located on nodes of a fine grid
 * edge and vice versa. The difference in levels is always equal to 1.
 *
 * The matrix is indexed the following way: [order][subinterval][coarse grid
 * DoF][fine grid DoF].
 */
extern double**** fineGridProjector1d;
}

#endif
