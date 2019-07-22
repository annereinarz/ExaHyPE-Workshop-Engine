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

/** \file DGBasisFunctions.h
 *  \brief Header including Gauss-Legendre quadrature weights and abscissas
 *
 *  A Gauss-Legendre quadrature with \f$n\f$ nodes integrates a polynomial
 *  of order \f$2\,n-1\f$ exactly.
 */

///////////////////////////////////////////////////////////////////////////////
#ifndef DGBASISFUNCTIONS_H_
#define DGBASISFUNCTIONS_H_

#include <set>

namespace kernels {

/**
 * A scalar-valued function that takes one argument.
 */
typedef double (* UnivariateFunction) (const double s);

/**
 * Initialises the lookup tables \p basisFunctions, \p basisFunctionFirstDerivatives,
 * and \p basisFunctionSecondDerivatives for the specified \p orders.
 *
 * \see freeGaussLegendreNodesAndWeights
 */
void initBasisFunctions(const std::set<int>& orders);

/**
 * Frees the memory that was allocated for the lookup tables \p basisFunctions, \p basisFunctionFirstDerivatives,
 * and \p basisFunctionSecondDerivatives for the specified \p orders.
 *
 * \see initGaussLegendreNodesAndWeights
 */
void freeBasisFunctions(const std::set<int>& orders);

/**
 * The scalar-valued univariate basis functions.
 */
extern UnivariateFunction** basisFunctions;

/**
 * The first derivatives of the scalar-valued univariate basis functions.
 */
extern UnivariateFunction** basisFunctionFirstDerivatives;

/**
 * The second derivatives of the scalar-valued univariate basis functions.
 */
extern UnivariateFunction** basisFunctionSecondDerivatives;

/**
 * Returns the exact (point-wise) interpoland for position for x
 *
 * If you interpolate onto a equidistant grid, you can alternatively use the
 * array equidistantGridProjector1d that holds all mapping quantities in a
 * precomputed table and thus is faster.
 *
 * @param offsetOfPatch Array of doubles of size DIMENSIONS. If you use
 *          Peano's tarch::la::Vector class, apply its data() function to
 *          get hold of a pointer to be passed into this function
 * @param x Position where to evaluate the function. This has to be a point
 *          within the patch
 * @param numberOfUnknowns Number of unknowns held per grid point withint the
 *          patch
 * @param unknown Which unknown to evaluate
 * @param order   Which order is used
 * @param u       Pointer to unknowns
 */
double interpolate(
  const double*                                      offsetOfPatch,
  const double*                                      sizeOfPatch,
  const double*                                      x,
  int                                          numberOfUnknowns,
  int                                          unknown,
  int                                          order,
  const double*                                      u
);
}

/** Power functions requiring a small number of multiplications. */
inline double pow_2  ( const double x ) { return x*x;   }
inline double pow_3  ( const double x ) { return x*x*x; }
inline double pow_4  ( const double x ) { double x2 = x*x;   return x2*x2;    }
inline double pow_5  ( const double x ) { double x2 = x*x;   return x2*x2*x;  }
inline double pow_6  ( const double x ) { double x2 = x*x;   return x2*x2*x2; }
inline double pow_7  ( const double x ) { double x3 = x*x*x; return x3*x3*x;  }
inline double pow_8  ( const double x ) { double x2 = x*x;   double x4 = x2*x2; return x4*x4; }
inline double pow_9  ( const double x ) { double x3 = x*x*x; return x3*x3*x3; }
inline double pow_10 ( const double x ) { double x2 = x*x;   double x4 = x2*x2; return x4*x4*x2; }

// Basis functions in interval (0,1).
/** Lagrange basis polynomial of order 0 which passes through the 0-th support point in interval (0,1). */
double basisFunction_0_0(const double s);

/** Lagrange basis polynomial of order 1 which passes through the 0-th support point in interval (0,1). */
double basisFunction_1_0(const double s);
/** Lagrange basis polynomial of order 1 which passes through the 1-th support point in interval (0,1). */
double basisFunction_1_1(const double s);

/** Lagrange basis polynomial of order 2 which passes through the 0-th support point in interval (0,1). */
double basisFunction_2_0(const double s);
/** Lagrange basis polynomial of order 2 which passes through the 1-th support point in interval (0,1). */
double basisFunction_2_1(const double s);
/** Lagrange basis polynomial of order 2 which passes through the 2-th support point in interval (0,1). */
double basisFunction_2_2(const double s);

/** Lagrange basis polynomial of order 3 which passes through the 0-th support point in interval (0,1). */
double basisFunction_3_0(const double s);
/** Lagrange basis polynomial of order 3 which passes through the 1-th support point in interval (0,1). */
double basisFunction_3_1(const double s);
/** Lagrange basis polynomial of order 3 which passes through the 2-th support point in interval (0,1). */
double basisFunction_3_2(const double s);
/** Lagrange basis polynomial of order 3 which passes through the 3-th support point in interval (0,1). */
double basisFunction_3_3(const double s);

/** Lagrange basis polynomial of order 4 which passes through the 0-th support point in interval (0,1). */
double basisFunction_4_0(const double s);
/** Lagrange basis polynomial of order 4 which passes through the 1-th support point in interval (0,1). */
double basisFunction_4_1(const double s);
/** Lagrange basis polynomial of order 4 which passes through the 2-th support point in interval (0,1). */
double basisFunction_4_2(const double s);
/** Lagrange basis polynomial of order 4 which passes through the 3-th support point in interval (0,1). */
double basisFunction_4_3(const double s);
/** Lagrange basis polynomial of order 4 which passes through the 4-th support point in interval (0,1). */
double basisFunction_4_4(const double s);

/** Lagrange basis polynomial of order 5 which passes through the 0-th support point in interval (0,1). */
double basisFunction_5_0(const double s);
/** Lagrange basis polynomial of order 5 which passes through the 1-th support point in interval (0,1). */
double basisFunction_5_1(const double s);
/** Lagrange basis polynomial of order 5 which passes through the 2-th support point in interval (0,1). */
double basisFunction_5_2(const double s);
/** Lagrange basis polynomial of order 5 which passes through the 3-th support point in interval (0,1). */
double basisFunction_5_3(const double s);
/** Lagrange basis polynomial of order 5 which passes through the 4-th support point in interval (0,1). */
double basisFunction_5_4(const double s);
/** Lagrange basis polynomial of order 5 which passes through the 5-th support point in interval (0,1). */
double basisFunction_5_5(const double s);

/** Lagrange basis polynomial of order 6 which passes through the 0-th support point in interval (0,1). */
double basisFunction_6_0(const double s);
/** Lagrange basis polynomial of order 6 which passes through the 1-th support point in interval (0,1). */
double basisFunction_6_1(const double s);
/** Lagrange basis polynomial of order 6 which passes through the 2-th support point in interval (0,1). */
double basisFunction_6_2(const double s);
/** Lagrange basis polynomial of order 6 which passes through the 3-th support point in interval (0,1). */
double basisFunction_6_3(const double s);
/** Lagrange basis polynomial of order 6 which passes through the 4-th support point in interval (0,1). */
double basisFunction_6_4(const double s);
/** Lagrange basis polynomial of order 6 which passes through the 5-th support point in interval (0,1). */
double basisFunction_6_5(const double s);
/** Lagrange basis polynomial of order 6 which passes through the 6-th support point in interval (0,1). */
double basisFunction_6_6(const double s);

/** Lagrange basis polynomial of order 7 which passes through the 0-th support point in interval (0,1). */
double basisFunction_7_0(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 1-th support point in interval (0,1). */
double basisFunction_7_1(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 2-th support point in interval (0,1). */
double basisFunction_7_2(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 3-th support point in interval (0,1). */
double basisFunction_7_3(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 4-th support point in interval (0,1). */
double basisFunction_7_4(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 5-th support point in interval (0,1). */
double basisFunction_7_5(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 6-th support point in interval (0,1). */
double basisFunction_7_6(const double s);
/** Lagrange basis polynomial of order 7 which passes through the 7-th support point in interval (0,1). */
double basisFunction_7_7(const double s);

/** Lagrange basis polynomial of order 8 which passes through the 0-th support point in interval (0,1). */
double basisFunction_8_0(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 1-th support point in interval (0,1). */
double basisFunction_8_1(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 2-th support point in interval (0,1). */
double basisFunction_8_2(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 3-th support point in interval (0,1). */
double basisFunction_8_3(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 4-th support point in interval (0,1). */
double basisFunction_8_4(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 5-th support point in interval (0,1). */
double basisFunction_8_5(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 6-th support point in interval (0,1). */
double basisFunction_8_6(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 7-th support point in interval (0,1). */
double basisFunction_8_7(const double s);
/** Lagrange basis polynomial of order 8 which passes through the 8-th support point in interval (0,1). */
double basisFunction_8_8(const double s);

/** Lagrange basis polynomial of order 9 which passes through the 0-th support point in interval (0,1). */
double basisFunction_9_0(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 1-th support point in interval (0,1). */
double basisFunction_9_1(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 2-th support point in interval (0,1). */
double basisFunction_9_2(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 3-th support point in interval (0,1). */
double basisFunction_9_3(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 4-th support point in interval (0,1). */
double basisFunction_9_4(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 5-th support point in interval (0,1). */
double basisFunction_9_5(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 6-th support point in interval (0,1). */
double basisFunction_9_6(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 7-th support point in interval (0,1). */
double basisFunction_9_7(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 8-th support point in interval (0,1). */
double basisFunction_9_8(const double s);
/** Lagrange basis polynomial of order 9 which passes through the 9-th support point in interval (0,1). */
double basisFunction_9_9(const double s);



// First derivatives of basis functions in interval (0,1).
/** First derivative of Lagrange basis polynomial of order 0 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_0_0(const double s);

/** First derivative of Lagrange basis polynomial of order 1 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_1_0(const double s);
/** First derivative of Lagrange basis polynomial of order 1 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_1_1(const double s);

/** First derivative of Lagrange basis polynomial of order 2 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_2_0(const double s);
/** First derivative of Lagrange basis polynomial of order 2 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_2_1(const double s);
/** First derivative of Lagrange basis polynomial of order 2 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_2_2(const double s);

/** First derivative of Lagrange basis polynomial of order 3 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_3_0(const double s);
/** First derivative of Lagrange basis polynomial of order 3 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_3_1(const double s);
/** First derivative of Lagrange basis polynomial of order 3 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_3_2(const double s);
/** First derivative of Lagrange basis polynomial of order 3 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_3_3(const double s);

/** First derivative of Lagrange basis polynomial of order 4 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_4_0(const double s);
/** First derivative of Lagrange basis polynomial of order 4 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_4_1(const double s);
/** First derivative of Lagrange basis polynomial of order 4 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_4_2(const double s);
/** First derivative of Lagrange basis polynomial of order 4 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_4_3(const double s);
/** First derivative of Lagrange basis polynomial of order 4 which passes through the 4-th support point in interval (0,1). */
double basisFunctionFirstDerivative_4_4(const double s);

/** First derivative of Lagrange basis polynomial of order 5 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_5_0(const double s);
/** First derivative of Lagrange basis polynomial of order 5 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_5_1(const double s);
/** First derivative of Lagrange basis polynomial of order 5 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_5_2(const double s);
/** First derivative of Lagrange basis polynomial of order 5 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_5_3(const double s);
/** First derivative of Lagrange basis polynomial of order 5 which passes through the 4-th support point in interval (0,1). */
double basisFunctionFirstDerivative_5_4(const double s);
/** First derivative of Lagrange basis polynomial of order 5 which passes through the 5-th support point in interval (0,1). */
double basisFunctionFirstDerivative_5_5(const double s);

/** First derivative of Lagrange basis polynomial of order 6 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_0(const double s);
/** First derivative of Lagrange basis polynomial of order 6 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_1(const double s);
/** First derivative of Lagrange basis polynomial of order 6 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_2(const double s);
/** First derivative of Lagrange basis polynomial of order 6 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_3(const double s);
/** First derivative of Lagrange basis polynomial of order 6 which passes through the 4-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_4(const double s);
/** First derivative of Lagrange basis polynomial of order 6 which passes through the 5-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_5(const double s);
/** First derivative of Lagrange basis polynomial of order 6 which passes through the 6-th support point in interval (0,1). */
double basisFunctionFirstDerivative_6_6(const double s);

/** First derivative of Lagrange basis polynomial of order 7 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_0(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_1(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_2(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_3(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 4-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_4(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 5-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_5(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 6-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_6(const double s);
/** First derivative of Lagrange basis polynomial of order 7 which passes through the 7-th support point in interval (0,1). */
double basisFunctionFirstDerivative_7_7(const double s);

/** First derivative of Lagrange basis polynomial of order 8 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_0(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_1(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_2(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_3(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 4-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_4(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 5-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_5(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 6-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_6(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 7-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_7(const double s);
/** First derivative of Lagrange basis polynomial of order 8 which passes through the 8-th support point in interval (0,1). */
double basisFunctionFirstDerivative_8_8(const double s);

/** First derivative of Lagrange basis polynomial of order 9 which passes through the 0-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_0(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 1-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_1(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 2-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_2(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 3-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_3(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 4-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_4(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 5-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_5(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 6-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_6(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 7-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_7(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 8-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_8(const double s);
/** First derivative of Lagrange basis polynomial of order 9 which passes through the 9-th support point in interval (0,1). */
double basisFunctionFirstDerivative_9_9(const double s);



// Second derivatives of basis functions in interval (0,1)
/** Second derivative of Lagrange basis polynomial of order 0 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_0_0(const double s);

/** Second derivative of Lagrange basis polynomial of order 1 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_1_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 1 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_1_1(const double s);

/** Second derivative of Lagrange basis polynomial of order 2 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_2_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 2 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_2_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 2 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_2_2(const double s);

/** Second derivative of Lagrange basis polynomial of order 3 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_3_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 3 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_3_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 3 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_3_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 3 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_3_3(const double s);

/** Second derivative of Lagrange basis polynomial of order 4 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_4_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 4 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_4_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 4 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_4_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 4 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_4_3(const double s);
/** Second derivative of Lagrange basis polynomial of order 4 which passes through the 4-th support point in interval (0,1). */
double basisFunctionSecondDerivative_4_4(const double s);

/** Second derivative of Lagrange basis polynomial of order 5 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_5_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 5 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_5_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 5 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_5_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 5 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_5_3(const double s);
/** Second derivative of Lagrange basis polynomial of order 5 which passes through the 4-th support point in interval (0,1). */
double basisFunctionSecondDerivative_5_4(const double s);
/** Second derivative of Lagrange basis polynomial of order 5 which passes through the 5-th support point in interval (0,1). */
double basisFunctionSecondDerivative_5_5(const double s);

/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_3(const double s);
/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 4-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_4(const double s);
/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 5-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_5(const double s);
/** Second derivative of Lagrange basis polynomial of order 6 which passes through the 6-th support point in interval (0,1). */
double basisFunctionSecondDerivative_6_6(const double s);

/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_3(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 4-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_4(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 5-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_5(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 6-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_6(const double s);
/** Second derivative of Lagrange basis polynomial of order 7 which passes through the 7-th support point in interval (0,1). */
double basisFunctionSecondDerivative_7_7(const double s);

/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_3(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 4-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_4(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 5-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_5(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 6-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_6(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 7-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_7(const double s);
/** Second derivative of Lagrange basis polynomial of order 8 which passes through the 8-th support point in interval (0,1). */
double basisFunctionSecondDerivative_8_8(const double s);

/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 0-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_0(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 1-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_1(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 2-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_2(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 3-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_3(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 4-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_4(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 5-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_5(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 6-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_6(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 7-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_7(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 8-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_8(const double s);
/** Second derivative of Lagrange basis polynomial of order 9 which passes through the 9-th support point in interval (0,1). */
double basisFunctionSecondDerivative_9_9(const double s);

#endif
