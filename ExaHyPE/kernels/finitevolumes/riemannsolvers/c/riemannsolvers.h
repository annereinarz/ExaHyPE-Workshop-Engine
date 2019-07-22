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
#ifndef KERNELS_FINITEVOLUMES_RIEMANNSOLVERS_C_RIEMANNSOLVERS_H_
#define KERNELS_FINITEVOLUMES_RIEMANNSOLVERS_C_RIEMANNSOLVERS_H_

#include "kernels/KernelUtils.h"
#include <iomanip>
#include <iostream>
#include <algorithm> // copy_n
#include "../../../RiemannSolverUtils.h"

namespace kernels {
namespace finitevolumes {
namespace riemannsolvers {
namespace c {

/**
 * A simple Rusanov flux considering pointwise
 * left and right values.
 *
 * @note This does not result in a well-balanced scheme.
 * It is not a good Riemann solver for the Shallow Water Equations (SWE) e.g.
 */
template <bool useNCP, bool useFlux, bool useViscousFlux, typename SolverType>
double rusanov(SolverType& solver, double* fL, double *fR, const double* qL, const double* qR,
             const double* gradQL, const double* gradQR, const double* cellSize, int direction);
/**
 * Implements a generalised osher type flux.
 *
 * @note Requires @p solver to implement a nonconservative product and an eigenvectors function which returns the
 * eigenvalues and eigenvectors. The kernel supplies the solver with reference coordinate indices.
 *
 * References:
 *
 * [1] M. Dumbser and E. F. Toro, “On Universal Osher-Type Schemes for General Nonlinear Hyperbolic Conservation Laws,” Communications in Computational Physics, vol. 10, no. 03, pp. 635–671, Sep. 2011.
 * [2] M. Dumbser and E. F. Toro, “A Simple Extension of the Osher Riemann Solver to Non-conservative Hyperbolic Systems,” Journal of Scientific Computing, vol. 48, no. 1–3, pp. 70–88, Jul. 2011.
 *
 * @note Currently, no viscous flux is supported.
 *
 * @tparam numQuadPoints the number of quadrature points the Legendre quadrature should use. 3 is chosen in paper [1].
 */
template <bool useNCP, bool useFlux, bool useViscousFlux, int numQuadPoints, typename SolverType>
double generalisedOsherSolomon(
    SolverType&         solver,
    double* const       fnL,
    double* const       fnR,
    const double* const qL,
    const double* const qR,
    const int           direction);
} // namespace c
} // namespace riemansolvers
} // namespace finitevolumes
} // namespace kernels

#include "rusanov.cpph"
#include "generalisedOsherSolomon.cpph"

#endif /* KERNELS_FINITEVOLUMES_RIEMANNSOLVERS_C_RIEMANNSOLVERS_H_ */
