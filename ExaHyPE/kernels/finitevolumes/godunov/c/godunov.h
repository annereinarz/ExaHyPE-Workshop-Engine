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
#ifndef KERNELS_FINITEVOLUMES_C_GODUNOV_H_
#define KERNELS_FINITEVOLUMES_C_GODUNOV_H_

#include "tarch/la/Vector.h"

#include "kernels/finitevolumes/commons/c/slope-limiters.h"

namespace kernels {
namespace finitevolumes {
namespace godunov {
namespace c {


  /**
   * The classic unsplit first-order (in terms of local truncation error) Godunov Finite Volumes scheme.
   *
   * @note robustDiagonalLimiting has no effect on the algorithm.
   *
   * @param solver     a user solver implementing the PDE kernels.
   * @param solution   the current (and then new) solution.
   * @param cellCentre the centre of the cell holding the FV subgrid.
   * @param cellSize   the dimensions of the cell holding the FV subgrid.
   * @param t          the time stamp.
   * @param dt         the used time step size.
   * @return the actual admissible time step size obtained from the Riemann solves.
   */
  template <
    bool useSource, bool useNCP, bool useFlux, bool useViscousFlux,
    bool robustDiagonalLimiting, // not used in 1st order Godunov
    kernels::finitevolumes::commons::c::slope_limiter slope_limiter, // not used in 1st order Godunov
    typename SolverType
  >
  double solutionUpdate(
      SolverType&                                  solver,
      double* const                                solution,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize,
      const double                                 t,
      const double                                 dt);
}  // namespace c
}  // namespace godunov
}  // namespace finitevolumes
}  // namespace kernels

#include "kernels/finitevolumes/godunov/c/2d/godunov.cpph"
#include "kernels/finitevolumes/godunov/c/3d/godunov.cpph"

#endif // KERNELS_FINITEVOLUMES_C_GODUNOV_H_
