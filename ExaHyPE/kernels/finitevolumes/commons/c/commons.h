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
 *
 * @authors: Dominic E. Charrier
 **/

#ifndef GHOSTLAYERS_H_
#define GHOSTLAYERS_H_

namespace kernels {
namespace finitevolumes {
namespace commons {
namespace c {

/**
 * todo docu
 */
template <typename SolverType>
void solutionAdjustment(
    SolverType& solver,
    double* luh, const tarch::la::Vector<DIMENSIONS, double>& center,
    const tarch::la::Vector<DIMENSIONS, double>& dx, const double t,const double dt);

/**
 * todo docu
 */
template <typename SolverType, bool useViscousFlux>
double stableTimeStepSize(
    SolverType& solver,
    const double* const luh,
    const tarch::la::Vector<DIMENSIONS, double>& dx);

/**
 * Extract the boundary layer of the patch \p luh and
 * store it in the array \p luhbnd.
 *
 * \param[in] boundaryPosition A d-dimensional vector
 *            which must be one of the triples (-1,0,0), (+1,0,0),
 *            (0,-1,0), (0,+1,0), (0,0,-1), or (0,0,+1) in 3D,
 *            or one of the pairs (-1,0), (+1,0), (0,-1), or (0,+1) int .
 *            Here, (-1,0), e.g., means that the boundary is
 *            left to the current cell.
 */
template <typename SolverType>
void boundaryLayerExtraction(
    SolverType& solver,
    double* luhbnd,const double* luh,
    const tarch::la::Vector<DIMENSIONS,int>& boundaryPosition);

/**
 * Impose boundary conditions.
 *
 * \note We mirror the inside values at
 * boundary face.
 */
template <typename SolverType>
void boundaryConditions(
    SolverType& solver,
    double* stateOut,
    const double* const stateIn,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS,double>& cellSize,
    const double t,const double dt,
    const int faceIndex,
    const int normalNonZero);

/**
 * Fill a part of the ghost layer of a patch with
 * values from an array. This function is used
 * for the MPI and boundary treatment.
 *
 * * \param[in] boundaryPosition A d-dimensional vector
 *            which must be one of the triples (-1,0,0), (+1,0,0),
 *            (0,-1,0), (0,+1,0), (0,0,-1), or (0,0,+1) in 3D,
 *            or one of the pairs (-1,0), (+1,0), (0,-1), or (0,+1) int .
 *            Here, (-1,0), e.g., means that the boundary is
 *            left to the current cell.
 *
 * \see boundaryLayerExtraction
 */
template <typename SolverType>
void ghostLayerFillingAtBoundary(
    SolverType& solver,
    double* luh,const double* luhbnd,
    const tarch::la::Vector<DIMENSIONS,int>& boundaryPosition);

/**
 * Fill a part of the ghost layer of a patch with
 * values from a neighbour.
 *
 * \param[in] neighbourPosition A d-dimensional vector
 *            which must be one of the triples (-1,0,0), (+1,0,0),
 *            (0,-1,0), (0,+1,0), (0,0,-1), or (0,0,+1) in 3D,
 *            or one of the pairs (-1,0), (+1,0), (0,-1), or (0,+1) int .
 *            Here, (-1,0), e.g., means that the neighbour is
 *            left to the current cell.
 *
 * \see boundaryLayerExtraction
 */
template <typename SolverType>
void ghostLayerFilling(
    SolverType& solver,
    double* luh,const double* luhNeighbour,
    const tarch::la::Vector<DIMENSIONS,int>& neighbourPosition);

}  // namespace c
}  // namespace commons
}  // namespace finitevolumesme
}  // namespace kernels

#include "commons.cpph"

#endif /* GHOSTLAYERS_H_ */
