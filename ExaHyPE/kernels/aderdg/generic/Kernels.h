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

#ifndef _EXAHYPE_KERNELS_ADERDG_GENERIC_PDEFLUXES_H_
#define _EXAHYPE_KERNELS_ADERDG_GENERIC_PDEFLUXES_H_

#include <string>
#include <vector>

#include "peano/utils/Globals.h"
#include "tarch/la/Vector.h"

#include "kernels/GaussLobattoQuadrature.h"
#include "kernels/GaussLegendreQuadrature.h"

#include "kernels/DGMatrices.h"

#include "tarch/la/Scalar.h"
#include "tarch/la/ScalarOperations.h"

#include "tarch/logging/Log.h"

#define MbasisSize 4
#define Mvar 5
#define Mdim 3
#define f2p5(var, dim, i, j, k)                                        \
  (var + Mvar * dim + Mvar * Mdim * i + Mvar * Mdim * MbasisSize * j + \
   Mvar * Mdim * MbasisSize * MbasisSize * k)
#define p2f5(var, dim, i, j, k)                                   \
  (dim * MbasisSize * MbasisSize * MbasisSize * Mvar + Mvar * i + \
   Mvar * MbasisSize * j + Mvar * MbasisSize * MbasisSize * k + var)

#define Mface 6
#define f2p4(var, face, a, b) \
  (var + Mvar * face + Mvar * Mface * a + Mvar * Mface * MbasisSize * b)
#define p2f4(var, face, a, b)                                                 \
  (face * MbasisSize * MbasisSize * Mvar + Mvar * a + Mvar * MbasisSize * b + \
   var)

// todo Dominic Etienne Charrier
// Possibly redundant definition of face indices
// see exahype/solvers/Solver.h
// On the other hand, the kernels should be
// more or less independent of ExaHyPE/exahype.
#define EXAHYPE_FACE_LEFT 0
#define EXAHYPE_FACE_RIGHT 1
#define EXAHYPE_FACE_FRONT 2
#define EXAHYPE_FACE_BACK 3
#define EXAHYPE_FACE_BOTTOM 4
#define EXAHYPE_FACE_TOP 5

namespace kernels {
namespace aderdg {
namespace generic {
namespace c {

/**
 * @param SolverType Has to be of type ADERDG Solver.
 */
template <bool usePointSource, bool useSource, bool useFlux, bool useNCP, bool useMM ,typename SolverType>
void spaceTimePredictorLinear(SolverType& solver,
    double* lQbnd, double* lFbnd,
    double* lQi, double* lFi, double* gradQ,
    double* PSi, double* PSderivatives, double* tmp_PSderivatives,
    double* lQhi,double* lFhi,
    const double* const luh,
    const tarch::la::Vector<DIMENSIONS, double>& invDx,
    const double dt);

template <bool useSource, bool useFlux, bool useNCP, bool noTimeAveraging, typename SolverType>
int spaceTimePredictorNonlinear(
    SolverType& solver,
    double*  lQhbnd, double* lFhbnd,
    double* lQi,double* rhs,double* lFi,double* gradQ,double* lQhi,double* lFhi,
    const double* const luh,
    const tarch::la::Vector<DIMENSIONS, double>& invDx,
    const double dt);

template <typename SolverType>
void solutionUpdate(SolverType& solver, double* luh, const double* const luhOld, const double* const lduh, const double dt);

template <bool useSourceOrNCP, bool useFlux, int numberOfVariables, int basisSize>
void volumeIntegralLinear(double* lduh, const double* const lFhi,
                          const tarch::la::Vector<DIMENSIONS, double>& dx);

template <bool useSourceOrNCP, bool useFlux, bool noTimeAveraging, int numberOfVariables, int basisSize>
void volumeIntegralNonlinear(double* lduh, const double* const lFi,
                             const tarch::la::Vector<DIMENSIONS, double>& dx);

template <int numberOfVariables, int basisSize>
void faceIntegralNonlinear(
    double *lduh, const double *const lFhbnd,
    const int direction, const int orientation,
    const tarch::la::Vector<DIMENSIONS, double> &dx);

template <int numberOfVariables, int basisSize>
void faceIntegralLinear(
    double *lduh, const double *const lFhbnd,
    const int direction, const int orientation,
    const tarch::la::Vector<DIMENSIONS, double> &dx);


// todo 10/02/16: Dominic
// Keep only one surfaceIntegral.
template <int numberOfVariables, int basisSize>
void surfaceIntegralNonlinear(double* lduh, const double* const lFbnd,
                              const tarch::la::Vector<DIMENSIONS, double>& dx);

template <int numberOfVariables, int basisSize>
void surfaceIntegralLinear(double* lduh, const double* const lFbnd,
                           const tarch::la::Vector<DIMENSIONS, double>& dx);

/*void surfaceIntegral2(
    double* lduh,
    const double* const lFhbnd,
    const tarch::la::Vector<DIMENSIONS,double>&  dx,
    const int numberOfVariables,
    const int basisSize
);*/


template <typename SolverType>
void solutionAdjustment(SolverType& solver, double* luh,
                        const tarch::la::Vector<DIMENSIONS, double>& center,
                        const tarch::la::Vector<DIMENSIONS, double>& dx,
                        const double t, const double dt);

// @todo Dominic Etienne Charrier
// Inconsistent ordering of inout and in arguments
// template argument functions and non-template argument function.
/**
 * Implements a Rusanov Riemann solver.
 *
 * @param solver
 * @param FL
 * @param FR
 * @param QL
 * @param QR
 * @param t
 * @param dt
 * @param direction
 */
template <bool useNCP, bool useViscousFlux, typename SolverType>
void riemannSolverNonlinear(
    SolverType& solver, double* FL, double* FR,
    const double* const QL,
    const double* const QR,
    const double t,
    const double dt,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    const int direction);


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
 *
 * @param solver    solver implementing an eigenvectors function (plus a nonconservative product) if required.
 * @param FL        "left"/"-" normal flux of size [0,nVar]^2.
 * @param FR        "right"/"+"normal flux of size [0,nVar]^2.
 * @param QL        "left"/"-" state variables (plus parameters); range: [0,nVar+nPar]^2.
 * @param QR        "right"/"+"state variables (plus parameters); range: [0,nVar+nPar]^2.
 * @param t         time stamp
 * @param dt        time step size
 * @param direction normal direction
 */
template <bool useFlux, bool useNCP, typename SolverType>
void generalisedOsherSolomon(
    SolverType&         solver,
    double* const       FL,
    double* const       FR,
    const double* const QL,
    const double* const QR,
    const double        t,
    const double        dt,
    const int           direction);

template <bool useGradientFlux, typename SolverType>
void boundaryConditions(
    SolverType& solver,
    double* fluxOut,
    double* stateOut,
    const double* const fluxIn,
    const double* const stateIn,
    const double* const gradStateIn,
    const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
    const tarch::la::Vector<DIMENSIONS,double>& cellSize,
    const double t,const double dt,
    const int faceIndex,
    const int direction);

template <typename SolverType,bool useViscousFlux>
double stableTimeStepSize(SolverType& solver, const double* const luh,
                          const tarch::la::Vector<DIMENSIONS, double>& dx);

/**
 * \note We need to consider material parameters in
 * lQhbndFine and lQhbndCoarse.
 */
template <int numberOfVariables,int numberOfParameters,int basisSize>
void faceUnknownsProlongation(
    double* lQhbndFine, double* lFhbndFine, const double* lQhbndCoarse,
    const double* lFhbndCoarse, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS - 1, int>& subfaceIndex);


template <int numberOfVariables,int basisSize>
void faceUnknownsRestriction(
    double* const       lFhbndCoarse,
    const double* const lFhbndFine,
    const tarch::la::Vector<DIMENSIONS-1, int>& subfaceIndex,
    const int levelDelta);

/**
 * \note We need to consider material parameters in
 * lQhbndFine and lQhbndCoarse.
 *
 * @\deprecated
 */
template <int numberOfVariables,int numberOfParameters,int basisSize>
void faceUnknownsRestriction(
    double* lQhbndCoarse, double* lFhbndCoarse, const double* lQhbndFine,
    const double* lFhbndFine, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS - 1, int>& subfaceIndex);

/**
 * \note We need to consider material parameters in
 * luhCoarse and luhFine.
 */
template <int numberOfVariables,int numberOfParameters,int basisSize>
void volumeUnknownsProlongation(
    double* luhFine, const double* luhCoarse, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex);

/**
 * \note We need to consider material parameters in
 * luhCoarse and luhFine.
 */
template <int numberOfVariables,int numberOfParameters,int basisSize>
void volumeUnknownsRestriction(
    double* luhCoarse, const double* luhFine, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex);


template <typename SolverType>
std::vector<int>* getPointSources(
    SolverType& solver,
    const tarch::la::Vector<DIMENSIONS, double>& center,
    const tarch::la::Vector<DIMENSIONS, double>& dx);
    
template <typename SolverType>
void deltaDistribution(
    SolverType& solver,
    const double* const luh,
    const double t,
    const double dt,
    const tarch::la::Vector<DIMENSIONS, double>& center,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    std::vector<int>* pointSources, // will be deleted in the end
    double* PSi);
 
}  // namespace c
}  // namespace generic
}  // namespace aderdg
}  // namespace kernels

#include "kernels/aderdg/generic/c/generalisedOsherSolomon.cpph"

#if DIMENSIONS == 2
#include "kernels/aderdg/generic/c/2d/boundaryConditions.cpph"
#include "kernels/aderdg/generic/c/2d/riemannSolverLinear.cpph"
#include "kernels/aderdg/generic/c/2d/riemannSolverNonlinear.cpph"
#include "kernels/aderdg/generic/c/2d/solutionAdjustment.cpph"
#include "kernels/aderdg/generic/c/2d/solutionUpdate.cpph"
#include "kernels/aderdg/generic/c/2d/spaceTimePredictorLinear.cpph"
#include "kernels/aderdg/generic/c/2d/spaceTimePredictorNonlinear.cpph"
#include "kernels/aderdg/generic/c/2d/stableTimeStepSize.cpph"
#include "kernels/aderdg/generic/c/2d/deltaDistribution.cpph"
#include "kernels/aderdg/generic/c/2d/faceIntegralLinear.cpph"
#include "kernels/aderdg/generic/c/2d/faceIntegralNonlinear.cpph"
#include "kernels/aderdg/generic/c/2d/surfaceIntegralLinear.cpph"
#include "kernels/aderdg/generic/c/2d/surfaceIntegralNonlinear.cpph"
#include "kernels/aderdg/generic/c/2d/volumeIntegralLinear.cpph"
#include "kernels/aderdg/generic/c/2d/volumeIntegralNonlinear.cpph"
#include "kernels/aderdg/generic/c/2d/amrRoutines.cpph"
#elif DIMENSIONS == 3
#include "kernels/aderdg/generic/c/3d/boundaryConditions.cpph"
#include "kernels/aderdg/generic/c/3d/riemannSolverLinear.cpph"
#include "kernels/aderdg/generic/c/3d/riemannSolverNonlinear.cpph"
#include "kernels/aderdg/generic/c/3d/solutionAdjustment.cpph"
#include "kernels/aderdg/generic/c/3d/solutionUpdate.cpph"
#include "kernels/aderdg/generic/c/3d/spaceTimePredictorLinear.cpph"
#include "kernels/aderdg/generic/c/3d/spaceTimePredictorNonlinear.cpph"
#include "kernels/aderdg/generic/c/3d/stableTimeStepSize.cpph"
#include "kernels/aderdg/generic/c/3d/deltaDistribution.cpph"
#include "kernels/aderdg/generic/c/3d/faceIntegralLinear.cpph"
#include "kernels/aderdg/generic/c/3d/faceIntegralNonlinear.cpph"
#include "kernels/aderdg/generic/c/3d/surfaceIntegralLinear.cpph"
#include "kernels/aderdg/generic/c/3d/surfaceIntegralNonlinear.cpph"
#include "kernels/aderdg/generic/c/3d/volumeIntegralLinear.cpph"
#include "kernels/aderdg/generic/c/3d/volumeIntegralNonlinear.cpph"
#include "kernels/aderdg/generic/c/3d/amrRoutines.cpph"
#endif

// Todo: Recasting the code from function templates to class templates
//       did not yet consider the Fortran kernels and probably never will,
//       as they are different anyway.

namespace kernels {
namespace aderdg {
namespace generic {
namespace fortran {


template <typename SolverType>
void spaceTimePredictorNonlinear(
    SolverType& solver,
    double*  lQhbnd, double* lFhbnd,
    double** tempSpaceTimeUnknowns,
    double** tempSpaceTimeFluxUnknowns,
    double*  tempUnknowns,
    double*  tempFluxUnknowns,
    const double* const luh,
    const tarch::la::Vector<DIMENSIONS, double>& dx,
    const double dt,
    double* tempPointForceSources);


template <typename SolverType>
void spaceTimePredictorLinear(
    SolverType& solver,
    double*  lQhbnd, double* lFhbnd,
    double** tempSpaceTimeUnknowns,
    double** tempSpaceTimeFluxUnknowns,
    double*  tempUnknowns,
    double*  tempFluxUnknowns,
    const double* const luh,
    const tarch::la::Vector<DIMENSIONS, double>& invDx,
    const double dt,
    double* tempPointForceSources);

/**
 * (At the moment, we always evaluate the time averaged space-time
 * predictor unknowns.)
 * todo docu
 */
void predictor(double* lQhi, double* lFhi, const double* const lQi,
               const double* const lFi, const double predictorTimeStepSize,
               const int numberOfVariables, const int basisSize);

void extrapolatedPredictor(double* lQhbnd, double* lFhbnd,
                           const double* const lQhi, const double* const lFhi,
                           const double predictorTimeStepSize,
                           const int numberOfVariables, const int basisSize);

// todo Dominic Etienne Charrier:
// The DIMENSIONS depending mesh size vector enables overloading at the moment.
// If we replace it by scalar mesh size, we have to add a template argument "int
// dim".

void volumeIntegralNonlinear(double* lduh, const double* const lFhi,
                             const tarch::la::Vector<DIMENSIONS, double>& dx,
                             const int numberOfVariables, const int numberOfParameters, 
                             const int basisSize);

void volumeIntegralLinear(double* lduh, const double* const lFhi,
                             const tarch::la::Vector<DIMENSIONS, double>& dx,
                             const int numberOfVariables, const int numberOfParameters, 
                             const int basisSize);

// todo 10/02/16: Dominic
// Keep only one surfaceIntegral.
template <int numberOfVariables, int basisSize>
void surfaceIntegralNonlinear(double* lduh, const double* const lFbnd,
                              const tarch::la::Vector<DIMENSIONS, double>& dx);

template <int numberOfVariables, int basisSize>
void surfaceIntegralLinear(double* lduh, const double* const lFbnd,
                            const tarch::la::Vector<DIMENSIONS, double>& dx); 

/*void surfaceIntegral2(
    double* lduh,
    const double* const lFhbnd,
    const tarch::la::Vector<DIMENSIONS,double>&  dx,
    const int numberOfVariables,
    const int basisSize
);*/


template <typename SolverType>
void solutionUpdate(SolverType& solver, double* luh, const double* const lduh, const double dt);

template <typename SolverType>
void solutionAdjustment(SolverType& solver, double* luh,
                        const tarch::la::Vector<DIMENSIONS, double>& center,
                        const tarch::la::Vector<DIMENSIONS, double>& dx,
                        const double t, const double dt);


template <typename SolverType>
void riemannSolverNonlinear(
    SolverType& solver,
    double* FL, double* FR, const double* const QL,
    const double* const QR,
    const double t,
    const double dt,
    const int direction);

template <typename SolverType>
void riemannSolverLinear(
    SolverType& solver,
    double* FL, double* FR,
    const double* const QL, const double* const QR,
    const double t,
    const double dt,
    const int direction);


template <typename SolverType>
double stableTimeStepSize(SolverType& solver, const double* const luh,
                          const tarch::la::Vector<DIMENSIONS, double>& dx);

void faceUnknownsProlongation(
    double* lQhbndFine, double* lFhbndFine, const double* lQhbndCoarse,
    const double* lFhbndCoarse, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS - 1, int>& subfaceIndex,
    const int numberOfVariables, const int basisSize);

void faceUnknownsRestriction(
    double* lQhbndCoarse, double* lFhbndCoarse, const double* lQhbndFine,
    const double* lFhbndFine, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS - 1, int>& subfaceIndex,
    const int numberOfVariables, const int basisSize);

void volumeUnknownsProlongation(
    double* luhFine, const double* luhCoarse, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
    const int numberOfVariables, const int basisSize);

void volumeUnknownsRestriction(
    double* luhCoarse, const double* luhFine, const int coarseGridLevel,
    const int fineGridLevel,
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
    const int numberOfVariables, const int basisSize);

}  // namespace fortran
}  // namespace generic
}  // namespace aderdg
}  // namespace kernels
#if DIMENSIONS == 3
#include "kernels/aderdg/generic/fortran/3d/riemannSolverLinear.cpph"
#include "kernels/aderdg/generic/fortran/3d/riemannSolverNonlinear.cpph"
#include "kernels/aderdg/generic/fortran/3d/solutionUpdate.cpph"
#include "kernels/aderdg/generic/fortran/3d/solutionAdjustment.cpph"
#include "kernels/aderdg/generic/fortran/3d/spaceTimePredictorLinear.cpph"
#include "kernels/aderdg/generic/fortran/3d/spaceTimePredictorNonlinear.cpph"
#include "kernels/aderdg/generic/fortran/3d/stableTimeStepSize.cpph"
// #elif DIMENSIONS == 2
// //@todo
// #include "kernels/aderdg/generic/fortran/2d/solutionAdjustment.cpph"
// #include "kernels/aderdg/generic/fortran/2d/stableTimeStepSize.cpph"
// #include "kernels/aderdg/generic/fortran/2d/spaceTimePredictorNonlinear.cpph"
// #include "kernels/aderdg/generic/fortran/2d/spaceTimePredictorLinear.cpph"
// #include "kernels/aderdg/generic/fortran/2d/riemannSolverNonlinear.cpph"
// #include "kernels/aderdg/generic/fortran/2d/riemannSolverLinear.cpph"
#endif

#endif /* _EXAHYPE_KERNELS_ADERDG_GENERIC_PDEFLUXES_H_ */
