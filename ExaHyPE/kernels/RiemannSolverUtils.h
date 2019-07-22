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

#ifndef _EXAHYPE_KERNELS_RIEMANN_SOLVERS_H_
#define _EXAHYPE_KERNELS_RIEMANN_SOLVERS_H_

#include "tarch/Assertions.h"
#include "tarch/la/Vector.h"
#include "peano/utils/Globals.h"

#include <sstream>

#include "kernels/GaussLegendreQuadrature.h"

namespace kernels {
namespace riemannsolvers {
namespace util {

/**
 * Computes ingredients for the generalised Osher Solomon flux [1,2].
 *
 * Integrates matrix |A|(Qs) := Rs(Qs) * |L(Qs)| * iRs(Qs) over phase space ( Qs in [QL, QR] ) using
 * a Legendre quadrature with @p numQuadPoints points. Stores the result in @p osherMatrix.
 * Computes the NCP (non-conservative product) directly instead of
 * returning a similarly averaged jacobian matrix for nonconservative PDEs.
 * Stores the result in @p osherNCP.
 *
 * @note Requires @p solver to implement a nonconservative product and an eigenvectors function which returns the
 * eigenvalues and eigenvectors. The kernel supplies the solver with reference coordinate indices.
 *
 * References:
 *
 * [1] M. Dumbser and E. F. Toro, “On Universal Osher-Type Schemes for General Nonlinear Hyperbolic Conservation Laws,” Communications in Computational Physics, vol. 10, no. 03, pp. 635–671, Sep. 2011.
 * [2] M. Dumbser and E. F. Toro, “A Simple Extension of the Osher Riemann Solver to Non-conservative Hyperbolic Systems,” Journal of Scientific Computing, vol. 48, no. 1–3, pp. 70–88, Jul. 2011.
 *
 * @param[in]    solver      a solver
 * @param[in]    qL          the "left"/"-"  state variables (plus parameters); range: [0,nVar+nPar].
 * @param[in]    qL          the "right"/"+" state variables (plus parameters); range: [0,nVar+nPar].
 * @param[in]    direction   coordinate direction for selecting the correct eigenvectors and jacobian
 * @param[inout] osherMatrix the Osher matrix for conservative flux
 * @param[inout] osherNCP    the Osher non-conservative product contributions.
 *
 * @return maximum absolute eigenvalue.
 */
template <bool useNCP,bool useFlux,int numberOfVariables, int numberOfParameters, int numQuadPoints, typename SolverType>
double computeOsherMatrix(
    SolverType&         solver,
    const double* const qL,
    const double* const qR,
    const int           direction,
    double              (&osherMatrix)[numberOfVariables][numberOfVariables] /*initialise with zeroes*/,
    double              (&osherNCP)[numberOfVariables]                    /*initialise with zeroes*/) {
  // coordinate axes permutations (if direction is template parameter, this can be constexpr)
  // original; keep a while for reference
  //  int in = 0; // direction == 0
  //  int is = 1;
  //  int it = 2;
  //  if ( direction == 1 ) {
  //     in=1;
  //     is=0;
  //     it=2;
  //  } else if ( direction == 2 ) {
  //     in=2;
  //     is=0;
  //     it=1;
  //  }
  constexpr int numberOfData = numberOfVariables + numberOfParameters;

  const int in = direction;
  const int is = (direction == 0) ?  1 : 0;
  const int it = (direction != 2) ?  2 : 1;

  // integrate matrix |A| := Rs * |L| * iRs matrix over phase space (QL -> QR)
  double s_max = -1.0;
  for(int i=0; i<numQuadPoints; i++) {
    const double& si  = kernels::gaussLegendreNodes[numQuadPoints-1][i];
    double Qs[numberOfData];
    for(int j=0; j < numberOfData; j++) {
      Qs[j] = qL[j] + si * (qR[j] - qL[j]);
    }

    if (useFlux) {
      // eigenstructure
      double Ls[numberOfVariables];                            // all eigenvalues need to be set by user
      double Rs[numberOfVariables][numberOfVariables] = {0.0}; // fill with zeros as user typically performs rotation via matrix product
      double iRs[numberOfVariables][numberOfVariables]= {0.0};
      solver.eigenvectors(Qs/*in*/,in,is,it,Rs/*inout*/,Ls/*inout*/,iRs/*inout*/);    // user solver can work internally with eigen system or eigen solvers;
      for (int j = 0; j < numberOfVariables; j++) {
        s_max = std::max( std::abs(Ls[j]), s_max );
      }

      #ifdef Asserts
      double sumOfEntries = 0;
      for(int j=0; j < numberOfVariables; j++) {
        for(int k=0; k < numberOfVariables; k++) {
          for(int a=0; a < numberOfVariables; a++) {
            sumOfEntries += iRs[j][a] * Rs[a][k];
          }
        }
      }
      if ( sumOfEntries < numberOfVariables - 1e-12 || sumOfEntries > numberOfVariables + 1e-12 ) {
        std::cerr << "Error: Left eigenvector matrix is not an inverse of the right one as sum of entries="<<sumOfEntries << "; direction="<<direction << std::endl;
        std::terminate();
      }
      #endif

      // compute Osher matrix
      // scale column vectors: (|L1|*col1,|L2|*col2,...)
      const double& wi = kernels::gaussLegendreWeights[numQuadPoints-1][i];
      for(int j=0; j < numberOfVariables; j++) {
        for(int k=0; k < numberOfVariables; k++) {
          for(int a=0; a < numberOfVariables; a++) {
            osherMatrix[j][k] += wi * Rs[j][a]*std::abs(Ls[a])*iRs[a][k];
          }
        }
      }
    }

    if (useNCP) { // we directly compute the nonconservative product instead of computing the jacobian matrix
      const double& wi = kernels::gaussLegendreWeights[numQuadPoints-1][i];
      double gradQs[DIMENSIONS][numberOfData] = {0.0};
      for(int j=0; j < numberOfData; j++) {
        gradQs[DIMENSIONS][j] = qR[j] - qL[j];
      }
      double ncp[numberOfData] = {0.0};
      solver.nonConservativeProduct(Qs, gradQs[in], ncp);
      for(int j=0; j < numberOfData; j++) {
        osherNCP[j] += wi * ncp[j];
      }
    }
  }
  return s_max;
}

/**
 * Average Riemann input data.
 *
 * @param[in]    Q         vector of state variables (plus parameters)
 * @param[in]    weights1D a set of 1D weights for all orders. Must have an entry for order @tp basisSize-1.
 * @param[inout] Qav       averaged state variables (plus parameters)
 */
template <int basisSize, int numberOfData>
void averageRiemannInputs(
    const double* const         Q,
    const double* const * const weights1D,
    double                      (&Qav)[numberOfData]) {
  constexpr int order = basisSize-1;
  #if DIMENSIONS==2
  idx2 idx_Q(basisSize, numberOfData);
  for (int j = 0; j < basisSize; j++) {
    const double wj = weights1D[order][j];
    for (int k = 0; k < numberOfData; k++) {
      Qav[k] += wj * Q[idx_Q(j, k)];
    }
  }
  #else
  idx3 idx_Q(basisSize, basisSize, numberOfData);
  for (int i = 0; i < basisSize; i++) {
    for (int j = 0; j < basisSize; j++) {
      const double wij = weights1D[order][i]*weights1D[order][j];
      for (int k = 0; k < numberOfData; k++) {
        Qav[k] += wij * Q[idx_Q(i,j, k)];
      }
    }
  }
  #endif
}


} // namespace util
} // namespace riemannsolvers
} // namespace kernels

#endif  // _EXAHYPE_KERNELS_RIEMANN_SOLVERS_H_
