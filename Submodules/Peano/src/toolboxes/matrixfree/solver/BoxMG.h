// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_BOXMG_H_
#define _MATRIXFREE_SOLVER_BOXMG_H_

#include "tarch/logging/Log.h"
#include "matrixfree/solver/Multigrid.h"

namespace matrixfree {
  namespace solver{
    namespace boxmg {
      /**
       * This routine is given the @f$ 4^d \cdot 3^d @f$ stencils within a
       * patch, the prolongation (or restriction) stencil of one of the
       * coarse grid vertices and the position of this vertex as integer
       * vectors with 0s and 1s. It overwrites all affected entries within
       * the prolongationStencil with recomputed BoxMG values. The routine
       * itself is state-less, i.e. you can call it in parallel for all
       * @f$ 2^d @f$ coarse vertices within a patch.
       *
       * <h2> Implementation </h2>
       *
       * Our BoxMG implementation works with a reference configuration where
       * the coarse grid vertex for which the inter-grid transfer operator's
       * entries are computed is the one in the bottom left. The algorithm
       * thus consists of five steps:
       *
       * - Mirror all input data such that it refers to the bottom left
       *   operator.
       * - Assemble the equation system M(s)p=f that describes the p entries
       *   that have to be computed.
       * - Solve M(s)p=f. We employ QR decomposition, i.e. determine Q*R=M(s)
       *   with a Gram Schmidt decomposition from Peano's tarch component.
       * - Once we have Q and R, we determine p=R^-1 * Q^T * f.
       * - Finally, we rotate p back (p originally refers to the reference
       *   configuration) and write it into prolongationStencil.
       */
      void computeBoxMGIntergridTransferOperator(
        const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
        tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
        const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
      );


      void computeAggregationOperator(
        const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
        tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
        const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
      );


      void computeSparsifiedSymmetricBoxMGIntergridTransferOperator(
        const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
        tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
        const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
      );


      void computeInjectionOperator(
        const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
        tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
        const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
      );
    }
  }
}


#endif
