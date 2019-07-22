// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_MULTIGRID_H_
#define _MATRIXFREE_SOLVER_MULTIGRID_H_

#include "tarch/logging/Log.h"
#include "tarch/la/Vector.h"
#include "tarch/la/Matrix.h"
#include "tarch/la/GramSchmidt.h"
#include "peano/utils/Globals.h"
#include "peano/utils/Loop.h"
#include "peano/grid/VertexEnumerator.h"
#include "matrixfree/stencil/ElementMatrix.h"

namespace matrixfree {
  namespace solver{
    class Multigrid;
  }
}

/**
 * Basis utility class for multigrid solvers
 *
 * The class provides many generic operations that allow you to realise
 * Galerkin multigrid and Petrov-Galerkin multigrid. Most operations hereby
 * require you to hand in the inter-grid operators manually. However, the
 * class provides dummies where P and R are assumed to be d-linear.
 *
 * @author Marion Weinzierl, Tobias Weinzierl
 */
class matrixfree::solver::Multigrid {
  private:
    static tarch::logging::Log _log;

  public:
    template <int StencilSize1d, int StencilSize>
    static tarch::la::Vector<THREE_POWER_D,double> getGalerkinMultigridOperatorForDLinearInterpolationAndRestriction( int fineGridSubdivisionFactor, const tarch::la::Vector<StencilSize,double>& fineGridStencil );

    template <int StencilSize1d, int StencilSize>
    static tarch::la::Vector<THREE_POWER_D,double> getGalerkinMultigridOperatorForDLinearInterpolationAndInjection( int fineGridSubdivisionFactor, const tarch::la::Vector<StencilSize,double>& fineGridStencil );

    static tarch::la::Vector<THREE_POWER_D,double> getGalerkinMultigridOperatorForDLinearInterpolationAndRestriction( int fineGridSubdivisionFactor, const tarch::la::Vector<THREE_POWER_D,double>& fineGridStencil );
    static tarch::la::Vector<THREE_POWER_D,double> getGalerkinMultigridOperatorForDLinearInterpolationAndInjection( int fineGridSubdivisionFactor, const tarch::la::Vector<THREE_POWER_D,double>& fineGridStencil );

    /**
     * Compute position relative to one coarse grid vertex.
     *
     * @image html StencilRelativeToCoarseGrid.png
     *
     * @param coarseGridVertexPosition Index of current coarse grid vertex. It is a
     *        d-dimensional integer vector that holds only 0 and 1. (0,0) in
     *        d=2, e.g., is the bottom left vertex, (1,1) is the top right one.
     * @param fineGridVertexPosition Fine grid position of the fine grid
     *        vertex within a @f$ 4^d @f$ patch. It is an integer vector of
     *        length d that holds entries of @f$ \{ 0,1,2,3 \} @f$.
     * @param entryOfCoarseGridStencil Is an out parameter and afterwards
     *        contains a d-dimensional integer entry out of @f$ \{ 0,1,2,3,4 \} @f$
     *        that tells you for a 5-point stencil which entry affects the fine
     *        grid vertex.
     * @param coarseGridStencilInfluencesFineGridVertex Tells you whether the
     *        5-point stencil on the coarse grid influences the fine grid vertex
     *        at all. If the fine grid vertex, e.g., is (3,0) and the coarse grid
     *        vertex is (0,1), then this flag is false.
     */
    static void getPositionIn5PowDStencilRelativeToKthCoarseVertex(
      const tarch::la::Vector<DIMENSIONS,int>&       coarseGridVertexPosition,
      const tarch::la::Vector<DIMENSIONS,int>&       fineGridVertexPosition,
      tarch::la::Vector<DIMENSIONS,int>&             entryOfCoarseGridStencil,
      int&                                           indexOfCoarseGridStencil,
      bool&                                          coarseGridStencilInfluencesFineGridVertex
    );

    /**
     * Simple d-linear Interpolation
     *
     * I moved this to the multigrid solver, as it is a standard routine
     * required all the time - for example by most visualisation environments.
     * It requires the @f$ 2^d @f$ coarse grid values and the vertex's position
     * within a @f$ 3^d @f$ patch. Also, the method expects a scaling factor to
     * avoid that someone forgets to scale appropriately.
     *
     * If you interpolate scalar data such as the solution, the scaling
     * typically is 1.0. If you interpolate FEM data such as the residual, the
     * scaling has to reflect the integral of the supports, i.e. if you have
     * the Laplacian, the scaling is 1.0/static_cast<double>(THREE_POWER_D).
     *
     * !!! Update stencil evaluation counter
     *
     * Done by computeContributionWeightOfInterGridTransfer().
     *
     * @param coarseGridValues         The @f$ 2^d @f$ coarse grid values to be interpolated.
     * @param fineGridPositionOfVertex Position of the vertex within the @f$ 3^d @f$ patch, i.e. an integer vector whose entries are somewhere between 0 and 4.
     */
    static double getDLinearInterpolatedValue(
      const tarch::la::Vector<TWO_POWER_D,double>&           coarseGridValues,
      const tarch::la::Vector<DIMENSIONS,int>&               fineGridPositionOfVertex
    );

    /**
     * Complex-valued variant of getDLinearInterpolationValue().
     */
    static std::complex<double> getDLinearInterpolatedValue(
      const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&  coarseGridValues,
      const tarch::la::Vector<DIMENSIONS,int>&                     fineGridPositionOfVertex
    );


    static tarch::la::Vector<TWO_POWER_D,double > getDLinearInterpolationWeights(
      const tarch::la::Vector<DIMENSIONS,int>&                     fineGridPositionOfVertex
    );


    /**
     * !!! Compute Transfer Contribution Weight for one Individual Fine Grid Vertex
     *
     * If you wanna compute @f$ Pu_\mbox{coarse} @f$ or @f$ Ru_\mbox{fine} @f$ (for
     * example in a a touchVertex FirstTime() event) for one individual fine
     * grid vertex, you can do this in @f$ 2^d @f$ steps: For each coarse grid
     * vertex
     * - you either compute its contribution/weight to the fine grid value and
     *   scale it with the corresponding coarse grid vertex's value
     *   (prolongation) or
     * - you compute its contribution/weight for the fine grid vertex and
     *   scale it with the fine grid vertex's value (restriction)
     * then you add this value to the fine grid vertex or the corresponding
     * coarse grid value, respectively.
     *
     * To split it up into @f$ 2^d @f$ steps does make sense,
     * as the P stencil of each vertex might be different. This operation
     * implements how to get the weight between fine grid vertex and one
     * coarse grid vertex.
     *
     * @image html Multigrid.png
     *
     * !! Usage (compute Pu, i.e. prolongation, for one individual fine grid vertex)
     *
     * - Create temporary value. It is typically a double, and it represents
     *   the new fine grid value.
     * - Run over all @f$ 2^d @f$ (typically a dfor2 loop):
     *   - We will now compute the contribution of one coarse grid vertex to
     *     our fine grid vertex.
     *   - Take the coarse grid inter-grid transfer stencil. It is a
     *     @f$ 5^d @f$ vector.
     *   - Pass the stencil, the @f$ (0,1)^d @f$ position of the current
     *     coarse grid vertex relative to the current fine grid vertex, and the
     *     fine grid vertex position within the @f$ 4^d @f$ patch to this
     *     operation. You get a weight.
     *   - Multiply this weight with the coarse grid vertex's value that is to
     *     be interpolated.
     *   - Add the result to your temporary variable.
     * - The temporary variable now holds the contributions from all
     *   @f$ 2^d @f$ coarse grid values. Set it on the fine grid vertex.
     *
     * Usage:
     * \code
  double interpolatedValue = 0.0;
  dfor2(k) // run over the coarse grid vertices (0,0), (1,0), (0,1),
           // and (1,1) of this one coarse cell if d=2. If d!=2 it
           // does a couple of iterations more. k is an integer vector.
    tarch::la::Vector<FIVE_POWER_D,double> coarseGridStencil = coming from your coarse grid vertex k;
    double                                 coarseGridValue   = coming from your coarse grid vertex k;
    interpolatedValue +=
      coarseGridValue *
      _multigrid.computeContributionWeightOfInterGridTransfer(
        k,
        coarseGridStencil,
        fineGridPositionOfVertexWithinPatch
      );
  enddforx
  fineGridVertex.setXXX( interpolatedValue );
       \endcode
     *
     *
     * !! Usage (compute Ru, i.e. restriction, for one individual fine grid vertex)
     *
     * - Create temporary value. It is typically a double, and it represents
     *   the new fine grid value.
     * - Run over all @f$ 2^d @f$ (typically a dfor2 loop):
     *   - We will now compute the contribution of one coarse grid vertex to
     *     our fine grid vertex.
     *   - Take the coarse grid inter-grid transfer stencil. It is a
     *     @f$ 5^d @f$ vector.
     *   - Pass the stencil, the @f$ (0,1)^d @f$ position of the current
     *     coarse grid vertex relative to the current fine grid vertex, and the
     *     fine grid vertex position within the @f$ 4^d @f$ patch to this
     *     operation. You get a weight.
     *   - Multiply this weight with the fine grid vertex's value that is to
     *     be interpolated.
     *   - Add the result to the current coarse grid value.
     *
     * Usage:
     * \code
  dfor2(k) // run over the coarse grid vertices (0,0), (1,0), (0,1),
           // and (1,1) of this one coarse cell if d=2. If d!=2 it
           // does a couple of iterations more. k is an integer vector.
    tarch::la::Vector<FIVE_POWER_D,double> coarseGridStencil = coming from your coarse grid vertex k;
    double contributionToCurrentCoarseGridVertex =
      fineGridValue *
      _multigrid.computeContributionWeightOfInterGridTransfer(
        k,
        coarseGridStencil,
        fineGridPositionOfVertexWithinPatch
      );
  enddforx
       \endcode
     *
     *
     * The operation is not const as it has to update the internal counters.
     */
    static double computeContributionWeightOfInterGridTransfer(
      const tarch::la::Vector<DIMENSIONS,int>&       currentCoarseGridVertex,
      const tarch::la::Vector<FIVE_POWER_D,double>&  currentCoarseGridVertexsInterGridTransferOperator,
      const tarch::la::Vector<DIMENSIONS,int>&       fineGridPositionOfVertex
    );


    /**
     * Inject values from fine grid onto coarse grid
     *
     * Typical application snippet:
     * \code
  VertexOperations::writeU(
    coarseGridVerticesEnumerator,
    coarseGridVertices,
    _multigrid.injectFineGridValues( VertexOperations::readAllU( fineGridVerticesEnumerator, fineGridVertices ) )
  );
       \endcode
     *
     * Many codes cannot use this operation as illustrated above as they
     * rely on touchVertexLastTime() to perform some operations before they
     * restrict. And this operation clearly is tied to ascend() which is called
     * before touchVertexLastTime(). In such a case, you have to induce all
     * vertex data manually vertex-by-vertex. Please consult in particular
     * SingleLevelEnumerator in this case. It comes along with some static
     * operations that are of great use for injection.
     */
    static tarch::la::Vector<TWO_POWER_D,double> injectFineGridValues(
      const tarch::la::Vector<FOUR_POWER_D,double>&  fineGridValues
    );


    static tarch::la::Vector<TWO_POWER_D,std::complex<double> > injectFineGridValues(
      const tarch::la::Vector<FOUR_POWER_D,std::complex<double> >&  fineGridValues
    );

    /**
     * Prolongs the four values along a hole @f$3^d@f$ patch.
     */
    static tarch::la::Vector<FOUR_POWER_D,double > dLinearInterpolation(
      const tarch::la::Vector<TWO_POWER_D,double >&  coarseGridValues
    );

    static tarch::la::Vector<FOUR_POWER_D,std::complex<double> > dLinearInterpolation(
      const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&  coarseGridValues
    );

    /*
     * May be const, as it does not directly apply the matrix. If you apply the
     * matrix manually, please be aware that the internal flop counter is not
     * accurate anymore.
     */
    static tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double> calculateP(
      const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&  coarsePStencils
    );

    /**
     * Special case where the @f$ 2^d @f$ P operators of all involved vertices are the same.
     */
    static tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double> calculateP(
      const tarch::la::Vector<FIVE_POWER_D, double>&  coarsePStencil
    );


    /**
     * Again a special case of the previous one where P is d-linear.
     */
    static tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double> calculateP();


    /**
     * Calculates the P weights only for one particular vertex
     *
     * This operation accepts the inter-grid transfer operators for the 2^d
     * coarse grid vertices. They are given as 5^d stencils. It extracts
     * those weights that do influence the vertex at fineGridVertexPosition.
     * That means the operation does not compute anything. It simply extracts
     * from coarsePStencils.
     *
     * <h2> Interpolation </h2>
     *
     * Operator-dependent interpolation can be realised as scalar product:
     * <pre>
const tarch::la::Vector<TWO_POWER_D,double > u_3h  = VertexOperations::readU(coarseGridVerticesEnumerator,coarseGridVertices);
const tarch::la::Vector<TWO_POWER_D, double> P     = matrixfree::solver::Multigrid::calculateP(coarseStencils,fineGridVertexPosition);
double                                       Pu_3h = P*u_3h;
     </pre>
     * -
     */
    static tarch::la::Vector<TWO_POWER_D, double> calculateP(
      const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&  coarsePStencils,
      const tarch::la::Vector<DIMENSIONS,int>&                          fineGridVertexPosition
    );

    static tarch::la::Vector<TWO_POWER_D, double> calculateP(
      const tarch::la::Vector<FIVE_POWER_D, double>&                    coarsePStencil,
      const tarch::la::Vector<DIMENSIONS,int>&                          fineGridVertexPosition
    );

    static tarch::la::Vector<TWO_POWER_D, double> calculateP(
      const tarch::la::Vector<DIMENSIONS,int>&                          fineGridVertexPosition
    );

    /*
     * Determine Petrov-Galerkin coarse grid operator
     *
     * Calculate the cell-wise (four times four in 2d) Petrov-Galerkin coarse grid operators for the coarse vertices of one cell.
     * As with the elementwise assembly matrix, the contributions of the adjacent cell-wise operators have to be added in order to get the complete
     * vertex-wise operator.
     *
     * <h1>Usage notes</h1>
     *
     * This operation is typically called in enter or leave cell.
     *
     * Many users use stencil::getElementWiseAssemblyMatrix() to derive
     * element-wise assembly matrices from stencils. In the context of this
     * operation, you might want to use reconstructUniformFragments() which is
     * the inverse and gives you the stencil (fragmnets) that belong to an
     * assembly matrix.
     *
     * @param fineGridPositionOfCell  Position of cell within parent position.
     *            Is an integer vector that may contain any entry from 0 to 2.
     *            0,0,0,... means that the fine grid cell is the left bottom
     *            child within the parent, 1,0,0,... is the next to the right
     *            and so forth.
     *
     */
    static tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> calculatePetrovGalerkinCoarseGridOperator(
      const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&   coarseGridVerticesP,
      const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&   coarseGridVerticesR,
      const tarch::la::Vector<DIMENSIONS,int>&                           fineGridPositionOfCell,
      tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double>                elementWiseAssemblyMatrix
    );

    /**
     * A variant of the more complicated function of the same name that
     * exclusively uses d-linear inter-grid transfer operators for restriction
     * and prolongation.
     */
    static tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> calculatePetrovGalerkinCoarseGridOperator(
      const tarch::la::Vector<DIMENSIONS,int>&                           fineGridPositionOfCell,
      tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double>                elementWiseAssemblyMatrix
    );

    /*
     * Get the 4x4 cell prolongation or restriction matrix for a fine grid cell from the 5x5 prolongation operator of the coarse grid vertices.
     * For Galerkin this is without scaling (according to the adjacent cells in the stencil), as the operator A ist already scaled correctly.
     * Otherwise, you have to scale it with 1/TWO_POWER_D.
     */
    static tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> calculateCellInterGridTransferOperator(
      const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&   coarseGridVerticesInterGridTransferOperators,
      const tarch::la::Vector<DIMENSIONS,int>&                           fineGridPositionOfCell
    );


    // Very simple: coarseGridVertexNumber*FIVE_POWER_D + positionInOperator
    static int getPositionInCellInterGridTransferOperatorVector(
      const int coarseGridVertexNumber,
      const int positionInOperator
    );
};


#include "matrixfree/solver/Multigrid.cpph"

#endif
