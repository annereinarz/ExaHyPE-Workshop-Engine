// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_LINEAR_SURPLUS_REFINEMENT_CRITERION_H_
#define _MATRIXFREE_SOLVER_LINEAR_SURPLUS_REFINEMENT_CRITERION_H_


#include "tarch/logging/Log.h"
#include "matrixfree/stencil/ElementMatrix.h"

#include <limits>
#include <complex>


namespace matrixfree {
  namespace adaptivitycriteria {
    class LinearSurplusRefinementCriterion;
  }
}



/**
 * Encapsulates Smoothness-based Refinement and Coarsening
 *
 * This class is not abstract, but it should not be used directly. Use a subclass instead.
 *
 *
 * <h2> Usage </h2>
 *
 * To use this class, you have to do the following operations:
 *
 * - Add your dof/vertex a double vector of size dimensions:
 *   <pre>
  discard parallelise double linearSurplus[DIMENSIONS];
     </pre>
 *   This attribute typically is a discard attribute. Alternatively, you can
 *   use a scalar. The vector version is more accurate, as it is able to
 *   detect saddle points. The scalar version however works as good in most
 *   cases.
 *
 * - You need the following operations on your vertex that are to be specified
 *   in the specification file:
 *   <pre>
     read vector(double): linearSurplus
     read vector(double): solution
     write vector(double): linearSurplus
     </pre>
 *
 *    Please adopt the term solution accordingly. If you decide to work with
 *    scalars only, exchange the vectors by scalars.
 *
 * - Make your PDE-solver/mapping hold an instance of matrixfree::solver::LinearSurplusRefinementCriterion.
 *
 * - Augment your mapping accordingly:
 *   - Constructor: Make it pass the instance of
 *     LinearSurplusRefinementCriterion reasonable values (you might also
 *     decide to plug in meaningless values for the time being and then reset
 *     those values later in beginIteration()). Please note that there are two
 *     subtypes of this class. You have to choose one of them.
 *   - beginIteration(): invoke clearMeasurements(). The operation might
 *     require an argument. Check the documentation of the subclass you have
 *     chosen.
 *   - touchVertexFirstTime(): clear the surplus values of your vertex. If you
 *     include VertexOperations.h, you may call
 *     <pre>
 *       VertexOperations::writeLinearSurplus(fineGridVertex,0.0);
 *     </pre>
 *   - enterCell(): invoke getNewLinearSurplus(). See the method's
 *     documentation for more details how to invoke the operation.
 *   - touchVertexLastTime(): invoke analyse() and
 *     getNewLinearSurplusContributionFromFineGrid(). See code snippet below
 *     for a typical usage pattern.
 *
 *  Please ensure that you do the analysis only for inner points. The linear
 *  surplus is invalid for boundary vertices.
 *
 *
 * <h2> Remarks on the refinement criterion </h2>
 *
 * touchVertexLastTime() invokes analyse to identify whether a vertex is to be
 * refined/coarsened or not. This is only a textbook guideline. In practice, it
 * does make sense to make the refinement decision anticipate
 * application-specific knowledge:
 *
 * - If the problem is elliptic, it usually makes sense to implement
 *   the Delete action as well. It often happens that one overestimates/wants to
 *   overestimate the refinement or that a regular refinement speeds up the
 *   convergence (either due to the vectorisation or mg speed) but then has to
 *   coarse the solution to reduce the memory footprint.
 * - If a part of the grid has not converged, it usually does not make sense to
 *   refine it further: Too high is the risk that the final solution is smooth
 *   and does not require further refinement (see notes on oscillations). I thus
 *   recommend to have an additional check within the Refine branch that takes
 *   the residual into account. Please be aware that the residual does not give
 *   any clue how big the change in the solution actually is, i.e. it usually
 *   gives better results to divide the residual by the diagonal element.
 *   Please do not the residual analysis around the overall refinement
 *   evaluation. analyse() has to be called always. Otherwise, the criterion has
 *   no idea of the overall surplus distribution.
 * - If you are interested in a smooth grid an a global energy minimisation, it
 *   also makes sense to penalty too deep refinements. In this case, I recommend
 *   to scale the refinement criterion with the volume of fineGridH. In most
 *   cases however this scaling has no severe impact.
 *
 * A typical snippet for an elliptic code then is given below.
 *
 * <h2>Typical touchVertexLastTime</h2>
 *
 * <pre>
touchVertexLastTime(
  multigrid::Vertex&                           fineGridVertex,
  const tarch::la::Vector<DIMENSIONS,double>&  fineGridX,
  const tarch::la::Vector<DIMENSIONS,double>&  fineGridH,
  multigrid::Vertex * const                    coarseGridVertices,
  const peano::grid::VertexEnumerator&         coarseGridVerticesEnumerator,
  multigrid::Cell&                             coarseGridCell,
  const tarch::la::Vector<DIMENSIONS,int>&     fineGridPositionOfVertex
) {
  logTraceInWith6Arguments( "touchVertexLastTime(...)", fineGridVertex, fineGridX, fineGridH, coarseGridVerticesEnumerator.toString(), coarseGridCell, fineGridPositionOfVertex );

  if ( fineGridVertex.isInside() ) {
    const tarch::la::Vector<TWO_POWER_D_TIMES_D,double > coarseGridLinearSurplus =
      VertexOperations::readLinearSurplus(coarseGridVerticesEnumerator, coarseGridVertices)
      +
      _refinementCriterion.getLinearSurplusContributionFromFineGrid(
        VertexOperations::readLinearSurplus( fineGridVertex ),
        fineGridVertex.getRefinementControl()==Vertex::Records::Unrefined,
        fineGridPositionOfVertex
      );

    VertexOperations::writeLinearSurplus( coarseGridVerticesEnumerator, coarseGridVertices, coarseGridLinearSurplus );

    switch (
      _refinementCriterion.analyse(
        VertexOperations::readLinearSurplus(fineGridVertex),
        fineGridVertex.getRefinementControl()==Vertex::Records::Refined,
        fineGridVertex.getRefinementControl()==Vertex::Records::Unrefined,
        fineGridH
      )
    ) {
      case matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Refine:
        if ( tarch::la::abs( fineGridVertex.getResidual() )<_convergenceThreshold) {
          fineGridVertex.refine();
        }
        else {
          logDebug( "touchVertexLastTime(...)", "skip refinement as vertex has not converged yet. r=" << fineGridVertex.getR() << ", |r|=" << tarch::la::abs(fineGridVertex.getR()) );
        }
        break;
      case matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Delete:
        //fineGridVertex.erase();
        break;
      case matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::NoAction:
        break;
    }
  }

  ...

  logTraceOutWith1Argument( "touchVertexLastTime(...)", fineGridVertex );
}
    </pre>
 *
 *
 * <h2> Oscillations with MLAT-type algorithms </h2>
 *
 * For partially overlapping domain decomposition approaches (MLAT, e.g.) in
 * combination with Jacobi or other single-level solvers, we observe that the
 * refinement criterion often introduces dents in the solution. The explanation
 * is straightforward: The solver is in O(n^2), i.e. the finer the grid the slower
 * convergence. If we introduce a transition from h into h/3, the finer part in
 * the next sweep will converge slower than the coarser part. If the solution
 * has not converged before reasonably, the faster part will move towards the
 * solution faster than the slower part. For an inhomogeneous rhs, this means
 * that it somehow overtakes the fine resolution area. This is the reason for a
 * dent seen.
 *
 * This is a solver-inherent problem. There are some ways to deal with it if
 * you want to (it is actually not wrong, so there is no urgent need to fix
 * this dent):
 *
 * - Use a higher order interpolation scheme when refining or apply few
 *   smoothing steps on added vertices prior to continuing. The latter picks up
 *   the smoothed aggregation idea. This does not completely solve the issue
 *   (it is still a solver-inherent think), but it smoothes out the dent
 *   slightly.
 * - Use a multigrid solver. This one is in O(n) and thus should not run into
 *   these problems.
 * - Do not refine before the solution has not converged globally. In this
 *   case, the dents cannot pop up as well.
 *
 * <h2> Complex-valued PDEs </h2>
 *
 * This class should also work for complex-valued PDEs.
 *
 * <h2> inf values on vertices (also important for plotting) and 3:1 balancing </h2>
 *
 * The refinement criterion ensures that it never erases more than one level.
 * For this, it does a very simple trick: Usually, getLinearSurplusContributionFromFineGrid()
 * restricts doubles/complex numbers from the fine grid to the next coarser
 * grid (cmp to blue arrow in illustration below).
 *
 * @image html LinearSurplusRefinementCriterion.png
 *
 * In the sketch above, vertices marked with a green circle are refined. The
 * vertex marked with a blue one is unrefined.
 *
 * If a vertex value from a refined vertex shall be restricted (red arrow), we
 * set the linear surplus on the parent vertex to infinity. This in turn means
 * that adjacent vertices on the coarse level might become infinity as well
 * (blue arrow). The might here is the right wording: Whether the infinity
 * marker propagates depends on the traversal order, i.e. whether the
 * corresponding cells have been visited before (then, infinity is not propagated)
 * or is processed afterward.
 *
 * Two properties hold for vertices marked with infinity:
 *
 * - We may not use them for our bucket sort. See analyse().
 * - We cannot make any statement on the linear surplus for infinity nodes,
 *   i.e. whether they have to be refined or not.
 *
 * In rare cases, our discussion implies that adaptivity patterns become
 * unsymmetric even though the problem itself has symmetry. This is due to the
 * fact that the infinity propagates along the cells not handled yet. In this
 * case, it helps to tweak the refinement conditions to yield a more regular
 * adaptivity pattern.
 *
 * If you want to plot the linear surplus, please add an additional if branch
 * where you exclude inf vertices on purpose. But be aware that the infinity
 * there is not a bug.
 *
 * The inf discussion reveals that we can use this behaviour for some balancing:
 * Whenever we encounter inf on an unrefined vertex, we can refine this one.
 * Such a strategy successively removes the infs from the grid, but it induces
 * a very smooth adaptivity pattern, i.e. it is way more aggressive than plain
 * 3:1 balancing. You can switch this behaviour on/off in the constructor of the
 * criterion. By default, it is switched off.
 *
 * @author Tobias Weinzierl
 * @version $Revision: 1.1 $
 */
class matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion {
  public:
    enum Action {
      Refine, Delete, NoAction
    };

    static std::string toString( const Action& action );
  protected:
    static tarch::logging::Log _log;

    struct Bin {
      /**
       * Counts how many vertices fall into this category.
       */
      int     _numberOfEntries;
      /**
       * Counts how many vertices fall into this category but cannot
       * perform the corresponding action (for example they should coarse
       */
      int     _numberOfCancelledActions;

      Action  _associatedAction;

      /**
       * Minimal mesh width of entries falling into this bin.
       */
      double  _minH;

      /**
       * Maximum mesh width of entries falling into this bin.
       */
      double  _maxH;

      std::string toString() const;
    };

    stencil::ElementWiseAssemblyMatrix _elementMatrix[DIMENSIONS];

    Bin*   _bins;

    int    _numberOfBins;
    int    _totalNumberOfSurplusEvaluations;
    int    _numberOfRefinedOrDeleteCalls;
    int    _maxNumberOfRefinementsOrCoarsenings;
    double _surplusMaximumOnFineGrid;
    double _surplusMaximumOnFineGridInNewIteration;

    /**
     * See class documentation, Section on balancing
     */
    bool   _smoothCoarseGrid;

    bool   _isSwitchedOn;

    double getBinSize() const;

    int getBin(double surplus) const;

    double getBinOffset(int bin) const;

    /**
     * @see Other analyse() operation.
     */
    Action analyse(
      double maxOfLinearSurplus,
      double maxOfH,
      bool   isRefined,
      bool   isUnrefined
    );

    /**
     * Surplus Calculator
     *
     * !!! Aspect of surplus to mesh width
     *
     * Depending on the smoothness of your solution, the surplus should
     * decrease with a higher polynomial rate than the mesh size.
     *
     * @param refinementPercentage How many of all the vertices should be refined usually per iteration. 10 percent (0.1) is a reasonable value. Value has to be between 0 and 1.
     * @param deletePercentage     How many of all the vertices should be refined usually per iteration. 10 percent (0.1) is a reasonable value. Value has to be between 0 and 1.
     * @param minimumMeshSize      What is the smallest mesh size allowed. Depends on scenario.
     * @param maximumMeshSize      What is the biggest mesh size allowed. Depends on scenario.
     * @param numberOfBins         How many bins shall class use (the more bins, the more accurate but slower the whole mechanism). At least three bins have to be used, ten is a reasonable value.
     * @param maxNumberOfRefinementsOrCoarsenings What is the maximum number of vertices the class is allowed to refine per iteration. You can switch this threshold off if you pass the maximum an integer can represent.
     * @param smooth               Smooth out adaptivity pattern, i.e. enforce at least a 3:1 balancing. See class documentation on infs.
     */
    LinearSurplusRefinementCriterion(
      bool   smoothCoarseGrid = false,
      int    numberOfBins = 10,
      int    maxNumberOfRefinementsOrCoarsenings = std::numeric_limits<int>::max()
    );

    LinearSurplusRefinementCriterion(const LinearSurplusRefinementCriterion& otherCalculator);

    static bool restrictToCoarseGridPoint(
      const tarch::la::Vector<DIMENSIONS,int>&  coarseGridPositionOfVertex,
      const tarch::la::Vector<DIMENSIONS,int>&  fineGridPositionOfVertex
    );
  public:
    void mergeWithLinearSurplusRefinementCriterionFromOtherThread(const LinearSurplusRefinementCriterion& otherCalculator);

    virtual ~LinearSurplusRefinementCriterion();

    /**
     * Computes contributions to the linear surplus of all adjacent vertices
     *
     * To be called by each enterCell operation, even if the cell is not
     * refined. A typical usage pattern looks as follows:
     * <pre>
    VertexOperations::writeLinearSurplus(
      fineGridVerticesEnumerator,
      fineGridVertices,
      _refinementCriterion.getNewLinearSurplus(
        VertexOperations::readU(fineGridVerticesEnumerator,fineGridVertices),
        VertexOperations::readLinearSurplus(fineGridVerticesEnumerator,fineGridVertices)
      )
    );
       </pre>
     */
    tarch::la::Vector<TWO_POWER_D_TIMES_D,double> getNewLinearSurplus(
      const tarch::la::Vector<TWO_POWER_D,double>&          u,
      const tarch::la::Vector<TWO_POWER_D_TIMES_D,double>&  linearSurplusSoFar
    ) const;

    tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> > getNewLinearSurplus(
      const tarch::la::Vector<TWO_POWER_D,std::complex<double>>&          u,
      const tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double>>&  linearSurplusSoFar
    ) const;

    tarch::la::Vector<TWO_POWER_D,double> getNewLinearSurplus(
      const tarch::la::Vector<TWO_POWER_D,double>&  u,
      const tarch::la::Vector<TWO_POWER_D,double>&  linearSurplusSoFar
    ) const;

    tarch::la::Vector<TWO_POWER_D,std::complex<double> > getNewLinearSurplus(
      const tarch::la::Vector<TWO_POWER_D,std::complex<double>>&  u,
      const tarch::la::Vector<TWO_POWER_D,std::complex<double>>&  linearSurplusSoFar
    ) const;

    /**
     * You should add the result to your linear surplus - or use getNewLinearSurplus straight away.
     */
    tarch::la::Vector<TWO_POWER_D,double> getLinearSurplusDelta(
      const tarch::la::Vector<TWO_POWER_D,double>&  u
    ) const;

    tarch::la::Vector<TWO_POWER_D,std::complex<double> > getLinearSurplusDelta(
      const tarch::la::Vector<TWO_POWER_D,std::complex<double>>&  u
    ) const;

    /**
     * Analyse a vertex and derive action to be taken
     *
     * Please call this operation solely for inside vertices. I also recommend
     * to do the analysis in iterative schemes only for vertices with a very
     * small residual, i.e. for converged solutions. Or, alternatively, only
     * every k steps. Otherwise, any grid restructuring might yield strange
     * grid structure oscillations.
     *
     *
     * <h1> Analysis Workflow </h1>
     *
     * - Check whether the vertex is refined though the surrounding cells are smaller than the minimum mesh size. If so, coarse it.
     * - Check whether the vertex is unrefined though the surrounding cells are bigger than the maximum mesh size. If so, refine it.
     * - Check whether the vertex holds inf as surplus (see getLinearSurplusContributionFromFineGrid()). If so, leave it as it is.
     * - If all this does not hold:
     *   - Update the local attribute _surplusMaximumOnFineGrid.
     *   - If _surplusMaximumOnFineGrid equals 0.0 jump out of analyser, as in this case we cannot do bin sorting.
     *   - Sort into bin and decide what to do.
     *
     * <h1> Comparison Details / Vetos </h1>
     *
     * As soon as the right bin has been found, a couple of veto constraints
     * have to be checked:
     *
     * - If refine has been derived for a refined vertex or if delete has been
     *   derived for an unrefined vertex, change action into no action.
     * - If the operation wants to refine but the maximum number of refinements
     *   already has been reached, change the refine command into no action.
     * - Each refine induces a smaller mesh width by a factor of three. If this
     *   mesh width is smaller than the minimum mesh width, reject the
     *   refinement command and change it into no action. For this analysis, we
     *   have to devide the current mesh width by 3.0, as this operation always
     *   is called for an unrefined vertex which then becomes a coarse grid
     *   vertex.
     * - If a delete is derived, this delete is derived for an already refined
     *   vertex. If the adjacent cells have a mesh width that is bigger than
     *   the maximum mesh width, it would not be a good idea to make the mesh
     *   coarser here. In this case, we change the action into no action. For
     *   the check, h does not have to be rescaled.
     *
     * @image html LinearSurplusRefinementCriterion.png
     *
     * @param linearSurplus Linear surplus in this vertex.
     * @param isRefined     Typically stems from vertex.getRefinementControl()==Vertex::Records::Refined
     */
    Action analyse(
      const tarch::la::Vector<DIMENSIONS,double>&   linearSurplus,
      bool                                          isRefined,
      bool                                          isUnrefined,
      const tarch::la::Vector<DIMENSIONS,double>&   h
    );

    Action analyse(
      double                                        linearSurplus,
      bool                                          isRefined,
      bool                                          isUnrefined,
      const tarch::la::Vector<DIMENSIONS,double>&   h
    );

    Action analyse(
      const tarch::la::Vector<DIMENSIONS,std::complex<double> >&   linearSurplus,
      bool                                                         isRefined,
      bool                                                         isUnrefined,
      const tarch::la::Vector<DIMENSIONS,double>&                  h
    );

    Action analyse(
      const std::complex<double>&                  linearSurplus,
      bool                                         isRefined,
      bool                                         isUnrefined,
      const tarch::la::Vector<DIMENSIONS,double>&  h
    );

    /**
     * Project surplus to coarser level
     *
     * This operation is to be invoked by the touchVertexLastTime() for each
     * vertex that is inside the domain. The result of the operation has to
     * be added the coarser cell's linear surplus values.
     *
     * !!! Avoid Coarsening of More Than One Level
     *
     * If the vertex that is analysed is refined, i.e. it is refined before we
     * store it away, we set the refinement flag of the @f$ 2^d @f$ coarser
     * vertices to 'do under no circumstances coarse the grid here'. This flag
     * is just infinity for the surplus.
     *
     * Please consult the class documentation for some remarks how this behaviour
     * interfers with 3:1 balancing.
     *
     * !!! Implementation
     *
     * Originally, my plan was just to restrict $7^{-d}$ of the surplus to all
     * parents, i.e. to restrict the average to the next coarser grid. In
     * principle, that would work. However, we have the spacetree restricting us:
     *
     * @image html linear-surplus-restriction.png
     *
     * For the could restrict the blue vertex to all parents and everything would
     * be fine. The red however may not be restricted to all parents. If we
     * restricted it to all parents, it would depend on the sfc whether which
     * parents are addressed. For symmetric problems, this inevitable leads to
     * non-symmetric restrictions of the linear surplus and, thus, to
     * non-symmetric refinement patterns.
     *
     * I thus stick with an averaging over $5^d$ points for one coarse grid vertex.
     */
    tarch::la::Vector<TWO_POWER_D_TIMES_D,double> getLinearSurplusContributionFromFineGrid(
      const tarch::la::Vector<DIMENSIONS,double>&  linearSurplusOfFineGridVertex,
      bool                                         fineGridVertexIsUnrefined,
      const tarch::la::Vector<DIMENSIONS,int>&     fineGridPositionOfVertex
    ) const;

    tarch::la::Vector<TWO_POWER_D,double> getLinearSurplusContributionFromFineGrid(
      double                                       linearSurplusOfFineGridVertex,
      bool                                         fineGridVertexIsUnrefined,
      const tarch::la::Vector<DIMENSIONS,int>&     fineGridPositionOfVertex
    ) const;

    tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> > getLinearSurplusContributionFromFineGrid(
      const tarch::la::Vector<DIMENSIONS,std::complex<double> >&  linearSurplusOfFineGridVertex,
      bool                                                        fineGridVertexIsUnrefined,
      const tarch::la::Vector<DIMENSIONS,int>&                    fineGridPositionOfVertex
    ) const;

    tarch::la::Vector<TWO_POWER_D,std::complex<double> > getLinearSurplusContributionFromFineGrid(
      const std::complex<double>&                  linearSurplusOfFineGridVertex,
      bool                                         fineGridVertexIsUnrefined,
      const tarch::la::Vector<DIMENSIONS,int>&     fineGridPositionOfVertex
    ) const;

    /**
     * @see getStatistics()
     */
    std::string toString() const;

    /**
     * Plot statistics alike
     *
     * (no-of-surplus-evaluations=520,no-of-refined-or-deleted-cells=56,surplus-max=0.174566,surplus-max(new)=0.174566)
     *
     * Please note that all actions enlisted here are the recommendations of
     * the refinement criterion. It does not mean that the code has realised
     * them. Many codes, e.g., neglect the criterion for grid entities where
     * the residual of a linear equation system has not underrun a certain
     * threshold yet.
     *
     * toString() displays the results getStatistics() plus additional information.
     */
    std::string getStatistics() const;

    void switchOn(bool value);
};


#endif
