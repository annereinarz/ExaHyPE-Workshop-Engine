// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_SURPLUS_CALCULATOR_H_
#define _MATRIXFREE_SOLVER_SURPLUS_CALCULATOR_H_


#include "tarch/logging/Log.h"
#include "matrixfree/stencil/ElementMatrix.h"
#include <limits>


namespace matrixfree {
  namespace adaptivitycriteria {
    class SurplusCalculator;
  }
}



/**
 * Encapsulates Smoothness-Based Refinement and Coarsening
 *
 * To use this class, you have to do the following operations:
 *
 * - Add your dof a double vector of size dimensions:
\code
  discard parallelise double linearSurplus[DIMENSIONS];
\endcode
 *   This attribute typically is a discard attribute.
 * - Make your PDE-solver/adapter hold an instance of
 *   this class.
 * - Adopt your adapter accordingly:
 *   - beginIteration(): invoke clearMeasurements()
 *   - touchVertexFirstTime(): clear the surplus values of your vertex
 *   - enterCell(): invoke getNewLinearSurplus()
 *   - touchVertexLastTime(): invoke analyse() and getNewLinearSurplusContributionFromFineGrid()
 *
 *
 * @author Tobias Weinzierl
 * @version $Revision: 1.28 $
 */
class matrixfree::adaptivitycriteria::SurplusCalculator {
  public:
    enum Action {
      Refine, Delete, NoAction
    };

    static std::string toString( const Action& action );
  private:
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

      std::string toString() const;
    };

    stencil::ElementWiseAssemblyMatrix _elementMatrix[DIMENSIONS];

    double _refinementPercentage;
    double _deletePercentage;

    Bin*   _bins;

    int    _numberOfBins;
    int    _totalNumberOfSurplusEvaluations;
    int    _numberOfRefinedOrDeleteCalls;
    int    _maxNumberOfRefinementsOrCoarsenings;
    double _surplusMaximumOnFineGrid;
    double _surplusMaximumOnFineGridInNewIteration;

    double _minimumMeshSize;
    double _maximumMeshSize;

    /**
     * Log the current statistics.
     *
     * Should be called before you reset the counters.
     */
    void logStatistics(double minimumSurplus) const;

    /**
     * Rescale maximum surplus
     *
     * The refinement criterion is based upon a binning of the interval
     * (0,max-surplus). This surplus is determined on-the-fly whenever
     * analyse() is called. If this were the only update mechanism, the
     * surplus would never shrink even if the whole solution decreases.
     * Hence, the operation clearMeasurements() invokes this operation
     * studying the biggest bin. If this bin is empty, no vertex anymore
     * has a linear surplus close to the maximum surplus, and the operation
     * halves the field _surplusMaximumOnFineGrid. If this halving was too
     * aggressive, the next iteration will increase it again anyway.
     *
     * @image html SurplusCalculator.png
     */
    void rescaleSurplusMaximum();

    double getBinSize() const;

    int getBin(double surplus) const;

    double getBinOffset(int bin) const;
  public:
    /**
     * Surplus Calculator
     *
     * !!! Aspect of surplus to mesh width
     *
     * Depending on the smoothness of your solution, the surplus should
     * decrease with a higher polynomial rate than the mesh size.
     *
     * @param refinementPercentage How many percents of the whole vertices should be refined usually per iteration. 10 percent is a reasonable value.
     * @param deletePercentage     How many percents of the whole vertices should be refined usually per iteration. 10 percent is a reasonable value.
     * @param minimumMeshSize      What is the smallest mesh size allowed. Depends on scenario.
     * @param maximumMeshSize      What is the biggest mesh size allowed. Depends on scenario.
     * @param numberOfBins         How many bins shall class use (the more bins, the more accurate but slower the whole mechanism). At least three bins have to be used, ten is a reasonable value.
     * @param maxNumberOfRefinementsOrCoarsenings What is the maximum number of vertices the class is allowed to refine per iteration. You can switch this threshold off if you pass the maximum an integer can represent.
     */
    SurplusCalculator(
      double refinementPercentage,
      double deletePercentage,
      double minimumMeshSize,
      double maximumMeshSize,
      int    numberOfBins = 10,
      int    maxNumberOfRefinementsOrCoarsenings = std::numeric_limits<int>::max()
    );

    SurplusCalculator(const SurplusCalculator& otherCalculator);

    void mergeWithSurplusCalculatorFromOtherThread(const SurplusCalculator& otherCalculator);

    ~SurplusCalculator();

    /**
     * Clear Measurements and Adopt Bins
     *
     * Is typically called by beginIteration(). The workflow is very simple:
     *
     * - Set all the bins either to refine or no operation. Which one to chose
     *   depends on the percentage of vertices to refine. We start with the
     *   biggest bin and then count down, while we analyse how many vertices
     *   belonged into which bin in the last iteration.
     * - Traverse the bins once more starting with the smallest bin. Reset the
     *   no-action bins to delete if vertices are to be deleted.
     *
     * If a bin has been set to refine first and afterwards to delete, there's
     * obviously a conflict: A bin has to be both a refine and a delete bin. In
     * this case, the bin is set to no-operation. The only exception is bins
     * whose range is smaller than _minimumSurplus. Those are always flagged
     * with delete.
     *
     * Finally, the operation logs the statistics due to logStatistics() and
     * resets all counters.
     *
     * If we didn't set the very first bin to delete, applications with a very
     * smooth solution yielding a regular grid and a permanently decreasing
     * solution fail: Here, the surpluses become smaller and smaller.
     * Consequently, also the binned interval becomes smaller. At the same time,
     * only the fine grid vertices fall into bin zero. Now, at some point all the
     * coarse grid points wander into bin 0 as the minimal bin size constraint of
     * rescaleSurplusMaximum() starts to hold. As the algorithm still tries to
     * refine, now bin(0) is set to no-action even though the solution is extremely
     * smooth and small. So I decided to make bin(0) hold the delete flag always
     * if the algorithm shall delete elements from the grid.
     *
     * @image html SurplusCalculator.png
     *
     *
     * !!! The smallest linear surplus
     *
     * For many cooling phenomena, it is important to prescribe a minimum
     * surplus, i.e. each vertex with a surplus smaller than this threshold is
     * deleted. At the same time, this is kind of a magic parameter that
     * influences your refinement pattern dramatically. I suggest to pass here
     * a scaled mesh size, i.e. to take the maximum mesh size (which defines
     * your worst-case accuracy) and to multiply it with the biggest absolute
     * value of the solution ever measured. This ensures that the mesh becomes
     * coarser and coarser if the whole solution disappears. How fast it is
     * coarsed is determined by the maximum mesh size. At the same time, it
     * ensures that very small surpluses do not fall into this category (delete)
     * if the overall solutino already is almost zero and we wanna study the
     * effect of small stimuli.
     *
     * @param minimumSurplus Is the smallest linear surplus that should not be
     *           deleted. Set to zero to switch this off.
     */
    void clearMeasurements(double minimumSurplus);

    /**
     * Computes contributions to the linear surplus of all adjacent vertices
     *
     * To be called by each enterCell operation, even if the cell is not
     * refined. A typical usage pattern looks as follows:
     * \code
    SpacetreeGridVertex::writeLinearSurplus(
      fineGridVerticesEnumerator,
      fineGridVertices,
      _surplusCalculator.getNewLinearSurplus(
        SpacetreeGridVertex::readU(fineGridVerticesEnumerator,fineGridVertices),
        SpacetreeGridVertex::readLinearSurplus(fineGridVerticesEnumerator,fineGridVertices)
      )
    );
       \endcode
     */
    tarch::la::Vector<TWO_POWER_D_TIMES_D,double> getNewLinearSurplus(
      const tarch::la::Vector<TWO_POWER_D,double>&          u,
      const tarch::la::Vector<TWO_POWER_D_TIMES_D,double>&  linearSurplusSoFar
    ) const;

    /**
     * Analyse a vertex and derive action to be taken
     *
     * Please call this operation solely for inside vertices.
     *
     * !!! Analysis Workflow
     *
     * - Check whether the vertex is refined though the surrounding cells are smaller than the minimum mesh size. If so, coarse it.
     * - Check whether the vertex is unrefined though the surrounding cells are bigger than the maximum mesh size. If so, refine it.
     * - Check whether the vertex holds inf as surplus (see getLinearSurplusContributionFromFineGrid()). If so, leave it as it is.
     * - If all this does not hold:
     *   - Update the local attribute _surplusMaximumOnFineGrid.
     *   - If _surplusMaximumOnFineGrid equals 0.0 jump out of analyser, as in this case we cannot do bin sorting.
     *   - Sort into bin and decide what to do.
     *
     * !!! Comparison Details / Vetos
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
     * @image html SurplusCalculator.png
     *
     * @param linearSurplus Linear surplus in this vertex.
     */
    Action analyse(
      const tarch::la::Vector<DIMENSIONS,double>&   linearSurplus,
      bool                                          isRefined,
      bool                                          isUnrefined,
      const tarch::la::Vector<DIMENSIONS,double>&   h
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
     * vertices to 'do under no circumstances coarse the grid here'.
     */
    tarch::la::Vector<TWO_POWER_D_TIMES_D,double> getLinearSurplusContributionFromFineGrid(
      const tarch::la::Vector<DIMENSIONS,double>&  linearSurplusOfFineGridVertex,
      bool                                         fineGridVertexIsUnrefined
    ) const;

    void setMinMaxMeshWidth( double minimumMeshSize, double maximumMeshSize );

    double getMaximumMeshWidth() const;
};


#endif
