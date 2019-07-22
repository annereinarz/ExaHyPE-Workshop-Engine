// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_LINEAR_SURPLUS_REFINEMENT_CRITERION_WITH_FIXED_MESH_SIZES_H_
#define _MATRIXFREE_SOLVER_LINEAR_SURPLUS_REFINEMENT_CRITERION_WITH_FIXED_MESH_SIZES_H_


#include "matrixfree/adaptivitycriteria/LinearSurplusRefinementCriterion.h"


namespace matrixfree {
  namespace adaptivitycriteria {
    class LinearSurplusRefinementCriterionWithFixedMeshSizes;
  }
}



class matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes: public matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion {
  private:
    static tarch::logging::Log _log;
  protected:

    double _minimumMeshSize;
    double _maximumMeshSize;

    double _refinementPercentage;
    double _deletePercentage;

  public:
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
    LinearSurplusRefinementCriterionWithFixedMeshSizes(
      double refinementPercentage,
      double deletePercentage,
      double minimumMeshSize,
      double maximumMeshSize,
      bool   smoothCoarseGrid = false,
      int    numberOfBins = 10,
      int    maxNumberOfRefinementsOrCoarsenings = std::numeric_limits<int>::max()
    );

    LinearSurplusRefinementCriterionWithFixedMeshSizes(const LinearSurplusRefinementCriterionWithFixedMeshSizes& otherCalculator);

    virtual ~LinearSurplusRefinementCriterionWithFixedMeshSizes();

    void setMinMaxMeshWidth( double minimumMeshSize, double maximumMeshSize );

    double getMaximumMeshWidth() const;


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
     * @image html LinearSurplusRefinementCriterion.png
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
     *
     * !!! Rescale maximum surplus
     *
     * The refinement criterion is based upon a binning of the interval
     * (0,max-surplus). This binning splits up the whole range of linear
     * surpluses into equidistant bins. To avoid non-symmetric refinement
     * patterns, I fix the bin size once per traversal (see clear
     * operation).
     *
     * Please note that the binning itself is done without taking into
     * account the maximum surplus: The bins are marked according to the
     * number of surpluses that fall into the respective class. The maximum
     * surplus thus is only important to identify which surplus belongs into
     * which class.
     *
     * In principle we thus can split up the range of surpluses once and then
     * proceed. In practice, this is not always a good choice, as the range of
     * surpluses might change over time signficiantly and then no grid
     * adaption is performed anymore.
     *
     * There are basically two interesting cases:
     *
     * - The maximum linear surplus has grown. This can happen, if strong
     *   (stiff) PDE parts haven't been resolved before or if we are
     *   tackling time-dependent problems. In this case, I bookkeep the
     *   new linear surplus and thus rescale all the bins to this new biggest
     *   surplus value in the next iteration.
     * - No refinement took place though the maximum linear surplus has not
     *   grown.
     *
     * For the latter case, there are two possible explanations illustrated
     * in the picture below.
     *
     * @image html LinearSurplusEvolution.png
     *
     * - All non-smooth regions have been resolved (red): In this case, all
     *   the regions of the domain with a big linear surplus have been refined,
     *   the linear surplus there became smaller and thus the whole spectrum
     *   has shifted to the left.
     * - The non-smooth regions are already resolved to the smallest mesh size
     *   (green): In this case, all rough regions have been refined. However,
     *   there are still rough vertices left, but we cannot refine them further
     *   due to the given minimum mesh size.
     *
     * In the latter case, the bin assignment should not be changed: It is due
     * to grid constraints that we cannot adopt the finest grid anymore. It is
     * up to the user to deal with this - the reason might be numerical
     * pollution, e.g.
     *
     * If we the maximum surplus has fallen, we do not rescale directly.
     * Instead, we slowly adopt the scaling. This way, we avoid that we have
     * a too aggressive refinement if the surplus goes down globally. However,
     * the current choice (around 1/4) might be too defensive. Actually for
     * some scenarios we even found that overweighting might make sense.
     * However, 0.25 seems to be convenient choice.
     *
     *
     * !!! Flagging of empty bins
     *
     * We occasionally had the situation that the finest bin was not used in a
     * computation. As a consequence, its hmin and hmax entries were wrong.
     * If we'd not check for the number of bin entries, those bins would be
     * flagged as refine/coarse even though they should not hold such a marker
     * due ot grid size constraints. Rather than expensive data analysis which
     * data is valid/uninitialised, we do not mark any bin that did not contain
     * any entries before, as these bins might hold invalid statistics.
     *
     *
     * @param minimumSurplus Is the smallest linear surplus that should not be
     *           deleted. Set to zero to switch this off.
     */
    void clearMeasurements(double minimumSurplus);
};


#endif
