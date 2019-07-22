// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_LINEAR_SURPLUS_REFINEMENT_CRITERION_WITH_FIXED_VERTEX_COUNTS_H_
#define _MATRIXFREE_SOLVER_LINEAR_SURPLUS_REFINEMENT_CRITERION_WITH_FIXED_VERTEX_COUNTS_H_


#include "matrixfree/adaptivitycriteria/LinearSurplusRefinementCriterion.h"


namespace matrixfree {
  namespace adaptivitycriteria {
    class LinearSurplusRefinementCriterionWithFixedVertexCounts;
  }
}


class matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts: public matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion {
  private:
    static tarch::logging::Log _log;

    double _maxNumberOfVertices;
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
    LinearSurplusRefinementCriterionWithFixedVertexCounts(
       double maxNumberOfVertices,
       bool   smoothCoarseGrid = false,
       int    numberOfBins = 10,
       int    maxNumberOfRefinementsOrCoarsenings = std::numeric_limits<int>::max()
     );

    LinearSurplusRefinementCriterionWithFixedVertexCounts(const LinearSurplusRefinementCriterionWithFixedVertexCounts& otherCalculator);

    virtual ~LinearSurplusRefinementCriterionWithFixedVertexCounts();

    /**
     * Different to clearMeasurements of the other criterion, this one is
     * typically to be called in endIteration().
     *
     * The current implementation is pretty simple. It only flags the biggest
     * and the smallest bin. All the others remain invariant. The biggest and
     * the smallest bin are flagged if the total number of vertices allows us
     * to do so and the linear surplus is not growing anymore, i.e. the solution
     * starts to converge smoothly.
     *
     * @param minimumSurplus Is the smallest linear surplus that should not be
     *           deleted. Set to zero to switch this off.
     */
    void clearMeasurements(double currentNumberOfVertices, double minimumSurplus, bool activate);

    static double getNumberOfVerticesCorrespondingToRegularGrid( double hInRegularGrid );
    static int    getTreeHeightOfRegularGrid( double hInRegularGrid );
};


#endif
