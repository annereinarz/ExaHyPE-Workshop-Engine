#include "matrixfree/adaptivitycriteria/LinearSurplusRefinementCriterionWithFixedVertexCounts.h"
#include "tarch/la/ScalarOperations.h"


tarch::logging::Log matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::_log( "matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts" );


int matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::getTreeHeightOfRegularGrid( double hInRegularGrid ) {
  int    height = 0;
  double currentH = 1.0;

  while (currentH>hInRegularGrid) {
    height++;
    currentH/=3.0;
  }

  return height;
}

double matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::getNumberOfVerticesCorrespondingToRegularGrid( double hInRegularGrid ) {
  const int treeHeight = getTreeHeightOfRegularGrid( hInRegularGrid );
  const int fineGridVerticesAlongOneAxis = tarch::la::aPowI(treeHeight,3)+1;

  return tarch::la::aPowI(DIMENSIONS,fineGridVerticesAlongOneAxis);
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::LinearSurplusRefinementCriterionWithFixedVertexCounts(
  double maxNumberOfVertices,
  bool   smoothCoarseGrid,
  int    numberOfBins,
  int    maxNumberOfRefinementsOrCoarsenings
):
  LinearSurplusRefinementCriterion(
    smoothCoarseGrid,
    numberOfBins,
    maxNumberOfRefinementsOrCoarsenings
  ),
  _maxNumberOfVertices(maxNumberOfVertices) {
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::LinearSurplusRefinementCriterionWithFixedVertexCounts(
  const LinearSurplusRefinementCriterionWithFixedVertexCounts& otherCriterion
):
  LinearSurplusRefinementCriterion(otherCriterion),
  _maxNumberOfVertices(otherCriterion._maxNumberOfVertices) {
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::~LinearSurplusRefinementCriterionWithFixedVertexCounts() {
}


void matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedVertexCounts::clearMeasurements(double currentNumberOfVertices, double minimumSurplus, bool activate) {
  logTraceInWith2Arguments( "clearMeasurements(double,double)", currentNumberOfVertices, minimumSurplus );

  const bool delta           = 0.1;
  const double normalisedSurplusGrowth = (_surplusMaximumOnFineGridInNewIteration - _surplusMaximumOnFineGrid) / _surplusMaximumOnFineGrid;
  const bool surplusHasGrown = normalisedSurplusGrowth > delta;

  _surplusMaximumOnFineGrid               = _surplusMaximumOnFineGridInNewIteration;
  _surplusMaximumOnFineGridInNewIteration = 0.0;

  const bool flagSmallestBinToDelete =
    (
      (currentNumberOfVertices-_bins[0]._numberOfEntries > _maxNumberOfVertices) ||
      (_bins[0]._maxH                                    < minimumSurplus)
    ) &&
    _bins[0]._numberOfEntries>0 &&
    !surplusHasGrown &&
    activate;

  const bool flagBiggestBinToRefined =
    (currentNumberOfVertices + _bins[_numberOfBins-1]._numberOfEntries < _maxNumberOfVertices) &&
    _bins[_numberOfBins-1]._numberOfEntries>0 &&
    !surplusHasGrown &&
    activate;

  if (flagBiggestBinToRefined) {
    logDebug(
      "clearMeasurements(double,double)",
      "flag biggest bin to refine; current vertex count=" << currentNumberOfVertices <<
      ", vertices in biggest bin=" << _bins[_numberOfBins-1]._numberOfEntries <<
      ", and biggest h in biggest bin=" << _bins[_numberOfBins-1]._maxH <<
      ", max no of vertices=" << _maxNumberOfVertices
    );
  }
  if (flagSmallestBinToDelete) {
    logDebug(
      "clearMeasurements(double,double)",
      "flag smallest bin to delete; current vertex count=" << currentNumberOfVertices <<
      ", vertices in smallest bin=" << _bins[0]._numberOfEntries <<
      ", and biggest h in smallest bin=" << _bins[0]._maxH <<
      ", minimum surplus=" << minimumSurplus
    );
  }

  for (int i=0; i<_numberOfBins; i++) {
     logDebug( "clearMeasurements()", "bin " << i << ": " <<  toString(_bins[i]._associatedAction) << "; entries=" << _bins[i]._numberOfEntries );
    _bins[i]._numberOfEntries          = 0;
    _bins[i]._numberOfCancelledActions = 0;
    _bins[i]._minH                     = std::numeric_limits<double>::max();
    _bins[i]._maxH                     = 0.0;
    _bins[i]._associatedAction         = NoAction;
  }

  _totalNumberOfSurplusEvaluations = 0;
  _numberOfRefinedOrDeleteCalls    = 0;

  if (flagBiggestBinToRefined) {
    _bins[ _numberOfBins-1 ]._associatedAction = Refine;
  }

  if (flagSmallestBinToDelete) {
    _bins[ 0 ]._associatedAction = Delete;
  }

  logTraceOut( "clearMeasurements(double,double)" );
}
