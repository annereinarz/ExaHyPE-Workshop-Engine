#include "matrixfree/adaptivitycriteria/LinearSurplusRefinementCriterionWithFixedMeshSizes.h"


tarch::logging::Log matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::_log( "matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes" );



matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::LinearSurplusRefinementCriterionWithFixedMeshSizes(
  double refinementPercentage,
  double deletePercentage,
  double minimumMeshSize,
  double maximumMeshSize,
  bool   smoothCoarseGrid,
  int    numberOfBins,
  int    maxNumberOfRefinementsOrCoarsenings
):
  LinearSurplusRefinementCriterion(
    smoothCoarseGrid,
    numberOfBins,
    maxNumberOfRefinementsOrCoarsenings
  ),
  _minimumMeshSize(minimumMeshSize),
  _maximumMeshSize(maximumMeshSize),
  _refinementPercentage(refinementPercentage),
  _deletePercentage(deletePercentage) {
  assertion1( _deletePercentage>=0.0, _deletePercentage );
  assertion1( _refinementPercentage>=0.0, _refinementPercentage );
  assertion2( _deletePercentage + _refinementPercentage <= 1.0, _deletePercentage, _refinementPercentage );

  if (1.0/numberOfBins>refinementPercentage) {
    logWarning( "LinearSurplusRefinementCriterionWithFixedMeshSizes()", "refinement percentage of " << refinementPercentage << "% cannot be resolved properly with only " << numberOfBins << " bin(s). Increase bin count." );
  }
  if (1.0/numberOfBins>deletePercentage && deletePercentage>0.0) {
    logWarning( "LinearSurplusRefinementCriterionWithFixedMeshSizes()", "delete percentage of " << deletePercentage << "% cannot be resolved properly with only " << numberOfBins << " bin(s). Increase bin count." );
  }
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::LinearSurplusRefinementCriterionWithFixedMeshSizes(
  const LinearSurplusRefinementCriterionWithFixedMeshSizes& otherCriterion
):
  LinearSurplusRefinementCriterion(otherCriterion),
  _minimumMeshSize(otherCriterion._minimumMeshSize),
  _maximumMeshSize(otherCriterion._maximumMeshSize),
  _refinementPercentage(otherCriterion._refinementPercentage),
  _deletePercentage(otherCriterion._deletePercentage) {
  assertionEquals( _minimumMeshSize, otherCriterion._minimumMeshSize );
  assertionEquals( _maximumMeshSize, otherCriterion._maximumMeshSize );
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::~LinearSurplusRefinementCriterionWithFixedMeshSizes() {
}


void matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::setMinMaxMeshWidth( double minimumMeshSize, double maximumMeshSize ) {
  assertion2( minimumMeshSize <= maximumMeshSize, minimumMeshSize, maximumMeshSize );
  _minimumMeshSize = minimumMeshSize;
  _maximumMeshSize = maximumMeshSize;
}


double matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::getMaximumMeshWidth() const {
  return _maximumMeshSize;
}


void matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterionWithFixedMeshSizes::clearMeasurements(double minimumSurplus) {
  logTraceInWith1Argument( "clearMeasurements(double)", minimumSurplus );

  double numberOfEntriesToRefine = _refinementPercentage * _totalNumberOfSurplusEvaluations;

  assertion( numberOfEntriesToRefine>=0 );
  int currentBin = _numberOfBins-1;

  while (currentBin>=0) {
    if ( numberOfEntriesToRefine>0 && _bins[currentBin]._minH/3.0>=_minimumMeshSize && _bins[currentBin]._numberOfEntries>0 ) {
      _bins[currentBin]._associatedAction  = Refine;
      numberOfEntriesToRefine             -= _bins[currentBin]._numberOfEntries;
    }
    else {
      _bins[currentBin]._associatedAction  = NoAction;
    }
    currentBin--;
  }

  double numberOfEntriesToDelete = _deletePercentage * _totalNumberOfSurplusEvaluations;

  currentBin = 0;
  while (currentBin<_numberOfBins) {
    const bool wouldLikeToDelete =
      (numberOfEntriesToDelete>0 && _bins[currentBin]._associatedAction == NoAction)
      ||
      (getBinOffset(currentBin) < minimumSurplus);

    if (
      wouldLikeToDelete &&
      (_bins[currentBin]._maxH*3.0<=_maximumMeshSize)
      && _bins[currentBin]._numberOfEntries>0
    ) {
      _bins[currentBin]._associatedAction  = Delete;
    }
    if (
      (numberOfEntriesToDelete>0 && _bins[currentBin]._associatedAction == Refine)
    ) {
      _bins[currentBin]._associatedAction  = NoAction;
    }
    numberOfEntriesToDelete             -= _bins[currentBin]._numberOfEntries;
    currentBin++;
  }



  const bool maximumSurplusHasGrown       = _surplusMaximumOnFineGridInNewIteration>_surplusMaximumOnFineGrid;

  if (maximumSurplusHasGrown) {
    _surplusMaximumOnFineGrid = _surplusMaximumOnFineGridInNewIteration;
  }
  else {
    const double weightOfNewSurplus = 0.25;
    _surplusMaximumOnFineGrid = (1.0-weightOfNewSurplus) * _surplusMaximumOnFineGrid + weightOfNewSurplus * _surplusMaximumOnFineGridInNewIteration;
  }

  _surplusMaximumOnFineGridInNewIteration = 0.0;



  for (int i=0; i<_numberOfBins; i++) {
     logDebug( "clearMeasurements()", "bin " << i << ": " <<  toString(_bins[i]._associatedAction) << "; entries=" << _bins[i]._numberOfEntries );
    _bins[i]._numberOfEntries          = 0;
    _bins[i]._numberOfCancelledActions = 0;
    _bins[i]._minH                     = std::numeric_limits<double>::max();
    _bins[i]._maxH                     = 0.0;
  }

  _totalNumberOfSurplusEvaluations = 0;
  _numberOfRefinedOrDeleteCalls    = 0;

  logTraceOut( "clearMeasurements(double)" );
}
