#include "matrixfree/adaptivitycriteria/SurplusCalculator.h"
#include "matrixfree/stencil/StencilFactory.h"
#include "tarch/la/MatrixMatrixOperations.h"
#include "tarch/la/VectorOperations.h"


tarch::logging::Log matrixfree::adaptivitycriteria::SurplusCalculator::_log( "matrixfree::adaptivitycriteria::SurplusCalculator" );


std::string matrixfree::adaptivitycriteria::SurplusCalculator::toString( const Action& action ) {
  switch (action) {
    case Refine:
      return "Refine";
    case Delete:
      return "Delete";
    case NoAction:
      return "NoAction";
  }

  return "<error>";
}


matrixfree::adaptivitycriteria::SurplusCalculator::SurplusCalculator(const SurplusCalculator& otherCalculator):
  _refinementPercentage(otherCalculator._refinementPercentage),
  _deletePercentage(otherCalculator._deletePercentage),
  _numberOfBins(otherCalculator._numberOfBins),
  _totalNumberOfSurplusEvaluations(0),
  _numberOfRefinedOrDeleteCalls(0),
  _maxNumberOfRefinementsOrCoarsenings(otherCalculator._maxNumberOfRefinementsOrCoarsenings),
  _surplusMaximumOnFineGrid(otherCalculator._surplusMaximumOnFineGrid),
  _surplusMaximumOnFineGridInNewIteration(0.0),
  _minimumMeshSize(otherCalculator._minimumMeshSize),
  _maximumMeshSize(otherCalculator._maximumMeshSize) {
  _bins = new Bin[_numberOfBins];

  for (int d=0; d<DIMENSIONS; d++) {
    _elementMatrix[d] = otherCalculator._elementMatrix[d];
  }

  for (int i=0; i<_numberOfBins; i++) {
    _bins[i]._numberOfCancelledActions = 0;
    _bins[i]._numberOfEntries          = 0;
    _bins[i]._associatedAction         = otherCalculator._bins[i]._associatedAction;
  }
}


void matrixfree::adaptivitycriteria::SurplusCalculator::mergeWithSurplusCalculatorFromOtherThread(const SurplusCalculator& otherCalculator) {
  assertionEquals( _refinementPercentage, otherCalculator._refinementPercentage );
  assertionEquals( _deletePercentage, otherCalculator._deletePercentage );
  assertionEquals( _numberOfBins, otherCalculator._numberOfBins );
  assertionEquals( _maxNumberOfRefinementsOrCoarsenings, otherCalculator._maxNumberOfRefinementsOrCoarsenings );
  assertionEquals( _surplusMaximumOnFineGrid, otherCalculator._surplusMaximumOnFineGrid );
  assertionEquals( _minimumMeshSize, otherCalculator._minimumMeshSize );
  assertionEquals( _maximumMeshSize, otherCalculator._maximumMeshSize );

  for (int i=0; i<_numberOfBins; i++) {
    _bins[i]._numberOfCancelledActions += otherCalculator._bins[i]._numberOfCancelledActions;
    _bins[i]._numberOfEntries          += otherCalculator._bins[i]._numberOfEntries;
  }

  _totalNumberOfSurplusEvaluations += otherCalculator._totalNumberOfSurplusEvaluations;
  _numberOfRefinedOrDeleteCalls    += otherCalculator._numberOfRefinedOrDeleteCalls;

  _surplusMaximumOnFineGridInNewIteration = otherCalculator._surplusMaximumOnFineGridInNewIteration > _surplusMaximumOnFineGridInNewIteration ? otherCalculator._surplusMaximumOnFineGridInNewIteration : _surplusMaximumOnFineGridInNewIteration;
}


matrixfree::adaptivitycriteria::SurplusCalculator::SurplusCalculator (
  double refinementPercentage,
  double deletePercentage,
  double minimumMeshSize,
  double maximumMeshSize,
  int    numberOfBins,
  int    maxNumberOfRefinementsOrCoarsenings
):
  _refinementPercentage(refinementPercentage),
  _deletePercentage(deletePercentage),
  _numberOfBins(numberOfBins),
  _totalNumberOfSurplusEvaluations(0),
  _numberOfRefinedOrDeleteCalls(0),
  _maxNumberOfRefinementsOrCoarsenings(maxNumberOfRefinementsOrCoarsenings),
  _surplusMaximumOnFineGrid(0.0),
  _surplusMaximumOnFineGridInNewIteration(0.0),
  _minimumMeshSize(minimumMeshSize),
  _maximumMeshSize(maximumMeshSize) {

  assertion1( _maxNumberOfRefinementsOrCoarsenings>0, _maxNumberOfRefinementsOrCoarsenings );
  assertion1( _deletePercentage>=0.0, _deletePercentage );
  assertion1( _refinementPercentage>=0.0, _refinementPercentage );
  assertion2( _deletePercentage + _refinementPercentage <= 1.0, _deletePercentage, _refinementPercentage );
  assertion1( _numberOfBins > 2 , _numberOfBins );

  _bins = new Bin[_numberOfBins];

  tarch::la::Vector<3,double> meanValueStencil = matrixfree::stencil::get1DMeanValueStencil();
  tarch::la::Vector<3,double> massStencil      = matrixfree::stencil::get1DMassStencil();

  #ifdef Dim2
  _elementMatrix[0] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        meanValueStencil,
        massStencil
      )
    );
  _elementMatrix[1] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        meanValueStencil
      )
    );
  #elif Dim3
  _elementMatrix[0] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        meanValueStencil,
        massStencil,
        massStencil
      )
    );
  _elementMatrix[1] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        meanValueStencil,
        massStencil
      )
    );
  _elementMatrix[2] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        massStencil,
        meanValueStencil
      )
    );
  #else
  assertionMsg( false, "dimension not supported");
  #endif
}


matrixfree::adaptivitycriteria::SurplusCalculator::~SurplusCalculator() {
  assertion( _bins!=0 );
  delete [] _bins;
  _bins = 0;
}


void matrixfree::adaptivitycriteria::SurplusCalculator::rescaleSurplusMaximum() {
  if (_surplusMaximumOnFineGridInNewIteration>_surplusMaximumOnFineGrid) {
    _surplusMaximumOnFineGrid = _surplusMaximumOnFineGridInNewIteration;
  }
  else if (_numberOfRefinedOrDeleteCalls==0){
    _surplusMaximumOnFineGrid = (_surplusMaximumOnFineGridInNewIteration + _surplusMaximumOnFineGrid) * 0.5;
  }
  _surplusMaximumOnFineGridInNewIteration = 0.0;
}


double matrixfree::adaptivitycriteria::SurplusCalculator::getBinOffset(int bin) const {
  return static_cast<double>(bin) * getBinSize();
}



void matrixfree::adaptivitycriteria::SurplusCalculator::clearMeasurements(double minimumSurplus) {
  logTraceInWith1Argument( "clearMeasurements(double)", minimumSurplus );

  double numberOfEntriesToRefine = _refinementPercentage * _totalNumberOfSurplusEvaluations;

  assertion( numberOfEntriesToRefine>=0 );
  int currentBin = _numberOfBins-1;

  while (currentBin>=0) {
    if (numberOfEntriesToRefine>0) {
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
    if (
      (numberOfEntriesToDelete>0 && _bins[currentBin]._associatedAction == NoAction) ||
      (getBinOffset(currentBin) < minimumSurplus)
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

  logStatistics(minimumSurplus);

  rescaleSurplusMaximum();

  for (int i=0; i<_numberOfBins; i++) {
     logDebug( "clearMeasurements()", "bin " << i << ": " <<  toString(_bins[i]._associatedAction) << "; entries=" << _bins[i]._numberOfEntries );
    _bins[i]._numberOfEntries   = 0;
    _bins[i]._numberOfCancelledActions = 0;
  }

  _totalNumberOfSurplusEvaluations = 0;
  _numberOfRefinedOrDeleteCalls    = 0;

  logTraceOut( "clearMeasurements(double)" );
}


void matrixfree::adaptivitycriteria::SurplusCalculator::logStatistics(double minimumSurplus) const {
  int refineBins = 0;
  int deleteBins = 0;
  for (int i=0; i<_numberOfBins; i++) {
    if (_bins[i]._associatedAction == Refine) {
      refineBins++;
    }
    if (_bins[i]._associatedAction == Delete) {
      deleteBins++;
    }
  }
  logInfo(
    "clearMeasurements()",
    "with a used maximum linear surplus of " << _surplusMaximumOnFineGrid << ", "
    << refineBins << " bin(s) is/are flagged to refine and "
    << deleteBins << " bin(s) is/are flagged to delete. "
    << "#bins=" << _numberOfBins
    << ", bin(0)=" << _bins[0].toString()
    << ", bin(max)=" << _bins[_numberOfBins-1].toString()
    << ", #surplus evaluations=" << _totalNumberOfSurplusEvaluations
    << ", #refine/delete actions=" << _numberOfRefinedOrDeleteCalls
    << ", bin-size="  << getBinSize()
    << ", min-surplus="  << minimumSurplus
  );
}



std::string matrixfree::adaptivitycriteria::SurplusCalculator::Bin::toString() const {
  std::ostringstream msg;
  msg << "(#fits=" << _numberOfEntries
      << ",#cancels=" << _numberOfCancelledActions
      << ",action=" << matrixfree::adaptivitycriteria::SurplusCalculator::toString(_associatedAction)
      << ")";
  return msg.str();
}


tarch::la::Vector<TWO_POWER_D_TIMES_D,double>
matrixfree::adaptivitycriteria::SurplusCalculator::getNewLinearSurplus(
  const tarch::la::Vector<TWO_POWER_D,double>&          u,
  const tarch::la::Vector<TWO_POWER_D_TIMES_D,double>&  linearSurplusSoFar
) const {
  logTraceInWith2Arguments( "getNewLinearSurplus(...)", u, linearSurplusSoFar );

  assertion(tarch::la::max(u)<std::numeric_limits<double>::infinity());
  assertion(tarch::la::max(u)==tarch::la::max(u));

  tarch::la::Vector<TWO_POWER_D_TIMES_D,double> result;
  for (int d=0; d<DIMENSIONS; d++) {
    tarch::la::Vector<TWO_POWER_D,double> update = _elementMatrix[d] * u;

    for (int i=0; i<TWO_POWER_D; i++) {
      result(i*DIMENSIONS+d) = update(d) + linearSurplusSoFar(i*DIMENSIONS+d);
    }
  }

  logTraceOutWith1Argument( "getNewLinearSurplus(...)", result );
  return result;
}


double matrixfree::adaptivitycriteria::SurplusCalculator::getBinSize() const {
  return _surplusMaximumOnFineGrid / static_cast<double>(_numberOfBins);
}


int matrixfree::adaptivitycriteria::SurplusCalculator::getBin(double surplus) const {
  int bin;

  assertion( surplus>=0.0 );

  if ( surplus >= _surplusMaximumOnFineGrid ) {
    bin = _numberOfBins-1;
  }
  else if (tarch::la::equals(surplus,0.0) || tarch::la::equals(_surplusMaximumOnFineGrid,0.0)) {
    bin = 0;
  }
  else {
    assertion( _surplusMaximumOnFineGrid>0.0 );
    bin = static_cast<int>( std::floor( surplus/_surplusMaximumOnFineGrid * _numberOfBins ));
    assertion3(  bin>=0, surplus, _surplusMaximumOnFineGrid, _numberOfBins );
    assertion3(  bin<_numberOfBins, surplus, _surplusMaximumOnFineGrid, _numberOfBins );
  }
  return bin;
}


matrixfree::adaptivitycriteria::SurplusCalculator::Action matrixfree::adaptivitycriteria::SurplusCalculator::analyse(
  const tarch::la::Vector<DIMENSIONS,double>&   linearSurplus,
  bool                                          isRefined,
  bool                                          isUnrefined,
  const tarch::la::Vector<DIMENSIONS,double>&   h
) {
  logTraceInWith1Argument( "analyse(...)", linearSurplus );

  Action result;

  const double maxOfLinearSurplus = tarch::la::max(tarch::la::abs(linearSurplus));
  const double maxOfH             = tarch::la::max(h);

  if ( maxOfH < _minimumMeshSize && isRefined ) {
    result = Delete;
  }
  else if ( maxOfH > _maximumMeshSize && isUnrefined ) {
    result = Refine;
  }
  else {
    if (isUnrefined && maxOfLinearSurplus>_surplusMaximumOnFineGridInNewIteration && maxOfLinearSurplus != std::numeric_limits<double>::infinity()) {
      _surplusMaximumOnFineGridInNewIteration = maxOfLinearSurplus;
    }

    if (_surplusMaximumOnFineGrid==0.0) {
      result = NoAction;
    }
    else {
      const int bin = getBin( maxOfLinearSurplus);

      assertion5( bin>=0, bin, maxOfLinearSurplus, _surplusMaximumOnFineGrid, _numberOfBins, linearSurplus );
      assertion5( bin<=_numberOfBins, bin, maxOfLinearSurplus, _surplusMaximumOnFineGrid, _numberOfBins, linearSurplus );

      _bins[bin]._numberOfEntries++;
      _totalNumberOfSurplusEvaluations++;

      result = _bins[bin]._associatedAction;
      if (result==Refine && _numberOfRefinedOrDeleteCalls>_maxNumberOfRefinementsOrCoarsenings) {
        result = NoAction;
        _bins[bin]._numberOfCancelledActions++;
      }
      else if (result==Refine && !isUnrefined ) {
        result = NoAction;
        _bins[bin]._numberOfCancelledActions++;
      }
      else if (result==Refine && maxOfH/3.0 < _minimumMeshSize) {
        result = NoAction;
        _bins[bin]._numberOfCancelledActions++;
      }
      else if (result==Refine) {
        _numberOfRefinedOrDeleteCalls++;
      }
      else if (result==Delete && !isRefined) {
        result = NoAction;
        _bins[bin]._numberOfCancelledActions++;
      }
      if (result==Delete&& _numberOfRefinedOrDeleteCalls>_maxNumberOfRefinementsOrCoarsenings) {
        result = NoAction;
        _bins[bin]._numberOfCancelledActions++;
      }
      else if (result==Delete && maxOfH > _maximumMeshSize) {
        result = NoAction;
        _bins[bin]._numberOfCancelledActions++;
      }
      else if (result==Delete) {
        _numberOfRefinedOrDeleteCalls++;
      }
    }
  }


  logTraceOutWith1Argument("analyse(...)", toString(result));
  return result;
}


tarch::la::Vector<TWO_POWER_D_TIMES_D,double> matrixfree::adaptivitycriteria::SurplusCalculator::getLinearSurplusContributionFromFineGrid(
  const tarch::la::Vector<DIMENSIONS,double>&  linearSurplusOfFineGridVertex,
  bool                                         fineGridVertexIsUnrefined
) const {
  const double scaling = 1.0 / static_cast<double>(SEVEN_POWER_D);
  return tarch::la::Vector<TWO_POWER_D_TIMES_D,double>(
    fineGridVertexIsUnrefined ? tarch::la::max(linearSurplusOfFineGridVertex)*scaling : std::numeric_limits<double>::infinity()
  );
}


void matrixfree::adaptivitycriteria::SurplusCalculator::setMinMaxMeshWidth( double minimumMeshSize, double maximumMeshSize ) {
  assertion( minimumMeshSize <= maximumMeshSize );
  _minimumMeshSize = minimumMeshSize;
  _maximumMeshSize = maximumMeshSize;
}


double matrixfree::adaptivitycriteria::SurplusCalculator::getMaximumMeshWidth() const {
  return _maximumMeshSize;
}
