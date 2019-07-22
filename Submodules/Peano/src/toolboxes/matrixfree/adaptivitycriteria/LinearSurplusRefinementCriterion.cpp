#include "matrixfree/adaptivitycriteria/LinearSurplusRefinementCriterion.h"
#include "matrixfree/stencil/StencilFactory.h"
#include "tarch/la/MatrixMatrixOperations.h"
#include "tarch/la/VectorOperations.h"
#include "peano/utils/Loop.h"


tarch::logging::Log matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::_log( "matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion" );


std::string matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::toString( const Action& action ) {
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


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::LinearSurplusRefinementCriterion(const LinearSurplusRefinementCriterion& otherCalculator):
  _numberOfBins(otherCalculator._numberOfBins),
  _totalNumberOfSurplusEvaluations(0),
  _numberOfRefinedOrDeleteCalls(0),
  _maxNumberOfRefinementsOrCoarsenings(otherCalculator._maxNumberOfRefinementsOrCoarsenings),
  _surplusMaximumOnFineGrid(otherCalculator._surplusMaximumOnFineGrid),
  _surplusMaximumOnFineGridInNewIteration(0.0),
  _smoothCoarseGrid(otherCalculator._smoothCoarseGrid),
  _isSwitchedOn(true) {
  _bins = new Bin[_numberOfBins];

  for (int d=0; d<DIMENSIONS; d++) {
    _elementMatrix[d] = otherCalculator._elementMatrix[d];
  }

  for (int i=0; i<_numberOfBins; i++) {
    _bins[i]._numberOfCancelledActions = 0;
    _bins[i]._numberOfEntries          = 0;
    _bins[i]._associatedAction         = otherCalculator._bins[i]._associatedAction;
    _bins[i]._minH                     = std::numeric_limits<double>::max();
    _bins[i]._maxH                     = 0.0;
  }
}


void matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::switchOn(bool value) {
  _isSwitchedOn = value;
}


void matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::mergeWithLinearSurplusRefinementCriterionFromOtherThread(const LinearSurplusRefinementCriterion& otherCalculator) {
  assertionEquals( _numberOfBins, otherCalculator._numberOfBins );
  assertionEquals( _maxNumberOfRefinementsOrCoarsenings, otherCalculator._maxNumberOfRefinementsOrCoarsenings );
  assertionEquals( _surplusMaximumOnFineGrid, otherCalculator._surplusMaximumOnFineGrid );

  for (int i=0; i<_numberOfBins; i++) {
    _bins[i]._numberOfCancelledActions += otherCalculator._bins[i]._numberOfCancelledActions;
    _bins[i]._numberOfEntries          += otherCalculator._bins[i]._numberOfEntries;
    _bins[i]._minH                     =  std::min(otherCalculator._bins[i]._minH,_bins[i]._minH);
    _bins[i]._maxH                     =  std::max(otherCalculator._bins[i]._maxH,_bins[i]._maxH);
  }

  _totalNumberOfSurplusEvaluations += otherCalculator._totalNumberOfSurplusEvaluations;
  _numberOfRefinedOrDeleteCalls    += otherCalculator._numberOfRefinedOrDeleteCalls;

  _surplusMaximumOnFineGridInNewIteration = otherCalculator._surplusMaximumOnFineGridInNewIteration > _surplusMaximumOnFineGridInNewIteration ? otherCalculator._surplusMaximumOnFineGridInNewIteration : _surplusMaximumOnFineGridInNewIteration;
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::LinearSurplusRefinementCriterion (
  bool   smoothCoarseGrid,
  int    numberOfBins,
  int    maxNumberOfRefinementsOrCoarsenings
):
  _numberOfBins(numberOfBins),
  _totalNumberOfSurplusEvaluations(0),
  _numberOfRefinedOrDeleteCalls(0),
  _maxNumberOfRefinementsOrCoarsenings(maxNumberOfRefinementsOrCoarsenings),
  _surplusMaximumOnFineGrid(0.0),
  _surplusMaximumOnFineGridInNewIteration(0.0),
  _smoothCoarseGrid(smoothCoarseGrid) {

  assertion1( _maxNumberOfRefinementsOrCoarsenings>0, _maxNumberOfRefinementsOrCoarsenings );
  assertion1( _numberOfBins > 2 , _numberOfBins );

  _bins = new Bin[_numberOfBins];

  tarch::la::Vector<3,double> meanValueStencil = matrixfree::stencil::get1DMeanValueStencil() - matrixfree::stencil::get1DIdentity();
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
  #elif Dim4
  _elementMatrix[0] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        meanValueStencil,
        massStencil,
        massStencil,
        massStencil
      )
    );
  _elementMatrix[1] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        meanValueStencil,
        massStencil,
        massStencil
      )
    );
  _elementMatrix[2] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        massStencil,
        meanValueStencil,
        massStencil
      )
    );
  _elementMatrix[3] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        massStencil,
        massStencil,
        meanValueStencil
      )
    );
  #elif Dim5
  _elementMatrix[0] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        meanValueStencil,
        massStencil,
        massStencil,
        massStencil,
        massStencil
      )
    );
  _elementMatrix[1] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        meanValueStencil,
        massStencil,
        massStencil,
        massStencil
      )
    );
  _elementMatrix[2] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        massStencil,
        meanValueStencil,
        massStencil,
        massStencil
      )
    );
  _elementMatrix[3] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        massStencil,
        massStencil,
        meanValueStencil,
        massStencil
      )
    );
  _elementMatrix[4] =
    matrixfree::stencil::getElementWiseAssemblyMatrix(
      matrixfree::stencil::stencilProduct(
        massStencil,
        massStencil,
        massStencil,
        massStencil,
        meanValueStencil
      )
    );
  #else
  assertionMsg( false, "dimension not supported");
  #endif
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::~LinearSurplusRefinementCriterion() {
  assertion( _bins!=0 );
  delete [] _bins;
  _bins = 0;
}


double matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getBinOffset(int bin) const {
  return static_cast<double>(bin) * getBinSize();
}


std::string matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Bin::toString() const {
  std::ostringstream msg;
  msg << "(#fits=" << _numberOfEntries
      << ",#cancels=" << _numberOfCancelledActions
      << ",action=" << matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::toString(_associatedAction)
      << ",hmin=" << _minH
      << ",hmax=" << _maxH
      << ")";
  return msg.str();
}


tarch::la::Vector<TWO_POWER_D_TIMES_D,double>
matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getNewLinearSurplus(
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


tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> >
matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getNewLinearSurplus(
  const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&          u,
  const tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> >&  linearSurplusSoFar
) const {
  logTraceInWith2Arguments( "getNewLinearSurplus(...)", u, linearSurplusSoFar );

  tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> > result = linearSurplusSoFar;
  for (int d=0; d<DIMENSIONS; d++) {
    tarch::la::Vector<TWO_POWER_D,std::complex<double> > update = _elementMatrix[d].convertScalar<std::complex<double> >() * u;

    for (int i=0; i<TWO_POWER_D; i++) {
      result(i*DIMENSIONS+d) += update(d);
    }
  }

  logTraceOutWith1Argument( "getNewLinearSurplus(...)", result );
  return result;
}


tarch::la::Vector<TWO_POWER_D,double>
matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getNewLinearSurplus(
  const tarch::la::Vector<TWO_POWER_D,double>&  u,
  const tarch::la::Vector<TWO_POWER_D,double>&  linearSurplusSoFar
) const {
  logTraceInWith2Arguments( "getNewLinearSurplus(...)", u, linearSurplusSoFar );

  assertion(tarch::la::max(u)<std::numeric_limits<double>::infinity());
  assertion(tarch::la::max(u)==tarch::la::max(u));

  tarch::la::Vector<TWO_POWER_D,double> result = linearSurplusSoFar;
  for (int d=0; d<DIMENSIONS; d++) {
    const tarch::la::Vector<TWO_POWER_D,double> update = _elementMatrix[d] * u;
    result += update;
  }

  logTraceOutWith1Argument( "getNewLinearSurplus(...)", result );
  return result;
}


tarch::la::Vector<TWO_POWER_D,std::complex<double> >
matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getNewLinearSurplus(
  const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&  u,
  const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&  linearSurplusSoFar
) const {
  logTraceInWith2Arguments( "getNewLinearSurplus(...)", u, linearSurplusSoFar );

  tarch::la::Vector<TWO_POWER_D,std::complex<double> > result = linearSurplusSoFar;
  for (int d=0; d<DIMENSIONS; d++) {
    const tarch::la::Vector<TWO_POWER_D,std::complex<double> > update = _elementMatrix[d].convertScalar<std::complex<double> >() * u;
    result += update;
  }

  logTraceOutWith1Argument( "getNewLinearSurplus(...)", result );
  return result;
}


tarch::la::Vector<TWO_POWER_D,double>
matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getLinearSurplusDelta(
  const tarch::la::Vector<TWO_POWER_D,double>&  u
) const {
  logTraceInWith1Argument( "getNewLinearSurplus(...)", u);

  assertion(tarch::la::max(u)<std::numeric_limits<double>::infinity());
  assertion(tarch::la::max(u)==tarch::la::max(u));

  tarch::la::Vector<TWO_POWER_D,double> result(0.0);
  for (int d=0; d<DIMENSIONS; d++) {
    const tarch::la::Vector<TWO_POWER_D,double> update = _elementMatrix[d] * u;
    result += update;
  }

  logTraceOutWith1Argument( "getNewLinearSurplus(...)", result );
  return result;
}


tarch::la::Vector<TWO_POWER_D,std::complex<double> >
matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getLinearSurplusDelta(
  const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&  u
) const {
  logTraceInWith1Argument( "getNewLinearSurplus(...)", u);

  tarch::la::Vector<TWO_POWER_D,std::complex<double> > result(0.0);
  for (int d=0; d<DIMENSIONS; d++) {
    const tarch::la::Vector<TWO_POWER_D,std::complex<double> > update = _elementMatrix[d].convertScalar<std::complex<double> >() * u;
    result += update;
  }

  logTraceOutWith1Argument( "getNewLinearSurplus(...)", result );
  return result;
}


double matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getBinSize() const {
  return _surplusMaximumOnFineGrid / static_cast<double>(_numberOfBins);
}


int matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getBin(double surplus) const {
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


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Action matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::analyse(
  double maxOfLinearSurplus,
  double maxOfH,
  bool   isRefined,
  bool   isUnrefined
) {
  if (!_isSwitchedOn) {
    return NoAction;
  }

  Action result;

  assertion4( maxOfLinearSurplus>=0.0, maxOfLinearSurplus, maxOfH, isRefined, isUnrefined );

  if ( _smoothCoarseGrid && maxOfLinearSurplus == std::numeric_limits<double>::infinity() && isUnrefined ) {
    result = Refine;
    _numberOfRefinedOrDeleteCalls++;
  }
  else if ( maxOfLinearSurplus == std::numeric_limits<double>::infinity() ) {
    result = NoAction;
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

      assertion4( bin>=0, bin, maxOfLinearSurplus, _surplusMaximumOnFineGrid, _numberOfBins );
      assertion4( bin<=_numberOfBins, bin, maxOfLinearSurplus, _surplusMaximumOnFineGrid, _numberOfBins );

      _bins[bin]._numberOfEntries++;
      _bins[bin]._minH = std::min(_bins[bin]._minH,maxOfH);
      _bins[bin]._maxH = std::max(_bins[bin]._maxH,maxOfH);
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
      else if (result==Delete) {
        _numberOfRefinedOrDeleteCalls++;
      }
    }
  }

  return result;
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Action matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::analyse(
  const tarch::la::Vector<DIMENSIONS,double>&   linearSurplus,
  bool                                          isRefined,
  bool                                          isUnrefined,
  const tarch::la::Vector<DIMENSIONS,double>&   h
) {
  if (!_isSwitchedOn) {
    return NoAction;
  }

  const double maxOfLinearSurplus = tarch::la::max(tarch::la::abs(linearSurplus));
  const double maxOfH             = tarch::la::max(h);

  return analyse(maxOfLinearSurplus,maxOfH,isRefined,isUnrefined);
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Action matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::analyse(
  double                                        linearSurplus,
  bool                                          isRefined,
  bool                                          isUnrefined,
  const tarch::la::Vector<DIMENSIONS,double>&   h
) {
  if (!_isSwitchedOn) {
    return NoAction;
  }

  // check for nan
  assertion(linearSurplus==linearSurplus);

  const double maxOfLinearSurplus = tarch::la::abs(linearSurplus);
  const double maxOfH             = tarch::la::max(h);

  return analyse(maxOfLinearSurplus,maxOfH,isRefined,isUnrefined);
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Action matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::analyse(
  const tarch::la::Vector<DIMENSIONS,std::complex<double> >&   linearSurplus,
  bool                                                         isRefined,
  bool                                                         isUnrefined,
  const tarch::la::Vector<DIMENSIONS,double>&                  h
) {
  if (!_isSwitchedOn) {
    return NoAction;
  }

  const double maxOfLinearSurplus = std::max(
    tarch::la::max(tarch::la::abs(tarch::la::real(linearSurplus))),
    tarch::la::max(tarch::la::abs(tarch::la::imag(linearSurplus)))
  );
  const double maxOfH             = tarch::la::max(h);

  return analyse(maxOfLinearSurplus,maxOfH,isRefined,isUnrefined);
}


matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::Action matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::analyse(
  const std::complex<double>&                   linearSurplus,
  bool                                          isRefined,
  bool                                          isUnrefined,
  const tarch::la::Vector<DIMENSIONS,double>&   h
) {
  if (!_isSwitchedOn) {
    return NoAction;
  }

  const double maxOfLinearSurplus = tarch::la::abs(linearSurplus);
  const double maxOfH             = tarch::la::max(h);

  return analyse(maxOfLinearSurplus,maxOfH,isRefined,isUnrefined);
}


bool matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::restrictToCoarseGridPoint(
  const tarch::la::Vector<DIMENSIONS,int>&  coarseGridPositionOfVertex,
  const tarch::la::Vector<DIMENSIONS,int>&  fineGridPositionOfVertex
) {
  bool result = true;
  for (int d=0; d<DIMENSIONS; d++) {
    result &= (
        (coarseGridPositionOfVertex(d)==0 && fineGridPositionOfVertex(d)==0)
        ||
        (coarseGridPositionOfVertex(d)==1 && fineGridPositionOfVertex(d)==3)
        ||
        (fineGridPositionOfVertex(d)==1)
        ||
        (fineGridPositionOfVertex(d)==2)
      );
  }
  return result;
}


tarch::la::Vector<TWO_POWER_D_TIMES_D,double> matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getLinearSurplusContributionFromFineGrid(
  const tarch::la::Vector<DIMENSIONS,double>&  linearSurplusOfFineGridVertex,
  bool                                         fineGridVertexIsUnrefined,
  const tarch::la::Vector<DIMENSIONS,int>&     fineGridPositionOfVertex
) const {
  if ( fineGridVertexIsUnrefined ) {
    const double scaling         = 1.0 / static_cast<double>(FIVE_POWER_D);
    tarch::la::Vector<TWO_POWER_D_TIMES_D,double> result;
    int entry = 0;
    dfor2(i)
      const double restrictedValue = restrictToCoarseGridPoint(i,fineGridPositionOfVertex) ? tarch::la::max(linearSurplusOfFineGridVertex)*scaling : 0.0;
      for (int d=0; d<DIMENSIONS; d++) {
        result(entry) = restrictedValue;
        entry++;
      }
    enddforx
    return result;
  } else {
    return tarch::la::Vector<TWO_POWER_D_TIMES_D,double>(std::numeric_limits<double>::infinity());
  }
}


tarch::la::Vector<TWO_POWER_D,double> matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getLinearSurplusContributionFromFineGrid(
  double                                    linearSurplusOfFineGridVertex,
  bool                                      fineGridVertexIsUnrefined,
  const tarch::la::Vector<DIMENSIONS,int>&  fineGridPositionOfVertex
) const {
  if ( fineGridVertexIsUnrefined ) {
    const double scaling = 1.0 / static_cast<double>(FIVE_POWER_D);
    tarch::la::Vector<TWO_POWER_D,double> result;
    dfor2(i)
      result(iScalar) = restrictToCoarseGridPoint(i,fineGridPositionOfVertex) ? linearSurplusOfFineGridVertex * scaling : 0.0;
    enddforx
    return result;
  } else {
    return tarch::la::Vector<TWO_POWER_D,double>(std::numeric_limits<double>::infinity());
  }
}


tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> > matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getLinearSurplusContributionFromFineGrid(
  const tarch::la::Vector<DIMENSIONS,std::complex<double> >&  linearSurplusOfFineGridVertex,
  bool                                                        fineGridVertexIsUnrefined,
  const tarch::la::Vector<DIMENSIONS,int>&                    fineGridPositionOfVertex
) const {
  if ( fineGridVertexIsUnrefined ) {
    const double scaling = 1.0 / static_cast<double>(FIVE_POWER_D);
    const double maxReal = tarch::la::maxReal( linearSurplusOfFineGridVertex ) * scaling;
    const double maxImag = tarch::la::maxImag( linearSurplusOfFineGridVertex ) * scaling;
    tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> > result;
    int entry = 0;
    dfor2(i)
      const std::complex<double> restrictedValue = restrictToCoarseGridPoint(i,fineGridPositionOfVertex) ? std::complex<double>(maxReal,maxImag) : 0.0;
      for (int d=0; d<DIMENSIONS; d++) {
        result(entry) = restrictedValue;
        entry++;
      }
    enddforx
    return result;
  } else {
    return tarch::la::Vector<TWO_POWER_D_TIMES_D,std::complex<double> >(std::numeric_limits<double>::infinity());
  }
}


tarch::la::Vector<TWO_POWER_D,std::complex<double> > matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getLinearSurplusContributionFromFineGrid(
  const std::complex<double>&               linearSurplusOfFineGridVertex,
  bool                                      fineGridVertexIsUnrefined,
  const tarch::la::Vector<DIMENSIONS,int>&  fineGridPositionOfVertex
) const {
  if ( fineGridVertexIsUnrefined ) {
    const double scaling = 1.0 / static_cast<double>(FIVE_POWER_D);
    tarch::la::Vector<TWO_POWER_D,std::complex<double> > result;
    dfor2(i)
      result(iScalar) = restrictToCoarseGridPoint(i,fineGridPositionOfVertex) ? linearSurplusOfFineGridVertex*scaling : 0.0;
    enddforx
    return result;
  } else {
    return tarch::la::Vector<TWO_POWER_D,std::complex<double> >(std::numeric_limits<double>::infinity());
  }
}


std::string matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::toString() const {
  std::ostringstream msg;

  msg << "(" << getStatistics()
      << ",number-of-bins=" << _numberOfBins
      << ",smooth-coarse-grid=" << _smoothCoarseGrid
      << ",max-number-refinements-or-coarsenings=" << _maxNumberOfRefinementsOrCoarsenings;

  for (int i=0; i<_numberOfBins; i++) {
    msg << ",bin#" << i << "=" << _bins[i].toString();
  }

  msg << ")";

  return msg.str();
}


std::string matrixfree::adaptivitycriteria::LinearSurplusRefinementCriterion::getStatistics() const {
  std::ostringstream msg;

  msg << "(no-of-surplus-evaluations=" << _totalNumberOfSurplusEvaluations
      << ",no-of-refined-or-deleted-cells=" << _numberOfRefinedOrDeleteCalls
      << ",surplus-max=" << _surplusMaximumOnFineGrid
      << ",surplus-max(new)=" << _surplusMaximumOnFineGridInNewIteration
      << ")";

  return msg.str();
}
