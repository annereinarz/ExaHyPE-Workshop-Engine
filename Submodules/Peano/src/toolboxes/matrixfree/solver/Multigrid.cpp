#include "matrixfree/solver/JacobiSmoother.h"
#include "matrixfree/solver/Multigrid.h"
#include "matrixfree/stencil/StencilFactory.h"

#include "peano/utils/Loop.h"


tarch::logging::Log matrixfree::solver::Multigrid::_log( "matrixfree::solver::Multigrid" );


namespace {
  tarch::la::Vector<FIVE_POWER_D,double>  _dLinearInterpolation( matrixfree::stencil::getDLinearInterpolation() );
}


tarch::la::Vector<TWO_POWER_D,double> matrixfree::solver::Multigrid::injectFineGridValues(
  const tarch::la::Vector<FOUR_POWER_D,double>&  fineGridValues
) {
  tarch::la::Vector<TWO_POWER_D,double> result;
  dfor2(k)
    const tarch::la::Vector<DIMENSIONS,int> fineGridIndex   = k*3;
    result(kScalar) = fineGridValues( peano::utils::dLinearised(fineGridIndex,4) );
  enddforx

  return result;
}


tarch::la::Vector<TWO_POWER_D,std::complex<double> > matrixfree::solver::Multigrid::injectFineGridValues(
  const tarch::la::Vector<FOUR_POWER_D,std::complex<double> >&  fineGridValues
) {
  tarch::la::Vector<TWO_POWER_D,std::complex<double> > result;
  dfor2(k)
    const tarch::la::Vector<DIMENSIONS,int> fineGridIndex   = k*3;
    result(kScalar) = fineGridValues( peano::utils::dLinearised(fineGridIndex,4) );
  enddforx

  return result;
}


tarch::la::Vector<TWO_POWER_D, double> matrixfree::solver::Multigrid::calculateP(
  const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&  coarsePStencils,
  const tarch::la::Vector<DIMENSIONS,int>&                          fineGridVertexPosition
) {
  tarch::la::Vector<TWO_POWER_D, double> result;

  dfor2(i)
    tarch::la::Vector<DIMENSIONS,int>  entryOfCoarseGridStencil;
    bool                               coarseGridStencilInfluencesFineGridVertex;
    int                                indexOfCoarseGridStencil;

    matrixfree::solver::Multigrid::getPositionIn5PowDStencilRelativeToKthCoarseVertex(
      i,
      fineGridVertexPosition,
      entryOfCoarseGridStencil,
      indexOfCoarseGridStencil,
      coarseGridStencilInfluencesFineGridVertex
    );

    if(coarseGridStencilInfluencesFineGridVertex){
      result(iScalar) = coarsePStencils(
        iScalar * FIVE_POWER_D + indexOfCoarseGridStencil
      );
    }
    else{
      result(iScalar) = 0.0;
    }
  enddforx

  return result;
}


tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double>  matrixfree::solver::Multigrid::calculateP(
  const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&  coarsePStencils
) {
  tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double> result;

  dfor4(j)
    const tarch::la::Vector<TWO_POWER_D, double> row = calculateP(coarsePStencils,j);
    for (int i=0; i<TWO_POWER_D; i++) {
      result(jScalar, i) = row(i);
    }
  enddforx

  return result;
}


tarch::la::Vector<TWO_POWER_D, double> matrixfree::solver::Multigrid::calculateP(
  const tarch::la::Vector<FIVE_POWER_D, double>&                    coarsePStencil,
  const tarch::la::Vector<DIMENSIONS,int>&                          fineGridVertexPosition
) {
  tarch::la::Vector<TWO_POWER_D, double> result;

  dfor2(i)
    tarch::la::Vector<DIMENSIONS,int>  entryOfCoarseGridStencil;
    bool                               coarseGridStencilInfluencesFineGridVertex;
    int                                indexOfCoarseGridStencil;

    matrixfree::solver::Multigrid::getPositionIn5PowDStencilRelativeToKthCoarseVertex(
      i,
      fineGridVertexPosition,
      entryOfCoarseGridStencil,
      indexOfCoarseGridStencil,
      coarseGridStencilInfluencesFineGridVertex
    );

    if(coarseGridStencilInfluencesFineGridVertex){
      result(iScalar) = coarsePStencil( indexOfCoarseGridStencil );
    }
    else{
      result(iScalar) = 0.0;
    }
  enddforx

  return result;
}


tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double>  matrixfree::solver::Multigrid::calculateP(
  const tarch::la::Vector<FIVE_POWER_D, double>&  coarsePStencil
) {
  tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double> result;

  dfor4(j)
    const tarch::la::Vector<TWO_POWER_D, double> row = calculateP(coarsePStencil,j);
    for (int i=0; i<TWO_POWER_D; i++) {
      result(jScalar, i) = row(i);
    }
  enddforx

  return result;
}


tarch::la::Vector<TWO_POWER_D, double> matrixfree::solver::Multigrid::calculateP(
  const tarch::la::Vector<DIMENSIONS,int>&                          fineGridVertexPosition
) {
  return calculateP(_dLinearInterpolation,fineGridVertexPosition);
}


tarch::la::Matrix<FOUR_POWER_D, TWO_POWER_D, double>  matrixfree::solver::Multigrid::calculateP() {
  return calculateP(_dLinearInterpolation);
}


tarch::la::Vector<THREE_POWER_D,double> matrixfree::solver::Multigrid::getGalerkinMultigridOperatorForDLinearInterpolationAndRestriction( int fineGridSubdivisionFactor, const tarch::la::Vector<THREE_POWER_D,double>& fineGridStencil ) {
  assertion(fineGridSubdivisionFactor>=2);

  tarch::la::Vector<THREE_POWER_D,double> result(0.0);

  const int numberOfVertices1d = 4*fineGridSubdivisionFactor+1;
  const int numberOfVertices   = tarch::la::aPowI(DIMENSIONS,numberOfVertices1d);
  double* fineGridP  = new double[numberOfVertices];
  double* fineGridAP = new double[numberOfVertices];

  dfor3(coarseGridVertex)
    // initialise fine grid with zero
    int counter = 0;
    dfor(i,numberOfVertices1d) {
      fineGridP[counter]  = 0.0;
      fineGridAP[counter] = 0.0;
      counter++;
    }

    // prolong
    dfor(fineGridVertex,numberOfVertices1d) {
      const int linearisedFineGridVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex,numberOfVertices1d);
      double weight = 1.0;
      for (int d=0; d<DIMENSIONS; d++) {
        const double weight1d = static_cast<double>((fineGridSubdivisionFactor - std::abs( (coarseGridVertex(d)+1)*fineGridSubdivisionFactor - fineGridVertex(d) ) )) / static_cast<double>(fineGridSubdivisionFactor);
        assertion(weight1d<=1.0);
        weight *= tarch::la::greaterEquals(weight1d,0.0) ? weight1d : 0.0;
      }
      fineGridP[linearisedFineGridVertex] = weight;
    }

    // apply stencil
    dfor(fineGridVertex,2*fineGridSubdivisionFactor-1) {
      const int linearisedDestVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex+fineGridSubdivisionFactor+1,numberOfVertices1d);
      dfor3(stencilEntry)
        const int linearisedSrcVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex+fineGridSubdivisionFactor+stencilEntry,numberOfVertices1d);
        fineGridAP[linearisedDestVertex] += fineGridP[linearisedSrcVertex] * fineGridStencil(stencilEntryScalar);
      enddforx
    }

    // restrict
    dfor(fineGridVertex,numberOfVertices1d) {
      const int linearisedFineGridVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex,numberOfVertices1d);
      double weight = 1.0;
      for (int d=0; d<DIMENSIONS; d++) {
        // always restrict towards centre
        const double weight1d = static_cast<double>((fineGridSubdivisionFactor - std::abs( 2*fineGridSubdivisionFactor - fineGridVertex(d) ) )) / static_cast<double>(fineGridSubdivisionFactor);
        assertion(weight1d<=1.0);
        weight *= tarch::la::greaterEquals(weight1d,0.0) ? weight1d : 0.0;
      }
      result(coarseGridVertexScalar) += fineGridAP[linearisedFineGridVertex] * weight;
    }
    logDebug( "getGalerkinMultigridOperatorForDLinearInterpolationAndRestriction(int,stencil)", "result stencil entry " << coarseGridVertex << ": " << result(coarseGridVertexScalar) );

  enddforx

  delete[] fineGridP;
  delete[] fineGridAP;
  return result;
}


tarch::la::Vector<THREE_POWER_D,double> matrixfree::solver::Multigrid::getGalerkinMultigridOperatorForDLinearInterpolationAndInjection( int fineGridSubdivisionFactor, const tarch::la::Vector<THREE_POWER_D,double>& fineGridStencil ) {
  assertion(fineGridSubdivisionFactor>=2);

  tarch::la::Vector<THREE_POWER_D,double> result(0.0);

  const int numberOfVertices1d = 4*fineGridSubdivisionFactor+1;
  const int numberOfVertices   = tarch::la::aPowI(DIMENSIONS,numberOfVertices1d);
  double* fineGridP  = new double[numberOfVertices];
  double* fineGridAP = new double[numberOfVertices];

  dfor3(coarseGridVertex)
    // initialise fine grid with zero
    int counter = 0;
    dfor(i,numberOfVertices1d) {
      fineGridP[counter]  = 0.0;
      fineGridAP[counter] = 0.0;
      counter++;
    }

    // prolong
    dfor(fineGridVertex,numberOfVertices1d) {
      const int linearisedFineGridVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex,numberOfVertices1d);
      double weight = 1.0;
      for (int d=0; d<DIMENSIONS; d++) {
        const double weight1d = static_cast<double>((fineGridSubdivisionFactor - std::abs( (coarseGridVertex(d)+1)*fineGridSubdivisionFactor - fineGridVertex(d) ) )) / static_cast<double>(fineGridSubdivisionFactor);
        assertion(weight1d<=1.0);
        weight *= tarch::la::greaterEquals(weight1d,0.0) ? weight1d : 0.0;
      }
      fineGridP[linearisedFineGridVertex] = weight;
    }

    // apply stencil (too generic, should be only central element)
    dfor(fineGridVertex,2*fineGridSubdivisionFactor-1) {
      const int linearisedDestVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex+fineGridSubdivisionFactor+1,numberOfVertices1d);
      dfor3(stencilEntry)
        const int linearisedSrcVertex = peano::utils::dLinearisedWithoutLookup(fineGridVertex+fineGridSubdivisionFactor+stencilEntry,numberOfVertices1d);
        fineGridAP[linearisedDestVertex] += fineGridP[linearisedSrcVertex] * fineGridStencil(stencilEntryScalar);
      enddforx
    }

    // restrict
    const int linearisedFineGridVertexCoincidingWithCoarseGridPoint = peano::utils::dLinearisedWithoutLookup(2*fineGridSubdivisionFactor,numberOfVertices1d);
    result(coarseGridVertexScalar) = fineGridAP[linearisedFineGridVertexCoincidingWithCoarseGridPoint];
    logDebug( "getGalerkinMultigridOperatorForDLinearInterpolationAndRestriction(int,stencil)", "result stencil entry " << coarseGridVertex << ": " << result(coarseGridVertexScalar) );

  enddforx

  delete[] fineGridP;
  delete[] fineGridAP;
  return result;
}


tarch::la::Vector<FOUR_POWER_D,double > matrixfree::solver::Multigrid::dLinearInterpolation(
  const tarch::la::Vector<TWO_POWER_D,double >&  coarseGridValues
) {
  tarch::la::Vector<FOUR_POWER_D,double > result;

  dfor4(k)
    result(kScalar) = getDLinearInterpolatedValue(coarseGridValues,k);
  enddforx

  return result;
}


tarch::la::Vector<FOUR_POWER_D,std::complex<double> > matrixfree::solver::Multigrid::dLinearInterpolation(
  const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&  coarseGridValues
) {
  tarch::la::Vector<FOUR_POWER_D,std::complex<double> > result;

  dfor4(k)
    result(kScalar) = getDLinearInterpolatedValue(coarseGridValues,k);
  enddforx

  return result;
}


double matrixfree::solver::Multigrid::getDLinearInterpolatedValue(
  const tarch::la::Vector<TWO_POWER_D,double>&            coarseGridValues,
  const tarch::la::Vector<DIMENSIONS,int>&                fineGridPositionOfVertex
) {
  logTraceInWith2Arguments( "interpolateWithDLinearShapeFunctions(...)", coarseGridValues, fineGridPositionOfVertex );

  double result = 0.0;
  double tmp;

  dfor2(k)
    tmp=
      coarseGridValues(kScalar) *
      computeContributionWeightOfInterGridTransfer(
        k,
        _dLinearInterpolation,
        fineGridPositionOfVertex
      );
    result+=tmp;
  enddforx

  logTraceOutWith1Argument( "interpolateWithDLinearShapeFunctions(...)", result );

  return result;
}


tarch::la::Vector<TWO_POWER_D,double >  matrixfree::solver::Multigrid::getDLinearInterpolationWeights(
  const tarch::la::Vector<DIMENSIONS,int>&                     fineGridPositionOfVertex
) {
  tarch::la::Vector<TWO_POWER_D,double > result;
  dfor2(k)
    const double weight =
        computeContributionWeightOfInterGridTransfer(
          k,
          _dLinearInterpolation,
          fineGridPositionOfVertex
        );
    result(kScalar) = weight;
  enddforx
  return result;
}


std::complex<double> matrixfree::solver::Multigrid::getDLinearInterpolatedValue(
  const tarch::la::Vector<TWO_POWER_D,std::complex<double> >&   coarseGridValues,
  const tarch::la::Vector<DIMENSIONS,int>&                      fineGridPositionOfVertex
) {
  logTraceInWith2Arguments( "interpolateWithDLinearShapeFunctions(...)", coarseGridValues, fineGridPositionOfVertex );

  std::complex<double> result = 0.0;

  const tarch::la::Vector<TWO_POWER_D,double > coarseGridValuesReal = tarch::la::real(coarseGridValues);
  const tarch::la::Vector<TWO_POWER_D,double > coarseGridValuesImag = tarch::la::imag(coarseGridValues);

  dfor2(k)
    const double weight =
        computeContributionWeightOfInterGridTransfer(
          k,
          _dLinearInterpolation,
          fineGridPositionOfVertex
        );

    /**
     * This version does not work with gcc 4.7 as this compiler version's std
     * lib does not offer these setters. Unfortunately, SuperMIC currently
     * runs exactly this gcc version (2015-04-08).
     */
    //result.real( coarseGridValuesReal(kScalar) * weight + result.real() );
    //result.imag( coarseGridValuesImag(kScalar) * weight + result.imag() );

    result += std::complex<double>( coarseGridValuesReal(kScalar) * weight, coarseGridValuesImag(kScalar) * weight );
  enddforx

  logTraceOutWith1Argument( "interpolateWithDLinearShapeFunctions(...)", result );

  return result;
}


double matrixfree::solver::Multigrid::computeContributionWeightOfInterGridTransfer(
  const tarch::la::Vector<DIMENSIONS,int>&       currentCoarseGridVertex,
  const tarch::la::Vector<FIVE_POWER_D,double>&  currentCoarseGridVertexsInterGridTransferOperator,
  const tarch::la::Vector<DIMENSIONS,int>&       fineGridPositionOfVertex
) {
  logTraceInWith3Arguments( "computeContributionWeightOfInterGridTransfer(...)", currentCoarseGridVertex, currentCoarseGridVertexsInterGridTransferOperator, fineGridPositionOfVertex );

  double result;

  tarch::la::Vector<DIMENSIONS,int>  positionRelativeToKthCoarseVertex;
  bool                               isInfluencedByVertexK;

  int stencilEntry;
  getPositionIn5PowDStencilRelativeToKthCoarseVertex(
    currentCoarseGridVertex,
    fineGridPositionOfVertex,
    positionRelativeToKthCoarseVertex,
    stencilEntry,
    isInfluencedByVertexK
  );

  if (isInfluencedByVertexK) {
    int baseOfGridVectorEntry = 1;
    int coarseGridVectorEntry = 0;
    for (int d=0; d<DIMENSIONS; d++) {
      coarseGridVectorEntry += currentCoarseGridVertex(d) * baseOfGridVectorEntry;
      baseOfGridVectorEntry *= 2;
    }
    result = currentCoarseGridVertexsInterGridTransferOperator(stencilEntry);
  }
  else {
    result = 0.0;
  }

  logTraceOutWith1Argument( "computeContributionWeightOfInterGridTransfer(...)", result );

  return result;
}


void matrixfree::solver::Multigrid::getPositionIn5PowDStencilRelativeToKthCoarseVertex(
      const tarch::la::Vector<DIMENSIONS,int>&       coarseGridVertexPosition,
      const tarch::la::Vector<DIMENSIONS,int>&       fineGridVertexPosition,
      tarch::la::Vector<DIMENSIONS,int>&             entryOfCoarseGridStencil,
      int&                                           indexOfCoarseGridStencil,
      bool&                                          coarseGridStencilInfluencesFineGridVertex
) {
  indexOfCoarseGridStencil                  = 0;
  coarseGridStencilInfluencesFineGridVertex = true;
  int baseOfStencilEntry                    = 1;
  for (int d=0; d<DIMENSIONS; d++) {
    assertion(coarseGridVertexPosition(d)>=0 && coarseGridVertexPosition(d)<=1);
    if (coarseGridVertexPosition(d)==1) {
      entryOfCoarseGridStencil(d) = 2 - (3-fineGridVertexPosition(d));
    }
    else {
      entryOfCoarseGridStencil(d) = 2 + fineGridVertexPosition(d);
    }
    coarseGridStencilInfluencesFineGridVertex &= entryOfCoarseGridStencil(d)>=0 && entryOfCoarseGridStencil(d)<5;
    indexOfCoarseGridStencil                  += entryOfCoarseGridStencil(d) * baseOfStencilEntry;
    baseOfStencilEntry                        *= 5;
  }
}


tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> matrixfree::solver::Multigrid::calculateCellInterGridTransferOperator(
  const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&  coarseGridVerticesInterGridTransferOperators,
  const tarch::la::Vector<DIMENSIONS,int>&                           fineGridPositionOfCell
) {
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> result;

  dfor2(l) // run over fine grid vertices
    tarch::la::Vector<DIMENSIONS,int> fineGridVertexPosition = fineGridPositionOfCell + l;

    dfor2(k) // run over coarse grid vertices
      tarch::la::Vector<DIMENSIONS,int>  entryOfCoarseGridStencil;
      bool                               coarseGridStencilInfluencesFineGridVertex;
      int                                indexOfCoarseGridStencil;

      getPositionIn5PowDStencilRelativeToKthCoarseVertex(
        k,
        fineGridVertexPosition,
        entryOfCoarseGridStencil,
        indexOfCoarseGridStencil,
        coarseGridStencilInfluencesFineGridVertex
      );

      int indexInOperator = getPositionInCellInterGridTransferOperatorVector(kScalar, indexOfCoarseGridStencil);

      if (coarseGridStencilInfluencesFineGridVertex) {
        result(lScalar, kScalar) = coarseGridVerticesInterGridTransferOperators(indexInOperator);
      }
      else{
        result(lScalar, kScalar) = 0.0;
      }
    enddforx
  enddforx

  return result;
}


int matrixfree::solver::Multigrid::getPositionInCellInterGridTransferOperatorVector(
  const int coarseGridVertexNumber,
  const int positionInOperator
) {
  return coarseGridVertexNumber*FIVE_POWER_D + positionInOperator;
}


tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> matrixfree::solver::Multigrid::calculatePetrovGalerkinCoarseGridOperator(
  const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&   coarseGridVerticesP,
  const tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>&   coarseGridVerticesR,
  const tarch::la::Vector<DIMENSIONS,int>&                           fineGridPositionOfCell,
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double>                elementWiseAssemblyMatrix
) {
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> cellP = calculateCellInterGridTransferOperator(coarseGridVerticesP, fineGridPositionOfCell);
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> cellR = calculateCellInterGridTransferOperator(coarseGridVerticesR, fineGridPositionOfCell);

  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> result;
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> cellAP;

  cellAP = elementWiseAssemblyMatrix * cellP;
  result = transpose(cellR) * cellAP;

  return result;
}


tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double> matrixfree::solver::Multigrid::calculatePetrovGalerkinCoarseGridOperator(
  const tarch::la::Vector<DIMENSIONS,int>&                           fineGridPositionOfCell,
  tarch::la::Matrix<TWO_POWER_D, TWO_POWER_D, double>                elementWiseAssemblyMatrix
) {
  tarch::la::Vector<TWO_POWER_D_TIMES_FIVE_POWER_D, double>  dLinearOperator;

  for (int i=0; i<TWO_POWER_D; i++) {
    for (int j=0; j<FIVE_POWER_D; j++) {
      dLinearOperator(i*FIVE_POWER_D+j) = _dLinearInterpolation(j);
    }
  }

  return calculatePetrovGalerkinCoarseGridOperator( dLinearOperator, dLinearOperator, fineGridPositionOfCell, elementWiseAssemblyMatrix );
}
