#include "matrixfree/stencil/ElementMatrix.h"

#include "peano/utils/Loop.h"



matrixfree::stencil::ElementWiseAssemblyMatrix
matrixfree::stencil::getElementWiseAssemblyMatrix( const matrixfree::stencil::Stencil& stencil ) {
  /**
   * @todo Die Abbildung sollte man im Konstruktor einmal bauen und dann hier
   * nur noch anwenden. Deshalb ist das Ding ja eine Methode und kein
   * statisches Ding.
   */
  ElementWiseAssemblyMatrix result;

  dfor2(j)
  dfor2(i)
    tarch::la::Vector<DIMENSIONS,int> stencilEntry;
    double    commonFacesPowerTwo = 1.0;
    for (int d=0; d<DIMENSIONS; d++) {
      stencilEntry(d) = i(d)-j(d)+1;
      if (i(d)==j(d)) commonFacesPowerTwo *= 2.0;
    }
    result(jScalar,iScalar) = stencil(peano::utils::dLinearised(stencilEntry,3)) / commonFacesPowerTwo;
  enddforx
  enddforx

  return result;
}


matrixfree::stencil::ComplexElementWiseAssemblyMatrix
matrixfree::stencil::getElementWiseAssemblyMatrix( const matrixfree::stencil::ComplexStencil& complexStencil ) {
  /**
   * @todo Die Abbildung sollte man im Konstruktor einmal bauen und dann hier
   * nur noch anwenden. Deshalb ist das Ding ja eine Methode und kein
   * statisches Ding.
   */
  ComplexElementWiseAssemblyMatrix result;

  dfor2(j)
  dfor2(i)
    tarch::la::Vector<DIMENSIONS,int> stencilEntry;
    double    commonFacesPowerTwo = 1.0;
    for (int d=0; d<DIMENSIONS; d++) {
      stencilEntry(d) = i(d)-j(d)+1;
      if (i(d)==j(d)) commonFacesPowerTwo *= 2.0;
    }
    result(jScalar,iScalar) = complexStencil(peano::utils::dLinearised(stencilEntry,3)) / commonFacesPowerTwo;
  enddforx
  enddforx

  return result;
}


double matrixfree::stencil::getDiagonalElement( const ElementWiseAssemblyMatrix& matrix ) {
  return matrix(0,0) * TWO_POWER_D;
}


double matrixfree::stencil::getDiagonalElement( const Stencil& stencil ) {
  return stencil(THREE_POWER_D / 2);
}


matrixfree::stencil::Stencil
matrixfree::stencil::reconstructUniformStencilFragments(const ElementWiseAssemblyMatrix& matrix ) {
  matrixfree::stencil::Stencil result(0.0);

  dfor2(j)
    dfor2(k)
      const int stencilEntry = mapElementMatrixEntryOntoStencilEntry(jScalar,kScalar);
      result(stencilEntry) += matrix(jScalar,kScalar);
    enddforx
  enddforx

  return result;
}


tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D, double> matrixfree::stencil::reconstructStencilFragments(const ElementWiseAssemblyMatrix& matrix ) {
  tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D, double> result(0.0);

  dfor2(j)
    dfor2(k)
      const int stencilEntry = mapElementMatrixEntryOntoStencilEntry(jScalar,kScalar);
      assertionEquals1(result(jScalar*THREE_POWER_D + stencilEntry),0.0,result);
      result(jScalar*THREE_POWER_D + stencilEntry) = matrix(jScalar,kScalar);
    enddforx
  enddforx

  return result;
}


matrixfree::stencil::ElementWiseAssemblyMatrix
matrixfree::stencil::getElementWiseAssemblyMatrix( const VectorOfStencils& vectorOfStencils ) {
  ElementWiseAssemblyMatrix result;

  dfor2(j)
  dfor2(i)
    const int                         stencilOffset = jScalar * THREE_POWER_D;
    tarch::la::Vector<DIMENSIONS,int> stencilEntry;
    double                            commonFacesPowerTwo = 1.0;
    for (int d=0; d<DIMENSIONS; d++) {
      if (i(d)==j(d)) {
        stencilEntry(d)      = 1;
        commonFacesPowerTwo *= 2.0;
      }
      else if (i(d)<j(d)) {
        stencilEntry(d)      = 0;
      }
      else {
        stencilEntry(d)      = 2;
      }
    }
    const int vectorOfStencilIndex = stencilOffset+peano::utils::dLinearised(stencilEntry,3);
    result(jScalar,iScalar) = vectorOfStencils( vectorOfStencilIndex ) / commonFacesPowerTwo;
  enddforx
  enddforx

  #ifdef Dim2
  assertionNumericalEquals2( result(0,0), vectorOfStencils(0*THREE_POWER_D+4)/4.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(0,1), vectorOfStencils(0*THREE_POWER_D+5)/2.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(0,2), vectorOfStencils(0*THREE_POWER_D+7)/2.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(0,3), vectorOfStencils(0*THREE_POWER_D+8)/1.0, result, vectorOfStencils );

  assertionNumericalEquals2( result(1,0), vectorOfStencils(1*THREE_POWER_D+3)/2.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(1,1), vectorOfStencils(1*THREE_POWER_D+4)/4.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(1,2), vectorOfStencils(1*THREE_POWER_D+6)/1.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(1,3), vectorOfStencils(1*THREE_POWER_D+7)/2.0, result, vectorOfStencils );

  assertionNumericalEquals2( result(2,0), vectorOfStencils(2*THREE_POWER_D+1)/2.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(2,1), vectorOfStencils(2*THREE_POWER_D+2)/1.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(2,2), vectorOfStencils(2*THREE_POWER_D+4)/4.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(2,3), vectorOfStencils(2*THREE_POWER_D+5)/2.0, result, vectorOfStencils );

  assertionNumericalEquals2( result(3,0), vectorOfStencils(3*THREE_POWER_D+0)/1.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(3,1), vectorOfStencils(3*THREE_POWER_D+1)/2.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(3,2), vectorOfStencils(3*THREE_POWER_D+3)/2.0, result, vectorOfStencils );
  assertionNumericalEquals2( result(3,3), vectorOfStencils(3*THREE_POWER_D+4)/4.0, result, vectorOfStencils );
  #endif

  return result;
}


matrixfree::stencil::ElementWiseAssemblyMatrix matrixfree::stencil::getElementWiseAssemblyMatrix( const VectorOfStencils& vectorOfStencils, const std::bitset<THREE_POWER_D>& cellIsInside ) {
  assertion2( cellIsInside[THREE_POWER_D/2], vectorOfStencils, cellIsInside );

  ElementWiseAssemblyMatrix result;

  dfor2(j)
  dfor2(i)
    const int                         stencilOffset = jScalar * THREE_POWER_D;
    tarch::la::Vector<DIMENSIONS,int> stencilEntry;
    int                               adjacentInnerCells = 0;

    dfor2(jj)
    dfor2(ii)
      if (jj+j==ii+i) {
        adjacentInnerCells += ( cellIsInside[peano::utils::dLinearised(jj+j,3)] ) ? 1 : 0;
      }
    enddforx
    enddforx

    for (int d=0; d<DIMENSIONS; d++) {
      if (i(d)==j(d)) {
        stencilEntry(d)      = 1;
      }
      else if (i(d)<j(d)) {
        stencilEntry(d)      = 0;
      }
      else {
        stencilEntry(d)      = 2;
      }
    }
    const int vectorOfStencilIndex = stencilOffset+peano::utils::dLinearised(stencilEntry,3);
    result(jScalar,iScalar) = vectorOfStencils( vectorOfStencilIndex ) / static_cast<double>(adjacentInnerCells);
  enddforx
  enddforx

  assertion4( !cellIsInside.all() || tarch::la::equals(result,getElementWiseAssemblyMatrix(vectorOfStencils) ), vectorOfStencils, cellIsInside, result, getElementWiseAssemblyMatrix(vectorOfStencils) );

  return result;
}


matrixfree::stencil::ComplexElementWiseAssemblyMatrix matrixfree::stencil::getElementWiseAssemblyMatrix( const VectorOfComplexStencils& vectorOfStencils, const std::bitset<THREE_POWER_D>& cellIsInside ) {
  assertionMsg(false, "not implemented yet" );
}


int matrixfree::stencil::mapElementMatrixEntryOntoStencilEntry(
  const tarch::la::Vector<DIMENSIONS,int>&  row,
  const tarch::la::Vector<DIMENSIONS,int>&  col
) {
  tarch::la::Vector<DIMENSIONS,int>  stencilEntry;

  for (int d=0; d<DIMENSIONS; d++) {
    if (col(d)>row(d)) {
      stencilEntry(d)=2;
    }
    else if (col(d)<row(d)) {
      stencilEntry(d)=0;
    }
    else {
      stencilEntry(d)=1;
    }
  }

  return getStencilEntryLinearisedIndex(stencilEntry);
}


int matrixfree::stencil::mapElementMatrixEntryOntoStencilEntry(int row, int col) {
  assertion( row>=0 );
  assertion( row<TWO_POWER_D );
  assertion( col>=0 );
  assertion( col<TWO_POWER_D );
  #if defined(Dim2)
  static const int result[] = {4,5,7,8,3,4,6,7,1,2,4,5,0,1,3,4};
  int thisResult = result[row*4+col];

  #ifdef Asserts
  tarch::la::Vector<DIMENSIONS,int>  toIndex   = peano::utils::dDelinearised(row,2);
  tarch::la::Vector<DIMENSIONS,int>  fromIndex = peano::utils::dDelinearised(col,2);
  assertionEquals2( thisResult, mapElementMatrixEntryOntoStencilEntry(toIndex,fromIndex), row, col );
  #endif

  return thisResult;
  #else
  tarch::la::Vector<DIMENSIONS,int>  toIndex   = peano::utils::dDelinearised(row,2);
  tarch::la::Vector<DIMENSIONS,int>  fromIndex = peano::utils::dDelinearised(col,2);
  return mapElementMatrixEntryOntoStencilEntry(toIndex,fromIndex);
  #endif
}


matrixfree::stencil::ComplexElementWiseAssemblyMatrix
matrixfree::stencil::getElementWiseAssemblyMatrix( const VectorOfComplexStencils& vectorOfComplexStencils ) {
  ComplexElementWiseAssemblyMatrix result;

  dfor2(j)
  dfor2(i)
    const int                         stencilOffset = jScalar * THREE_POWER_D;
    tarch::la::Vector<DIMENSIONS,int> stencilEntry;
    double                            commonFacesPowerTwo = 1.0;
    for (int d=0; d<DIMENSIONS; d++) {
      if (i(d)==j(d)) {
        stencilEntry(d)      = 1;
        commonFacesPowerTwo *= 2.0;
      }
      else if (i(d)<j(d)) {
        stencilEntry(d)      = 0;
      }
      else {
        stencilEntry(d)      = 2;
      }
    }
    const int vectorOfComplexStencilIndex = stencilOffset+peano::utils::dLinearised(stencilEntry,3);
    result(jScalar,iScalar) = vectorOfComplexStencils( vectorOfComplexStencilIndex ) / commonFacesPowerTwo;
  enddforx
  enddforx

  #ifdef Dim2
  assertionNumericalEquals2( result(0,0), vectorOfComplexStencils(0*THREE_POWER_D+4)/4.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(0,1), vectorOfComplexStencils(0*THREE_POWER_D+5)/2.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(0,2), vectorOfComplexStencils(0*THREE_POWER_D+7)/2.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(0,3), vectorOfComplexStencils(0*THREE_POWER_D+8)/1.0, result, vectorOfComplexStencils );

  assertionNumericalEquals2( result(1,0), vectorOfComplexStencils(1*THREE_POWER_D+3)/2.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(1,1), vectorOfComplexStencils(1*THREE_POWER_D+4)/4.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(1,2), vectorOfComplexStencils(1*THREE_POWER_D+6)/1.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(1,3), vectorOfComplexStencils(1*THREE_POWER_D+7)/2.0, result, vectorOfComplexStencils );

  assertionNumericalEquals2( result(2,0), vectorOfComplexStencils(2*THREE_POWER_D+1)/2.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(2,1), vectorOfComplexStencils(2*THREE_POWER_D+2)/1.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(2,2), vectorOfComplexStencils(2*THREE_POWER_D+4)/4.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(2,3), vectorOfComplexStencils(2*THREE_POWER_D+5)/2.0, result, vectorOfComplexStencils );

  assertionNumericalEquals2( result(3,0), vectorOfComplexStencils(3*THREE_POWER_D+0)/1.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(3,1), vectorOfComplexStencils(3*THREE_POWER_D+1)/2.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(3,2), vectorOfComplexStencils(3*THREE_POWER_D+3)/2.0, result, vectorOfComplexStencils );
  assertionNumericalEquals2( result(3,3), vectorOfComplexStencils(3*THREE_POWER_D+4)/4.0, result, vectorOfComplexStencils );
/*
  result = stencil0[4]/4.0, stencil0[5]/2.0, stencil0[7]/2.0, stencil0[8]/1.0,
           stencil1[3]/2.0, stencil1[4]/4.0, stencil1[6]/1.0, stencil1[7]/2.0,
           stencil2[1]/2.0, stencil2[2]/1.0, stencil2[4]/4.0, stencil2[5]/2.0,
           stencil3[0]/1.0, stencil3[1]/2.0, stencil3[3]/2.0, stencil3[4]/4.0;
*/
  #endif

  return result;
}
