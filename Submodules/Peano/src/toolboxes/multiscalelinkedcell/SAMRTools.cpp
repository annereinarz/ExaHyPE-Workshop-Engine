#include "multiscalelinkedcell/SAMRTools.h"


int multiscalelinkedcell::SAMRTools::getNumberOfCellsPerCellDescription(
  const tarch::la::Vector<DIMENSIONS, int>&  cells,
  const tarch::la::Vector<DIMENSIONS, int>&  haloCells
) {
  int result = 1;
  for (int d=0; d<DIMENSIONS; d++) {
    result *= (cells(d)+2*haloCells(d));
  }
  return result;
}


int multiscalelinkedcell::SAMRTools::getNumberOfVerticesPerCellDescription(
  const tarch::la::Vector<DIMENSIONS, int>&  cells,
  const tarch::la::Vector<DIMENSIONS, int>&  haloCells
) {
  int result = 1;
  for (int d=0; d<DIMENSIONS; d++) {
    result *= (cells(d)+2*haloCells(d))+1;
  }
  return result;
}


void multiscalelinkedcell::SAMRTools::computeCellDescriptionOverlapWithGhostLayer(
  const tarch::la::Vector<DIMENSIONS, double>&   destOffset,
  const tarch::la::Vector<DIMENSIONS, double>&   destH,
  const tarch::la::Vector<DIMENSIONS, double>&   srcOffset,
  const tarch::la::Vector<DIMENSIONS, double>&   srcH,
  const tarch::la::Vector<DIMENSIONS, float>&    dxdy,
  tarch::la::Vector<DIMENSIONS, double>&         leftBottom,
  tarch::la::Vector<DIMENSIONS, double>&         rightTop
) {
  for (int d=0; d<DIMENSIONS; d++) {
    if (tarch::la::greater(srcOffset(d),destOffset(d))) {
      assertionNumericalEquals(destOffset(d) + destH(d), srcOffset(d) );
      leftBottom(d) =  srcOffset(d);
    }
    else if (tarch::la::smaller(srcOffset(d),destOffset(d))) {
      leftBottom(d) =  destOffset(d) - dxdy(d);
    }
    else {
      leftBottom(d) =  destOffset(d);
    }
  }

  for (int d=0; d<DIMENSIONS; d++) {
    if (tarch::la::greater(srcOffset(d),destOffset(d))) {
      rightTop(d) = srcOffset(d) + dxdy(d);
    }
    else if (tarch::la::smaller(srcOffset(d),destOffset(d))) {
      rightTop(d) = destOffset(d);
    }
    else {
      rightTop(d) = destOffset(d) + destH(d);
    }
  }
}


void multiscalelinkedcell::SAMRTools::computeOppositeOffsetFromRelativePositionForSourceImage(
  const tarch::la::Vector<DIMENSIONS, int>&  relativePosition,
  const tarch::la::Vector<DIMENSIONS, int>&  cellDescriptionSize,
  const tarch::la::Vector<DIMENSIONS, int>&  haloSize,
  tarch::la::Vector<DIMENSIONS, int>&        cellOffset
) {
  cellOffset = relativePosition;

  for (int d=0; d<DIMENSIONS; d++) {
    if (relativePosition(d)==0) {
      cellOffset(d) = cellDescriptionSize(d) + haloSize(d) - haloSize(d);
    }
    else if (relativePosition(d)==2) {
      cellOffset(d) = haloSize(d);
    }
    else {
      cellOffset(d) = haloSize(d);
    }
  }

}



void multiscalelinkedcell::SAMRTools::computeIterationRangeFromCellDescriptionOverlap(
  const tarch::la::Vector<DIMENSIONS, double>&   offset,
  const tarch::la::Vector<DIMENSIONS, double>&   dxdy,
  const tarch::la::Vector<DIMENSIONS, int>&      haloSize,
  const tarch::la::Vector<DIMENSIONS, double>&   leftBottom,
  const tarch::la::Vector<DIMENSIONS, double>&   rightTop,
  tarch::la::Vector<DIMENSIONS, int>&            cellOffset,
  tarch::la::Vector<DIMENSIONS, int>&            range
) {
  #ifdef CompilerICC
  #pragma forceinline recursive
  #endif
  for (int d=0; d<DIMENSIONS; d++) {
    assertion2( tarch::la::smallerEquals(leftBottom(d),rightTop(d)),leftBottom,rightTop);

    const double numberOfCells = ( leftBottom(d)-offset(d) ) / dxdy(d);
//    cellOffset(d) = tarch::la::round( (leftBottom(d)-offset(d)+dxdy(d)) / dxdy(d)); //-> 3.1
    cellOffset(d) = std::floor( numberOfCells ) + haloSize(d); //-> 2.1
//    cellOffset(d) = std::ceil( (leftBottom(d)-offset(d)+dxdy(d)) / dxdy(d)); -> 3.1
  }

  for (int d=0; d<DIMENSIONS; d++) {
//    range(d) = std::ceil( continuousRage(d)/dxdy(d) );
    // it should be ceil logically, but then we have rounding effects, i.e. 1.0000000000001 becoming 2.
    // @todo introduce my ceil in the la component and remove second for loop.
    const double numberOfCells = (rightTop(d)-leftBottom(d))/dxdy(d);

    range(d) = std::ceil( numberOfCells );
  }
}


int multiscalelinkedcell::SAMRTools::getIndexOfFirstInnerUnknownInCellDescription(
  const tarch::la::Vector<DIMENSIONS, int>&  cells,
  const tarch::la::Vector<DIMENSIONS, int>&  haloCells
) {
  int result = 0;
  int base   = 1;

  for (int d=0; d<DIMENSIONS; d++) {
    result += haloCells(d)*base;
    base   *= (2*haloCells(d) + cells(d));
  }

  return result;
}



int multiscalelinkedcell::SAMRTools::getIndexOfUnknownInCellDescription(
  const tarch::la::Vector<DIMENSIONS, int>&  cells,
  const tarch::la::Vector<DIMENSIONS, int>&  haloCells,
  const std::bitset<TWO_POWER_D>&            index
) {
  int result = 0;
  int base   = 1;
  for (int d=0; d<DIMENSIONS; d++) { // 9
    const int mult = static_cast<int>( index[d] );
    result += haloCells(d)*base;
    result += mult * (cells(d)-1) * base;
    base   *= (2*haloCells(d) + cells(d));
  }
  return result;
}


void multiscalelinkedcell::SAMRTools::computeIterationRangeFromRelativePosition(
  const tarch::la::Vector<DIMENSIONS, int>&  relativePosition,
  const tarch::la::Vector<DIMENSIONS, int>&  cellDescriptionSize,
  const tarch::la::Vector<DIMENSIONS, int>&  haloSize,
  tarch::la::Vector<DIMENSIONS, int>&        cellOffset,
  tarch::la::Vector<DIMENSIONS, int>&        range
) {
  for (int d=0; d<DIMENSIONS; d++) {
    assertion( relativePosition(d)>=0 );
    assertion( relativePosition(d)<=2 );

    if (relativePosition(d)==0) {
      cellOffset(d) = 0;
      range(d)      = haloSize(d);
    }
    else if (relativePosition(d)==1) {
      cellOffset(d) = haloSize(d);
      range(d)      = cellDescriptionSize(d);
    }
    else {
      cellOffset(d) = cellDescriptionSize(d) + haloSize(d);
      range(d)      = haloSize(d);
    }
  }
}
