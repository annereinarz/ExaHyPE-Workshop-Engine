/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/

#include "exahype/amr/AdaptiveMeshRefinement.h"

tarch::la::Vector<DIMENSIONS,int> exahype::amr::computeSubcellIndex(
    const tarch::la::Vector<DIMENSIONS,double>& childOffset,
    const tarch::la::Vector<DIMENSIONS,double>& childSize,
    const tarch::la::Vector<DIMENSIONS,double>& parentOffset) {
  tarch::la::Vector<DIMENSIONS,int> subcellIndex;
  for (int xi = 0; xi < DIMENSIONS; ++xi) {
    assertion2(tarch::la::greaterEquals(childOffset(xi),parentOffset(xi),0.33*childSize(xi)),childOffset,parentOffset); // tol=dx/3 <= dx/2
    subcellIndex[xi] = tarch::la::round(
        (childOffset(xi) - parentOffset(xi))/childSize(xi));
  }

  return subcellIndex;
}

tarch::la::Vector<DIMENSIONS-1,int> exahype::amr::getSubfaceIndex(
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
    const int d) {
  tarch::la::Vector<DIMENSIONS-1,int> subfaceIndex;

  int i = 0;
  for (int j = 0; j < DIMENSIONS; j++) {
    if (j != d) {
      subfaceIndex[i] = subcellIndex[j];
      i++;
    }
  }

  return subfaceIndex;
}

std::bitset<DIMENSIONS_TIMES_TWO> exahype::amr::determineInsideAndOutsideFacesOfChild(
    const tarch::la::Vector<DIMENSIONS,double>& childOffset,
    const tarch::la::Vector<DIMENSIONS,double>& childSize,
    const tarch::la::Vector<DIMENSIONS,double>& parentOffset,
    const int                                   levelDelta,
    const std::bitset<DIMENSIONS_TIMES_TWO>&    parentIsInside) {
  std::bitset<DIMENSIONS_TIMES_TWO> isInside;
  tarch::la::Vector<DIMENSIONS,int> subcellIndex =
      computeSubcellIndex(childOffset,childSize,parentOffset);

  for (int d=0; d<DIMENSIONS; d++) {
    isInside[2*d+0] = parentIsInside[2*d+0] || (subcellIndex[d] != 0);
    isInside[2*d+1] = parentIsInside[2*d+1] || (subcellIndex[d] != tarch::la::aPowI(levelDelta,3)-1);
  }

  return isInside;
}

bool exahype::amr::onBoundaryOfParent(
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
    const int levelDelta){
  const int end = tarch::la::aPowI(levelDelta,3)-1;
  bool result = false;
  for (int d = 0; d < DIMENSIONS; d++) {
    result |= subcellIndex[d] == 0;
    result |= subcellIndex[d] == end;
  }
  return result;
}

bool exahype::amr::faceIsOnBoundaryOfParent(
    const int faceIndex,
    const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
    const int levelDelta) {
  const int direction   = faceIndex / 2;
  const int orientation = faceIndex % 2;
  return subcellIndex[direction]==orientation*(tarch::la::aPowI(levelDelta,3)-1);
}
