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

#ifndef ADAPTIVEMESHREFINEMENT_H_
#define ADAPTIVEMESHREFINEMENT_H_

#include "peano/utils/Globals.h"

#include "exahype/Cell.h"

#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

namespace exahype {

namespace amr {
  /**
   * Per coordinate direction xi, count the number of shifts
   * of step size @p childSize(xi) necessary to
   * reach @p childOffset from @p parentOffset.
   *
   * @param[in] childOffset  Offset of a child cell.
   * @param[in] childSize    Size of the child cell.
   * @param[in] parentOffset Offset of the parent cell.
   *
   * @see getSubfaceIndex
   */
  tarch::la::Vector<DIMENSIONS,int> computeSubcellIndex(
        const tarch::la::Vector<DIMENSIONS,double>& childOffset,
        const tarch::la::Vector<DIMENSIONS,double>& childSize,
        const tarch::la::Vector<DIMENSIONS,double>& parentOffset);

  /**
   * Collect all the element with index!=d
   * from @p subcellIndex.
   *
   * @see getSubcellIndex
   */
  tarch::la::Vector<DIMENSIONS-1,int> getSubfaceIndex(
        const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
        const int d);

  /**
   * Per coordinate direction xi, check if
   * subcellIndex[xi] is either 0 or 3^levelDelta - 1.
   * If this is the case for at least one subcellIndex[xi]
   * return true. Otherwise return false.
   */
  bool onBoundaryOfParent(
      const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
      const int levelDelta);

  /**
   * Determines if the face @p faceIndex is
   * a subset of the hull of the parent
   *
   * This is the case if
   *
   * subcellIndex[direction]==orientation*(tarch::la::aPowI(levelDelta,3)-1),
   *
   * i.e. if the cell belonging to this face is at the boundary
   * of the parent cell and the face aligns with the
   * parent cell's hull.
   *
   * The above equation takes the direction of the face's normal vector (x,y,z -> 0,1,2)
   * and its orientation (-1,1 -> 0,1).
   *
   * @param faceIndex    an index in the range [0,DIMENSIONS_TIMES_TWO-1]
   * @param subcellIndex a tuple of size DIMENSIONS giving the cell
   * @param levelDelta   the differene in levels between the coarse grid
   *                     parent and the child cell.
   * @return true if the face is on the boundary of the parent.
   */
  bool faceIsOnBoundaryOfParent(
      const int faceIndex,
      const tarch::la::Vector<DIMENSIONS, int>& subcellIndex,
      const int levelDelta);

  /**
   * This method deemed to be necessary since I could
   * not trust the Boundary markers.
   *
   * Per coordinate direction xi, check if the cell
   * is at the boundary of the parent cell, i.e. if
   * subcellIndex[xi] is either 0 or 3^levelDelta - 1,
   * and if the parent face is a boundary face.
   * If so, the face of the child is on the boundary as well.
   */
  std::bitset<DIMENSIONS_TIMES_TWO> determineInsideAndOutsideFacesOfChild(
      const tarch::la::Vector<DIMENSIONS,double>& childOffset,
      const tarch::la::Vector<DIMENSIONS,double>& childSize,
      const tarch::la::Vector<DIMENSIONS,double>& parentOffset,
      const int                                   levelDelta,
      const std::bitset<DIMENSIONS_TIMES_TWO>&    parentIsInside);

  /**
   * Determine the position of a Descendant with respect
   * to a Cell or the top most Descendant.
   *
   * This method is required for the face data prolongation and restriction, and the
   * volume data prolongation and restriction.
   */
  template <class CellDescription,class CellDescriptionHeap>
  exahype::solvers::Solver::SubcellPosition
  computeSubcellPositionOfDescendant(const CellDescription& pChild);
}  // namespace amr
}  // namespace exahype


#include "AdaptiveMeshRefinement.cpph"


#endif /* ADAPTIVEMESHREFINEMENT_H_ */
