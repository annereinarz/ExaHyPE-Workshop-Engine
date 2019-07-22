// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MULISCALE_LINKED_CELL_SAMR_TOOLS_H_
#define _MULISCALE_LINKED_CELL_SAMR_TOOLS_H_


#include "tarch/logging/Log.h"

#include "tarch/la/Vector.h"

#include "peano/utils/Globals.h"
#include "peano/grid/VertexEnumerator.h"


namespace multiscalelinkedcell {
  class SAMRTools;
}



class multiscalelinkedcell::SAMRTools {
  public:
    static int getNumberOfCellsPerCellDescription( const tarch::la::Vector<DIMENSIONS, int>&  cells, const tarch::la::Vector<DIMENSIONS, int>&  haloCells );
    static int getNumberOfVerticesPerCellDescription( const tarch::la::Vector<DIMENSIONS, int>&  cells, const tarch::la::Vector<DIMENSIONS, int>&  haloCells );

    /**
     * Computes the continuous overlap of a cellDescription with the ghost layer. The
     * result is independent of the actual adaptivity structure.
     *
     * @param dxdy Size of one cell of the cellDescription embedded into the spacetree
     *             cells. This information is used to derive the halo width.
     *             The operation handles halo   width one.
     *
     * @param srcH Is not required at all
     */
    static void computeCellDescriptionOverlapWithGhostLayer(
      const tarch::la::Vector<DIMENSIONS, double>&   destOffset,
      const tarch::la::Vector<DIMENSIONS, double>&   destH,
      const tarch::la::Vector<DIMENSIONS, double>&   srcOffset,
      const tarch::la::Vector<DIMENSIONS, double>&   srcH,
      const tarch::la::Vector<DIMENSIONS, float>&    dxdy,
      tarch::la::Vector<DIMENSIONS, double>&         leftBottom,
      tarch::la::Vector<DIMENSIONS, double>&         rightTop
    );


    /**
     * This operation is given the continuous overlap of two cellDescriptiones (see
     * computeCellDescriptionOverlapWithGhostLayer()) and then computes the iteration
     * range, i.e. which cells have to be touched. Usually used to determine
     * the preimage in an adaptive setting.
     */
    static void computeIterationRangeFromCellDescriptionOverlap(
      const tarch::la::Vector<DIMENSIONS, double>&   offset,
      const tarch::la::Vector<DIMENSIONS, double>&   dxdy,
      const tarch::la::Vector<DIMENSIONS, int>&      haloSize,
      const tarch::la::Vector<DIMENSIONS, double>&   leftBottom,
      const tarch::la::Vector<DIMENSIONS, double>&   rightTop,
      tarch::la::Vector<DIMENSIONS, int>&            cellOffset,
      tarch::la::Vector<DIMENSIONS, int>&            range
    );



    /**
     * This operation is given a cell specified by a cellDescriptiones size and a number
     * of halo cells. If returns the iteration range of one of the
     * @f$ 3^d-1 @f$ ghost layers. This operation typically is used to determine
     * the iteration range in the destination data structure, i.e. which cells
     * of the halo layers have to be initialised.
     */
    static void computeIterationRangeFromRelativePosition(
      const tarch::la::Vector<DIMENSIONS, int>&  relativePosition,
      const tarch::la::Vector<DIMENSIONS, int>&  cellDescriptionSize,
      const tarch::la::Vector<DIMENSIONS, int>&  haloSize,
      tarch::la::Vector<DIMENSIONS, int>&        cellOffset,
      tarch::la::Vector<DIMENSIONS, int>&        range
    );

    /**
     * This operation is given a cell specified by a cellDescriptiones size and a number
     * of halo cells. If returns the iteration range in the source cellDescription while
     * it assumes that this one has the same dimension as the local cellDescription.
     */
    static void computeOppositeOffsetFromRelativePositionForSourceImage(
      const tarch::la::Vector<DIMENSIONS, int>&  relativePosition,
      const tarch::la::Vector<DIMENSIONS, int>&  cellDescriptionSize,
      const tarch::la::Vector<DIMENSIONS, int>&  haloSize,
      tarch::la::Vector<DIMENSIONS, int>&        cellOffset
    );

    /**
     * If the cellDescription is a huge array, the very first unknown has a certain
     * offset. If the cellDescription size is for example three cells and we have a one
     * cell  halo, then the first real inner unknown has index six. This
     * operation returns that index.
     *
     * @return Index of first real unknown within cellDescription (without ghost cells).
     *
     * Equals getIndexOfUnkownCellDescription(...,0)
     */
    static int getIndexOfFirstInnerUnknownInCellDescription(
      const tarch::la::Vector<DIMENSIONS, int>&  cells,
      const tarch::la::Vector<DIMENSIONS, int>&  haloCells
    );


    /**
     * @todo write some stupid comments
     *
     * Remark: std::bitset has an integer constructor which makes using this
     *         operation reasonable easy.
     */
    static int getIndexOfUnknownInCellDescription(
      const tarch::la::Vector<DIMENSIONS, int>&  cells,
      const tarch::la::Vector<DIMENSIONS, int>&  haloCells,
      const std::bitset<TWO_POWER_D>&            index
    );
};


#endif
