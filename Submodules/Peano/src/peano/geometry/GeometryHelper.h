// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _PEANO_GEOMETRY_GEOMETRY_HELPER_H_
#define _PEANO_GEOMETRY_GEOMETRY_HELPER_H_


#include "tarch/la/Vector.h"
#include "tarch/logging/Log.h"
#include "peano/utils/Dimensions.h"


namespace peano {
  namespace geometry {
    class GeometryHelper;
  }
}


/**
 * Helper for geometry queries
 *
 * This class is a collection of some static operations that are used by the
 * different grid realisations. It encapsulates knowledge how to deal with
 * different geometry information.
 *
 * @author Tobias Weinzierl
 */
class peano::geometry::GeometryHelper {
  private:
    GeometryHelper();

    static tarch::logging::Log _log;

  public:
    enum CurrentVertexState {
      Inside,
      Boundary,
      Outside
    };

    enum VertexAction {
      LeaveVertexUnaltered,
      LeaveVertexUnalteredButRefine,
      CreateInnerVertex,
      CreateBoundaryVertex,
      EraseOutsideVertex,
      DestroyVertexAndSwitchToOutside
    };

    enum CellAction {
      LeaveCellUnaltered,
      CreateInnerCell,
      CreateOuterCell,
      CreateOuterCellAndDoNotAnalyseItFurtherIfItsRefined
    };

    ~GeometryHelper();

    /**
     *
     * There is a rather naive interpretation of geometric information:
     *
     * - If a point plus its h environment is inside, then the vertex is
     *   inside.
     * - If a point is outside the closure of the computational domain,
     *   then this point is outside.
     * - Otherwise, the point is a boundary point and we can coarsen it.
     *
     * <h3> Hidden Geometries </h3>
     *
     * Unfortunately, this is working this way. In principle, we should
     * always coarsen outer vertices. However, if a vertex is outside and
     * all cell-connected neighbours are outside, too, it can happen that
     * we don't see inclusions.
     *
     * Besides the geometry update, we also have to check whether the
     * computationally is hidden in the cell: This happens if all the vertices
     * adjacent to a cell are outside but the domain is contained within the
     * cell.
     *
     * @image html peano/grid/nodes/Node_InvokeEnterCell.png
     *
     * I would like to do all the artificial boundary refinement here, but this is
     * not possible. To run the analysis, I have to know whether cell-connected
     * vertices are refined. However, the present routine has only geometric data.
     * As a consequence, you find this artificial refinement in Node::updateCellsGeometryInformationAfterLoad().
     */
    static VertexAction getVertexCommand(
      bool  pointWithHEnvironmentIsInside,
      bool  pointIsOutsideOfDomainClosure,
      bool  pointWithHEnvironmentIsOutside,
      bool  allCellConnectedPointsAreOutside,
	  bool  mayEraseAlongArtificiallyRefinedBoundary,
      const CurrentVertexState& currentVertexState
    );

    static CellAction getCellCommand(
      bool centerOfCellWithH2EnvironmentIsInside,
      bool centerOfCellWithH2EnvironmentIsOutside,
      bool currentCellIsInside
    );

    /**
     * Determine center of a cell.
     *
     * @param  bottomVertex Position of left bottom vertex.
     * @param  h            Cell size
     *
     * @return @f$ bottomVertex + h/2 @f$
     */
    static tarch::la::Vector<DIMENSIONS,double> getCellCenter(
      const tarch::la::Vector<DIMENSIONS,double>& bottomVertex,
      const tarch::la::Vector<DIMENSIONS,double>& h
    );
};

#endif
