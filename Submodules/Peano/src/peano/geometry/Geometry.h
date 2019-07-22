// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _GEOMETRY_GEOMETRY_H_
#define _GEOMETRY_GEOMETRY_H_
#include "tarch/la/Vector.h"
#include "peano/utils/Globals.h"


namespace peano {
  namespace geometry {
    class Geometry;
  }
}


/**
 * Interface for builtin or other geometry descriptions
 *
 * When you create a repository, you have to pass an instance of a class
 * implementing Geometry. This object is responsible to control the geometry,
 * i.e. you can do things such as switchToOutside() in your mapping, but it is
 * tricky. Instead, we recommend to control the shape of the computational
 * domain due to a subclass of this interface. If this geometry decides to
 * change, Peano should automatically identify this and call destruction and
 * creation events automatically. It is some kind of indirect control.
 *
 * <h2> Changing Geometries in Peano </h2>
 *
 * Peano's handling of changing geometries relies on a callback pattern. If you
 * implement a changing geometry, you have to do two things:
 *
 * - Modify your geometry and
 * - ensure that domainHasChanged() returns true for the subdomain that has
 *   changed.
 *
 * Peano will in the subsequent traversal evalutuate domainHasChanged() on the
 * tree and check whether new vertices have to be created or grid elements have
 * to be destroyed. You'll receive the corresponding events, i.e. ensure your
 * mapping implements the create and destroy operations.
 *
 * Typically, geometry updates thus are distributed among two classes:
 *
 * - The geometry ensures that the geometry has changed and that Peano can find
 *   out which subdomains have changed. The geometry handles the where.
 * - The mapping's create and destroy operations update the grid, i.e. the
 *   mapping implements the how to update the geometry.
 *
 * <h2> Design Rationale </h2>
 *
 * I tried to make the geometry interface minimal, i.e. to provide only
 * operations absolutely necessary. Please browse through the method
 * descriptions to get an idea on the semantics of the class.
 *
 * @author Tobias Weinzierl
 */
class peano::geometry::Geometry {
  public:
    virtual ~Geometry(){}

    /**
     * Routine for decision if the point and the surrounding box lies outside
     * the geometry.
     *
     * With value zero for the parameter resolution, this gives the exact
     * geometry information of a single point position x (outside yes or no).
     *
     * Please note that the computational domain splits up the whole
     * space @f$ \mathbf{R}^d @f$ into outside, inside and a set of points
     * (submanifold) that are neither inside nor outside. Thus, a pointwise
     * query (h equals the zero vector) returns one of three values mapped onto
     * the two functions isCompletelyInside() and isCompletelyOutside(). The
     * spatial query, where the query point x is added a surrounding box with
     * size 2*resolution where x is in the center is a little bit more complicated: The surrounding box
     * is interpreted to be an open domain and isCompletelyInside() /
     * isCompletelyOutside() returns true iff the intersection of the inner /
     * outer domain with the open surrounding box is the box itself.
     *
     * @image html peano/geometry/geometry-vertex-inside-outside.png
     * @image html peano/geometry/geometry-cell-inside-outside.png
     *
     * The query is not const anymore, as you might want to cache requests, e.g.
     */
     virtual bool isCompletelyOutside( const tarch::la::Vector<DIMENSIONS,double>& x, const tarch::la::Vector<DIMENSIONS,double> &resolution ) = 0;

    /**
     * Routine for decision if the point and the surrounding box lies inside
     * the geometry.
     *
     * With value zero for the parameter resolution, this gives the exact
     * geometry information of a single point position x (outside yes or no).
     *
     * Please note that the computational domain splits up the whole
     * space @f$ \mathbf{R}^d @f$ into outside, inside and a set of points
     * (submanifold) that are neither inside nor outside. Thus, a pointwise
     * query (h equals the zero vector) returns one of three values mapped onto
     * the two functions isCompletelyInside() and isCompletelyOutside(). The
     * spatial query, where the query point x is added a surrounding box with
     * size 2*resolution where x is in the center is a little bit more complicated: The surrounding box
     * is interpreted to be an open domain and isCompletelyInside() /
     * isCompletelyOutside() returns true iff the intersection of the inner /
     * outer domain with the open surrounding box is the box itself.
     *
     * @image html peano/geometry/geometry-vertex-inside-outside.png
     * @image html peano/geometry/geometry-cell-inside-outside.png
     *
     */
    virtual bool isCompletelyInside( const tarch::la::Vector<DIMENSIONS,double>& x, const tarch::la::Vector<DIMENSIONS,double> &resolution ) = 0;

    /**
     * @todo Well could someone write a description here?
     */
    virtual bool isOutsideClosedDomain( const tarch::la::Vector<DIMENSIONS,double>& x ) = 0;

    /**
     * Ask geometry whether subdomain has changed.
     *
     * @param x          Center of subdomain
     * @param resolution Resolution of subdomain, i.e. hexahedron surrounding x
     */
    virtual bool domainHasChanged( const tarch::la::Vector<DIMENSIONS,double>& x, const tarch::la::Vector<DIMENSIONS,double> &resolution ) = 0;
};

#endif
