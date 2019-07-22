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
 * 
 * @authors: Sven Koeppel
 **/
#ifndef EXAHYPE_PLOTTERS_SLICING_CARTESIAN
#define EXAHYPE_PLOTTERS_SLICING_CARTESIAN

namespace exahype {
  namespace plotters {
    struct CartesianSlicer;
  }
}

#include <algorithm>
#include "exahype/plotters/slicing/Slicer.h"
#include "tarch/la/Vector.h"
#include "exahype/parser/ParserView.h"
#include <iostream>

/**
 * <h2>Cartesian slicing for ExaHyPE</h2>
 * 
 * The term cartesian slicing defines a very basic subset of dimensional reductions:
 *
 *  a) slicing on planes parallel to the xy, xz or yz plane
 *  b) slicing on lines parallel to the x, y or z axis.
 *
 * ie. typically it goes from 3D to 2D or 1D or from 2D to 1D.
 * 
 * By using this slicer, users can define to plot on a plane very easily by defining
 * this plane in their plotterParametersion strings very close to the mathematical description:
 * 
 *   x:0  is the plane defined by x=0, ie. the y-z-plane
 *   y:0,z:0  is the axis defined by y=0 and z=0, ie. the x axis
 *   y:7  is the plane defined by y=7, ie. the x-z plane shifted to y=7
 *   x:2.74,y:42,z:3.14  is a single point. As the slicer is supposed to be used for
 *        extended objects (1D/2D/3D), most plotters won't properly deal with this
 *        selection, despite the CartesianSlicer can represent it. In ExaHyPE, use a
 *        probe plotter for that task.
 *   x:123,y:234,z:456,y:819 is something which cannot be represented by the
 *        class members but may appear in a plotterParametersion string, so it's in the
 *        domain of the parser to interpret this. In the new toolkit/parser infrastructure,
 *        such overdefinitions will be prohibited anyway.
 * 
 * <h3>A sub-patch slicer</h3>
 * While for instance the RegionSlicer slices per patch (ie. either a patch is plotted
 * or not), this slicer is able to project coordinates and indices onto the plane/line.
 * This allows to really restrict the interpolation of values onto the requested
 * manifold. To do so, plotters have to make use of the project() methods.
 * 
 **/
struct exahype::plotters::CartesianSlicer : public exahype::plotters::Slicer {
	typedef tarch::la::Vector<DIMENSIONS, double> dvec;
	typedef tarch::la::Vector<DIMENSIONS, int> ivec;
	
	static constexpr int disabled = -1;
	const int targetDim; ///< The computed lower dimension. Typically 1 or 2.
	const int baseDim; ///< Actually DIMENSIONS. Typically 2 or 3.
	const dvec req; ///< The requested abscissa in each axis, for instance [NaN,NaN,42] for z=42 and [0,0,NaN] for x=x0, y=y0
	const ivec active; ///< (effective) boolean determining wether this axis is not NaN, for instance [0,0,1] for z=z0 and [1,1,0] for x=x0, y=y0
	ivec activeAxes; ///< A vector (starting from 0) indicating the active axis, for instance [2,-1,-1] for z=z0 and [0,1,-1] for x=x0, y=y0
	ivec runningAxes; ///< A vector indicating the free axis indices, for instance [0,1,-1] for z=z0 and [2,-1,-1] for x=x0, y=y0
	
	/**
	 * @arg _req  The vector of coordinates in each dimension which is requested. If
	 *            there is no slice in a certain direction requested, `disabled` (-1)
	 *            should be set. In principle the value doesn't matter as _active serves
	 *            as a mask.
	 * @arg _active A boolean mask determining which entry in _req should be considered.
	 *            Allowed values are 0 or 1. The type is a Vector<DIM,int> as thus it
	 *            can be easily summed while Vector<DIM,bool> cannot. Value 1 means that
	 *            the dimension/axis should contribute for reduction/slicing while Value 0 means
	 *            that it is a free/running dimension/axis.
	 **/
	CartesianSlicer(const dvec& _req, const ivec& _active, int _baseDim=DIMENSIONS);
	
	/**
	 * Parse an ExaHyPE specfile query string and construct a CartesianSlicer object.
	 * The syntax is described in the class documentation.
	 **/
	static CartesianSlicer* fromSelectionQuery(const exahype::parser::ParserView& plotterParameters);
	
	/// The inverse of the attribute "active".
	int running(int d) const { return active(d) ? 0 : 1; }
	
	// TODO: it would be nice to also replace ivec active by
	// bool active(int d) const { return req(d) != disabled; }
	
	bool clips() const override {
		return targetDim < baseDim;
	}

	/**
	 * Coarse patch plotterParametersion criterion. This will give true whenever the requested
	 * plane/axis/point touches the given cell. That is, in the bordercase two neighboring
	 * patches are active/plotted.
	 **/
	bool isPatchActive(const dvec& offsetOfPatch, const dvec& sizeOfPatch) const override {
		for(int axis=0; axis<baseDim; axis++) {
			if(active(axis)) {
				if( (offsetOfPatch(axis)+sizeOfPatch(axis) < req(axis)) || // upper right bound smaller than requested coordinate
				    (offsetOfPatch(axis) > req(axis))                   ){ // lowe left bound smaller than requested coordinate
					return false; // patch does not touch req(axis)
				}
			}
		}
		return true;
	}

	/**
	 * Project physical point onto the slice, ie onto the 2D plane or onto a 1d line.
	 *
	 * The projection is not the shorted distance to the plane/line but a projection
	 * in terms of the coordinate axis, ie. replacing the coordinates. I didn't find
	 * a better name for this...
	 **/
	dvec project(dvec point) const {
		for(int i=0; i<DIMENSIONS; i++) {
			if(active(i)) {
				point(i) = req(i);
			}
		}
		return point;
	}
	
	/**
	 * Project index onto 2D plane or 1D line in a way that it lives afterwards on
	 * the object and could serve as basis vectors on the object. It's a cheap
	 * cartesian way to define a local coordinate system by restricting certain
	 * dimensionsional freedoms.
	 **/
	ivec project(ivec index) const {
		ivec ret(0);
		for(int i=0; i<DIMENSIONS; i++) {
			if(running(i)) {
				ret(i) = index(i);
			}
		}
		return ret;
	}
	
	/// Produces the string "CartesianSlicer"
	/// Used for the Slicer registry
	std::string getIdentifier() const override { return "CartesianSlicer"; }
	
	/// Gives a string like "xy" or "x" indicating the names of the requested planes.
	/// Useful for constructing file names of output files, for instance in Carpet.
	std::string planeLabel() const;
	
	/// Produces a string like "CartesianSlicer(Dim[3] to Dim[1], req=[0,0,nan])"
	/// Useful for stdout.
	std::string toString() const override;
	
	/// Produces 5-lines output including all internal states.
	/// Useful for debugging output.
	std::string debugVerbose();
	
	//std::ostream& operator<<(std::ostream &s,const exahype::plotters::CartesianSlicer& c);

}; // class CartesianSlicer


#endif /* EXAHYPE_PLOTTERS_SLICING_CARTESIAN */
