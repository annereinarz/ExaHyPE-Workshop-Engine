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
#ifndef EXAHYPE_PLOTTERS_SLICING_REGION
#define EXAHYPE_PLOTTERS_SLICING_REGION

namespace exahype {
  namespace plotters {
    struct RegionSlicer;
  }
}

#include "exahype/plotters/slicing/Slicer.h"
#include "tarch/la/Vector.h"
#include <string>

/**
 * <h2>Simple region based patch slicing</h2>
 * 
 * This slicer is the basic bounding box slicer/clipper which was present in all VTK
 * plotters before we had the slicing API (introduced at 2017-05-09). The rectangular
 * bounding box is defined by two points (lower left and upper right). By default,
 * these points are at infinity (or their numerical equivalents, cf. the default... constants).
 * Therefore by default there is no slicing taking place.
 **/
struct exahype::plotters::RegionSlicer : public exahype::plotters::Slicer {
	typedef tarch::la::Vector<DIMENSIONS, double> dvec;
	typedef tarch::la::Vector<DIMENSIONS, int> ivec;

	static const double defaultLeftBottomFront, defaultRightTopBack;	
	const dvec  _regionOfInterestLeftBottomFront, _regionOfInterestRightTopBack;
	
	RegionSlicer(const dvec& regionOfInterestLeftBottomFront, const dvec& regionOfInterestRightTopBack) :
		_regionOfInterestLeftBottomFront(regionOfInterestLeftBottomFront),
		_regionOfInterestRightTopBack(regionOfInterestRightTopBack) {}

	/**
	 * Create a RegionSlicer from an ExaHyPE specfile query string. This method
	 * parses the string and creates a new instance on the heap.
	 **/
	static RegionSlicer* fromSelectionQuery(const exahype::parser::ParserView& plotterParameters);
	
	std::string getIdentifier() const override { return "RegionSlicer"; }
	std::string toString() const override;
	
	bool clips() const override {
		return !(
			tarch::la::min(_regionOfInterestLeftBottomFront) == defaultLeftBottomFront && 
			tarch::la::max(_regionOfInterestLeftBottomFront) == defaultRightTopBack
		);
	}
	
	bool isPatchActive(const dvec& offsetOfPatch, const dvec& sizeOfPatch) const override {
		return
			tarch::la::allSmaller(_regionOfInterestLeftBottomFront,offsetOfPatch+sizeOfPatch)
			&&
			tarch::la::allGreater(_regionOfInterestRightTopBack,offsetOfPatch);
	}
	
	/**
	 * An auxilliary function demonstrating point-wise slicing which could be applied
	 * if plotters do so.s
	 **/
	bool isPointInside(const dvec& point) const {
		return
			tarch::la::allSmaller(_regionOfInterestLeftBottomFront,point)
			&&
			tarch::la::allGreater(_regionOfInterestRightTopBack,point);
	}
};

#endif /* EXAHYPE_PLOTTERS_SLICING_REGION */ 
