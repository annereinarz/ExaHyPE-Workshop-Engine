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
#ifndef EXAHYPE_PLOTTERS_SLICING_BASE
#define EXAHYPE_PLOTTERS_SLICING_BASE

namespace exahype {
  namespace plotters {
    struct Slicer;
    struct NonSlicer;
  }
}

#include "exahype/parser/ParserView.h"
#include "peano/utils/Dimensions.h"
#include "tarch/la/Vector.h"
#include <iostream>

/**
 * <h2>An abstract base class for clipping and slicing during ExaHyPE output</h2>
 * 
 * This class provides an interface to define <b>patchwise</b> slicings based on
 * geometric arguments. As in principle one can think of complicated geometric
 * objects, the subcell structure is not determined by this interface. Instead,
 * individual subclasses define their own methods how to identify grids on
 * certain manifolds or verify that points are within simple bounding boxes.
 *
 **/
struct exahype::plotters::Slicer {
	typedef tarch::la::Vector<DIMENSIONS, double> dvec;
	typedef tarch::la::Vector<DIMENSIONS, int> ivec;
	
	/**
	 * Shall give True if this slicer actually clips something and false
	 * if it is effectively doing nothing. However, each slicer subclass
	 * should be implemented in a way that it gives reasonable output also
	 * when clips() returns false.
	 **/
	virtual bool clips() const = 0;
	
	/**
	 * Patchwise slicing: Decide whether a patch shall be plotted or not.
	 **/
	virtual bool isPatchActive(const dvec& offsetOfPatch, const dvec& sizeOfPatch) const = 0;
	
	/**
	 * Return a short identification string of the class name, with no
	 * state information. For instance: "Slicer", or "RegionSlicer"
	 **/
	virtual std::string getIdentifier() const = 0;
	
	/**
	 * Return a short string about the internal state, for instance
	 * "AxisSlicer(-1,inf)".
	 **/
	virtual std::string toString() const = 0;

	virtual ~Slicer() {} // needed
	
	/**
	 * Factory/Registry to create instances of the Slicer (better said its
	 * subclasses). This particular method makes an educated guess which slicer
	 * subclass is fitting best.
	 * 
	 * All subclasses should provide similar static methods to create instances
	 * based on ExaHyPE specfile plotterParametersion strings.
	 **/
	static Slicer* bestFromSelectionQuery(const exahype::parser::ParserView& plotterParameters);
};

/**
 * A dummy implementation
 **/
struct exahype::plotters::NonSlicer : public exahype::plotters::Slicer {
	typedef tarch::la::Vector<DIMENSIONS, double> dvec;
	bool clips() const override { return false; }
	bool isPatchActive(const dvec& offsetOfPatch, const dvec& sizeOfPatch) const override { return true; }
	std::string toString() const override { return "NonSlicer"; }
	std::string getIdentifier() const override { return "NonSlicer()"; }
};

#endif /* EXAHYPE_PLOTTERS_SLICING_BASE */
