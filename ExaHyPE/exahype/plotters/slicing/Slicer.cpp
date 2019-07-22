// Implementing all of the slicers in a single file to decrease
// compilation time, for the moment.

#include "Slicer.h"
#include "RegionSlicer.h"
#include "CartesianSlicer.h"

#include "tarch/logging/Log.h"
#include "exahype/parser/Parser.h"

#include <sstream>

using namespace exahype::plotters;
using namespace std;

// storage for the RegionSlicer limits
const double RegionSlicer::defaultLeftBottomFront = -std::numeric_limits<double>::infinity();
const double RegionSlicer::defaultRightTopBack    = +std::numeric_limits<double>::infinity();

static tarch::logging::Log _log("exahype::plotters::Slicer");

CartesianSlicer::CartesianSlicer(const dvec& _req, const ivec& _active, int _baseDim) : 
		targetDim(_baseDim - tarch::la::sum(_active)),
		baseDim(_baseDim),
		req(_req),
		active(_active),
		activeAxes(-1),
		runningAxes(-1) {
		
	for(int i=0; i<DIMENSIONS; i++) {
		activeAxes(i) = disabled;
		runningAxes(i) = disabled;
		
		// This algorithm is crazy. Needed a lot of debugging with standalone
		// examples, but now its tested for DIM<=3.
		
		for(int j=i; j<DIMENSIONS; j++) { // forward check for actives
			if(active(j)) {
				activeAxes(i)=j;
				for(int k=0; k<i; k++) { // backward check if not already included
					if(activeAxes(k)==j)
						activeAxes(i)=disabled;
				}
				if(activeAxes(i)!=disabled)
					break;
			}
		}
		
		for(int j=i; j<DIMENSIONS; j++) { // forward check for actives
			if(!active(j)) {
				runningAxes(i)=j;
				for(int k=0; k<i; k++) { // backward check if not already included
					if(runningAxes(k)==j)
						runningAxes(i)=disabled;
				}
				if(runningAxes(i)!=disabled)
					break;
			}
		}
	}
}
	
CartesianSlicer* CartesianSlicer::fromSelectionQuery(const exahype::parser::ParserView& plotterParameters) {
	dvec r; ivec v;
	v(0) = plotterParameters.isValueValidDouble("select/x");
	if(v(0)) r(0) = plotterParameters.getValueAsDouble("select/x");
	v(1) = plotterParameters.isValueValidDouble("select/y");
	if(v(1)) r(1) = plotterParameters.getValueAsDouble("select/y");
	#if DIMENSIONS==3
	v(2) = plotterParameters.isValueValidDouble("select/z");
	if(v(2)) r(2) = plotterParameters.getValueAsDouble("select/z");
	#endif

	// v(i) == true == 1 means that value was provided, v(i) == false == 0 means that not.
	return new CartesianSlicer(r, v);
}

/**
 * A variant of tarch::la::Vector::toString which replcaes infVal with repl.
 **/
std::string valueReplPrinter(const tarch::la::Vector<DIMENSIONS, double>& vec, const double infVal, const std::string& repl) {
	stringstream s;
	s << "[";
	for(int i=0; i < DIMENSIONS; i++) {
		if(vec[i] == infVal) s << repl;
		else s << vec[i];
		if(i + 1 < DIMENSIONS) s << ",";
	}
	s << "]";
	return s.str();
}

std::string RegionSlicer::toString() const {
	// a beautiful infinity character in utf8. Could also just use "inf".
	// Unicode works in most Linux and Mac terminals but not Windows. (https://stackoverflow.com/a/12020179)
	std::string inf = "\u221E", plus = "+", minus = "-";
	
	stringstream s;
	s << "RegionSlicer("
	  << valueReplPrinter(_regionOfInterestLeftBottomFront, defaultLeftBottomFront, minus + inf)
	  << ","
	  << valueReplPrinter(_regionOfInterestRightTopBack, defaultRightTopBack, plus + inf)
	  << ")";
	
	return s.str();
}

std::string CartesianSlicer::toString() const {
	stringstream s;
	s << "CartesianSlicer(Dim["<<baseDim<<" -> "<<targetDim<<"], req="<<req<<"="<<planeLabel()<<")";
	return s.str();
}

// debugging stuff, should not be operator<< but be named like "debugString" or so.
//std::ostream& operator<<(std::ostream &s,const CartesianSlicer& c) {
std::string CartesianSlicer::debugVerbose() {
	stringstream s;
	s << "CartesianSlicer, Reducing Dimension " << baseDim << " to " << targetDim << ":\n";
	s << "   req = " << req << "\n";
	s << "   active = " << active << "\n";
	s << "   activeAxes = " << activeAxes << "\n";
	s << "   runningAxes = " << runningAxes << "\n";
	return s.str();
}

std::string CartesianSlicer::planeLabel() const {
	// 1D cutting:
	bool active_2 = DIMENSIONS == 3 ? active(2) : true;
	if(!active(0) &&  active(1) &&  active_2) return "x";
	if( active(0) && !active(1) &&  active_2) return "y";
	if( active(0) &&  active(1) && !active_2) return "z";
	
	// 2D cutting, returns "xy", "xz" or "yz"
	if(!active(0) && !active(1) &&  active_2) return "xy";
	if( active(0) && !active(1) && !active_2) return "yz";
	if(!active(0) &&  active(1) &&  active_2) return "xz";
	return "unknowns";
}

RegionSlicer* RegionSlicer::fromSelectionQuery(const exahype::parser::ParserView& plotterParameters) {
	dvec regionOfInterestLeftBottomFront, regionOfInterestRightTopBack;
		
	regionOfInterestLeftBottomFront(0) = plotterParameters.getValueAsDoubleOrDefault("select/left", defaultLeftBottomFront); // "-", min
	regionOfInterestLeftBottomFront(1) = plotterParameters.getValueAsDoubleOrDefault("select/bottom", defaultLeftBottomFront); // "-", min
	#if DIMENSIONS==3
	regionOfInterestLeftBottomFront(2) = plotterParameters.getValueAsDoubleOrDefault("select/front", defaultLeftBottomFront); // "-", min
	#endif
	
	regionOfInterestRightTopBack(0) = plotterParameters.getValueAsDoubleOrDefault("select/right", defaultRightTopBack);
	regionOfInterestRightTopBack(1) = plotterParameters.getValueAsDoubleOrDefault("select/top", defaultRightTopBack);
	#if DIMENSIONS==3
	regionOfInterestRightTopBack(2) = plotterParameters.getValueAsDoubleOrDefault("select/back", defaultRightTopBack);
	#endif
	
	return new RegionSlicer(regionOfInterestLeftBottomFront, regionOfInterestRightTopBack);
}

Slicer* Slicer::bestFromSelectionQuery(const exahype::parser::ParserView& plotterParameters) {
	if ( plotterParameters.hasKey("select")) {
	  logInfo("bestFromSelectionQuery", "Scanning plotting plotter parameters for selection query '"<<plotterParameters.dump("select")<<"'");
	  
	  // Build up the registry.
	  //
	  // Once a larger number of slicers is available, they should be managed
	  // in a list which is traversed. For the few slicers here, we manage them
	  // manually.
	  //
	  Slicer *a = CartesianSlicer::fromSelectionQuery(plotterParameters);
	  Slicer *b = RegionSlicer::fromSelectionQuery(plotterParameters);

	  // This will get more of an issue once we have more then two slicers available ;-)
	  if(a->clips() && b->clips()) {
	    logInfo("bestFromSelectionQuery", "Several slicing strategies apply to the given arguments '"<<plotterParameters.dump("select")<<"'. I choose " << a->getIdentifier());
	  }

	  if(a->clips()) { delete b; return a;}
	  if(b->clips()) { delete a; return b; }

	  // nothing clips
	  delete a; delete b;
	  return new NonSlicer;
	} else {
	  logInfo("bestFromSelectionQuery", "No slicing requested for plotter configuration '"<<plotterParameters.toString()<<"'");
	  return nullptr;
	}
}

	
