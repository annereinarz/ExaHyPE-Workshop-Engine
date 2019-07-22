#ifndef __EXAHYPE_PLOTTERS_ASCII_MULTIPLEREDUCTIONSWRITER__
#define __EXAHYPE_PLOTTERS_ASCII_MULTIPLEREDUCTIONSWRITER__

namespace exahype {
  namespace plotters {
    namespace ascii {
	class MultipleReductionsWriter;
    }
  }
}

#include "exahype/plotters/ascii/ReductionsWriter.h"
#include <vector>


/**
 * Convenience class to organize reducing multiple fields in a single call.
 * This class basically manages a list of ReductionWriters, each with names and associated
 * field indices in the vector of conserved variables Q. This allows quick mapping of
 * a vector onto reduction files.
 * 
 * 
 * @author SvenK
 * 
 **/
class exahype::plotters::ascii::MultipleReductionsWriter {
public:
	std::vector<exahype::plotters::ascii::ReductionsWriter*> reductions;
	std::vector<int> positionsQ; ///< the positions in reductions corresponding to the reductions

	/// sth like "output/" or "output/reductions-" to put before each filename
	std::string reductionsPrefix;
	
	/// sth. like ".asc" to put afer each filename
	std::string reductionsSuffix;

	/// passed to ReductionsWriter
	exahype::plotters::ascii::parallel strategy;
	
	MultipleReductionsWriter(
	  const std::string _reductionsPrefix="",
	  const std::string _reductionsSuffix=".asc",
	  exahype::plotters::ascii::parallel _strategy=exahype::plotters::ascii::parallel::global) :
		reductionsPrefix(_reductionsPrefix) ,
		reductionsSuffix(_reductionsSuffix) ,
		strategy(_strategy) {}
	
	/**
	 * Make sure your vector Q is always larger than any posq you pass here.
	 * 
	 **/
	void add(int posq, const std::string& redname) {
		reductions.push_back(
			new ReductionsWriter(reductionsPrefix + redname, reductionsSuffix, strategy)
		);
		positionsQ.push_back(posq);
	}
	
	void startRow(double current_time) {
		for (auto& r : reductions)
			r->startRow(current_time);
	}
	
	void finishRow() {
		for (auto& r : reductions)
			r->finishRow();
	}
	
	void addValue(double val, double dx) {
		for (auto& r : reductions)
			r->addValue(val, dx);
	}
	
	void addValue(double Q[], double dx) {
		for (size_t i = 0; i < reductions.size(); i++)
			reductions[i]->addValue(Q[ positionsQ[i] ], dx);
	}
};

#endif /* __EXAHYPE_PLOTTERS_ASCII_MULTIPLEREDUCTIONSWRITER__ */
