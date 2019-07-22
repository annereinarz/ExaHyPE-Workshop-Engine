#ifndef __EXAHYPE_PLOTTERS_ASCII_REDUCTIONSWRITER__
#define __EXAHYPE_PLOTTERS_ASCII_REDUCTIONSWRITER__

namespace exahype {
  namespace plotters {
    namespace ascii {
	class ReductionsWriter;

	enum class parallel {
		postprocess, ///< one reduction file per rank
		global       ///< do one global reduction
	};
    }
  }
}

#include "exahype/plotters/ascii/TimeSeriesReductions.h"

#include <stdio.h>
#include <string>


/**
 * Actual MPI-aware writer of reductions over all ranks.
 * 
 * Added to engine via ncp2 from CCZ4 at 2017-04-01.
 * 
 * @author SvenK
 * 
 **/
class exahype::plotters::ascii::ReductionsWriter : public exahype::plotters::ascii::TimeSeriesReductions {
    FILE* asc;
    std::string fnsuffix;
    bool parallel; ///< has value of "#ifdef Parallel"
    bool master; ///< same with isGlobalMaster() from tarch.
    int reductionTag;
    bool globalReductions;
public:
    std::string filename;

    ReductionsWriter(
    	const std::string _filenamebase,
	const std::string _reductionsSuffix=".asc",
	exahype::plotters::ascii::parallel _strategy=exahype::plotters::ascii::parallel::global);

    ~ReductionsWriter();

    void openFile();

    void writeRow();
    void writeRow(FILE* stream);
    void finishRow();
};

#endif /* __EXAHYPE_PLOTTERS_ASCII_REDUCTIONSWRITER__ */
