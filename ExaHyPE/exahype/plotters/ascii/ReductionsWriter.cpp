#include "exahype/plotters/ascii/ReductionsWriter.h"

#include "tarch/parallel/Node.h" // for MPI
#include "tarch/parallel/NodePool.h" 
#include "tarch/logging/Log.h"

static tarch::logging::Log _log("exahype::plotters::ascii::ReductionsWriter");

exahype::plotters::ascii::ReductionsWriter::ReductionsWriter(
    	const std::string _filenamebase,
	const std::string _reductionsSuffix,
	exahype::plotters::ascii::parallel _strategy)
      : 
      fnsuffix(_reductionsSuffix), 
      globalReductions(_strategy==exahype::plotters::ascii::parallel::global),
      filename(_filenamebase) {
        #ifdef Parallel
	parallel = true;
	#else
	parallel = false;
	#endif
	master = tarch::parallel::Node::getInstance().isGlobalMaster();

	if(master || !globalReductions)
		openFile();

	if(parallel && globalReductions) {
		reductionTag = tarch::parallel::Node::getInstance().reserveFreeTag(
			std::string("TimeSeriesReductions(")+filename+")::finishRow()");
	}
}
    
exahype::plotters::ascii::ReductionsWriter::~ReductionsWriter() {
	if(asc)
		fclose(asc);

	if(parallel && globalReductions) {
		tarch::parallel::Node::getInstance().releaseTag(reductionTag);
	}
}

void exahype::plotters::ascii::ReductionsWriter::openFile() {
	// compose filename

	if(parallel && !globalReductions) {
		// this imitates how the VTK writes compose their filenames, cf.
		// tarch::plotter::griddata::unstructured::vtk::VTKTextFileWriter::writeToFile(...) at
		// Peano/tarch/plotter/griddata/unstructured/vtk/VTKTextFileWriter.cpp
                filename += "-rank-" + 	std::to_string(tarch::parallel::Node::getInstance().getRank());
	}

	// add suffix (.asc or similar)
	filename += fnsuffix;

	if (master) {
		logInfo("openFile()", "Opening reduction file '"<< filename << "'");
	}
	
	const char* fn = filename.c_str();
	asc = fopen(fn, "w");
	if(asc == NULL) {
		//fprintf(stderr, "ASCII writer: Could not open output file '%s'\n", fn);//, strerror(errno));
		// in MPI environment:
		logError("openFile()", "Cannot open output ASCII file at '" << filename << "'.");
		exit(-1);
	}
	
	// print a header meta line
	/*
	sstream meta;
	if(parallel) {} // ...
	fprintf(asc, "# %s", meta.str().c_str());
	*/

	for(int i=0; i<LEN; i++) {
		fputs(colnames[i], asc);
		fputs(" ", asc);
	}
	fprintf(asc, "\n");
}

void exahype::plotters::ascii::ReductionsWriter::writeRow() {
	if(asc == NULL) {
		logError("writeRow()", "Oh no.");
		exit(-1);
	}
	writeRow(asc);
}

void exahype::plotters::ascii::ReductionsWriter::writeRow(FILE* stream) {
	for(int i=0; i<LEN; i++) {
            fprintf(stream, colformat[i], data[i]);
        }
        fprintf(stream, "\n");
	fflush(stream); // write out this line immediately
}

void exahype::plotters::ascii::ReductionsWriter::finishRow() {
	#ifdef Parallel
	if(parallel && globalReductions) {
		if(master) {
			double recieved[LEN];
			for (int rank=1; rank<tarch::parallel::Node::getInstance().getNumberOfNodes(); rank++) {
				if(!tarch::parallel::NodePool::getInstance().isIdleNode(rank)) {
					MPI_Recv( &recieved[0], LEN, MPI_DOUBLE, rank, reductionTag, tarch::parallel::Node::getInstance().getCommunicator(), MPI_STATUS_IGNORE );
					addValues(recieved);
				}
			}
		} else {
			MPI_Send( &data[0], LEN, MPI_DOUBLE, tarch::parallel::Node::getGlobalMasterRank(), reductionTag, tarch::parallel::Node::getInstance().getCommunicator());
		}
	}
	#endif
	if(master || !globalReductions) {
		TimeSeriesReductions::finishRow();
		writeRow();
	}
}
