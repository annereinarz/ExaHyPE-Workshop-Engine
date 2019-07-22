#include "tarch/logging/Log.h"
#include "tarch/tests/TestCaseRegistry.h"
#include "tarch/logging/CommandLineLogger.h"
#include "tarch/parallel/Node.h"

#include "peano/peano.h"

#include "petsc/runners/Runner.h"

#include "petscsys.h"


tarch::logging::Log _log("");


int main(int argc, char** argv) {
  peano::fillLookupTables();

  int parallelSetup = peano::initParallelEnvironment(&argc,&argv);
  if ( parallelSetup!=0 ) {
    #ifdef Parallel
    // Please do not use the logging if MPI doesn't work properly.
    std::cerr << "mpi initialisation wasn't successful. Application shut down" << std::endl;
    #else
    _log.error("main()", "mpi initialisation wasn't successful. Application shut down");
    #endif
    return parallelSetup;
  }

  int sharedMemorySetup = peano::initSharedMemoryEnvironment();
  if (sharedMemorySetup!=0) {
    logError("main()", "shared memory initialisation wasn't successful. Application shut down");
    return sharedMemorySetup;
  }

  PetscInitialize(&argc,&argv,(char*)0,(char*)0);

  int programExitCode = 0;

  if (programExitCode==0) {
    tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "debug", -1, "petsc", true ) );
    tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "debug", -1, "peano", true ) );
    tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "debug", -1, "tarch", true ) );
    tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "info", -1, "peano::parallel", true ) );
    petsc::runners::Runner runner;
    programExitCode = runner.run();
  }

  PetscFinalize();
  
  if (programExitCode==0) {
    #ifdef Parallel
    if (tarch::parallel::Node::getInstance().isGlobalMaster()) {
      logInfo( "main()", "Peano terminates successfully" );
    }
    #else
    logInfo( "main()", "Peano terminates successfully" );
    #endif
  }
  else {
    logInfo( "main()", "quit with error code " << programExitCode );
  }

  peano::shutdownParallelEnvironment();
  peano::shutdownSharedMemoryEnvironment();
  peano::releaseCachedData();

  return programExitCode;
}
