#include "tarch/logging/Log.h"
#include "tarch/tests/TestCaseRegistry.h"
#include "tarch/logging/CommandLineLogger.h"
#include "tarch/parallel/Node.h"

#include "peano/peano.h"

#include "multigrid/runners/Runner.h"
#include "multigrid/mappings/CreateGrid.h"
#include "multigrid/mappings/JacobiSmoother.h"


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

  int                                programExitCode = 0;
  multigrid::runners::Runner::Solver solver          = multigrid::runners::Runner::None;
  if (argc!=4) {
    std::cout << "Usage: ./executable scenario solver omega" << std::endl
              << std::endl
              << "Valid scenarios:" << std::endl
              << "\tPoissonX" << std::endl
              << "\tAdaptivePoissonX" << std::endl
              << std::endl
              << "Valid solvers:" << std::endl
              << "\tJacobi" << std::endl
              << "\tAdditiveMG" << std::endl
              << "\tMultiplicativeV11" << std::endl
              << "\tMultiplicativeV22" << std::endl
              << "\tMultiplicativeV33" << std::endl
              << std::endl
/*
              <<  "AnisotropicPoisson" << std::endl
              <<  "ShiftedMinimalCheckerboard" << std::endl
              <<  "LayerProblem" << std::endl
              <<  "DiagonalFlow" << std::endl
              <<  "RecirculatingFlow" << std::endl
*/
              << std::endl
              << std::endl
              << "Please replace the X with 2,3,4 or 5. It denotes the maximum depth of the gid."
              << std::endl;
    programExitCode = 1;
  }
  else {
      if (std::string(argv[1])=="Poisson2") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::Poisson2;
      }
      else if (std::string(argv[1])=="Poisson3") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::Poisson3;
      }
      else if (std::string(argv[1])=="Poisson4") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::Poisson4;
      }
      else if (std::string(argv[1])=="Poisson5") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::Poisson5;
      }
      else if (std::string(argv[1])=="AdaptivePoisson2") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::AdaptivePoisson2;
      }
      else if (std::string(argv[1])=="AdaptivePoisson3") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::AdaptivePoisson3;
      }
      else if (std::string(argv[1])=="AdaptivePoisson4") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::AdaptivePoisson4;
      }
      else if (std::string(argv[1])=="AdaptivePoisson5") {
        multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::AdaptivePoisson5;
      }
/*
    else if (std::string(argv[1])=="AnisotropicPoisson") {
      multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::AnisotropicPoisson;
    }
    else if (std::string(argv[1])=="ShiftedMinimalCheckerboard") {
      multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::ShiftedMinimalCheckerboard;
    }
    else if (std::string(argv[1])=="LayerProblem") {
      multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::LayerProblem;
    }
    else if (std::string(argv[1])=="DiagonalFlow") {
      multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::DiagonalFlow;
    }
    else if (std::string(argv[1])=="RecirculatingFlow") {
      multigrid::mappings::CreateGrid::_scenario = multigrid::mappings::CreateGrid::RecirculatingFlow;
    }
*/
    else {
      std::cerr << "invalid scenario. Please run without arguments to see list of supported scenarios" << std::endl;
      programExitCode = 2;
    }

    if (std::string(argv[2])=="Jacobi") {
      solver = multigrid::runners::Runner::Jacobi;
    }
    else if (std::string(argv[2])=="AdditiveMG") {
      solver = multigrid::runners::Runner::AdditiveMG;
    }
    else if (std::string(argv[2])=="MultiplicativeV11") {
      solver = multigrid::runners::Runner::MultiplicativeV11;
    }
    else if (std::string(argv[2])=="MultiplicativeV22") {
      solver = multigrid::runners::Runner::MultiplicativeV22;
    }
    else if (std::string(argv[2])=="MultiplicativeV33") {
      solver = multigrid::runners::Runner::MultiplicativeV33;
    }
    else {
      std::cerr << "invalid solver. Please run without arguments to see list of supported solvers" << std::endl;
      programExitCode = 3;
    }

    multigrid::mappings::JacobiSmoother::omega = atof( argv[3] );
  }

  // Configure the output
  tarch::logging::CommandLineLogger::getInstance().clearFilterList();
  tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "info", false ) );
  tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "debug", true ) );
  tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "info", -1, "peano::grid", true ) );

  // Runs the unit tests
  if (programExitCode==0) {
    tarch::tests::TestCaseRegistry::getInstance().getTestCaseCollection().run();
    programExitCode = tarch::tests::TestCaseRegistry::getInstance().getTestCaseCollection().getNumberOfErrors();
  }

  // dummy call to runner
  if (programExitCode==0) {
    tarch::logging::CommandLineLogger::getInstance().addFilterListEntry( ::tarch::logging::CommandLineLogger::FilterListEntry( "debug", -1, "multigrid", false ) );
    multigrid::runners::Runner runner;
    programExitCode = runner.run( solver );
  }
  
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
