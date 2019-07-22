/**
 * Version.cpp implements the "--version" program string.
 * 
 * In order to have this holding correct information, this cpp file should be always recompiled.
 * You can typically archive that with a command like "touch Version.cpp" to touch the last
 * modification time.
 * 
 **/

#include "main.h"

// build information about parallelization.
// this assumes TBB to be available.
#if defined(SharedTBB)
#include "tarch/multicore/Core.h"
#include "tbb/tbb_stddef.h"
#endif

// the buildinfo file is expected in the user application directory
#include "buildinfo.h"

// the KernelCalls file is in the ExaHyPE/kernels directory but the
// implementation is in the user application, KernelCalls.cpp.
#include "kernels/KernelCalls.h"

#include <string>
#include <sstream>

std::string exahype::version(const std::string& programname) {
  std::stringstream out;
  out << "This is " << programname << ", an ExaHyPE executable (http://exahype.eu)\n";
  out << "Compiled on host " << EXAHYPE_BUILD_HOST << " at " << EXAHYPE_BUILD_DATE << "\n";
#ifdef EXAHYPE_GIT_INFO
  out << "ExaHyPE git version: " << EXAHYPE_GIT_INFO << "\n";
#else
  out << "ExaHyPE git version: n/a\n";
#endif
#ifdef PEANO_SVN_INFO
  out << "Peano svn version:   " << PEANO_SVN_INFO << "\n";
#else
  out << "Peano svn version:   n/a\n";
#endif
  out << "\n";

  out << "Compile time options\n";
  out << "====================\n";
#ifdef DIMENSIONS
  out << "Dimensions:    "<< DIMENSIONS << "\n";
#else
  out << "Dimensions:    not determinable!\n";
#endif

#ifdef Debug
  out << "Debug:         YES\n";
#else
  out << "Debug:         no\n";
#endif
  
#ifdef Asserts
  out << "Assertions:    YES\n";
#else
  out << "Assertions:    no\n";
#endif

#ifdef Parallel
  out << "MPI Support:   YES\n";
#else
  out << "MPI Support:   no\n";
#endif

#ifdef SharedMemoryParallelisation // cf. tarch/multicore
  out << "Shared Memory library:   ";
#if defined(SharedTBB)
  out << "TBB\n";
#elif defined(SharedOMP)
  out << "OpenMP\n";
#else
  out << "I don't know\n";
#endif

#else // no SharedMemoryParallelisation
  out << "Shared Memory support:   no\n";
#endif

#if defined(SharedTBB)
  out << "TBB Compile time interface version: " << TBB_INTERFACE_VERSION  << "\n";
  out << "TBB Runtime interface version:      " << tbb::TBB_runtime_interface_version() << "\n";
#endif

  out << "\n";
  out << "Makesystem build options\n";
  out << "========================\n";
#ifdef EXAHYPE_BUILDINFO_AVAILABLE
  out << EXAHYPE_BUILD_INFO << "\n";
#else
  out << "Symbols n/a" << "\n";
#endif

  out << "\n";
  out << "Toolkit static registry info\n";
  out << "============================\n";
  kernels::toString(out);
  out << "\n";
  return out.str();
}
