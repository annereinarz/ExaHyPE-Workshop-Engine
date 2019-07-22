#if defined(SharedTBB)
#include "tarch/multicore/tbb/PinningObserver.h"
#include "tarch/Assertions.h"

#include <sys/resource.h>
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>


tarch::logging::Log tarch::multicore::PinningObserver::_log( "tarch::multicore::PinningObserver" );


tarch::multicore::PinningObserver::PinningObserver():
  _totalAvailableCores(0),
  _remainingAvailableCores(0),
  _mutex() {
}


tarch::multicore::PinningObserver::CPUSetBitfield tarch::multicore::PinningObserver::getAvailableCPUs() {
  CPUSetBitfield result(0);

  // reconstruct Unix mask
  cpu_set_t* mask;
  for ( int numberOfCPUs = sizeof(cpu_set_t)/CHAR_BIT; numberOfCPUs < MaxCores; numberOfCPUs <<= 1 ) {
    mask = CPU_ALLOC( numberOfCPUs );
    if ( !mask ) break;
    const size_t size = CPU_ALLOC_SIZE( numberOfCPUs );
    CPU_ZERO_S( size, mask );
    const int err = sched_getaffinity( 0, size, mask );
    if ( !err ) break;

    CPU_FREE( mask );
    mask = NULL;
    if ( errno != EINVAL )  break;
  }

  // Convert into bitset to make it more C++ish
  if ( mask ) {
    result = 0;
    for (int i=0; i<MaxCores; i++) {
      if (CPU_ISSET(i, mask)) {
        result[i] = true;
      }
    }

    CPU_FREE( mask );

    logInfo( "observe(bool)", "identified available cores: " << result );
  }
  else {
    logError( "observe(bool)","failed to obtain process affinity mask");
  }

  return result;
}


void tarch::multicore::PinningObserver::observe(bool toggle) {
  if (toggle) {
    assertion( _totalAvailableCores.count()==0 );
    assertion( _remainingAvailableCores.count()==0 );
    _totalAvailableCores     = getAvailableCPUs();
    _remainingAvailableCores = _totalAvailableCores;
  }

  tbb::task_scheduler_observer::observe(toggle);
}


tarch::multicore::PinningObserver::~PinningObserver() {
}


void tarch::multicore::PinningObserver::on_scheduler_entry( bool ) {
  _mutex.lock();

  if (_remainingAvailableCores.count()==0) {
	logWarning(
      "on_scheduler_entry(bool)",
	  "too many threads, i.e. no idle core available anymore. Originally, " << _totalAvailableCores.count() << " core(s) running on " << _totalAvailableCores << " have been available, but they all have been used for pinning already"
	);
  }
  else {
    int targetCore = 0;
    for (int i=0; i<MaxCores; i++) {
      if (_remainingAvailableCores[i]) {
    	targetCore = i;
    	_remainingAvailableCores[i] = false;
        i = MaxCores;
      }
    }

	cpu_set_t*   target_mask = CPU_ALLOC( _totalAvailableCores.count() );
    const size_t size        = CPU_ALLOC_SIZE( _totalAvailableCores.count() );
    CPU_ZERO_S( size, target_mask );
    CPU_SET_S( targetCore, size, target_mask );
	const int err = sched_setaffinity( 0, size, target_mask );

    if ( err ) {
      logError( "on_scheduler_entry(bool)", "pinning new thread to core " << targetCore << " failed with error code " << err );
    }
    else {
      const int   currentCore     = sched_getcpu();
   	  logInfo( "on_scheduler_entry(bool)", "pinned new thread currently running on core " << currentCore << " to core " << targetCore );
    }

    CPU_FREE( target_mask );
  }

  _mutex.unlock();
}


void tarch::multicore::PinningObserver::on_scheduler_exit( bool ) {
  _mutex.lock();

  const int   currentCore     = sched_getcpu();
  if (_remainingAvailableCores[currentCore]) {
    logWarning( "on_scheduler_exit(bool)", "thread running on core " << currentCore << " exits though core is already marked as idle" );
  }
  else {
    _remainingAvailableCores[currentCore] = true;
    logInfo( "on_scheduler_exit(bool)", "thread exits and frees core " << currentCore );
  }

  _mutex.unlock();
}



#endif
