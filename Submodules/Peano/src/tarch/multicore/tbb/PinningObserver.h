#if !defined(_TARCH_MULTICORE_PINNING_OBSERVER_) && defined(SharedTBB)
#define _TARCH_MULTICORE_PINNING_OBSERVER_


#include "tarch/logging/Log.h"

#include <bitset>
#include <map>

#include <tbb/spin_mutex.h>
#include <tbb/task_scheduler_observer.h>


namespace tarch {
  namespace multicore {
    class PinningObserver;
  }
}


/**
 *
 * <h2> Rationale </h2>
 *
 * My original implementation followed very closely
 *
 * https://software.intel.com/en-us/blogs/2013/10/31/applying-intel-threading-building-blocks-observers-for-thread-affinity-on-intel
 *
 * However, the solution there is flawed for long-running simulations: The code
 * counts the threads one by one and pins them in ascending order. Now, TBB uses
 * an additional load balancing thread from time to time plus it does create and
 * destroy threads in kind of a mysterious manner. This usually doesn't harm
 * short-running codes (doesn't happen there too frequently or hardly at all) but
 * for long-running codes, we end up with the situation that some threads are
 * pinned to the same core.
 *
 * @author Tobias Weinzierl
 */
class tarch::multicore::PinningObserver: public tbb::task_scheduler_observer {
  public:
	/**
	 * Maximum number of cores we do support theoretically.
	 */
    static constexpr int MaxCores = sizeof(long int)*8;

    /**
     * Masks are bitfields which hold an entry for each core (hardware thread)
     * the present application is allowed to run on. If you run multiple MPI
     * ranks for example, this is a subset of the actual cores available on a
     * node. Pinning is realised by disabling all bits but one for a particular
     * thread. Obviously, the pin mask has to be a subset of the process mask.
     */
    typedef std::bitset<MaxCores> CPUSetBitfield;

    /**
     * Returns the bitfield identifying the available CPUs. This is not the same
     * as the C++14 concurrency/thread getter, as it does not ask for the number
     * of physical cores, but it analyses the process' mask to count how many
     * cores the scheduler assigns to the process.
     */
    static CPUSetBitfield getAvailableCPUs();
  private:
    static tarch::logging::Log  _log;

    /**
     * We may not initialise this field before we
     * Is initialised in the constructor PinningObserver().
     */
    CPUSetBitfield           _totalAvailableCores;

    CPUSetBitfield           _remainingAvailableCores;

    tbb::spin_mutex          _mutex;
  public:
    /**
     * Initialise the field _availableCores.
     */
    PinningObserver();
    virtual ~PinningObserver();

    /**
     * Search for a free core in the bitset.
     */
    void on_scheduler_entry( bool ) override;
    void on_scheduler_exit( bool ) override;

    /**
     * Is an override though TBB developers didn't want it to be overwritten for
     * whatever reason
     */
    void observe(bool toggle);
};

#endif

