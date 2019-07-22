// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _TARCH_MULTICORE_JOBS_H_
#define _TARCH_MULTICORE_JOBS_H_


#include <functional>
#include <limits>


namespace tarch {
  namespace multicore {
    /**
     * Default priority. The bigger the value the more important a job.
     */
    constexpr int DefaultPriority = 32;

    /**
     * Jobs are Peano's abstraction of tasks. They generalise the
     * term tasks. A task in Peano's notion follows Intel's TBB and is an
     * atomic unit. That is, it may have children which have to be processed
     * first, but it can not be interrupted and it does not depend on anybody
     * besides its children. Notably, the only way to interrupt a task is to
     * spawn children which implies that the task stops immediately, waits
     * for the children and then returns. Peano's jobs are a generalisation:
     * they may depend on other jobs. If they do not, a job is equivalent to
     * a task.
     */
    namespace jobs {
       enum class JobType {
         Job,
		 /**
		  * It does not really make sense to specify this flag by a user.
		  * But it is used internally if background threads are disabled.
		  */
		 ProcessImmediately,
		 BandwidthBoundTask,
         BackgroundTask
       };

       /**
        * Effectively switches off all background tasks. This could lead to
        * deadlocks!
        */
       constexpr int DontUseAnyBackgroundJobs            = -1;
       constexpr int UseDefaultNumberOFBackgroundJobs    =  0;

       /**
        * Abstract super class for a job. Job class is an integer. A job may
        * depend on input data from other jobs and may write out data to
        * another job as well (through a shared memory region protected by
        * a semaphore). However, it should never exchange information with
        * another job of the same class.
        *
        */
       class Job {
         private:
           const JobType  _jobType;
    	   const int      _jobClass;
    	   int            _priority;

    	   friend void startToProcessBackgroundJobs();
    	   friend bool finishToProcessBackgroundJobs();

         public:
    	   static int _maxNumberOfRunningBackgroundThreads;

    	   /**
    	    * A task is a job without any dependencies on other jobs though it
    	    * might have children. Tasks form a tree structure. Jobs may form
    	    * a DAG.
    	    *
    	    * @priority The bigger the priority the more important the job
    	    */
    	   Job( JobType jobType, int jobClass, int priority );

           virtual bool run() = 0;

           /**
            * This operation is called prior to run(). We try to make it as
            * close to run as possible. The idea is that codes use it to
            * insert their prefetch macros. Peano gives no guarantee that this
            * operation is called, i.e. everything done here is solely
            * optional.
            *
            * Prefetching for Intel is specified at
            *
            * https://software.intel.com/en-us/node/684213
            *
            * or
            *
            * https://software.intel.com/en-us/cpp-compiler-developer-guide-and-reference-cacheability-support-intrinsics-1
            *
            * There are basically the following modes: _MM_HINT_T0,
            * _MM_HINT_T1, _MM_HINT_T2 and _MM_HINT_NTA. T0 means a
            * prefetch into all cache levels. T1 means into all levels
            * except L1 (L1 might be populated but we do not enforce it),
            * T2 means at least L3. NTA is the most defensive one.
            * It loads stuff as close to the core as possible without
            * polluting any other caches.
            *
            * We usually use mode/hint _MM_HINT_NTA for data on machines with
            * Optane. This makes the data be loaded into L3. For urgent
            * computations it might however make sense to use T0.
	        *
            */
           virtual void prefetchData();
           virtual ~Job();
           int getClass() const;
           JobType getJobType() const;
           int getPriority() const;
    	   void resetPriority( int value );

           /**
            * If jobs are enqueued, they are typically not processed
            * immediately (unless we run on a single core machine), but Peano
            * puts them into a queue. This queue then is processed by
            * tasks/threads/whatever they are called. The maximum nmber of
            * these guys that work themselves through the queue of jobs is
            * set by this routine. By default, there is no upper number, so
            * you might end up with all your cores doing background jobs.
            *
            * @param maxNumberOfRunningBackgroundThreads -1 Switch off any tasks
            *   running in the background.
            * @param maxNumberOfRunningBackgroundThreads 0 Do not use background tasks
            *   unless the user instructs the component that this background task is a
            *   very long running task. If a long-lasting task is issued, the component
            *   launches a task for it specifically.
            * @param maxNumberOfRunningBackgroundThreads Usually, 1 or any small
            *   number should be sufficient. Small is to be read relative to the
            *   threads available. You don't want your system to spend all of its tasks
            *   onto background activities at any time. You can also use the constants.
            *
            * @see DontUseAnyBackgroundJobs
            * @see ProcessNormalBackgroundJobsImmediately
            */
           static void setMaxNumberOfRunningBackgroundThreads(int maxNumberOfRunningBackgroundThreads);
       };


       /**
        * This operator is used by the std::less class which in turn is used by
        * the concurrent queue to determine an ordering upon jobs.
        */
       class CompareJobPointers {
         public:
           bool operator()(tarch::multicore::jobs::Job* lhs, tarch::multicore::jobs::Job* rhs ) const;
       };

       /**
        * Frequently used implementation for job with a functor.
        */
       class GenericJobWithCopyOfFunctor: public Job {
         private:
    	   /**
            * See the outer class description for an explanation why this is an
            * attribute, i.e. why we copy the functor here always.
            */
    	   std::function<bool()>   _functor;
         public:
           GenericJobWithCopyOfFunctor( const std::function<bool()>& functor, JobType jobType, int jobClass, int priority );

           bool run() override;

           virtual ~GenericJobWithCopyOfFunctor();
       };

       /**
        * Frequently used implementation for job with a functor.
        */
       class GenericJobWithoutCopyOfFunctor: public Job {
         private:
    	   /**
            * See the outer class description for an explanation why this is an
            * attribute, i.e. why we copy the functor here always.
            */
    	   std::function<bool()>&   _functor;
         public:
           GenericJobWithoutCopyOfFunctor(std::function<bool()>& functor, JobType jobType, int jobClass, int priority );

           bool run() override;

           virtual ~GenericJobWithoutCopyOfFunctor();
       };


       /**
        * Standard job that accepts a pointer to a functor object. After
        * construction, the ownership of the pointer goes over to the job
        * which eventually destroys it.
        */
       template <typename T>
       class GenericJobWithPointer: public Job {
         private:
    	   T*   _functor;
         public:
    	   GenericJobWithPointer(T* functor, JobType jobType, int jobClass, int priority  ):
             Job(jobType,jobClass),
             _functor(functor)  {
    	   }


           bool run() override {
             return (*_functor)();
           }

           virtual ~GenericJobWithPointer() {
        	 delete _functor;
           }
       };

       
       /**
        * Tell job system that pending background tasks and highy priority tasks now should be done asap.
        *
        * All of the discussion below highlights the usage pattern. In
        * practice, it is very convenient to invoke startToProcessBackgroundJobs()
        * in endIteration(), i.e. once you know all hard stuff is done.
        * This implies that the exchange of MPI data teams up with background
        * job processing. The snippets below abstract from this fact.
        *
        * <h2> Usage pattern A: wait for all background jobs prior to subsequent traversal</h2>
        *
        * Some codes want to wait until all background jobs have terminated
        * before they issue a new traversal. IF the background jobs restrict
        * some global variable, e.g., there's kind of a synchronisation point
        * where we have to stop.  In this case, the program snippets typically
        * look similar to
        *
        * <pre>
          while (!tarch::multicore::jobs::finishToProcessBackgroundJobs()) {
            tarch::parallel::Node::getInstance().receiveDanglingMessages();
          }
          </pre>
        *
        * This example illustrates that you can invoke finishToProcessBackgroundJobs()
        * without a corresponding start invocation. However, I consider it to
        * be good practice to issue start explicitly. So you'd have:
        *
        * <pre>
          tarch::multicore::jobs::startToProcessBackgroundJobs();
          while (!tarch::multicore::jobs::finishToProcessBackgroundJobs()) {
            tarch::parallel::Node::getInstance().receiveDanglingMessages();
          }
          </pre>
        *
        *
        * <h2> Usage pattern B: wait for particular background or high priority job </h2>
        *
        * Some codes wanna wait for particular background jobs only. In this
        * case, see the example below on hybrid runs how to realise this.
        *
        * The example would also work without a start invocation. However,
        * I consider it to be good practice to issue start explicitly. This is
        * then done prior to the while loop.
        *
        * Some hybrid codes use processBackgroundJobs at certain points as they
        * have to wait for some low priority stuff to terminate. Such a method
        * invocation then often is embedded into a while loop. If you do this,
        * please ensure you receive dangling messages, i.e. that your while
        * loop looks similar to
        *
        *
        * <pre>
           bool terminate = false;
           tarch::multicore::jobs::startToProcessBackgroundJobs();
           while ( !terminate ) {
             tarch::parallel::Node::getInstance().receiveDanglingMessages();
             do real stuff
             reevaluate terminate
           }
           tarch::multicore::jobs::finishToProcessBackgroundJobs();
           </pre>
        *
        * Please note that it is convenient to remove/comment out the
        * finishToProcessBackgroundJobs() here: you know that you have the data
        * you are waiting for, so why bother with some cleanup/finish calls.
        *
        *
        * <h2> Number of background jobs </h2>
        *
        * Most shared memory implementations in Peano do constrain the maximum
        * number of background jobs. If all threads do background jobs at a
        * time, one hardly can speak of a background job anymore. Once such
        * jobs however enter a phase where they wait (for MPI, e.g.), it makes
        * sense to weaken this constraint. Then, it is not a problem anymore if
        * (almost) all cores do process the background jobs. If you call
        * startToProcessBackgroundJobs(), then this tells the runtime system
        * that background jobs and high priority jobs from hereon shall have a high priority.
        */
       void startToProcessBackgroundJobs();

       /**
        * Wait for all background jobs to have terminated
        *
        * If you use MPI+TBB, I strongly recommend to add a
        * receiveDanglingMessages() call do any while loop iterating over
        * finishToProcessBackgroundJobs(). See startToProcessBackgroundJobs()
        * for examples.
        *
        * @see startToProcessBackgroundJobs()
        * @return Have processed some jobs. Please note that this is not multithreaded
        *   i.e. the routine might return true while some of the jobs are still under
        *   work.
        */
       bool finishToProcessBackgroundJobs();

       /**
        * This is the logical number of background tasks, i.e. how many things
        * could, in theory, run the the background.
        */
       int getNumberOfWaitingBackgroundJobs();

       /**
        * Kick out a new job. The job's type has to be set properly: It
        * has to be clear whether the job is a job or even a task, i.e. a
        * special type of job. See JobType.
        *
        * Ownership goes over to Peano's job namespace, i.e. you don't have
        * to delete the pointer.
        */
       void spawn(Job*  job);

       /**
        * Wrapper around other spawn operation.
        */
       void spawn(std::function<bool()>& job, JobType jobType, int jobClass, int priority = DefaultPriority);

       void spawnAndWait(
         std::function<bool()>&  job0,
         std::function<bool()>&  job1,
		 JobType                 jobType0,
		 JobType                 jobType1,
		 int                     jobClass0,
		 int                     jobClass1,
		 int priority0 = DefaultPriority, int priority1 = DefaultPriority
       );

       void spawnAndWait(
         std::function<bool()>&  job0,
         std::function<bool()>&  job1,
         std::function<bool()>&  job2,
		 JobType                 jobType0,
		 JobType                 jobType1,
		 JobType                 jobType2,
		 int                     jobClass0,
		 int                     jobClass1,
		 int                     jobClass2,
		 int priority0 = DefaultPriority, int priority1 = DefaultPriority, int priority2 = DefaultPriority
       );

       void spawnAndWait(
         std::function<bool()>&  job0,
         std::function<bool()>&  job1,
         std::function<bool()>&  job2,
         std::function<bool()>&  job3,
		 JobType                 jobType0,
		 JobType                 jobType1,
		 JobType                 jobType2,
		 JobType                 jobType3,
		 int                     jobClass0,
		 int                     jobClass1,
		 int                     jobClass2,
		 int                     jobClass3,
		 int priority0 = DefaultPriority, int priority1 = DefaultPriority, int priority2 = DefaultPriority, int priority3 = DefaultPriority
       );

       void spawnAndWait(
         std::function<bool()>&  job0,
         std::function<bool()>&  job1,
         std::function<bool()>&  job2,
         std::function<bool()>&  job3,
         std::function<bool()>&  job4,
		 JobType                 jobType0,
		 JobType                 jobType1,
		 JobType                 jobType2,
		 JobType                 jobType3,
		 JobType                 jobType4,
		 int                     jobClass0,
		 int                     jobClass1,
		 int                     jobClass2,
		 int                     jobClass3,
		 int                     jobClass4,
		 int priority0 = DefaultPriority, int priority1 = DefaultPriority, int priority2 = DefaultPriority, int priority3 = DefaultPriority, int priority4 = DefaultPriority
       );

       void spawnAndWait(
         std::function<bool()>&  job0,
         std::function<bool()>&  job1,
         std::function<bool()>&  job2,
         std::function<bool()>&  job3,
         std::function<bool()>&  job4,
         std::function<bool()>&  job5,
		 JobType                 jobType0,
		 JobType                 jobType1,
		 JobType                 jobType2,
		 JobType                 jobType3,
		 JobType                 jobType4,
		 JobType                 jobType5,
		 int                     jobClass0,
		 int                     jobClass1,
		 int                     jobClass2,
		 int                     jobClass3,
		 int                     jobClass4,
		 int                     jobClass5,
		 int priority0 = DefaultPriority, int priority1 = DefaultPriority, int priority2 = DefaultPriority, int priority3 = DefaultPriority, int priority4 = DefaultPriority, int priority5 = DefaultPriority
       );

       void spawnAndWait(
         std::function<bool()>&  job0,
         std::function<bool()>&  job1,
         std::function<bool()>&  job2,
         std::function<bool()>&  job3,
         std::function<bool()>&  job4,
         std::function<bool()>&  job5,
         std::function<bool()>&  job6,
         std::function<bool()>&  job7,
         std::function<bool()>&  job8,
         std::function<bool()>&  job9,
         std::function<bool()>&  job10,
         std::function<bool()>&  job11,
		 JobType                 jobType0,
		 JobType                 jobType1,
		 JobType                 jobType2,
		 JobType                 jobType3,
		 JobType                 jobType4,
		 JobType                 jobType5,
		 JobType                 jobType6,
		 JobType                 jobType7,
		 JobType                 jobType8,
		 JobType                 jobType9,
		 JobType                 jobType10,
		 JobType                 jobType11,
		 int                     jobClass0,
		 int                     jobClass1,
		 int                     jobClass2,
		 int                     jobClass3,
		 int                     jobClass4,
		 int                     jobClass5,
		 int                     jobClass6,
		 int                     jobClass7,
		 int                     jobClass8,
		 int                     jobClass9,
		 int                     jobClass10,
		 int                     jobClass11,
		 int priority0 = DefaultPriority, int priority1 = DefaultPriority, int priority2 = DefaultPriority, int priority3 = DefaultPriority, int priority4 = DefaultPriority, int priority5 = DefaultPriority, int priority6 = DefaultPriority, int priority7 = DefaultPriority, int priority8 = DefaultPriority, int priority9 = DefaultPriority, int priority10 = DefaultPriority, int priority11 = DefaultPriority
       );

       int getNumberOfPendingJobs();

       /**
        * Handle only jobs of one job class.
        *
        * @param jobClass        Class job
        * @param maxNumberOfJobs How many jobs to process at most. By default,
        *          the routine processes all jobs in the queue
        * @param priorities      Filter out which jobs to process. If you pass
        *                        something negative, all jobs of the class are
        *                        done, i.e. I ignore priorities.
        */
       bool processJobs(int jobClass, int maxNumberOfJobs = std::numeric_limits<int>::max(), int priorities = -1 );

       /**
        * This operation is often used when a system is waiting for some jobs
        * to come in. If you have only jobs that do not reschedule themselves,
        * then this function with its default parameter is fine. If you have
        * jobs that do reschedule, then the function is problematic: it could
        * be that a polling thread simply always gets a (high priority)
        * rescheduling job.
        *
        * It should work nevertheless as long as there are some job consumers
        * active as well. I however simply recommend another pattern: If you
        * wait within a while loop, add a counter. It is initialised with 1.
        * After each while iteration (you still wait for jobs), you increase
        * the counter. Let the counter serve as function argument.
        *
        * @see processJobs
        */
       bool processBackgroundJobs(int maxNumberOfJobs = 1, int priorities = -1);
       bool processHighBandwidthJobs(int maxNumberOfJobs = 1, int priorities = -1);
    }
  }
}


#endif

