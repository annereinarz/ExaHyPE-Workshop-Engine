#if defined(SharedTBB)
#include "JobProcessingService.h"
#include "Jobs.h"
#include "tarch/multicore/Jobs.h"


#ifdef MPIWaitsProcessJobs
#include "tarch/services/ServiceFactory.h"
registerService(tarch::multicore::tbb::JobProcessingService)
#endif


tarch::multicore::tbb::JobProcessingService::JobProcessingService() {
}


tarch::multicore::tbb::JobProcessingService& tarch::multicore::tbb::JobProcessingService::getInstance() {
  static tarch::multicore::tbb::JobProcessingService singleton;
  return singleton;
}


void tarch::multicore::tbb::JobProcessingService::receiveDanglingMessages() {
  static int NumberOfJobsPerInvocation = 1;

  const int previousJobCount = tarch::multicore::jobs::internal::getJobQueueSize( tarch::multicore::jobs::internal::BackgroundTasksJobClassNumber )
                             + tarch::multicore::jobs::internal::getJobQueueSize( tarch::multicore::jobs::internal::HighBandwidthTasksJobClassNumber );

  tarch::multicore::jobs::processBackgroundJobs(NumberOfJobsPerInvocation);
  tarch::multicore::jobs::processHighBandwidthJobs(NumberOfJobsPerInvocation);

  const int newJobCount = tarch::multicore::jobs::internal::getJobQueueSize( tarch::multicore::jobs::internal::BackgroundTasksJobClassNumber )
                        + tarch::multicore::jobs::internal::getJobQueueSize( tarch::multicore::jobs::internal::HighBandwidthTasksJobClassNumber );

  if (newJobCount>=previousJobCount and previousJobCount>0 and NumberOfJobsPerInvocation<newJobCount) {
    // seems I catched only reschulding jobs
    NumberOfJobsPerInvocation++;
  }
  else if (NumberOfJobsPerInvocation>1) {
    NumberOfJobsPerInvocation--;
  }
}

#endif
