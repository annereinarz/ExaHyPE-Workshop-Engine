// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#if !defined( _TARCH_MULTICORE_TBB_JOB_PROCESSING_SERVICE_H_) && defined(SharedTBB)
#define _TARCH_MULTICORE_TBB_JOB_PROCESSING_SERVICE_H_


#include "tarch/services/Service.h"

#ifndef noMPIWaitsProcessJobs
#define MPIWaitsProcessJobs
#endif


namespace tarch {
  namespace multicore {
    namespace tbb {
      class JobProcessingService;
    }
  }
}


class tarch::multicore::tbb::JobProcessingService: public tarch::services::Service {
  private:
	JobProcessingService();
  public:
	static JobProcessingService& getInstance();

    void receiveDanglingMessages() override;
};


#endif

