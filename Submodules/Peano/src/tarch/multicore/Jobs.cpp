#include "tarch/multicore/Jobs.h"
#include "tarch/Assertions.h"


#include "tarch/multicore/MulticoreDefinitions.h"

#include <thread>
#include <queue>


int tarch::multicore::jobs::Job::_maxNumberOfRunningBackgroundThreads( UseDefaultNumberOFBackgroundJobs );


void tarch::multicore::jobs::Job::setMaxNumberOfRunningBackgroundThreads(int maxNumberOfRunningBackgroundThreads) {
  assertion(
    maxNumberOfRunningBackgroundThreads==DontUseAnyBackgroundJobs
	||
	maxNumberOfRunningBackgroundThreads==UseDefaultNumberOFBackgroundJobs
	||
	maxNumberOfRunningBackgroundThreads>=1
  );
  _maxNumberOfRunningBackgroundThreads = maxNumberOfRunningBackgroundThreads;
}


tarch::multicore::jobs::JobType tarch::multicore::jobs::Job::getJobType() const {
  return _jobType;
}


tarch::multicore::jobs::Job::Job( JobType jobType, int jobClass, int priority ):
  _jobType(jobType),
  _jobClass(jobClass),
  _priority(priority) {
}


tarch::multicore::jobs::Job::~Job() {
}


int tarch::multicore::jobs::Job::getClass() const {
  return _jobClass;
}


int tarch::multicore::jobs::Job::getPriority() const {
  return _priority;
}


void tarch::multicore::jobs::Job::resetPriority( int value ) {
  _priority = value;
}


void tarch::multicore::jobs::Job::prefetchData() {
}


bool tarch::multicore::jobs::CompareJobPointers::operator()(tarch::multicore::jobs::Job* lhs, tarch::multicore::jobs::Job* rhs ) const {
  return lhs->getPriority() < rhs->getPriority();
}


tarch::multicore::jobs::GenericJobWithCopyOfFunctor::GenericJobWithCopyOfFunctor( const std::function<bool()>& functor, JobType jobType, int jobClass, int priority ):
  Job(jobType,jobClass,priority),
  _functor(functor)  {
}


bool tarch::multicore::jobs::GenericJobWithCopyOfFunctor::run() {
  return _functor();
}


tarch::multicore::jobs::GenericJobWithCopyOfFunctor::~GenericJobWithCopyOfFunctor() {
}


tarch::multicore::jobs::GenericJobWithoutCopyOfFunctor::GenericJobWithoutCopyOfFunctor(std::function<bool()>& functor, JobType jobType, int jobClass, int priority ):
  Job(jobType,jobClass,priority),
  _functor(functor)  {
}


bool tarch::multicore::jobs::GenericJobWithoutCopyOfFunctor::run() {
  return _functor();
}


tarch::multicore::jobs::GenericJobWithoutCopyOfFunctor::~GenericJobWithoutCopyOfFunctor() {
}


#ifndef SharedMemoryParallelisation

namespace {
  std::queue<tarch::multicore::jobs::Job* > backgroundJobs;
}


void tarch::multicore::jobs::startToProcessBackgroundJobs() {
}


bool tarch::multicore::jobs::finishToProcessBackgroundJobs() {
  // Note: Only invoked if no shared memory parallelisation activated. If
  // TBB/C++/OpenMP are enabled, the routine of the respective subfolder is
  // invoked
  if (backgroundJobs.empty()) {
    return false;
  }
  else {
    while ( !backgroundJobs.empty() ) {
      Job* p = backgroundJobs.front();
      backgroundJobs.pop();
      while (p->run()) {};
      delete p;
    }
    return true;
  }
}


int tarch::multicore::jobs::getNumberOfWaitingBackgroundJobs() {
  return 0;
}


void tarch::multicore::jobs::spawn(Job*  job) {
  while( job->run() ) {};
  delete job;
}


void tarch::multicore::jobs::spawn(std::function<bool()>& job, JobType jobType, int jobClass, int priority) {
  job();
}


int tarch::multicore::jobs::getNumberOfPendingJobs() {
  return 0;
}



bool tarch::multicore::jobs::processJobs(int jobClass, int maxNumberOfJobs, int priorities) {
  return false;
}


bool tarch::multicore::jobs::processBackgroundJobs(int maxNumberOfJobs, int priorities) {
  return false;
}


bool tarch::multicore::jobs::processHighBandwidthJobs(int maxNumberOfJobs, int priorities ) {
  return false;
}


void tarch::multicore::jobs::spawnAndWait(
  std::function<bool()>&  job0,
  std::function<bool()>&  job1,
  JobType                 jobType0,
  JobType                 jobType1,
  int                     jobClass0,
  int                     jobClass1,
	 int priority0, int priority1
) {
  while (job0()) {};
  while (job1()) {};
}


void tarch::multicore::jobs::spawnAndWait(
  std::function<bool()>& job0,
  std::function<bool()>& job1,
  std::function<bool()>& job2,
  JobType                    jobType0,
  JobType                    jobType1,
  JobType                    jobType2,
	 int                     jobClass0,
	 int                     jobClass1,
	 int                     jobClass2,
	 int priority0, int priority1, int priority2
) {
  while (job0()) {};
  while (job1()) {};
  while (job2()) {};
}


void tarch::multicore::jobs::spawnAndWait(
  std::function<bool()>& job0,
  std::function<bool()>& job1,
  std::function<bool()>& job2,
  std::function<bool()>& job3,
  JobType                    jobType0,
  JobType                    jobType1,
  JobType                    jobType2,
  JobType                    jobType3,
	 int                     jobClass0,
	 int                     jobClass1,
	 int                     jobClass2,
	 int                     jobClass3,
	 int priority0, int priority1, int priority2, int priority3
) {
  while (job0()) {};
  while (job1()) {};
  while (job2()) {};
  while (job3()) {};
}


void tarch::multicore::jobs::spawnAndWait(
  std::function<bool()>& job0,
  std::function<bool()>& job1,
  std::function<bool()>& job2,
  std::function<bool()>& job3,
  std::function<bool()>& job4,
  JobType                    jobType0,
  JobType                    jobType1,
  JobType                    jobType2,
  JobType                    jobType3,
  JobType                    jobType4,
	 int                     jobClass0,
	 int                     jobClass1,
	 int                     jobClass2,
	 int                     jobClass3,
	 int                     jobClass4,
	 int priority0, int priority1, int priority2, int priority3, int priority4
) {
  while (job0()) {};
  while (job1()) {};
  while (job2()) {};
  while (job3()) {};
  while (job4()) {};
}


void tarch::multicore::jobs::spawnAndWait(
  std::function<bool()>& job0,
  std::function<bool()>& job1,
  std::function<bool()>& job2,
  std::function<bool()>& job3,
  std::function<bool()>& job4,
  std::function<bool()>& job5,
  JobType                    jobType0,
  JobType                    jobType1,
  JobType                    jobType2,
  JobType                    jobType3,
  JobType                    jobType4,
  JobType                    jobType5,
	 int                     jobClass0,
	 int                     jobClass1,
	 int                     jobClass2,
	 int                     jobClass3,
	 int                     jobClass4,
	 int                     jobClass5,
	 int priority0, int priority1, int priority2, int priority3, int priority4, int priority5
) {
  while (job0()) {};
  while (job1()) {};
  while (job2()) {};
  while (job3()) {};
  while (job4()) {};
  while (job5()) {};
}


void tarch::multicore::jobs::spawnAndWait(
  std::function<bool()>& job0,
  std::function<bool()>& job1,
  std::function<bool()>& job2,
  std::function<bool()>& job3,
  std::function<bool()>& job4,
  std::function<bool()>& job5,
  std::function<bool()>& job6,
  std::function<bool()>& job7,
  std::function<bool()>& job8,
  std::function<bool()>& job9,
  std::function<bool()>& job10,
  std::function<bool()>& job11,
  JobType                    jobType0,
  JobType                    jobType1,
  JobType                    jobType2,
  JobType                    jobType3,
  JobType                    jobType4,
  JobType                    jobType5,
  JobType                    jobType6,
  JobType                    jobType7,
  JobType                    jobType8,
  JobType                    jobType9,
  JobType                    jobType10,
  JobType                    jobType11,
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
	 int priority0, int priority1, int priority2, int priority3, int priority4, int priority5, int priority6, int priority7, int priority8, int priority9, int priority10, int priority11
) {
  while (job0()) {};
  while (job1()) {};
  while (job2()) {};
  while (job3()) {};
  while (job4()) {};
  while (job5()) {};
  while (job6()) {};
  while (job7()) {};
  while (job8()) {};
  while (job9()) {};
  while (job10()) {};
  while (job11()) {};
}


#endif

