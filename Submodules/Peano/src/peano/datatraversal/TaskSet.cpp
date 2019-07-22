#include "peano/datatraversal/TaskSet.h"


#include <chrono>
#include <thread>


#include "tarch/multicore/Jobs.h"
#include "tarch/Assertions.h"



tarch::logging::Log  peano::datatraversal::TaskSet::_log( "peano::datatraversal::TaskSet" );


#if defined(TBBInvade)
#include "tarch/multicore/Core.h"


shminvade::SHMInvade*  peano::datatraversal::TaskSet::_backgroundTaskInvade = nullptr;
#endif



int  peano::datatraversal::TaskSet::translateIntoJobClass( TaskType type ) {
  const int Default = 7;
  switch ( type ) {
    case TaskType::IsTaskAndRunImmediately:
     	  return Default;
    case TaskType::LoadCells:
 	  return 1;
    case TaskType::LoadVertices:
 	  return 2;
    case TaskType::TriggerEvents:
 	  return 3;
    case TaskType::StoreCells:
 	  return 4;
    case TaskType::StoreVertices:
 	  return 5;
    case TaskType::Background:
   	  return Default;
    case TaskType::IsBandwidthBoundTask:
      return Default;
  }
  return Default;
}


tarch::multicore::jobs::JobType peano::datatraversal::TaskSet::translateIntoJobType( TaskType type ) {
  switch ( type ) {
    case TaskType::IsTaskAndRunImmediately:
   	  return tarch::multicore::jobs::JobType::ProcessImmediately;
    case TaskType::LoadCells:
    case TaskType::LoadVertices:
    case TaskType::TriggerEvents:
    case TaskType::StoreCells:
    case TaskType::StoreVertices:
   	  return tarch::multicore::jobs::JobType::Job;
    case TaskType::Background:
   	  return tarch::multicore::jobs::JobType::BackgroundTask;
    case TaskType::IsBandwidthBoundTask:
      return tarch::multicore::jobs::JobType::BandwidthBoundTask;
  }
  return tarch::multicore::jobs::JobType::ProcessImmediately;
}


void peano::datatraversal::TaskSet::waitForLoadCellsTask() {
  logDebug( "waitForAllLoadCellsTasks()", "start to wait for load cell tasks" );
  tarch::multicore::jobs::processJobs(translateIntoJobClass(TaskType::LoadCells));
  logDebug( "waitForAllLoadCellsTasks()", "wait for load cell tasks finished" );
}


void peano::datatraversal::TaskSet::waitForLoadVerticesTask() {
  logDebug( "waitForAllLoadVerticesTasks()", "start to wait for load vertices tasks" );
  tarch::multicore::jobs::processJobs(translateIntoJobClass(TaskType::LoadVertices));
  logDebug( "waitForAllLoadVerticesTasks()", "wait for load vertices tasks finished" );
}


void peano::datatraversal::TaskSet::waitForEventTask() {
  logDebug( "waitForAllEventTasks()", "start to wait for event tasks" );
  tarch::multicore::jobs::processJobs(translateIntoJobClass(TaskType::TriggerEvents));
  logDebug( "waitForAllEventTasks()", "wait for event tasks finished" );
}


void peano::datatraversal::TaskSet::waitForStoreCellsTask() {
  logDebug( "waitForAllStoreCellsTasks()", "start to wait for store cell tasks" );
  tarch::multicore::jobs::processJobs(translateIntoJobClass(TaskType::StoreCells));
  logDebug( "waitForAllStoreCellsTasks()", "wait for store cell tasks finished" );
}


void peano::datatraversal::TaskSet::waitForStoreVerticesTask() {
  logDebug( "waitForAllStoreVerticesTasks()", "start to wait for store vertices tasks" );
  tarch::multicore::jobs::processJobs(translateIntoJobClass(TaskType::StoreVertices));
  logDebug( "waitForAllStoreVerticesTasks()", "wait for store vertices tasks finished" );
}


peano::datatraversal::TaskSet::TaskSet(
  std::function<bool ()>&& function1,
  std::function<bool ()>&& function2,
  TaskType                 type1,
  TaskType                 type2,
  bool                     parallelise
) {
  if (parallelise) {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(2,2);

    #if defined(TBBInvade)
    shminvade::SHMInvade invade( 2 );
    #endif

    tarch::multicore::jobs::spawnAndWait(
      function1,
	  function2,
	  translateIntoJobType(type1),
	  translateIntoJobType(type2),
	  translateIntoJobClass(type1),
	  translateIntoJobClass(type2)
    );

    #if defined(TBBInvade)
    invade.retreat();
    #endif

    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(-2,-2);
  }
  else {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,2);
    function1();
    function2();
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,-2);
  }
}


peano::datatraversal::TaskSet::TaskSet(
  std::function<bool ()>&& function1,
  std::function<bool ()>&& function2,
  std::function<bool ()>&& function3,
  TaskType                 type1,
  TaskType                 type2,
  TaskType                 type3,
  bool                     parallelise
) {
  if (parallelise) {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(3,3);

    #if defined(TBBInvade)
    shminvade::SHMInvade invade( 3 );
    #endif

    tarch::multicore::jobs::spawnAndWait(
      function1,
	  function2,
	  function3,
	  translateIntoJobType(type1),
	  translateIntoJobType(type2),
	  translateIntoJobType(type3),
	  translateIntoJobClass(type1),
	  translateIntoJobClass(type2),
	  translateIntoJobClass(type3)
    );

    #if defined(TBBInvade)
    invade.retreat();
    #endif

    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(-3,-3);
  }
  else {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,3);
    function1();
    function2();
    function3();
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,-3);
  }
}


peano::datatraversal::TaskSet::TaskSet(
  std::function<bool ()>&& function1,
  std::function<bool ()>&& function2,
  std::function<bool ()>&& function3,
  std::function<bool ()>&& function4,
  TaskType                 type1,
  TaskType                 type2,
  TaskType                 type3,
  TaskType                 type4,
  bool                     parallelise
) {
  if (parallelise) {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(4,4);

    #if defined(TBBInvade)
    shminvade::SHMInvade invade( 4 );
    #endif

    tarch::multicore::jobs::spawnAndWait(
      function1,
	  function2,
	  function3,
	  function4,
	  translateIntoJobType(type1),
	  translateIntoJobType(type2),
	  translateIntoJobType(type3),
	  translateIntoJobType(type4),
	  translateIntoJobClass(type1),
	  translateIntoJobClass(type2),
	  translateIntoJobClass(type3),
	  translateIntoJobClass(type4)
    );

    #if defined(TBBInvade)
    invade.retreat();
    #endif

    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(-4,-4);
  }
  else {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,4);
    function1();
    function2();
    function3();
    function4();
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,-4);
  }
}


peano::datatraversal::TaskSet::TaskSet(
  std::function<bool ()>&& function1,
  std::function<bool ()>&& function2,
  std::function<bool ()>&& function3,
  std::function<bool ()>&& function4,
  std::function<bool ()>&& function5,
  TaskType                 type1,
  TaskType                 type2,
  TaskType                 type3,
  TaskType                 type4,
  TaskType                 type5,
  bool                     parallelise
) {
  if (parallelise) {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(4,4);

    #if defined(TBBInvade)
    shminvade::SHMInvade invade( 5 );
    #endif

    tarch::multicore::jobs::spawnAndWait(
      function1,
	  function2,
	  function3,
	  function4,
	  function5,
	  translateIntoJobType(type1),
	  translateIntoJobType(type2),
	  translateIntoJobType(type3),
	  translateIntoJobType(type4),
	  translateIntoJobType(type5),
	  translateIntoJobClass(type1),
	  translateIntoJobClass(type2),
	  translateIntoJobClass(type3),
	  translateIntoJobClass(type4),
	  translateIntoJobClass(type5)
    );

    #if defined(TBBInvade)
    invade.retreat();
    #endif

    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(-4,-4);
  }
  else {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,4);
    function1();
    function2();
    function3();
    function4();
    function5();
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,-4);
  }
}





peano::datatraversal::TaskSet::TaskSet(
  std::function<bool ()>&& function1,
  std::function<bool ()>&& function2,
  std::function<bool ()>&& function3,
  std::function<bool ()>&& function4,
  std::function<bool ()>&& function5,
  std::function<bool ()>&& function6,
  std::function<bool ()>&& function7,
  std::function<bool ()>&& function8,
  std::function<bool ()>&& function9,
  std::function<bool ()>&& function10,
  std::function<bool ()>&& function11,
  std::function<bool ()>&& function12,
  TaskType                 type1,
  TaskType                 type2,
  TaskType                 type3,
  TaskType                 type4,
  TaskType                 type5,
  TaskType                 type6,
  TaskType                 type7,
  TaskType                 type8,
  TaskType                 type9,
  TaskType                 type10,
  TaskType                 type11,
  TaskType                 type12,
  bool                     parallelise
) {
  if (parallelise) {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(12-1,12-1);

    #if defined(TBBInvade)
    shminvade::SHMInvade invade( 12 );
    #endif

    tarch::multicore::jobs::spawnAndWait(
      function1,
	  function2,
	  function3,
	  function4,
	  function5,
      function6,
	  function7,
	  function8,
	  function9,
	  function10,
	  function11,
	  function12,
	  translateIntoJobType(type1),
	  translateIntoJobType(type2),
	  translateIntoJobType(type3),
	  translateIntoJobType(type4),
	  translateIntoJobType(type5),
	  translateIntoJobType(type6),
	  translateIntoJobType(type7),
	  translateIntoJobType(type8),
	  translateIntoJobType(type9),
	  translateIntoJobType(type10),
	  translateIntoJobType(type11),
	  translateIntoJobType(type12),
	  translateIntoJobClass(type1),
	  translateIntoJobClass(type2),
	  translateIntoJobClass(type3),
	  translateIntoJobClass(type4),
	  translateIntoJobClass(type5),
	  translateIntoJobClass(type6),
	  translateIntoJobClass(type7),
	  translateIntoJobClass(type8),
	  translateIntoJobClass(type9),
	  translateIntoJobClass(type10),
	  translateIntoJobClass(type11),
	  translateIntoJobClass(type12)
    );

    #if defined(TBBInvade)
    invade.retreat();
    #endif

    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(-(12-1),-(12-1));
  }
  else {
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,12-1);
    function1();
    function2();
    function3();
    function4();
    function5();
    function6();
    function7();
    function8();
    function9();
    function10();
    function11();
    function12();
    peano::performanceanalysis::Analysis::getInstance().changeConcurrencyLevel(0,-(12-1));
  }
}


peano::datatraversal::TaskSet::TaskSet(
  std::function<bool ()>&&  myTask,
  TaskType                  taskType
) {
  typedef tarch::multicore::jobs::GenericJobWithCopyOfFunctor           Job;
  tarch::multicore::jobs::spawn( new Job(myTask,translateIntoJobType(taskType),translateIntoJobClass(taskType), tarch::multicore::DefaultPriority ) );
}


void peano::datatraversal::TaskSet::startToProcessBackgroundJobs() {
  #if defined(TBBInvade)
  if (_backgroundTaskInvade==nullptr) {
	_backgroundTaskInvade = new shminvade::SHMInvade(
	  tarch::multicore::Core::getInstance().getNumberOfThreads()
    );
  }
  #endif

  tarch::multicore::jobs::startToProcessBackgroundJobs();
}


bool peano::datatraversal::TaskSet::finishToProcessBackgroundJobs() {
  bool result = tarch::multicore::jobs::finishToProcessBackgroundJobs();
  peano::performanceanalysis::Analysis::getInstance().minuteNumberOfBackgroundTasks(
    tarch::multicore::jobs::getNumberOfWaitingBackgroundJobs()
  );

  #if defined(TBBInvade)
  if (_backgroundTaskInvade!=nullptr) {
	delete _backgroundTaskInvade;
	_backgroundTaskInvade= nullptr;
  }
  #endif

  return result;
}
