/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 *
 * \author Dominic E. Charrier, Ben Hazelwood
 **/

#ifndef _EXAHYPE_PARALLEL_NonBlockingExchanger_H_
#define _EXAHYPE_PARALLEL_NonBlockingExchanger_H_

#ifdef Parallel
#include <mpi.h>

#include <stdlib.h>
#include <vector>
#include <map>

#include "peano/utils/Globals.h"

#include "tarch/logging/Log.h"

#include "tarch/la/Vector.h"

#include "tarch/multicore/Lock.h"
#include "tarch/multicore/BooleanSemaphore.h"

namespace exahype {
namespace parallel {

class AbstractNonBlockingExchanger {
public:
  virtual ~AbstractNonBlockingExchanger(){}

  virtual bool useProgressThread() const = 0;
  virtual void progressAllSendAndReceiveTasks() = 0;
  virtual void finishedPostingSendsAndReceives(const bool isTraverselInverted) = 0;
};

extern tarch::multicore::BooleanSemaphore ProgressSemaphore;

extern std::vector<AbstractNonBlockingExchanger*> NonBlockingExchangers;

extern void allNonBlockingExchangersFinishedPostingSendsAndReceives(const bool isTraverselInverted);

/**
 * This is a job which simply calls
 * progressAllSendAndReceiveTasks while the main thread can continue performing
 * and orchestrating computations.
 */
class MPIBackgroundJob {
public:
  MPIBackgroundJob();
  bool operator()();
};

template<int Alignment,typename Type,MPI_Datatype MPIType,bool copySendBuffer>
class NonBlockingExchanger : public AbstractNonBlockingExchanger {
private:
  static tarch::logging::Log _log;

  static tarch::multicore::BooleanSemaphore ModifyTasksSemaphore;

public:

  typedef struct SendReceiveTask {
    // meta data
    const tarch::la::Vector<DIMENSIONS,double> _x;
    const int _level;
    const int _userTag; // this is not the MPI tag
    // MPI
    Type*       _sendBuffer;
    Type*       _receiveBuffer;
    MPI_Request _sendRequest;
    MPI_Request _receiveRequest;
    bool        _complete;

    SendReceiveTask(
        const tarch::la::Vector<DIMENSIONS,double>& x,
        const int                                   level,
        const int                                   userTag // not the MPI tag
    ) :
      _x(x),
      _level(level),
      _userTag(userTag), // not the MPI tag
      _sendBuffer(nullptr),
      _receiveBuffer(nullptr),
      _sendRequest(),
      _receiveRequest(),
      _complete(false)
    {}

    bool matches(
        const tarch::la::Vector<DIMENSIONS,double>& x,
        const int                                   level,
        const int                                   userTag) {// not the MPI tag
      return
          level==_level &&
          userTag==_userTag &&
          tarch::la::equals(_x,x,
              tarch::la::NUMERICAL_ZERO_DIFFERENCE *
              std::max( 1.0, std::max( tarch::la::maxAbs(_x), tarch::la::maxAbs(x) ) ) );
    }

  } SendReceiveTask;

private:
  /**
   * Communicator of the asynchronous exchangers.
   *
   * We do not want to interfere with Peano's message passing.
   */
  MPI_Comm _communicator;

  /** per rank a list of tasks */
  std::map<int,std::vector<SendReceiveTask*>> _tasks;

  /** counter for the number of SendReceiveTasks */
  int _numberOfNewSendReceiveTasks       = 0;
  int _numberOfProcessedSendReceiveTasks = 0;
  int _numberOfDeletedSendReceiveTasks   = 0;

  /** indicates if traversal was reversed while posting sends and receives */
  bool _pickupMessagesInReversedOrder    = false;

  /** name of this exchanger */
  std::string _name = "unknown";
  bool _useProgressThread = false;

  NonBlockingExchanger();

  /**
   * Allocate a receive or send buffer.
   */
  Type* allocateBuffer(const int size,const bool aligned) const;

  /** Loop bodies for the public methods with same name */
  Type* waitForSendAndReceiveTaskToComplete(SendReceiveTask* task);
  void deleteSendAndReceiveTask(SendReceiveTask* task);

public:

  static NonBlockingExchanger& getInstance();

  /**
   * Assign the exchanger a name (identifier) such that messages from the exchanger can
   * be assigned to the exchanger type. In particular important if you handle
   * multiple exchangers, so you can distinguish error messages, e.g.
   *
   * Further specify if the exchanger should deploy the progression
   * of MPI messages to a thread.
   * The thread will progress all messages until they are completed.
   * We do not use a dedicated MPI thread so after the thread finishes,
   * it is free to use for other jobs such as computations.
   */
  void configure(std::string name,const bool useProgressThread);

  bool useProgressThread() const final override;

  /**
   * \param sendBuffer        a pointer to the send buffer.
   * \param sendBufferSize    the size of the receive buffer, i.e.
   *                          the length of the message to send out (might be chosen as 0).
   * \param receiveBufferSize the size of the receive buffer. Note that this is not necessarily the size of
   *                          the expected message. However, the expected message should
   *                          fit into the receive buffer. This must be ensured by you.
   *
   * \note Not thread-safe as standard vector
   * is modified.
   */
  void postSendAndReceive(
      const Type* const                           sendBuffer,
      const int                                   sendBufferSize,
      const int                                   receiveBufferSize,
      const int                                   rank,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int                                   level,
      const int                                   userTag);

  /**
   * Block until send and receive request are completed.
   *
   * \return pointer to the start of the receive buffer.
   * You have to know what length the received message has.
   */
  Type* waitForSendAndReceiveTaskToComplete(
      const int                                   rank,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int                                   level,
      const int                                   userTag);

  /**
   * Delete a SendAndReceiveTask.
   *
   * \note Not thread-safe as standard vector
   * is modified.
   */
  void deleteSendAndReceiveTask(
      const int                                   rank,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int                                   level,
      const int                                   userTag);

  /**
   * Notify the exchanger that we are done with
   * posting sends and receives.
   *
   * \param isTraversalInverted is a hint for looking
   * up send receive tasks in the next iteration.
   */
  void finishedPostingSendsAndReceives(const bool isTraverselInverted) final override;

  /**
   * A background task can work with this.
   *
   * \note Thread-safe.
   */
   void progressAllSendAndReceiveTasks() final override;
};

} /* namespace parallel */
} /* namespace exahype */

#include "exahype/parallel/NonBlockingExchanger.cpph"

#endif // #ifdef Parallel

#endif /* _EXAHYPE_PARALLEL_NonBlockingExchanger_H_ */
