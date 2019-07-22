// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _PEANO_HEAP_SEND_RECEIVE_TASK_H_
#define _PEANO_HEAP_SEND_RECEIVE_TASK_H_


#include "peano/utils/Globals.h"
#include "peano/utils/PeanoOptimisations.h"
#include "peano/heap/records/MetaInformation.h"


#include <vector>

#ifdef Parallel
#include <mpi.h>
#endif



namespace peano {
  namespace heap {
    template<class Data>
    struct SendReceiveTask;

    /**
     * Specialisation
     */
    template<>
    struct peano::heap::SendReceiveTask<double>;

    template<>
    struct peano::heap::SendReceiveTask<char>;
  }
}


#ifdef PackRecordsInHeaps
 #pragma pack (push, 1)
#endif


/**
 * Wrapper for a send or receive task
 *
 * Holds the information for a send or receive task, i.e. it holds the MPI
 * request to check weather the task has been finished (both sends and
 * receives are realised as non-blocking MPI data exchange) as well as a
 * pointer to the allocated data where the copy of the data is
 * stored (send task) or is to be stored (receive task).
 */
template<class Data>
class peano::heap::SendReceiveTask {
  public:
    /**
     * We always use the plain meta information as record, i.e. we do not pack
     * anything here as the meta information usually is one integer only
     * anyway.
     */
    typedef peano::heap::records::MetaInformation          MetaInformation;
  private:
    static tarch::logging::Log _log;

    #ifdef Parallel
    MPI_Request     _request;
    #endif

    MetaInformation _metaInformation;

    /**
     * Without semantics for send tasks but important for receive tasks as we
     * have to store from which rank the data arrived from.
     */
    int             _rank;

    /**
     * Pointer to the actual data. If meta data marks a message without
     * content, this pointer is 0.
     */
    Data*           _data;

    bool            _freeDataPointer;

    bool            _dataExchangeHasCompleted;
  public:
    SendReceiveTask();

    bool hasDataExchangeFinished();

    /**
     * Prelude to sendData().
     *
     * Please note that you have the responsibility to free data yourself.
     * Typically, data is from a heap map and will be reused later on, so
     * there's nothing to care about. However, the task object has to
     * call delete[] on its internal copy held in _data. This is done through
     * operation freeMemory(), i.e. it is absolutely essential that the user
     * invokes freeMemory(). freeMemory() actually calls delete if and only
     * if the internal flag _freeDataPointer is set. So we do it here.
     *
     * We assume that the meta data already encodes the correct size.
     */
    void wrapData(const Data* const data);

    /**
     * Counterpart of wrapData(). The task will send away the data directly from the
     * specified buffer when you later call triggerSend(). Please call
     * freeMemory() nevertheless. The task memorised internally through
     * _freeDataPointer whether to call delete or not, i.e. by calling
     * freeMemory(), you are always on the safe side.
     *
     * We assume that the meta data already encodes the correct size.
     */
    void sendDataDirectlyFromBuffer(const Data* const data);

    /**
     * Trigger send issues the actual send. Before this, you either have to
     * configure the send through sendDataDirectlyFromBuffer() or through
     * wrapData().
     *
     *
     * @see triggerReceive() for implementation remarks.
     */
    void triggerSend(int tag);

    /**
     * If you use the task in combination with containers, please push or pop
     * data first and then call trigger. Trigger uses MPI and MPI uses memory
     * references. If you move the tasks around later on, MPI references invalid
     * memory addresses and data consistency is not given anymore.
     *
     * triggerReceive() internally issues the MPI send. For this, it needs a
     * well-suited receive buffer. So the first thing the routine does is that
     * it actually allocs a reasonable buffer. This buffer is to be deleted
     * later on again through freeMemory(), i.e. it is absolutely essential
     * that the user calls freeMemory(). However, freeMemory() frees data if
     * and only if the internal flag _freeDataPointer is set. So the trigger
     * has to set _freeDataPointer.
     *
     * @see   BoundaryDataExchanger::receiveDanglingMessages()
     */
    void triggerReceive(int tag);

    /**
     * Frees local memory. Is safe to call even if the message might be empty. Is
     * not safe to call if you don't work with copies and the message length is
     * bigger than 0.
     */
    void freeMemory();

    /**
     * Set a task invalid explicitly. Messages marked that way will pass the
     * validation though their data is not in agreement with checks: it is
     * explicitly known that the message is invalid and can be ignored. I use
     * this for null messages, i.e. messages without content that are often
     * squeezed (together with their meta data) by sophisticated communication
     * schemes.
     */
    void setInvalid();

    /**
     * A task fits if it is
     *
     * - either invalid (see setInvalid())
     * - or position and level coincide.
     *
     * Fits should only be called in assert mode. However, some compiler seem to
     * translate it also if the function is not used at all. For them, I provide
     * a non-asserts version returning true all the time.
     */
    bool fits(
      const tarch::la::Vector<DIMENSIONS, double>&  position,
      int                                           level
    ) const;

    std::string toString() const;

    Data* data();
    const Data* data() const;
    int getRank() const;
    void setRank(int value);

    bool hasCommunicationCompleted();

    MetaInformation& getMetaInformation();
    MetaInformation getMetaInformation() const;
};




/**
 * @see Generic (template) class. This is a specialisation with the same semantics.
 */
template<>
class peano::heap::SendReceiveTask<double> {
  public:
    typedef peano::heap::records::MetaInformation          MetaInformation;
  private:
    static tarch::logging::Log _log;

    #ifdef Parallel
    MPI_Request     _request;
    #endif

    MetaInformation _metaInformation;

    int             _rank;

    double*         _data;

    bool            _freeDataPointer;

    bool            _dataExchangeHasCompleted;
  public:
    SendReceiveTask();

    bool hasDataExchangeFinished();
    void wrapData(const double* const data);
    void sendDataDirectlyFromBuffer(const double* const  data);
    void triggerSend(int tag);
    void triggerReceive(int tag);
    void freeMemory();
    void setInvalid();
    bool fits(
      const tarch::la::Vector<DIMENSIONS, double>&  position,
      int                                           level
    ) const;

    std::string toString() const;
    double* data();
    const double* data() const;
    int getRank() const;
    void setRank(int value);

    bool hasCommunicationCompleted();
    MetaInformation& getMetaInformation();
    MetaInformation getMetaInformation() const;
};




/**
 * @see Generic (template) class. This is a specialisation with the same semantics.
 */
template<>
class peano::heap::SendReceiveTask<char> {
  public:
    typedef peano::heap::records::MetaInformation          MetaInformation;

  private:
    static tarch::logging::Log _log;


    #ifdef Parallel
    MPI_Request     _request;
    #endif

    MetaInformation _metaInformation;

    int             _rank;

    char*           _data;

    bool            _freeDataPointer;

    bool            _dataExchangeHasCompleted;
  public:
    SendReceiveTask();

    void wrapData(const char* const data);
    void sendDataDirectlyFromBuffer(const char* const  data);
    void triggerSend(int tag);
    void triggerReceive(int tag);
    void freeMemory();
    void setInvalid();
    bool fits(
      const tarch::la::Vector<DIMENSIONS, double>&  position,
      int                                           level
    ) const;

    std::string toString() const;
    char* data();
    const char* data() const;
    int getRank() const;
    void setRank(int value);

    bool hasCommunicationCompleted();
    bool hasDataExchangeFinished();
    MetaInformation& getMetaInformation();
    MetaInformation getMetaInformation() const;
};

#ifdef PackRecordsInHeaps
#pragma pack (pop)
#endif


#include "peano/heap/SendReceiveTask.cpph"


#endif
