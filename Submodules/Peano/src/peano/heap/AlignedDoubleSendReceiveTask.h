// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _PEANO_HEAP_ALIGNED_DOUBLE_SEND_RECEIVE_TASK_H_
#define _PEANO_HEAP_ALIGNED_DOUBLE_SEND_RECEIVE_TASK_H_


#include "peano/utils/Globals.h"
#include "peano/utils/PeanoOptimisations.h"
#include "peano/heap/records/MetaInformation.h"
#include "peano/heap/HeapAllocator.h"


#include <vector>

#ifdef Parallel
#include <mpi.h>
#endif


namespace peano {
  namespace heap {
    template<int Alignment>
    class AlignedDoubleSendReceiveTask;
  }
}


/**
 * @see Generic (template) class. This is a specialisation with the same semantics.
 */
template<int Alignment>
class peano::heap::AlignedDoubleSendReceiveTask {
  public:
    typedef peano::heap::records::MetaInformation          MetaInformation;

  private:
    typedef std::vector< double, HeapAllocator<double, Alignment > >  DataVectorType;

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
    double*         _data;

    bool            _freeDataPointer;
  public:
    AlignedDoubleSendReceiveTask();

    void wrapData(const double* const data);
    void sendDataDirectlyFromBuffer(const double* const data);
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
    bool hasDataExchangeFinished();

    MetaInformation& getMetaInformation();
    MetaInformation getMetaInformation() const;
};


#include "peano/heap/AlignedDoubleSendReceiveTask.cpph"


#endif
