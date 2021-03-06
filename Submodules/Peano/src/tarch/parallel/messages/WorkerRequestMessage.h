#ifndef _TARCH_PARALLEL_MESSAGES_WORKERREQUESTMESSAGE_H
#define _TARCH_PARALLEL_MESSAGES_WORKERREQUESTMESSAGE_H

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "peano/utils/PeanoOptimisations.h"
#ifdef Parallel
	#include "tarch/parallel/Node.h"
#endif
#ifdef Parallel
	#include <mpi.h>
#endif
#include "tarch/logging/Log.h"
#include "tarch/la/Vector.h"
#include <bitset>
#include <complex>
#include <string>
#include <iostream>

namespace tarch {
   namespace parallel {
      namespace messages {
         class WorkerRequestMessage;
         class WorkerRequestMessagePacked;
      }
   }
}


namespace mpibalancing {
  namespace tests {
    class SFCDiffusionNodePoolStrategyTest;
  }
}


/**
 * @author This class is generated by DaStGen
 * 		   DataStructureGenerator (DaStGen)
 * 		   2007-2009 Wolfgang Eckhardt
 * 		   2012      Tobias Weinzierl
 *
 * 		   build date: 09-02-2014 14:40
 *
 * @date   31/03/2018 19:30
 */
class tarch::parallel::messages::WorkerRequestMessage { 
   
   public:
      
      typedef tarch::parallel::messages::WorkerRequestMessagePacked Packed;
      
      
      friend class mpibalancing::tests::SFCDiffusionNodePoolStrategyTest;
      struct PersistentRecords {
         int _numberOfRequestedWorkers;
         /**
          * Generated
          */
         PersistentRecords();
         
         /**
          * Generated
          */
         PersistentRecords(const int& numberOfRequestedWorkers);
         
         /**
          * Generated
          */
          int getNumberOfRequestedWorkers() const ;
         
         /**
          * Generated
          */
          void setNumberOfRequestedWorkers(const int& numberOfRequestedWorkers) ;
         
         
      };
      private: 
         PersistentRecords _persistentRecords;
         
      public:
         /**
          * Generated
          */
         WorkerRequestMessage();
         
         /**
          * Generated
          */
         WorkerRequestMessage(const PersistentRecords& persistentRecords);
         
         /**
          * Generated
          */
         WorkerRequestMessage(const int& numberOfRequestedWorkers);
         
         /**
          * Generated
          */
         virtual ~WorkerRequestMessage();
         
         /**
          * Generated
          */
          int getNumberOfRequestedWorkers() const ;
         
         /**
          * Generated
          */
          void setNumberOfRequestedWorkers(const int& numberOfRequestedWorkers) ;
         
         /**
          * Generated
          */
         std::string toString() const;
         
         /**
          * Generated
          */
         void toString(std::ostream& out) const;
         
         
         PersistentRecords getPersistentRecords() const;
         /**
          * Generated
          */
         WorkerRequestMessagePacked convert() const;
         
         
      #ifdef Parallel
         protected:
            static tarch::logging::Log _log;
            
            int _senderDestinationRank;
            
         public:
            
            /**
             * Global that represents the mpi datatype.
             * There are two variants: Datatype identifies only those attributes marked with
             * parallelise. FullDatatype instead identifies the whole record with all fields.
             */
            static MPI_Datatype Datatype;
            static MPI_Datatype FullDatatype;
            
            /**
             * Initializes the data type for the mpi operations. Has to be called
             * before the very first send or receive operation is called.
             */
            static void initDatatype();
            
            static void shutdownDatatype();
            
            enum class ExchangeMode { Blocking, NonblockingWithPollingLoopOverTests, LoopOverProbeWithBlockingReceive };
            
            void send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode );
            
            void receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode );
            
            static bool isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise);
            
            int getSenderRank() const;
            #endif
   
};

/**
 * @author This class is generated by DaStGen
 * 		   DataStructureGenerator (DaStGen)
 * 		   2007-2009 Wolfgang Eckhardt
 * 		   2012      Tobias Weinzierl
 *
 * 		   build date: 09-02-2014 14:40
 *
 * @date   31/03/2018 19:30
 */
class tarch::parallel::messages::WorkerRequestMessagePacked { 
   
   public:
      
      
      friend class mpibalancing::tests::SFCDiffusionNodePoolStrategyTest;
      struct PersistentRecords {
         int _numberOfRequestedWorkers;
         /**
          * Generated
          */
         PersistentRecords();
         
         /**
          * Generated
          */
         PersistentRecords(const int& numberOfRequestedWorkers);
         
         /**
          * Generated
          */
          int getNumberOfRequestedWorkers() const ;
         
         /**
          * Generated
          */
          void setNumberOfRequestedWorkers(const int& numberOfRequestedWorkers) ;
         
         
      };
      private: 
         PersistentRecords _persistentRecords;
         
      public:
         /**
          * Generated
          */
         WorkerRequestMessagePacked();
         
         /**
          * Generated
          */
         WorkerRequestMessagePacked(const PersistentRecords& persistentRecords);
         
         /**
          * Generated
          */
         WorkerRequestMessagePacked(const int& numberOfRequestedWorkers);
         
         /**
          * Generated
          */
         virtual ~WorkerRequestMessagePacked();
         
         /**
          * Generated
          */
          int getNumberOfRequestedWorkers() const ;
         
         /**
          * Generated
          */
          void setNumberOfRequestedWorkers(const int& numberOfRequestedWorkers) ;
         
         /**
          * Generated
          */
         std::string toString() const;
         
         /**
          * Generated
          */
         void toString(std::ostream& out) const;
         
         
         PersistentRecords getPersistentRecords() const;
         /**
          * Generated
          */
         WorkerRequestMessage convert() const;
         
         
      #ifdef Parallel
         protected:
            static tarch::logging::Log _log;
            
            int _senderDestinationRank;
            
         public:
            
            /**
             * Global that represents the mpi datatype.
             * There are two variants: Datatype identifies only those attributes marked with
             * parallelise. FullDatatype instead identifies the whole record with all fields.
             */
            static MPI_Datatype Datatype;
            static MPI_Datatype FullDatatype;
            
            /**
             * Initializes the data type for the mpi operations. Has to be called
             * before the very first send or receive operation is called.
             */
            static void initDatatype();
            
            static void shutdownDatatype();
            
            enum class ExchangeMode { Blocking, NonblockingWithPollingLoopOverTests, LoopOverProbeWithBlockingReceive };
            
            void send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode );
            
            void receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode );
            
            static bool isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise);
            
            int getSenderRank() const;
            #endif
   
};

#endif

