#include "exahype/records/RepositoryState.h"

exahype::records::RepositoryState::PersistentRecords::PersistentRecords() {
   
}


exahype::records::RepositoryState::PersistentRecords::PersistentRecords(const Action& action, const int& numberOfIterations, const bool& exchangeBoundaryVertices):
_action(action),
_numberOfIterations(numberOfIterations),
_exchangeBoundaryVertices(exchangeBoundaryVertices) {
   
}

exahype::records::RepositoryState::RepositoryState() {
   
}


exahype::records::RepositoryState::RepositoryState(const PersistentRecords& persistentRecords):
_persistentRecords(persistentRecords._action, persistentRecords._numberOfIterations, persistentRecords._exchangeBoundaryVertices) {
   
}


exahype::records::RepositoryState::RepositoryState(const Action& action, const int& numberOfIterations, const bool& exchangeBoundaryVertices):
_persistentRecords(action, numberOfIterations, exchangeBoundaryVertices) {
   
}


exahype::records::RepositoryState::~RepositoryState() { }

std::string exahype::records::RepositoryState::toString(const Action& param) {
   switch (param) {
      case WriteCheckpoint: return "WriteCheckpoint";
      case ReadCheckpoint: return "ReadCheckpoint";
      case Terminate: return "Terminate";
      case RunOnAllNodes: return "RunOnAllNodes";
      case UseAdapterMeshRefinement: return "UseAdapterMeshRefinement";
      case UseAdapterMeshRefinementAndPlotTree: return "UseAdapterMeshRefinementAndPlotTree";
      case UseAdapterFinaliseMeshRefinement: return "UseAdapterFinaliseMeshRefinement";
      case UseAdapterFinaliseMeshRefinementOrLocalRollback: return "UseAdapterFinaliseMeshRefinementOrLocalRollback";
      case UseAdapterInitialPrediction: return "UseAdapterInitialPrediction";
      case UseAdapterFusedTimeStep: return "UseAdapterFusedTimeStep";
      case UseAdapterPredictionRerun: return "UseAdapterPredictionRerun";
      case UseAdapterBroadcastAndDropNeighbourMessages: return "UseAdapterBroadcastAndDropNeighbourMessages";
      case UseAdapterRefinementStatusSpreading: return "UseAdapterRefinementStatusSpreading";
      case UseAdapterPredictionOrLocalRecomputation: return "UseAdapterPredictionOrLocalRecomputation";
      case UseAdapterMergeNeighbours: return "UseAdapterMergeNeighbours";
      case UseAdapterUpdateAndReduce: return "UseAdapterUpdateAndReduce";
      case UseAdapterPrediction: return "UseAdapterPrediction";
      case UseAdapterCorrection: return "UseAdapterCorrection";
      case NumberOfAdapters: return "NumberOfAdapters";
   }
   return "undefined";
}

std::string exahype::records::RepositoryState::getActionMapping() {
   return "Action(WriteCheckpoint=0,ReadCheckpoint=1,Terminate=2,RunOnAllNodes=3,UseAdapterMeshRefinement=4,UseAdapterMeshRefinementAndPlotTree=5,UseAdapterFinaliseMeshRefinement=6,UseAdapterFinaliseMeshRefinementOrLocalRollback=7,UseAdapterInitialPrediction=8,UseAdapterFusedTimeStep=9,UseAdapterPredictionRerun=10,UseAdapterBroadcastAndDropNeighbourMessages=11,UseAdapterRefinementStatusSpreading=12,UseAdapterPredictionOrLocalRecomputation=13,UseAdapterMergeNeighbours=14,UseAdapterUpdateAndReduce=15,UseAdapterPrediction=16,UseAdapterCorrection=17,NumberOfAdapters=18)";
}


std::string exahype::records::RepositoryState::toString() const {
   std::ostringstream stringstr;
   toString(stringstr);
   return stringstr.str();
}

void exahype::records::RepositoryState::toString (std::ostream& out) const {
   out << "("; 
   out << "action:" << toString(getAction());
   out << ",";
   out << "numberOfIterations:" << getNumberOfIterations();
   out << ",";
   out << "exchangeBoundaryVertices:" << getExchangeBoundaryVertices();
   out <<  ")";
}


exahype::records::RepositoryState::PersistentRecords exahype::records::RepositoryState::getPersistentRecords() const {
   return _persistentRecords;
}

exahype::records::RepositoryStatePacked exahype::records::RepositoryState::convert() const{
   return RepositoryStatePacked(
      getAction(),
      getNumberOfIterations(),
      getExchangeBoundaryVertices()
   );
}

#ifdef Parallel
   tarch::logging::Log exahype::records::RepositoryState::_log( "exahype::records::RepositoryState" );
   
   MPI_Datatype exahype::records::RepositoryState::Datatype = 0;
   MPI_Datatype exahype::records::RepositoryState::FullDatatype = 0;
   
   
   void exahype::records::RepositoryState::initDatatype() {
      {
         RepositoryState dummyRepositoryState[2];
         
         #ifdef MPI2
         const int Attributes = 3;
         #else
         const int Attributes = 4;
         #endif
         MPI_Datatype subtypes[Attributes] = {
              MPI_INT		 //action
            , MPI_INT		 //numberOfIterations
            , MPI_CXX_BOOL		 //exchangeBoundaryVertices
            #ifndef MPI2
            , MPI_UB
            #endif
            
         };
         
         int blocklen[Attributes] = {
              1		 //action
            , 1		 //numberOfIterations
            , 1		 //exchangeBoundaryVertices
            #ifndef MPI2
            , 1
            #endif
            
         };
         
         MPI_Aint  disp[Attributes];
         MPI_Aint  base;
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState))), &base);
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState))), &base);
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._action))), 		&disp[0] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._action))), 		&disp[0] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #endif
         #ifdef MPI2
         for (int i=1; i<Attributes; i++) {
         #else
         for (int i=1; i<Attributes-1; i++) {
         #endif
            assertion1( disp[i] > disp[i-1], i );
         }
         #ifdef MPI2
         for (int i=0; i<Attributes; i++) {
         #else
         for (int i=0; i<Attributes-1; i++) {
         #endif
            disp[i] = disp[i] - base; // should be MPI_Aint_diff(disp[i], base); but this is not supported by most MPI-2 implementations
            assertion4(disp[i]<static_cast<int>(sizeof(RepositoryState)), i, disp[i], Attributes, sizeof(RepositoryState));
         }
         #ifndef MPI2
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[1]))), 		&disp[3] );
         disp[3] -= base;
         disp[3] += disp[0];
         #endif
         #ifdef MPI2
         MPI_Datatype tmpType; 
         MPI_Aint lowerBound, typeExtent; 
         MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
         MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
         MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &RepositoryState::Datatype );
         MPI_Type_commit( &RepositoryState::Datatype );
         #else
         MPI_Type_struct( Attributes, blocklen, disp, subtypes, &RepositoryState::Datatype);
         MPI_Type_commit( &RepositoryState::Datatype );
         #endif
         
      }
      {
         RepositoryState dummyRepositoryState[2];
         
         #ifdef MPI2
         const int Attributes = 3;
         #else
         const int Attributes = 4;
         #endif
         MPI_Datatype subtypes[Attributes] = {
              MPI_INT		 //action
            , MPI_INT		 //numberOfIterations
            , MPI_CXX_BOOL		 //exchangeBoundaryVertices
            #ifndef MPI2
            , MPI_UB
            #endif
            
         };
         
         int blocklen[Attributes] = {
              1		 //action
            , 1		 //numberOfIterations
            , 1		 //exchangeBoundaryVertices
            #ifndef MPI2
            , 1
            #endif
            
         };
         
         MPI_Aint  disp[Attributes];
         MPI_Aint  base;
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState))), &base);
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState))), &base);
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._action))), 		&disp[0] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._action))), 		&disp[0] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #endif
         #ifdef MPI2
         for (int i=1; i<Attributes; i++) {
         #else
         for (int i=1; i<Attributes-1; i++) {
         #endif
            assertion1( disp[i] > disp[i-1], i );
         }
         #ifdef MPI2
         for (int i=0; i<Attributes; i++) {
         #else
         for (int i=0; i<Attributes-1; i++) {
         #endif
            disp[i] = disp[i] - base; // should be MPI_Aint_diff(disp[i], base); but this is not supported by most MPI-2 implementations
            assertion4(disp[i]<static_cast<int>(sizeof(RepositoryState)), i, disp[i], Attributes, sizeof(RepositoryState));
         }
         #ifndef MPI2
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryState[1]))), 		&disp[3] );
         disp[3] -= base;
         disp[3] += disp[0];
         #endif
         #ifdef MPI2
         MPI_Datatype tmpType; 
         MPI_Aint lowerBound, typeExtent; 
         MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
         MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
         MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &RepositoryState::FullDatatype );
         MPI_Type_commit( &RepositoryState::FullDatatype );
         #else
         MPI_Type_struct( Attributes, blocklen, disp, subtypes, &RepositoryState::FullDatatype);
         MPI_Type_commit( &RepositoryState::FullDatatype );
         #endif
         
      }
      
   }
   
   
   void exahype::records::RepositoryState::shutdownDatatype() {
      MPI_Type_free( &RepositoryState::Datatype );
      MPI_Type_free( &RepositoryState::FullDatatype );
      
   }
   
   void exahype::records::RepositoryState::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
      // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::RepositoryState " 
             << toString() 
             << " to node " << destination 
             << ": " << tarch::parallel::MPIReturnValueToString(result); 
         _log.error( "send(int)",msg.str() ); 
       } 
    } 
    break; 
   case ExchangeMode::NonblockingWithPollingLoopOverTests: 
    {
      MPI_Request* sendRequestHandle = new MPI_Request(); 
      int          flag = 0; 
       int          result; 
       clock_t      timeOutWarning   = -1; 
       clock_t      timeOutShutdown  = -1; 
       bool         triggeredTimeoutWarning = false;  
       result = MPI_Isend(  
         this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination,  
         tag, tarch::parallel::Node::getInstance().getCommunicator(), 
         sendRequestHandle  
       ); 
       if  (result!=MPI_SUCCESS) {  
         std::ostringstream msg;  
         msg << "was not able to send message exahype::records::RepositoryState "  
             << toString() 
             << " to node " << destination 
             << ": " << tarch::parallel::MPIReturnValueToString(result);  
         _log.error( "send(int)",msg.str() );  
       }  
       result = MPI_Test( sendRequestHandle, &flag, MPI_STATUS_IGNORE ); 
       while (!flag) { 
         if (timeOutWarning==-1)   timeOutWarning   = tarch::parallel::Node::getInstance().getDeadlockWarningTimeStamp(); 
         if (timeOutShutdown==-1)  timeOutShutdown  = tarch::parallel::Node::getInstance().getDeadlockTimeOutTimeStamp(); 
         result = MPI_Test( sendRequestHandle, &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
           std::ostringstream msg; 
           msg << "testing for finished send task for exahype::records::RepositoryState " 
               << toString() 
               << " sent to node " << destination 
               << " failed: " << tarch::parallel::MPIReturnValueToString(result); 
           _log.error("send(int)", msg.str() ); 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutWarningEnabled() && 
           (clock()>timeOutWarning) && 
           (!triggeredTimeoutWarning) 
         ) { 
           tarch::parallel::Node::getInstance().writeTimeOutWarning( 
             "exahype::records::RepositoryState", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::RepositoryState", 
             "send(int)", destination,tag,1 
           ); 
         } 
 	       tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
       } 
       delete sendRequestHandle; 
     }  
     break; 
   case ExchangeMode::LoopOverProbeWithBlockingReceive: 
    assertionMsg(false,"should not be called"); 
    break; 
} 
 // ============================= 
// end injected snippet/aspect 
// ============================= 

      
   }
   
   
   
   void exahype::records::RepositoryState::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
      // ============================= 
// start injected snippet/aspect 
// ============================= 
MPI_Status status; 
switch (mode) { 
  case ExchangeMode::Blocking: 
    { 
      const int   result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::RepositoryState from node " 
            << source << ": " << tarch::parallel::MPIReturnValueToString(result); 
        _log.error( "receive(int)", msg.str() ); 
      } 
    } 
    break; 
  case ExchangeMode::NonblockingWithPollingLoopOverTests: 
    { 
      int          flag = 0; 
      int          result; 
      clock_t      timeOutWarning   = -1; 
      clock_t      timeOutShutdown  = -1; 
      bool         triggeredTimeoutWarning = false; 
      MPI_Request* sendRequestHandle = new MPI_Request(); 
       result = MPI_Irecv( 
        this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, 
        tarch::parallel::Node::getInstance().getCommunicator(), sendRequestHandle 
      ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::RepositoryState from node " 
             << source << ": " << tarch::parallel::MPIReturnValueToString(result); 
        _log.error( "receive(int)", msg.str() ); 
      } 
      result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      while (!flag) { 
        if (timeOutWarning==-1)   timeOutWarning   = tarch::parallel::Node::getInstance().getDeadlockWarningTimeStamp(); 
        if (timeOutShutdown==-1)  timeOutShutdown  = tarch::parallel::Node::getInstance().getDeadlockTimeOutTimeStamp(); 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutWarningEnabled() && 
          (clock()>timeOutWarning) && 
          (!triggeredTimeoutWarning) 
        ) { 
          tarch::parallel::Node::getInstance().writeTimeOutWarning( 
            "exahype::records::RepositoryState", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::RepositoryState", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::RepositoryState failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      delete sendRequestHandle; 
    }    break; 
  case ExchangeMode::LoopOverProbeWithBlockingReceive: 
    {
      int flag; 
      clock_t      timeOutWarning   = -1; 
      clock_t      timeOutShutdown  = -1; 
      bool         triggeredTimeoutWarning = false; 
      int result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
       if (result!=MPI_SUCCESS) { 
        std::ostringstream msg; 
        msg << "testing for finished receive task for exahype::records::RepositoryState failed: " 
            << tarch::parallel::MPIReturnValueToString(result); 
        _log.error("receive(int)", msg.str() ); 
      } 
      while (!flag) { 
        if (timeOutWarning==-1)   timeOutWarning   = tarch::parallel::Node::getInstance().getDeadlockWarningTimeStamp(); 
        if (timeOutShutdown==-1)  timeOutShutdown  = tarch::parallel::Node::getInstance().getDeadlockTimeOutTimeStamp(); 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutWarningEnabled() && 
          (clock()>timeOutWarning) && 
          (!triggeredTimeoutWarning) 
        ) { 
          tarch::parallel::Node::getInstance().writeTimeOutWarning( 
            "exahype::records::RepositoryState", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::RepositoryState", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::RepositoryState failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::RepositoryState from node " 
            << source << ": " << tarch::parallel::MPIReturnValueToString(result); 
        _log.error( "receive(int)", msg.str() ); 
      } 
    }
    break; 
  } 
// =========================== 
// end injected snippet/aspect 
// =========================== 

      
     _senderDestinationRank = source==MPI_ANY_SOURCE ? status.MPI_SOURCE : source;
   }
   
   
   
   bool exahype::records::RepositoryState::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
      MPI_Status status;
      int  flag        = 0;
      MPI_Iprobe(
         MPI_ANY_SOURCE, tag,
         tarch::parallel::Node::getInstance().getCommunicator(), &flag, &status
      );
      if (flag) {
         int  messageCounter;
         if (exchangeOnlyAttributesMarkedWithParallelise) {
            MPI_Get_count(&status, Datatype, &messageCounter);
         }
         else {
            MPI_Get_count(&status, FullDatatype, &messageCounter);
         }
         return messageCounter > 0;
      }
      else return false;
      
   }
   
   int exahype::records::RepositoryState::getSenderRank() const {
      assertion( _senderDestinationRank!=-1 );
      return _senderDestinationRank;
      
   }
#endif


exahype::records::RepositoryStatePacked::PersistentRecords::PersistentRecords() {
   
}


exahype::records::RepositoryStatePacked::PersistentRecords::PersistentRecords(const Action& action, const int& numberOfIterations, const bool& exchangeBoundaryVertices):
_action(action),
_numberOfIterations(numberOfIterations),
_exchangeBoundaryVertices(exchangeBoundaryVertices) {
   
}

exahype::records::RepositoryStatePacked::RepositoryStatePacked() {
   
}


exahype::records::RepositoryStatePacked::RepositoryStatePacked(const PersistentRecords& persistentRecords):
_persistentRecords(persistentRecords._action, persistentRecords._numberOfIterations, persistentRecords._exchangeBoundaryVertices) {
   
}


exahype::records::RepositoryStatePacked::RepositoryStatePacked(const Action& action, const int& numberOfIterations, const bool& exchangeBoundaryVertices):
_persistentRecords(action, numberOfIterations, exchangeBoundaryVertices) {
   
}


exahype::records::RepositoryStatePacked::~RepositoryStatePacked() { }

std::string exahype::records::RepositoryStatePacked::toString(const Action& param) {
   return exahype::records::RepositoryState::toString(param);
}

std::string exahype::records::RepositoryStatePacked::getActionMapping() {
   return exahype::records::RepositoryState::getActionMapping();
}



std::string exahype::records::RepositoryStatePacked::toString() const {
   std::ostringstream stringstr;
   toString(stringstr);
   return stringstr.str();
}

void exahype::records::RepositoryStatePacked::toString (std::ostream& out) const {
   out << "("; 
   out << "action:" << toString(getAction());
   out << ",";
   out << "numberOfIterations:" << getNumberOfIterations();
   out << ",";
   out << "exchangeBoundaryVertices:" << getExchangeBoundaryVertices();
   out <<  ")";
}


exahype::records::RepositoryStatePacked::PersistentRecords exahype::records::RepositoryStatePacked::getPersistentRecords() const {
   return _persistentRecords;
}

exahype::records::RepositoryState exahype::records::RepositoryStatePacked::convert() const{
   return RepositoryState(
      getAction(),
      getNumberOfIterations(),
      getExchangeBoundaryVertices()
   );
}

#ifdef Parallel
   tarch::logging::Log exahype::records::RepositoryStatePacked::_log( "exahype::records::RepositoryStatePacked" );
   
   MPI_Datatype exahype::records::RepositoryStatePacked::Datatype = 0;
   MPI_Datatype exahype::records::RepositoryStatePacked::FullDatatype = 0;
   
   
   void exahype::records::RepositoryStatePacked::initDatatype() {
      {
         RepositoryStatePacked dummyRepositoryStatePacked[2];
         
         #ifdef MPI2
         const int Attributes = 3;
         #else
         const int Attributes = 4;
         #endif
         MPI_Datatype subtypes[Attributes] = {
              MPI_INT		 //action
            , MPI_INT		 //numberOfIterations
            , MPI_CXX_BOOL		 //exchangeBoundaryVertices
            #ifndef MPI2
            , MPI_UB
            #endif
            
         };
         
         int blocklen[Attributes] = {
              1		 //action
            , 1		 //numberOfIterations
            , 1		 //exchangeBoundaryVertices
            #ifndef MPI2
            , 1
            #endif
            
         };
         
         MPI_Aint  disp[Attributes];
         MPI_Aint  base;
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked))), &base);
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked))), &base);
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._action))), 		&disp[0] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._action))), 		&disp[0] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #endif
         #ifdef MPI2
         for (int i=1; i<Attributes; i++) {
         #else
         for (int i=1; i<Attributes-1; i++) {
         #endif
            assertion1( disp[i] > disp[i-1], i );
         }
         #ifdef MPI2
         for (int i=0; i<Attributes; i++) {
         #else
         for (int i=0; i<Attributes-1; i++) {
         #endif
            disp[i] = disp[i] - base; // should be MPI_Aint_diff(disp[i], base); but this is not supported by most MPI-2 implementations
            assertion4(disp[i]<static_cast<int>(sizeof(RepositoryStatePacked)), i, disp[i], Attributes, sizeof(RepositoryStatePacked));
         }
         #ifndef MPI2
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[1]))), 		&disp[3] );
         disp[3] -= base;
         disp[3] += disp[0];
         #endif
         #ifdef MPI2
         MPI_Datatype tmpType; 
         MPI_Aint lowerBound, typeExtent; 
         MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
         MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
         MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &RepositoryStatePacked::Datatype );
         MPI_Type_commit( &RepositoryStatePacked::Datatype );
         #else
         MPI_Type_struct( Attributes, blocklen, disp, subtypes, &RepositoryStatePacked::Datatype);
         MPI_Type_commit( &RepositoryStatePacked::Datatype );
         #endif
         
      }
      {
         RepositoryStatePacked dummyRepositoryStatePacked[2];
         
         #ifdef MPI2
         const int Attributes = 3;
         #else
         const int Attributes = 4;
         #endif
         MPI_Datatype subtypes[Attributes] = {
              MPI_INT		 //action
            , MPI_INT		 //numberOfIterations
            , MPI_CXX_BOOL		 //exchangeBoundaryVertices
            #ifndef MPI2
            , MPI_UB
            #endif
            
         };
         
         int blocklen[Attributes] = {
              1		 //action
            , 1		 //numberOfIterations
            , 1		 //exchangeBoundaryVertices
            #ifndef MPI2
            , 1
            #endif
            
         };
         
         MPI_Aint  disp[Attributes];
         MPI_Aint  base;
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked))), &base);
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked))), &base);
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._action))), 		&disp[0] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._action))), 		&disp[0] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._numberOfIterations))), 		&disp[1] );
         #endif
         #ifdef MPI2
         MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #else
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[0]._persistentRecords._exchangeBoundaryVertices))), 		&disp[2] );
         #endif
         #ifdef MPI2
         for (int i=1; i<Attributes; i++) {
         #else
         for (int i=1; i<Attributes-1; i++) {
         #endif
            assertion1( disp[i] > disp[i-1], i );
         }
         #ifdef MPI2
         for (int i=0; i<Attributes; i++) {
         #else
         for (int i=0; i<Attributes-1; i++) {
         #endif
            disp[i] = disp[i] - base; // should be MPI_Aint_diff(disp[i], base); but this is not supported by most MPI-2 implementations
            assertion4(disp[i]<static_cast<int>(sizeof(RepositoryStatePacked)), i, disp[i], Attributes, sizeof(RepositoryStatePacked));
         }
         #ifndef MPI2
         MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyRepositoryStatePacked[1]))), 		&disp[3] );
         disp[3] -= base;
         disp[3] += disp[0];
         #endif
         #ifdef MPI2
         MPI_Datatype tmpType; 
         MPI_Aint lowerBound, typeExtent; 
         MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
         MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
         MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &RepositoryStatePacked::FullDatatype );
         MPI_Type_commit( &RepositoryStatePacked::FullDatatype );
         #else
         MPI_Type_struct( Attributes, blocklen, disp, subtypes, &RepositoryStatePacked::FullDatatype);
         MPI_Type_commit( &RepositoryStatePacked::FullDatatype );
         #endif
         
      }
      
   }
   
   
   void exahype::records::RepositoryStatePacked::shutdownDatatype() {
      MPI_Type_free( &RepositoryStatePacked::Datatype );
      MPI_Type_free( &RepositoryStatePacked::FullDatatype );
      
   }
   
   void exahype::records::RepositoryStatePacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
      // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::RepositoryStatePacked " 
             << toString() 
             << " to node " << destination 
             << ": " << tarch::parallel::MPIReturnValueToString(result); 
         _log.error( "send(int)",msg.str() ); 
       } 
    } 
    break; 
   case ExchangeMode::NonblockingWithPollingLoopOverTests: 
    {
      MPI_Request* sendRequestHandle = new MPI_Request(); 
      int          flag = 0; 
       int          result; 
       clock_t      timeOutWarning   = -1; 
       clock_t      timeOutShutdown  = -1; 
       bool         triggeredTimeoutWarning = false;  
       result = MPI_Isend(  
         this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination,  
         tag, tarch::parallel::Node::getInstance().getCommunicator(), 
         sendRequestHandle  
       ); 
       if  (result!=MPI_SUCCESS) {  
         std::ostringstream msg;  
         msg << "was not able to send message exahype::records::RepositoryStatePacked "  
             << toString() 
             << " to node " << destination 
             << ": " << tarch::parallel::MPIReturnValueToString(result);  
         _log.error( "send(int)",msg.str() );  
       }  
       result = MPI_Test( sendRequestHandle, &flag, MPI_STATUS_IGNORE ); 
       while (!flag) { 
         if (timeOutWarning==-1)   timeOutWarning   = tarch::parallel::Node::getInstance().getDeadlockWarningTimeStamp(); 
         if (timeOutShutdown==-1)  timeOutShutdown  = tarch::parallel::Node::getInstance().getDeadlockTimeOutTimeStamp(); 
         result = MPI_Test( sendRequestHandle, &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
           std::ostringstream msg; 
           msg << "testing for finished send task for exahype::records::RepositoryStatePacked " 
               << toString() 
               << " sent to node " << destination 
               << " failed: " << tarch::parallel::MPIReturnValueToString(result); 
           _log.error("send(int)", msg.str() ); 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutWarningEnabled() && 
           (clock()>timeOutWarning) && 
           (!triggeredTimeoutWarning) 
         ) { 
           tarch::parallel::Node::getInstance().writeTimeOutWarning( 
             "exahype::records::RepositoryStatePacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::RepositoryStatePacked", 
             "send(int)", destination,tag,1 
           ); 
         } 
 	       tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
       } 
       delete sendRequestHandle; 
     }  
     break; 
   case ExchangeMode::LoopOverProbeWithBlockingReceive: 
    assertionMsg(false,"should not be called"); 
    break; 
} 
 // ============================= 
// end injected snippet/aspect 
// ============================= 

      
   }
   
   
   
   void exahype::records::RepositoryStatePacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
      // ============================= 
// start injected snippet/aspect 
// ============================= 
MPI_Status status; 
switch (mode) { 
  case ExchangeMode::Blocking: 
    { 
      const int   result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::RepositoryStatePacked from node " 
            << source << ": " << tarch::parallel::MPIReturnValueToString(result); 
        _log.error( "receive(int)", msg.str() ); 
      } 
    } 
    break; 
  case ExchangeMode::NonblockingWithPollingLoopOverTests: 
    { 
      int          flag = 0; 
      int          result; 
      clock_t      timeOutWarning   = -1; 
      clock_t      timeOutShutdown  = -1; 
      bool         triggeredTimeoutWarning = false; 
      MPI_Request* sendRequestHandle = new MPI_Request(); 
       result = MPI_Irecv( 
        this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, 
        tarch::parallel::Node::getInstance().getCommunicator(), sendRequestHandle 
      ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::RepositoryStatePacked from node " 
             << source << ": " << tarch::parallel::MPIReturnValueToString(result); 
        _log.error( "receive(int)", msg.str() ); 
      } 
      result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      while (!flag) { 
        if (timeOutWarning==-1)   timeOutWarning   = tarch::parallel::Node::getInstance().getDeadlockWarningTimeStamp(); 
        if (timeOutShutdown==-1)  timeOutShutdown  = tarch::parallel::Node::getInstance().getDeadlockTimeOutTimeStamp(); 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutWarningEnabled() && 
          (clock()>timeOutWarning) && 
          (!triggeredTimeoutWarning) 
        ) { 
          tarch::parallel::Node::getInstance().writeTimeOutWarning( 
            "exahype::records::RepositoryStatePacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::RepositoryStatePacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::RepositoryStatePacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      delete sendRequestHandle; 
    }    break; 
  case ExchangeMode::LoopOverProbeWithBlockingReceive: 
    {
      int flag; 
      clock_t      timeOutWarning   = -1; 
      clock_t      timeOutShutdown  = -1; 
      bool         triggeredTimeoutWarning = false; 
      int result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
       if (result!=MPI_SUCCESS) { 
        std::ostringstream msg; 
        msg << "testing for finished receive task for exahype::records::RepositoryStatePacked failed: " 
            << tarch::parallel::MPIReturnValueToString(result); 
        _log.error("receive(int)", msg.str() ); 
      } 
      while (!flag) { 
        if (timeOutWarning==-1)   timeOutWarning   = tarch::parallel::Node::getInstance().getDeadlockWarningTimeStamp(); 
        if (timeOutShutdown==-1)  timeOutShutdown  = tarch::parallel::Node::getInstance().getDeadlockTimeOutTimeStamp(); 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutWarningEnabled() && 
          (clock()>timeOutWarning) && 
          (!triggeredTimeoutWarning) 
        ) { 
          tarch::parallel::Node::getInstance().writeTimeOutWarning( 
            "exahype::records::RepositoryStatePacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::RepositoryStatePacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::RepositoryStatePacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::RepositoryStatePacked from node " 
            << source << ": " << tarch::parallel::MPIReturnValueToString(result); 
        _log.error( "receive(int)", msg.str() ); 
      } 
    }
    break; 
  } 
// =========================== 
// end injected snippet/aspect 
// =========================== 

      
     _senderDestinationRank = source==MPI_ANY_SOURCE ? status.MPI_SOURCE : source;
   }
   
   
   
   bool exahype::records::RepositoryStatePacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
      MPI_Status status;
      int  flag        = 0;
      MPI_Iprobe(
         MPI_ANY_SOURCE, tag,
         tarch::parallel::Node::getInstance().getCommunicator(), &flag, &status
      );
      if (flag) {
         int  messageCounter;
         if (exchangeOnlyAttributesMarkedWithParallelise) {
            MPI_Get_count(&status, Datatype, &messageCounter);
         }
         else {
            MPI_Get_count(&status, FullDatatype, &messageCounter);
         }
         return messageCounter > 0;
      }
      else return false;
      
   }
   
   int exahype::records::RepositoryStatePacked::getSenderRank() const {
      assertion( _senderDestinationRank!=-1 );
      return _senderDestinationRank;
      
   }
#endif



