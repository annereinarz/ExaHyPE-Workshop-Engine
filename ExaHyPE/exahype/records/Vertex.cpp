#include "exahype/records/Vertex.h"

#if defined(Parallel) && defined(PersistentRegularSubtrees) && defined(Asserts)
   exahype::records::Vertex::PersistentRecords::PersistentRecords() {
      
   }
   
   
   exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
   _CellDescriptionsIndex(CellDescriptionsIndex),
   _ADERDGCellDescriptions(ADERDGCellDescriptions),
   _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
   _isHangingNode(isHangingNode),
   _refinementControl(refinementControl),
   _adjacentCellsHeight(adjacentCellsHeight),
   _insideOutsideDomain(insideOutsideDomain),
   _x(x),
   _level(level),
   _adjacentRanks(adjacentRanks),
   _adjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank),
   _parentRegularPersistentSubgrid(parentRegularPersistentSubgrid),
   _parentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration) {
      
   }
   
   exahype::records::Vertex::Vertex() {
      
   }
   
   
   exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
   _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._x, persistentRecords._level, persistentRecords._adjacentRanks, persistentRecords._adjacentSubtreeForksIntoOtherRank, persistentRecords._parentRegularPersistentSubgrid, persistentRecords._parentRegularPersistentSubgridInPreviousIteration) {
      
   }
   
   
   exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
   _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
      
   }
   
   
   exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
   _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
   _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
      
   }
   
   exahype::records::Vertex::~Vertex() { }
   
   std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
      switch (param) {
         case Inside: return "Inside";
         case Boundary: return "Boundary";
         case Outside: return "Outside";
      }
      return "undefined";
   }
   
   std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
      return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
   }
   std::string exahype::records::Vertex::toString(const RefinementControl& param) {
      switch (param) {
         case Unrefined: return "Unrefined";
         case Refined: return "Refined";
         case RefinementTriggered: return "RefinementTriggered";
         case Refining: return "Refining";
         case EraseTriggered: return "EraseTriggered";
         case Erasing: return "Erasing";
         case RefineDueToJoinThoughWorkerIsAlreadyErasing: return "RefineDueToJoinThoughWorkerIsAlreadyErasing";
         case EnforceRefinementTriggered: return "EnforceRefinementTriggered";
      }
      return "undefined";
   }
   
   std::string exahype::records::Vertex::getRefinementControlMapping() {
      return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5,RefineDueToJoinThoughWorkerIsAlreadyErasing=6,EnforceRefinementTriggered=7)";
   }
   
   
   std::string exahype::records::Vertex::toString() const {
      std::ostringstream stringstr;
      toString(stringstr);
      return stringstr.str();
   }
   
   void exahype::records::Vertex::toString (std::ostream& out) const {
      out << "("; 
      out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
      out << ",";
      out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
      out << ",";
      out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
      out << ",";
      out << "isHangingNode:" << getIsHangingNode();
      out << ",";
      out << "refinementControl:" << toString(getRefinementControl());
      out << ",";
      out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
      out << ",";
      out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
      out << ",";
      out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
      out << ",";
      out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
      out << ",";
      out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
      out << ",";
      out << "level:" << getLevel();
      out << ",";
      out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
      out << ",";
      out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
      out << ",";
      out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
      out << ",";
      out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
      out <<  ")";
   }
   
   
   exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
      return _persistentRecords;
   }
   
   exahype::records::VertexPacked exahype::records::Vertex::convert() const{
      return VertexPacked(
         getCellDescriptionsIndex(),
         getADERDGCellDescriptions(),
         getFiniteVolumesCellDescriptions(),
         getIsHangingNode(),
         getRefinementControl(),
         getAdjacentCellsHeight(),
         getAdjacentCellsHeightOfPreviousIteration(),
         getNumberOfAdjacentRefinedCells(),
         getInsideOutsideDomain(),
         getX(),
         getLevel(),
         getAdjacentRanks(),
         getAdjacentSubtreeForksIntoOtherRank(),
         getParentRegularPersistentSubgrid(),
         getParentRegularPersistentSubgridInPreviousIteration()
      );
   }
   
   #ifdef Parallel
      tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
      
      MPI_Datatype exahype::records::Vertex::Datatype = 0;
      MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
      
      
      void exahype::records::Vertex::initDatatype() {
         {
            Vertex dummyVertex[2];
            
            #ifdef MPI2
            const int Attributes = 10;
            #else
            const int Attributes = 11;
            #endif
            MPI_Datatype subtypes[Attributes] = {
                 MPI_CXX_BOOL		 //isHangingNode
               , MPI_INT		 //refinementControl
               , MPI_INT		 //insideOutsideDomain
               , MPI_DOUBLE		 //x
               , MPI_INT		 //level
               , MPI_INT		 //adjacentRanks
               , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
               , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
               , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
               , MPI_INT		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , MPI_UB
               #endif
               
            };
            
            int blocklen[Attributes] = {
                 1		 //isHangingNode
               , 1		 //refinementControl
               , 1		 //insideOutsideDomain
               , DIMENSIONS		 //x
               , 1		 //level
               , TWO_POWER_D		 //adjacentRanks
               , 1		 //adjacentSubtreeForksIntoOtherRank
               , 1		 //parentRegularPersistentSubgrid
               , 1		 //parentRegularPersistentSubgridInPreviousIteration
               , 1		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , 1
               #endif
               
            };
            
            MPI_Aint  disp[Attributes];
            MPI_Aint  base;
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[5] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[5] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[6] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[6] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[7] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[7] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[8] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[8] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[9] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[9] );
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
               assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
            }
            #ifndef MPI2
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[10] );
            disp[10] -= base;
            disp[10] += disp[0];
            #endif
            #ifdef MPI2
            MPI_Datatype tmpType; 
            MPI_Aint lowerBound, typeExtent; 
            MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
            MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
            MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
            MPI_Type_commit( &Vertex::Datatype );
            #else
            MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
            MPI_Type_commit( &Vertex::Datatype );
            #endif
            
         }
         {
            Vertex dummyVertex[2];
            
            #ifdef MPI2
            const int Attributes = 15;
            #else
            const int Attributes = 16;
            #endif
            MPI_Datatype subtypes[Attributes] = {
                 MPI_INT		 //CellDescriptionsIndex
               , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
               , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
               , MPI_CXX_BOOL		 //isHangingNode
               , MPI_INT		 //refinementControl
               , MPI_INT		 //adjacentCellsHeight
               , MPI_INT		 //insideOutsideDomain
               , MPI_DOUBLE		 //x
               , MPI_INT		 //level
               , MPI_INT		 //adjacentRanks
               , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
               , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
               , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
               , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
               , MPI_INT		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , MPI_UB
               #endif
               
            };
            
            int blocklen[Attributes] = {
                 TWO_POWER_D		 //CellDescriptionsIndex
               , TWO_POWER_D		 //ADERDGCellDescriptions
               , TWO_POWER_D		 //FiniteVolumesCellDescriptions
               , 1		 //isHangingNode
               , 1		 //refinementControl
               , 1		 //adjacentCellsHeight
               , 1		 //insideOutsideDomain
               , DIMENSIONS		 //x
               , 1		 //level
               , TWO_POWER_D		 //adjacentRanks
               , 1		 //adjacentSubtreeForksIntoOtherRank
               , 1		 //parentRegularPersistentSubgrid
               , 1		 //parentRegularPersistentSubgridInPreviousIteration
               , 1		 //adjacentCellsHeightOfPreviousIteration
               , 1		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , 1
               #endif
               
            };
            
            MPI_Aint  disp[Attributes];
            MPI_Aint  base;
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[9] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[9] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[10] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[10] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[11] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[11] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[12] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[12] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[13] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[13] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[14] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[14] );
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
               assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
            }
            #ifndef MPI2
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[15] );
            disp[15] -= base;
            disp[15] += disp[0];
            #endif
            #ifdef MPI2
            MPI_Datatype tmpType; 
            MPI_Aint lowerBound, typeExtent; 
            MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
            MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
            MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
            MPI_Type_commit( &Vertex::FullDatatype );
            #else
            MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
            MPI_Type_commit( &Vertex::FullDatatype );
            #endif
            
         }
         
      }
      
      
      void exahype::records::Vertex::shutdownDatatype() {
         MPI_Type_free( &Vertex::Datatype );
         MPI_Type_free( &Vertex::FullDatatype );
         
      }
      
      void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
         // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
      
      
      
      void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
      
      
      
      bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
      
      int exahype::records::Vertex::getSenderRank() const {
         assertion( _senderDestinationRank!=-1 );
         return _senderDestinationRank;
         
      }
   #endif
   
   
   exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
      if ((9 >= (8 * sizeof(int)))) {
         std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
         std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
         std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
      }
      assertion((9 < (8 * sizeof(int))));
      
   }
   
   
   exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
   _CellDescriptionsIndex(CellDescriptionsIndex),
   _ADERDGCellDescriptions(ADERDGCellDescriptions),
   _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
   _adjacentCellsHeight(adjacentCellsHeight),
   _x(x),
   _level(level),
   _adjacentRanks(adjacentRanks) {
      setIsHangingNode(isHangingNode);
      setRefinementControl(refinementControl);
      setInsideOutsideDomain(insideOutsideDomain);
      setAdjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank);
      setParentRegularPersistentSubgrid(parentRegularPersistentSubgrid);
      setParentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration);
      if ((9 >= (8 * sizeof(int)))) {
         std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
         std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
         std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
      }
      assertion((9 < (8 * sizeof(int))));
      
   }
   
   exahype::records::VertexPacked::VertexPacked() {
      if ((9 >= (8 * sizeof(int)))) {
         std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
         std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
         std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
      }
      assertion((9 < (8 * sizeof(int))));
      
   }
   
   
   exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
   _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords._x, persistentRecords._level, persistentRecords._adjacentRanks, persistentRecords.getAdjacentSubtreeForksIntoOtherRank(), persistentRecords.getParentRegularPersistentSubgrid(), persistentRecords.getParentRegularPersistentSubgridInPreviousIteration()) {
      if ((9 >= (8 * sizeof(int)))) {
         std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
         std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
         std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
      }
      assertion((9 < (8 * sizeof(int))));
      
   }
   
   
   exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
   _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
      if ((9 >= (8 * sizeof(int)))) {
         std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
         std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
         std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
      }
      assertion((9 < (8 * sizeof(int))));
      
   }
   
   
   exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
   _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
   _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
      if ((9 >= (8 * sizeof(int)))) {
         std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
         std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
         std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
      }
      assertion((9 < (8 * sizeof(int))));
      
   }
   
   exahype::records::VertexPacked::~VertexPacked() { }
   
   std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
      return exahype::records::Vertex::toString(param);
   }
   
   std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
      return exahype::records::Vertex::getInsideOutsideDomainMapping();
   }
   
   std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
      return exahype::records::Vertex::toString(param);
   }
   
   std::string exahype::records::VertexPacked::getRefinementControlMapping() {
      return exahype::records::Vertex::getRefinementControlMapping();
   }
   
   
   
   std::string exahype::records::VertexPacked::toString() const {
      std::ostringstream stringstr;
      toString(stringstr);
      return stringstr.str();
   }
   
   void exahype::records::VertexPacked::toString (std::ostream& out) const {
      out << "("; 
      out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
      out << ",";
      out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
      out << ",";
      out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
      out << ",";
      out << "isHangingNode:" << getIsHangingNode();
      out << ",";
      out << "refinementControl:" << toString(getRefinementControl());
      out << ",";
      out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
      out << ",";
      out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
      out << ",";
      out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
      out << ",";
      out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
      out << ",";
      out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
      out << ",";
      out << "level:" << getLevel();
      out << ",";
      out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
      out << ",";
      out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
      out << ",";
      out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
      out << ",";
      out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
      out <<  ")";
   }
   
   
   exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
      return _persistentRecords;
   }
   
   exahype::records::Vertex exahype::records::VertexPacked::convert() const{
      return Vertex(
         getCellDescriptionsIndex(),
         getADERDGCellDescriptions(),
         getFiniteVolumesCellDescriptions(),
         getIsHangingNode(),
         getRefinementControl(),
         getAdjacentCellsHeight(),
         getAdjacentCellsHeightOfPreviousIteration(),
         getNumberOfAdjacentRefinedCells(),
         getInsideOutsideDomain(),
         getX(),
         getLevel(),
         getAdjacentRanks(),
         getAdjacentSubtreeForksIntoOtherRank(),
         getParentRegularPersistentSubgrid(),
         getParentRegularPersistentSubgridInPreviousIteration()
      );
   }
   
   #ifdef Parallel
      tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
      
      MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
      MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
      
      
      void exahype::records::VertexPacked::initDatatype() {
         {
            VertexPacked dummyVertexPacked[2];
            
            #ifdef MPI2
            const int Attributes = 5;
            #else
            const int Attributes = 6;
            #endif
            MPI_Datatype subtypes[Attributes] = {
                 MPI_DOUBLE		 //x
               , MPI_INT		 //level
               , MPI_INT		 //adjacentRanks
               , MPI_INT		 //_packedRecords0
               , MPI_INT		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , MPI_UB
               #endif
               
            };
            
            int blocklen[Attributes] = {
                 DIMENSIONS		 //x
               , 1		 //level
               , TWO_POWER_D		 //adjacentRanks
               , 1		 //_packedRecords0
               , 1		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , 1
               #endif
               
            };
            
            MPI_Aint  disp[Attributes];
            MPI_Aint  base;
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[3] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[3] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
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
               assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
            }
            #ifndef MPI2
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[5] );
            disp[5] -= base;
            disp[5] += disp[0];
            #endif
            #ifdef MPI2
            MPI_Datatype tmpType; 
            MPI_Aint lowerBound, typeExtent; 
            MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
            MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
            MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
            MPI_Type_commit( &VertexPacked::Datatype );
            #else
            MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
            MPI_Type_commit( &VertexPacked::Datatype );
            #endif
            
         }
         {
            VertexPacked dummyVertexPacked[2];
            
            #ifdef MPI2
            const int Attributes = 10;
            #else
            const int Attributes = 11;
            #endif
            MPI_Datatype subtypes[Attributes] = {
                 MPI_INT		 //CellDescriptionsIndex
               , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
               , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
               , MPI_INT		 //adjacentCellsHeight
               , MPI_DOUBLE		 //x
               , MPI_INT		 //level
               , MPI_INT		 //adjacentRanks
               , MPI_INT		 //_packedRecords0
               , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
               , MPI_INT		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , MPI_UB
               #endif
               
            };
            
            int blocklen[Attributes] = {
                 TWO_POWER_D		 //CellDescriptionsIndex
               , TWO_POWER_D		 //ADERDGCellDescriptions
               , TWO_POWER_D		 //FiniteVolumesCellDescriptions
               , 1		 //adjacentCellsHeight
               , DIMENSIONS		 //x
               , 1		 //level
               , TWO_POWER_D		 //adjacentRanks
               , 1		 //_packedRecords0
               , 1		 //adjacentCellsHeightOfPreviousIteration
               , 1		 //numberOfAdjacentRefinedCells
               #ifndef MPI2
               , 1
               #endif
               
            };
            
            MPI_Aint  disp[Attributes];
            MPI_Aint  base;
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[6] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[6] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[7] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[7] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[8] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[8] );
            #endif
            #ifdef MPI2
            MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[9] );
            #else
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[9] );
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
               assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
            }
            #ifndef MPI2
            MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[10] );
            disp[10] -= base;
            disp[10] += disp[0];
            #endif
            #ifdef MPI2
            MPI_Datatype tmpType; 
            MPI_Aint lowerBound, typeExtent; 
            MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
            MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
            MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
            MPI_Type_commit( &VertexPacked::FullDatatype );
            #else
            MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
            MPI_Type_commit( &VertexPacked::FullDatatype );
            #endif
            
         }
         
      }
      
      
      void exahype::records::VertexPacked::shutdownDatatype() {
         MPI_Type_free( &VertexPacked::Datatype );
         MPI_Type_free( &VertexPacked::FullDatatype );
         
      }
      
      void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
         // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
      
      
      
      void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
      
      
      
      bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
      
      int exahype::records::VertexPacked::getSenderRank() const {
         assertion( _senderDestinationRank!=-1 );
         return _senderDestinationRank;
         
      }
   #endif
   
   
   #elif defined(PersistentRegularSubtrees) && defined(Asserts) && !defined(Parallel)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain),
      _x(x),
      _level(level),
      _parentRegularPersistentSubgrid(parentRegularPersistentSubgrid),
      _parentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._x, persistentRecords._level, persistentRecords._parentRegularPersistentSubgrid, persistentRecords._parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
         out << ",";
         out << "level:" << getLevel();
         out << ",";
         out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
         out << ",";
         out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getX(),
            getLevel(),
            getParentRegularPersistentSubgrid(),
            getParentRegularPersistentSubgridInPreviousIteration()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 8;
               #else
               const int Attributes = 9;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //insideOutsideDomain
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //parentRegularPersistentSubgrid
                  , 1		 //parentRegularPersistentSubgridInPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[8] );
               disp[8] -= base;
               disp[8] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 13;
               #else
               const int Attributes = 14;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //parentRegularPersistentSubgrid
                  , 1		 //parentRegularPersistentSubgridInPreviousIteration
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[9] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[10] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[10] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[11] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[11] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[12] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[12] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[13] );
               disp[13] -= base;
               disp[13] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight),
      _x(x),
      _level(level) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         setParentRegularPersistentSubgrid(parentRegularPersistentSubgrid);
         setParentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration);
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords._x, persistentRecords._level, persistentRecords.getParentRegularPersistentSubgrid(), persistentRecords.getParentRegularPersistentSubgridInPreviousIteration()) {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
         out << ",";
         out << "level:" << getLevel();
         out << ",";
         out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
         out << ",";
         out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getX(),
            getLevel(),
            getParentRegularPersistentSubgrid(),
            getParentRegularPersistentSubgridInPreviousIteration()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 4;
               #else
               const int Attributes = 5;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[3] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[4] );
               disp[4] -= base;
               disp[4] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 9;
               #else
               const int Attributes = 10;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[8] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[9] );
               disp[9] -= base;
               disp[9] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   #elif defined(Parallel) && !defined(PersistentRegularSubtrees) && defined(Asserts)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain),
      _x(x),
      _level(level),
      _adjacentRanks(adjacentRanks),
      _adjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._x, persistentRecords._level, persistentRecords._adjacentRanks, persistentRecords._adjacentSubtreeForksIntoOtherRank) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
            case RefineDueToJoinThoughWorkerIsAlreadyErasing: return "RefineDueToJoinThoughWorkerIsAlreadyErasing";
            case EnforceRefinementTriggered: return "EnforceRefinementTriggered";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5,RefineDueToJoinThoughWorkerIsAlreadyErasing=6,EnforceRefinementTriggered=7)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
         out << ",";
         out << "level:" << getLevel();
         out << ",";
         out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
         out << ",";
         out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getX(),
            getLevel(),
            getAdjacentRanks(),
            getAdjacentSubtreeForksIntoOtherRank()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 8;
               #else
               const int Attributes = 9;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //adjacentRanks
                  , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //insideOutsideDomain
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //adjacentSubtreeForksIntoOtherRank
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[8] );
               disp[8] -= base;
               disp[8] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 13;
               #else
               const int Attributes = 14;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //adjacentRanks
                  , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //adjacentSubtreeForksIntoOtherRank
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[9] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[10] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[10] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[11] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[11] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[12] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[12] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[13] );
               disp[13] -= base;
               disp[13] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight),
      _x(x),
      _level(level),
      _adjacentRanks(adjacentRanks) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         setAdjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank);
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords._x, persistentRecords._level, persistentRecords._adjacentRanks, persistentRecords.getAdjacentSubtreeForksIntoOtherRank()) {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank) {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level, adjacentRanks, adjacentSubtreeForksIntoOtherRank),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
         out << ",";
         out << "level:" << getLevel();
         out << ",";
         out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
         out << ",";
         out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getX(),
            getLevel(),
            getAdjacentRanks(),
            getAdjacentSubtreeForksIntoOtherRank()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 5;
               #else
               const int Attributes = 6;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //adjacentRanks
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    DIMENSIONS		 //x
                  , 1		 //level
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[5] );
               disp[5] -= base;
               disp[5] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 10;
               #else
               const int Attributes = 11;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //adjacentRanks
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[9] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[10] );
               disp[10] -= base;
               disp[10] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   #elif defined(Parallel) && defined(PersistentRegularSubtrees) && !defined(Asserts)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain),
      _adjacentRanks(adjacentRanks),
      _adjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank),
      _parentRegularPersistentSubgrid(parentRegularPersistentSubgrid),
      _parentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._adjacentRanks, persistentRecords._adjacentSubtreeForksIntoOtherRank, persistentRecords._parentRegularPersistentSubgrid, persistentRecords._parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
            case RefineDueToJoinThoughWorkerIsAlreadyErasing: return "RefineDueToJoinThoughWorkerIsAlreadyErasing";
            case EnforceRefinementTriggered: return "EnforceRefinementTriggered";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5,RefineDueToJoinThoughWorkerIsAlreadyErasing=6,EnforceRefinementTriggered=7)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
         out << ",";
         out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
         out << ",";
         out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
         out << ",";
         out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getAdjacentRanks(),
            getAdjacentSubtreeForksIntoOtherRank(),
            getParentRegularPersistentSubgrid(),
            getParentRegularPersistentSubgridInPreviousIteration()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 7;
               #else
               const int Attributes = 8;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentRanks
                  , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //adjacentSubtreeForksIntoOtherRank
                  , 1		 //parentRegularPersistentSubgrid
                  , 1		 //parentRegularPersistentSubgridInPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[6] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[7] );
               disp[7] -= base;
               disp[7] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 13;
               #else
               const int Attributes = 14;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_INT		 //adjacentRanks
                  , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //adjacentSubtreeForksIntoOtherRank
                  , 1		 //parentRegularPersistentSubgrid
                  , 1		 //parentRegularPersistentSubgridInPreviousIteration
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[9] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[10] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[10] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[11] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[11] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[12] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[12] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[13] );
               disp[13] -= base;
               disp[13] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((9 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((9 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight),
      _adjacentRanks(adjacentRanks) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         setAdjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank);
         setParentRegularPersistentSubgrid(parentRegularPersistentSubgrid);
         setParentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration);
         if ((9 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((9 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((9 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((9 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords._adjacentRanks, persistentRecords.getAdjacentSubtreeForksIntoOtherRank(), persistentRecords.getParentRegularPersistentSubgrid(), persistentRecords.getParentRegularPersistentSubgridInPreviousIteration()) {
         if ((9 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((9 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
         if ((9 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((9 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((9 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((9 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
         out << ",";
         out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
         out << ",";
         out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
         out << ",";
         out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getAdjacentRanks(),
            getAdjacentSubtreeForksIntoOtherRank(),
            getParentRegularPersistentSubgrid(),
            getParentRegularPersistentSubgridInPreviousIteration()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 3;
               #else
               const int Attributes = 4;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //adjacentRanks
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //adjacentRanks
                  , 1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[2] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[3] );
               disp[3] -= base;
               disp[3] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 8;
               #else
               const int Attributes = 9;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //adjacentRanks
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[8] );
               disp[8] -= base;
               disp[8] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   #elif defined(PersistentRegularSubtrees) && !defined(Parallel) && !defined(Asserts)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain),
      _parentRegularPersistentSubgrid(parentRegularPersistentSubgrid),
      _parentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._parentRegularPersistentSubgrid, persistentRecords._parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
         out << ",";
         out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getParentRegularPersistentSubgrid(),
            getParentRegularPersistentSubgridInPreviousIteration()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 5;
               #else
               const int Attributes = 6;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //parentRegularPersistentSubgrid
                  , 1		 //parentRegularPersistentSubgridInPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[5] );
               disp[5] -= base;
               disp[5] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 11;
               #else
               const int Attributes = 12;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgrid
                  , MPI_CXX_BOOL		 //parentRegularPersistentSubgridInPreviousIteration
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , 1		 //parentRegularPersistentSubgrid
                  , 1		 //parentRegularPersistentSubgridInPreviousIteration
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgrid))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._parentRegularPersistentSubgridInPreviousIteration))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[9] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[10] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[10] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[11] );
               disp[11] -= base;
               disp[11] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         setParentRegularPersistentSubgrid(parentRegularPersistentSubgrid);
         setParentRegularPersistentSubgridInPreviousIteration(parentRegularPersistentSubgridInPreviousIteration);
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords.getParentRegularPersistentSubgrid(), persistentRecords.getParentRegularPersistentSubgridInPreviousIteration()) {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration) {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const bool& parentRegularPersistentSubgrid, const bool& parentRegularPersistentSubgridInPreviousIteration):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, parentRegularPersistentSubgrid, parentRegularPersistentSubgridInPreviousIteration),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((8 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((8 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "parentRegularPersistentSubgrid:" << getParentRegularPersistentSubgrid();
         out << ",";
         out << "parentRegularPersistentSubgridInPreviousIteration:" << getParentRegularPersistentSubgridInPreviousIteration();
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getParentRegularPersistentSubgrid(),
            getParentRegularPersistentSubgridInPreviousIteration()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 2;
               #else
               const int Attributes = 3;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[1] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[2] );
               disp[2] -= base;
               disp[2] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 7;
               #else
               const int Attributes = 8;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[6] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[7] );
               disp[7] -= base;
               disp[7] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   #elif defined(Parallel) && !defined(PersistentRegularSubtrees) && !defined(Asserts)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain),
      _adjacentRanks(adjacentRanks),
      _adjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._adjacentRanks, persistentRecords._adjacentSubtreeForksIntoOtherRank) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
            case RefineDueToJoinThoughWorkerIsAlreadyErasing: return "RefineDueToJoinThoughWorkerIsAlreadyErasing";
            case EnforceRefinementTriggered: return "EnforceRefinementTriggered";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5,RefineDueToJoinThoughWorkerIsAlreadyErasing=6,EnforceRefinementTriggered=7)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
         out << ",";
         out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getAdjacentRanks(),
            getAdjacentSubtreeForksIntoOtherRank()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 5;
               #else
               const int Attributes = 6;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentRanks
                  , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //adjacentSubtreeForksIntoOtherRank
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[4] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[5] );
               disp[5] -= base;
               disp[5] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 11;
               #else
               const int Attributes = 12;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_INT		 //adjacentRanks
                  , MPI_CXX_BOOL		 //adjacentSubtreeForksIntoOtherRank
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //adjacentSubtreeForksIntoOtherRank
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentRanks[0]))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentSubtreeForksIntoOtherRank))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[9] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[10] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[10] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[11] );
               disp[11] -= base;
               disp[11] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight),
      _adjacentRanks(adjacentRanks) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         setAdjacentSubtreeForksIntoOtherRank(adjacentSubtreeForksIntoOtherRank);
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords._adjacentRanks, persistentRecords.getAdjacentSubtreeForksIntoOtherRank()) {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank) {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks, const bool& adjacentSubtreeForksIntoOtherRank):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, adjacentRanks, adjacentSubtreeForksIntoOtherRank),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((7 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((7 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "adjacentRanks:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getAdjacentRanks(i) << ",";
   }
   out << getAdjacentRanks(TWO_POWER_D-1) << "]";
         out << ",";
         out << "adjacentSubtreeForksIntoOtherRank:" << getAdjacentSubtreeForksIntoOtherRank();
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getAdjacentRanks(),
            getAdjacentSubtreeForksIntoOtherRank()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 3;
               #else
               const int Attributes = 4;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //adjacentRanks
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //adjacentRanks
                  , 1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[2] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[3] );
               disp[3] -= base;
               disp[3] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 8;
               #else
               const int Attributes = 9;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //adjacentRanks
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , TWO_POWER_D		 //adjacentRanks
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentRanks[0]))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[7] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[8] );
               disp[8] -= base;
               disp[8] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   #elif !defined(PersistentRegularSubtrees) && defined(Asserts) && !defined(Parallel)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain),
      _x(x),
      _level(level) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain, persistentRecords._x, persistentRecords._level) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
         out << ",";
         out << "level:" << getLevel();
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getX(),
            getLevel()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 6;
               #else
               const int Attributes = 7;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //insideOutsideDomain
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[5] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[6] );
               disp[6] -= base;
               disp[6] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 11;
               #else
               const int Attributes = 12;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._x[0]))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._level))), 		&disp[8] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[9] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[9] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[10] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[10] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[11] );
               disp[11] -= base;
               disp[11] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight),
      _x(x),
      _level(level) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain(), persistentRecords._x, persistentRecords._level) {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level) {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain, const tarch::la::Vector<DIMENSIONS,double>& x, const int& level):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain, x, level),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out << ",";
         out << "x:[";
   for (int i = 0; i < DIMENSIONS-1; i++) {
      out << getX(i) << ",";
   }
   out << getX(DIMENSIONS-1) << "]";
         out << ",";
         out << "level:" << getLevel();
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain(),
            getX(),
            getLevel()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 4;
               #else
               const int Attributes = 5;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[3] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[4] );
               disp[4] -= base;
               disp[4] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 9;
               #else
               const int Attributes = 10;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_DOUBLE		 //x
                  , MPI_INT		 //level
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , DIMENSIONS		 //x
                  , 1		 //level
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._x[0]))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._level))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[8] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[9] );
               disp[9] -= base;
               disp[9] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   #elif !defined(PersistentRegularSubtrees) && !defined(Parallel) && !defined(Asserts)
      exahype::records::Vertex::PersistentRecords::PersistentRecords() {
         
      }
      
      
      exahype::records::Vertex::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _isHangingNode(isHangingNode),
      _refinementControl(refinementControl),
      _adjacentCellsHeight(adjacentCellsHeight),
      _insideOutsideDomain(insideOutsideDomain) {
         
      }
      
      exahype::records::Vertex::Vertex() {
         
      }
      
      
      exahype::records::Vertex::Vertex(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords._isHangingNode, persistentRecords._refinementControl, persistentRecords._adjacentCellsHeight, persistentRecords._insideOutsideDomain) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain) {
         
      }
      
      
      exahype::records::Vertex::Vertex(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         
      }
      
      exahype::records::Vertex::~Vertex() { }
      
      std::string exahype::records::Vertex::toString(const InsideOutsideDomain& param) {
         switch (param) {
            case Inside: return "Inside";
            case Boundary: return "Boundary";
            case Outside: return "Outside";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getInsideOutsideDomainMapping() {
         return "InsideOutsideDomain(Inside=0,Boundary=1,Outside=2)";
      }
      std::string exahype::records::Vertex::toString(const RefinementControl& param) {
         switch (param) {
            case Unrefined: return "Unrefined";
            case Refined: return "Refined";
            case RefinementTriggered: return "RefinementTriggered";
            case Refining: return "Refining";
            case EraseTriggered: return "EraseTriggered";
            case Erasing: return "Erasing";
         }
         return "undefined";
      }
      
      std::string exahype::records::Vertex::getRefinementControlMapping() {
         return "RefinementControl(Unrefined=0,Refined=1,RefinementTriggered=2,Refining=3,EraseTriggered=4,Erasing=5)";
      }
      
      
      std::string exahype::records::Vertex::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::Vertex::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out <<  ")";
      }
      
      
      exahype::records::Vertex::PersistentRecords exahype::records::Vertex::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::VertexPacked exahype::records::Vertex::convert() const{
         return VertexPacked(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::Vertex::_log( "exahype::records::Vertex" );
         
         MPI_Datatype exahype::records::Vertex::Datatype = 0;
         MPI_Datatype exahype::records::Vertex::FullDatatype = 0;
         
         
         void exahype::records::Vertex::initDatatype() {
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 3;
               #else
               const int Attributes = 4;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[2] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[3] );
               disp[3] -= base;
               disp[3] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::Datatype );
               MPI_Type_commit( &Vertex::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::Datatype);
               MPI_Type_commit( &Vertex::Datatype );
               #endif
               
            }
            {
               Vertex dummyVertex[2];
               
               #ifdef MPI2
               const int Attributes = 9;
               #else
               const int Attributes = 10;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_CXX_BOOL		 //isHangingNode
                  , MPI_INT		 //refinementControl
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //insideOutsideDomain
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //isHangingNode
                  , 1		 //refinementControl
                  , 1		 //adjacentCellsHeight
                  , 1		 //insideOutsideDomain
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._isHangingNode))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._refinementControl))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._adjacentCellsHeight))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._persistentRecords._insideOutsideDomain))), 		&disp[6] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[7] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[7] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[8] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[0]._numberOfAdjacentRefinedCells))), 		&disp[8] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(Vertex)), i, disp[i], Attributes, sizeof(Vertex));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertex[1]))), 		&disp[9] );
               disp[9] -= base;
               disp[9] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &Vertex::FullDatatype );
               MPI_Type_commit( &Vertex::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &Vertex::FullDatatype);
               MPI_Type_commit( &Vertex::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::Vertex::shutdownDatatype() {
            MPI_Type_free( &Vertex::Datatype );
            MPI_Type_free( &Vertex::FullDatatype );
            
         }
         
         void exahype::records::Vertex::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::Vertex " 
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
         msg << "was not able to send message exahype::records::Vertex "  
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
           msg << "testing for finished send task for exahype::records::Vertex " 
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
             "exahype::records::Vertex", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::Vertex", 
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
         
         
         
         void exahype::records::Vertex::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
        msg << "testing for finished receive task for exahype::records::Vertex failed: " 
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
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::Vertex", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::Vertex failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::Vertex from node " 
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
         
         
         
         bool exahype::records::Vertex::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::Vertex::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords() {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::PersistentRecords::PersistentRecords(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain):
      _CellDescriptionsIndex(CellDescriptionsIndex),
      _ADERDGCellDescriptions(ADERDGCellDescriptions),
      _FiniteVolumesCellDescriptions(FiniteVolumesCellDescriptions),
      _adjacentCellsHeight(adjacentCellsHeight) {
         setIsHangingNode(isHangingNode);
         setRefinementControl(refinementControl);
         setInsideOutsideDomain(insideOutsideDomain);
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::VertexPacked() {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const PersistentRecords& persistentRecords):
      _persistentRecords(persistentRecords._CellDescriptionsIndex, persistentRecords._ADERDGCellDescriptions, persistentRecords._FiniteVolumesCellDescriptions, persistentRecords.getIsHangingNode(), persistentRecords.getRefinementControl(), persistentRecords._adjacentCellsHeight, persistentRecords.getInsideOutsideDomain()) {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const InsideOutsideDomain& insideOutsideDomain):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain) {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      
      exahype::records::VertexPacked::VertexPacked(const tarch::la::Vector<TWO_POWER_D,int>& CellDescriptionsIndex, tarch::la::Vector<TWO_POWER_D,void*> ADERDGCellDescriptions, tarch::la::Vector<TWO_POWER_D,void*> FiniteVolumesCellDescriptions, const bool& isHangingNode, const RefinementControl& refinementControl, const int& adjacentCellsHeight, const int& adjacentCellsHeightOfPreviousIteration, const int& numberOfAdjacentRefinedCells, const InsideOutsideDomain& insideOutsideDomain):
      _persistentRecords(CellDescriptionsIndex, ADERDGCellDescriptions, FiniteVolumesCellDescriptions, isHangingNode, refinementControl, adjacentCellsHeight, insideOutsideDomain),_adjacentCellsHeightOfPreviousIteration(adjacentCellsHeightOfPreviousIteration),
      _numberOfAdjacentRefinedCells(numberOfAdjacentRefinedCells) {
         if ((6 >= (8 * sizeof(int)))) {
            std::cerr << "Packed-Type in " << __FILE__ << " too small. Either use bigger data type or append " << std::endl << std::endl;
            std::cerr << "  Packed-Type: int hint-size no-of-bits;  " << std::endl << std::endl;
            std::cerr << "to your data type spec to guide DaStGen how many bits (no-of-bits) a data type has on your machine. DaStGen then can split up the bitfields into several attributes. " << std::endl; 
         }
         assertion((6 < (8 * sizeof(int))));
         
      }
      
      exahype::records::VertexPacked::~VertexPacked() { }
      
      std::string exahype::records::VertexPacked::toString(const InsideOutsideDomain& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getInsideOutsideDomainMapping() {
         return exahype::records::Vertex::getInsideOutsideDomainMapping();
      }
      
      std::string exahype::records::VertexPacked::toString(const RefinementControl& param) {
         return exahype::records::Vertex::toString(param);
      }
      
      std::string exahype::records::VertexPacked::getRefinementControlMapping() {
         return exahype::records::Vertex::getRefinementControlMapping();
      }
      
      
      
      std::string exahype::records::VertexPacked::toString() const {
         std::ostringstream stringstr;
         toString(stringstr);
         return stringstr.str();
      }
      
      void exahype::records::VertexPacked::toString (std::ostream& out) const {
         out << "("; 
         out << "CellDescriptionsIndex:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getCellDescriptionsIndex(i) << ",";
   }
   out << getCellDescriptionsIndex(TWO_POWER_D-1) << "]";
         out << ",";
         out << "ADERDGCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getADERDGCellDescriptions(i) << ",";
   }
   out << getADERDGCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "FiniteVolumesCellDescriptions:[";
   for (int i = 0; i < TWO_POWER_D-1; i++) {
      out << getFiniteVolumesCellDescriptions(i) << ",";
   }
   out << getFiniteVolumesCellDescriptions(TWO_POWER_D-1) << "]";
         out << ",";
         out << "isHangingNode:" << getIsHangingNode();
         out << ",";
         out << "refinementControl:" << toString(getRefinementControl());
         out << ",";
         out << "adjacentCellsHeight:" << getAdjacentCellsHeight();
         out << ",";
         out << "adjacentCellsHeightOfPreviousIteration:" << getAdjacentCellsHeightOfPreviousIteration();
         out << ",";
         out << "numberOfAdjacentRefinedCells:" << getNumberOfAdjacentRefinedCells();
         out << ",";
         out << "insideOutsideDomain:" << toString(getInsideOutsideDomain());
         out <<  ")";
      }
      
      
      exahype::records::VertexPacked::PersistentRecords exahype::records::VertexPacked::getPersistentRecords() const {
         return _persistentRecords;
      }
      
      exahype::records::Vertex exahype::records::VertexPacked::convert() const{
         return Vertex(
            getCellDescriptionsIndex(),
            getADERDGCellDescriptions(),
            getFiniteVolumesCellDescriptions(),
            getIsHangingNode(),
            getRefinementControl(),
            getAdjacentCellsHeight(),
            getAdjacentCellsHeightOfPreviousIteration(),
            getNumberOfAdjacentRefinedCells(),
            getInsideOutsideDomain()
         );
      }
      
      #ifdef Parallel
         tarch::logging::Log exahype::records::VertexPacked::_log( "exahype::records::VertexPacked" );
         
         MPI_Datatype exahype::records::VertexPacked::Datatype = 0;
         MPI_Datatype exahype::records::VertexPacked::FullDatatype = 0;
         
         
         void exahype::records::VertexPacked::initDatatype() {
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 2;
               #else
               const int Attributes = 3;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //_packedRecords0
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    1		 //_packedRecords0
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[1] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[2] );
               disp[2] -= base;
               disp[2] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::Datatype );
               MPI_Type_commit( &VertexPacked::Datatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::Datatype);
               MPI_Type_commit( &VertexPacked::Datatype );
               #endif
               
            }
            {
               VertexPacked dummyVertexPacked[2];
               
               #ifdef MPI2
               const int Attributes = 7;
               #else
               const int Attributes = 8;
               #endif
               MPI_Datatype subtypes[Attributes] = {
                    MPI_INT		 //CellDescriptionsIndex
                  , MPI_UNSIGNED_LONG		 //ADERDGCellDescriptions
                  , MPI_UNSIGNED_LONG		 //FiniteVolumesCellDescriptions
                  , MPI_INT		 //adjacentCellsHeight
                  , MPI_INT		 //_packedRecords0
                  , MPI_INT		 //adjacentCellsHeightOfPreviousIteration
                  , MPI_INT		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , MPI_UB
                  #endif
                  
               };
               
               int blocklen[Attributes] = {
                    TWO_POWER_D		 //CellDescriptionsIndex
                  , TWO_POWER_D		 //ADERDGCellDescriptions
                  , TWO_POWER_D		 //FiniteVolumesCellDescriptions
                  , 1		 //adjacentCellsHeight
                  , 1		 //_packedRecords0
                  , 1		 //adjacentCellsHeightOfPreviousIteration
                  , 1		 //numberOfAdjacentRefinedCells
                  #ifndef MPI2
                  , 1
                  #endif
                  
               };
               
               MPI_Aint  disp[Attributes];
               MPI_Aint  base;
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked))), &base);
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._CellDescriptionsIndex[0]))), 		&disp[0] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._ADERDGCellDescriptions[0]))), 		&disp[1] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._FiniteVolumesCellDescriptions[0]))), 		&disp[2] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._adjacentCellsHeight))), 		&disp[3] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[4] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._persistentRecords._packedRecords0))), 		&disp[4] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[5] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._adjacentCellsHeightOfPreviousIteration))), 		&disp[5] );
               #endif
               #ifdef MPI2
               MPI_Get_address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[6] );
               #else
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[0]._numberOfAdjacentRefinedCells))), 		&disp[6] );
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
                  assertion4(disp[i]<static_cast<int>(sizeof(VertexPacked)), i, disp[i], Attributes, sizeof(VertexPacked));
               }
               #ifndef MPI2
               MPI_Address( const_cast<void*>(static_cast<const void*>(&(dummyVertexPacked[1]))), 		&disp[7] );
               disp[7] -= base;
               disp[7] += disp[0];
               #endif
               #ifdef MPI2
               MPI_Datatype tmpType; 
               MPI_Aint lowerBound, typeExtent; 
               MPI_Type_create_struct( Attributes, blocklen, disp, subtypes, &tmpType );
               MPI_Type_get_extent( tmpType, &lowerBound, &typeExtent );
               MPI_Type_create_resized( tmpType, lowerBound, typeExtent, &VertexPacked::FullDatatype );
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #else
               MPI_Type_struct( Attributes, blocklen, disp, subtypes, &VertexPacked::FullDatatype);
               MPI_Type_commit( &VertexPacked::FullDatatype );
               #endif
               
            }
            
         }
         
         
         void exahype::records::VertexPacked::shutdownDatatype() {
            MPI_Type_free( &VertexPacked::Datatype );
            MPI_Type_free( &VertexPacked::FullDatatype );
            
         }
         
         void exahype::records::VertexPacked::send(int destination, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
            // ============================= 
// start injected snippet/aspect 
// ============================= 
switch (mode) { 
  case ExchangeMode::Blocking: 
    {
      const int result = MPI_Send(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, destination, tag, tarch::parallel::Node::getInstance().getCommunicator()); 
       if  (result!=MPI_SUCCESS) { 
         std::ostringstream msg; 
         msg << "was not able to send message exahype::records::VertexPacked " 
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
         msg << "was not able to send message exahype::records::VertexPacked "  
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
           msg << "testing for finished send task for exahype::records::VertexPacked " 
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
             "exahype::records::VertexPacked", 
             "send(int)", destination,tag,1 
           ); 
           triggeredTimeoutWarning = true; 
         } 
         if ( 
           tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
           (clock()>timeOutShutdown) 
         ) { 
           tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
             "exahype::records::VertexPacked", 
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
         
         
         
         void exahype::records::VertexPacked::receive(int source, int tag, bool exchangeOnlyAttributesMarkedWithParallelise, ExchangeMode mode) {
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Test( sendRequestHandle, &flag, source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
        if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
        msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
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
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
          triggeredTimeoutWarning = true; 
        } 
        if ( 
          tarch::parallel::Node::getInstance().isTimeOutDeadlockEnabled() && 
          (clock()>timeOutShutdown) 
        ) { 
          tarch::parallel::Node::getInstance().triggerDeadlockTimeOut( 
            "exahype::records::VertexPacked", 
            "receive(int)", source,tag,1 
          ); 
        } 
        tarch::parallel::Node::getInstance().receiveDanglingMessages(); 
        result = MPI_Iprobe(source, tag, tarch::parallel::Node::getInstance().getCommunicator(), &flag, MPI_STATUS_IGNORE ); 
         if (result!=MPI_SUCCESS) { 
          std::ostringstream msg; 
          msg << "testing for finished receive task for exahype::records::VertexPacked failed: " 
              << tarch::parallel::MPIReturnValueToString(result); 
          _log.error("receive(int)", msg.str() ); 
        } 
      } 
      result = MPI_Recv(this, 1, exchangeOnlyAttributesMarkedWithParallelise ? Datatype : FullDatatype, source, tag, tarch::parallel::Node::getInstance().getCommunicator(), source==MPI_ANY_SOURCE ? &status : MPI_STATUS_IGNORE ); 
      if ( result != MPI_SUCCESS ) { 
        std::ostringstream msg; 
        msg << "failed to start to receive exahype::records::VertexPacked from node " 
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
         
         
         
         bool exahype::records::VertexPacked::isMessageInQueue(int tag, bool exchangeOnlyAttributesMarkedWithParallelise) {
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
         
         int exahype::records::VertexPacked::getSenderRank() const {
            assertion( _senderDestinationRank!=-1 );
            return _senderDestinationRank;
            
         }
      #endif
      
      
      
   
#endif


