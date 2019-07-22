#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

#include "peano/utils/Loop.h"
#include "tarch/parallel/Node.h"

const int multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex         = -1;
const int multiscalelinkedcell::HangingVertexBookkeeper::RemoteAdjacencyIndex          = -2;
const int multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex  = -3;


tarch::logging::Log  multiscalelinkedcell::HangingVertexBookkeeper::_log( "multiscalelinkedcell::HangingVertexBookkeeper" );


std::string multiscalelinkedcell::indexToString( int index ) {
  std::ostringstream msg;
  switch (index) {
    case multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex:
      msg << "invalid";
      break;
    case multiscalelinkedcell::HangingVertexBookkeeper::RemoteAdjacencyIndex:
      msg << "remote";
      break;
    case multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex:
      msg << "boundary";
      break;
    default:
      msg << index;
      break;
  }
  return msg.str();
}


std::string multiscalelinkedcell::indicesToString( const tarch::la::Vector<THREE_POWER_D,int>& indices ) {
  std::ostringstream msg;

  msg << "(";
  for (int i=0; i<THREE_POWER_D; i++) {
    if (i!=0) msg << ",";
    switch (indices(i)) {
      case multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex:
        msg << "invalid";
        break;
      case multiscalelinkedcell::HangingVertexBookkeeper::RemoteAdjacencyIndex:
        msg << "remote";
        break;
      case multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex:
        msg << "boundary";
        break;
      default:
        msg << indices(i);
        break;
    }
  }
  msg << ")";

  return msg.str();
}


std::string multiscalelinkedcell::indicesToString( const tarch::la::Vector<TWO_POWER_D,int>& indices ) {
  std::ostringstream msg;

  msg << "(";
  for (int i=0; i<TWO_POWER_D; i++) {
    if (i!=0) msg << ",";
    switch (indices(i)) {
      case multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex:
        msg << "invalid";
        break;
      case multiscalelinkedcell::HangingVertexBookkeeper::RemoteAdjacencyIndex:
        msg << "remote";
        break;
      case multiscalelinkedcell::HangingVertexBookkeeper::DomainBoundaryAdjacencyIndex:
        msg << "boundary";
        break;
      default:
        msg << indices(i);
        break;
    }
  }
  msg << ")";

  return msg.str();
}

bool multiscalelinkedcell::adjacencyInformationIsConsistent(
    const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>&  indices
) {
  tarch::la::Vector<DIMENSIONS,int> centre(1); // Initialize center(1) as (1,1,...,1).
  dfor2(v1) // Loop over vertices.
    dfor2(v2) // Loop over vertices.
      if (v1Scalar < v2Scalar) { // Allow only single check per pair.
        dfor2(a1) // Loop over indices vertex v1.
          dfor2(a2) // Loop over indices of vertex v2.
            if (
                tarch::la::equals(v1+a1,v2+a2) &&
                tarch::la::countEqualEntries(a1+v1,centre) >= DIMENSIONS-1
            ) { // Detect overlap of adjacency data. Only include cell (i.e., centre) and face neighbours of cell.
              if( indices( v1Scalar * TWO_POWER_D + a1Scalar )!=indices( v2Scalar * TWO_POWER_D + a2Scalar ) ) {
                return false;
              }
            }
          enddforx // a2
        enddforx // a1
      }
    enddforx // v2
  enddforx // v1

  return true;
}

tarch::la::Vector<THREE_POWER_D,int> multiscalelinkedcell::getIndicesAroundCell(
  const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>&  indices
) {
  static tarch::logging::Log _log( "multiscalelinkedcell" );

  logTraceInWith1Argument( "getIndicesAroundCell(...)", indices );

  tarch::la::Vector<THREE_POWER_D,int> result;

  assertion1( adjacencyInformationIsConsistent(indices), indices );

  #ifdef Dim2
  assertionEquals1(indices( 0*TWO_POWER_D + 1 ), indices( 1*TWO_POWER_D + 0 ), indices);
  assertionEquals1(indices( 0*TWO_POWER_D + 3 ), indices( 3*TWO_POWER_D + 0 ), indices);
  assertionEquals1(indices( 3*TWO_POWER_D + 2 ), indices( 2*TWO_POWER_D + 3 ), indices);
  assertionEquals1(indices( 3*TWO_POWER_D + 1 ), indices( 1*TWO_POWER_D + 3 ), indices);

  assertionEquals1(indices( 0*TWO_POWER_D + 3 ), indices( 1*TWO_POWER_D + 2 ), indices);
  assertionEquals1(indices( 1*TWO_POWER_D + 2 ), indices( 3*TWO_POWER_D + 0 ), indices);
  assertionEquals1(indices( 3*TWO_POWER_D + 0 ), indices( 2*TWO_POWER_D + 1 ), indices);
  result(0) = indices( 0*TWO_POWER_D + 0 );
  result(1) = indices( 0*TWO_POWER_D + 1 );
  result(2) = indices( 1*TWO_POWER_D + 1 );
  result(3) = indices( 0*TWO_POWER_D + 2 );
  result(4) = indices( 0*TWO_POWER_D + 3 );
  result(5) = indices( 1*TWO_POWER_D + 3 );
  result(6) = indices( 2*TWO_POWER_D + 2 );
  result(7) = indices( 2*TWO_POWER_D + 3 );
  result(8) = indices( 3*TWO_POWER_D + 3 );

  #else
  result(0) = indices( 0*TWO_POWER_D + 0 );
  result(1) = indices( 0*TWO_POWER_D + 1 );
  result(2) = indices( 1*TWO_POWER_D + 1 );
  result(3) = indices( 0*TWO_POWER_D + 2 );
  result(4) = indices( 0*TWO_POWER_D + 3 );
  result(5) = indices( 1*TWO_POWER_D + 3 );
  result(6) = indices( 2*TWO_POWER_D + 2 );
  result(7) = indices( 2*TWO_POWER_D + 3 );
  result(8) = indices( 3*TWO_POWER_D + 3 );

  result( 9) = indices( 0*TWO_POWER_D + 4 );
  result(10) = indices( 0*TWO_POWER_D + 5 );
  result(11) = indices( 1*TWO_POWER_D + 5 );
  result(12) = indices( 0*TWO_POWER_D + 6 );
  result(13) = indices( 0*TWO_POWER_D + 7 );
  result(14) = indices( 1*TWO_POWER_D + 7 );
  result(15) = indices( 2*TWO_POWER_D + 6 );
  result(16) = indices( 2*TWO_POWER_D + 7 );
  result(17) = indices( 3*TWO_POWER_D + 7 );

  result(18) = indices( 4*TWO_POWER_D + 4 );
  result(19) = indices( 4*TWO_POWER_D + 5 );
  result(20) = indices( 5*TWO_POWER_D + 5 );
  result(21) = indices( 4*TWO_POWER_D + 6 );
  result(22) = indices( 4*TWO_POWER_D + 7 );
  result(23) = indices( 5*TWO_POWER_D + 7 );
  result(24) = indices( 6*TWO_POWER_D + 6 );
  result(25) = indices( 6*TWO_POWER_D + 7 );
  result(26) = indices( 7*TWO_POWER_D + 7 );
  #endif

  logTraceOutWith1Argument( "getIndicesAroundCell(...)", result );
  return result;
}


multiscalelinkedcell::HangingVertexBookkeeper::HangingVertexBookkeeper():
  _vertexMap() {
}


multiscalelinkedcell::HangingVertexBookkeeper&  multiscalelinkedcell::HangingVertexBookkeeper::getInstance() {
  static multiscalelinkedcell::HangingVertexBookkeeper instance;
  return instance;
}


tarch::la::Vector<TWO_POWER_D,int> multiscalelinkedcell::HangingVertexBookkeeper::createVertexLinkMapForNewVertex() {
  return tarch::la::Vector<TWO_POWER_D,int>(InvalidAdjacencyIndex);
}


tarch::la::Vector<TWO_POWER_D,int> multiscalelinkedcell::HangingVertexBookkeeper::createVertexLinkMapForBoundaryVertex() {
  return tarch::la::Vector<TWO_POWER_D,int>(DomainBoundaryAdjacencyIndex);
}


bool multiscalelinkedcell::HangingVertexBookkeeper::allAdjacencyInformationIsAvailable(const tarch::la::Vector<TWO_POWER_D,int>&  arg) {
  return !tarch::la::oneEquals(arg,InvalidAdjacencyIndex);
}


bool multiscalelinkedcell::HangingVertexBookkeeper::allAdjacencyInformationIsAvailable(const tarch::la::Vector<THREE_POWER_D,int>&  arg) {
  return !tarch::la::oneEquals(arg,InvalidAdjacencyIndex);
}


bool multiscalelinkedcell::HangingVertexBookkeeper::allAdjacencyInformationIsAvailable(const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>&  arg) {
  return !tarch::la::oneEquals(arg,InvalidAdjacencyIndex);
}


tarch::la::Vector<DIMENSIONS+1,double > multiscalelinkedcell::HangingVertexBookkeeper::getKey(
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  int                                          level
) const {
  tarch::la::Vector<DIMENSIONS+1, double> result;
  for(int d = 0; d < DIMENSIONS; d++) {
    result(d) = x(d);
  }
  result(DIMENSIONS) = level;
  return result;
}


bool multiscalelinkedcell::HangingVertexBookkeeper::holdsVertex(
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  int                                          level
) const {
  const tarch::la::Vector<DIMENSIONS+1,double > key = getKey(x,level);
  return (_vertexMap.count(key)>0);
}


bool multiscalelinkedcell::HangingVertexBookkeeper::usedVertexInThisTraversal(
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  int                                          level
) const {
  logTraceInWith2Arguments( "usedVertexInThisTraversal(...)", x, level );
  assertion(holdsVertex(x,level));
  const tarch::la::Vector<DIMENSIONS+1,double > key = getKey(x,level);
  const bool result = _vertexMap.at(key).usedInLastTraversal;
  logTraceOutWith1Argument( "usedVertexInThisTraversal(...)", result );
  return result;
}



tarch::la::Vector<TWO_POWER_D,int>&  multiscalelinkedcell::HangingVertexBookkeeper::getAdjacencyEntriesOfVertex(
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  int                                          level
) {
  const tarch::la::Vector<DIMENSIONS+1,double > key = getKey(x,level);
  assertion3(_vertexMap.count(key)==1 || level<=1,x,level,key);
  _vertexMap[key].usedInLastTraversal = true;
  return _vertexMap[key].indicesOfAdjacentCells;
}


void multiscalelinkedcell::HangingVertexBookkeeper::beginIteration() {
  for (
    VertexMap::iterator p = _vertexMap.begin();
    p != _vertexMap.end();
    p++
  ) {
    p->second.usedInLastTraversal = false;
  }
}


void multiscalelinkedcell::HangingVertexBookkeeper::endIteration() {
  for (
    VertexMap::iterator p = _vertexMap.begin();
    p != _vertexMap.end();
  ) {
    if (p->second.usedInLastTraversal==false) {
      #ifdef CompilerDoesNotSupportC11ContainerErase
      VertexMap::iterator pCopy(p);
      p--;
      _vertexMap.erase(pCopy);
      p++;
      #else
      p = _vertexMap.erase(p);
      #endif
    }
    else {
      p++;
    }
  }
}


void multiscalelinkedcell::HangingVertexBookkeeper::destroyCell(int cellIndex) {
  for (
    VertexMap::iterator p = _vertexMap.begin();
    p != _vertexMap.end();
  ) {
    bool refersToDeletedCell = false;
    for (int i=0; i<TWO_POWER_D; i++) {
      refersToDeletedCell |= p->second.indicesOfAdjacentCells(i)==cellIndex;
    }
    if (refersToDeletedCell) {
      #ifdef CompilerDoesNotSupportC11ContainerErase
      VertexMap::iterator pCopy(p);
      p--;
      _vertexMap.erase(pCopy);
      p++;
      #else
      p = _vertexMap.erase(p);
      #endif
    }
    else {
      p++;
    }
  }
}


tarch::la::Vector<TWO_POWER_D,int> multiscalelinkedcell::HangingVertexBookkeeper::createHangingVertex(
  const tarch::la::Vector<DIMENSIONS,double>&                  x,
  int                                                          level,
  const tarch::la::Vector<DIMENSIONS,int>&                     fineGridPositionOfVertex,
  const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>&  coarseGridAdjacencyEntries
) {
  const tarch::la::Vector<DIMENSIONS+1, double> key = getKey(x,level);

  logTraceInWith6Arguments( "createHangingVertex(...)",x,level,fineGridPositionOfVertex,coarseGridAdjacencyEntries,key,_vertexMap.count(key));

  if (_vertexMap.count(key)==0) {
    HangingVertexIdentifier   newHangingVertexIdentifier;
    newHangingVertexIdentifier.indicesOfAdjacentCells = createVertexLinkMapForNewVertex();
    newHangingVertexIdentifier.usedInLastTraversal    = false;
    _vertexMap.insert( std::pair<tarch::la::Vector<DIMENSIONS+1,double >, HangingVertexIdentifier>(key,newHangingVertexIdentifier) );
  }


  tarch::la::Vector<DIMENSIONS,int>   fromCoarseGridVertex;
  tarch::la::Vector<DIMENSIONS,int>   coarseGridVertexAdjacentCellDescriptionIndex;

  dfor2(k)
    for (int d=0; d<DIMENSIONS; d++) {
      if (fineGridPositionOfVertex(d)==0) {
        fromCoarseGridVertex(d)               = 0;
        coarseGridVertexAdjacentCellDescriptionIndex(d) = k(d);
      }
      else if (fineGridPositionOfVertex(d)==3) {
        fromCoarseGridVertex(d)               = 1;
        coarseGridVertexAdjacentCellDescriptionIndex(d) = k(d);
      }
      else if (k(d)==0) {
        fromCoarseGridVertex(d)               = 0;
        coarseGridVertexAdjacentCellDescriptionIndex(d) = 1;
      }
      else {
        fromCoarseGridVertex(d)               = 1;
        coarseGridVertexAdjacentCellDescriptionIndex(d) = 0;
      }
    }

    if (
      (_vertexMap[key].indicesOfAdjacentCells(kScalar)==InvalidAdjacencyIndex)
      ||
      (_vertexMap[key].indicesOfAdjacentCells(kScalar)==DomainBoundaryAdjacencyIndex)
     ) {
      _vertexMap[key].indicesOfAdjacentCells(kScalar) = coarseGridAdjacencyEntries(
        peano::utils::dLinearised(fromCoarseGridVertex,2) * TWO_POWER_D +
        peano::utils::dLinearised(coarseGridVertexAdjacentCellDescriptionIndex,2)
      );
    }
  enddforx

  logTraceOutWith2Arguments( "createHangingVertex(...)",_vertexMap[key].indicesOfAdjacentCells,_vertexMap[key].usedInLastTraversal);

  return _vertexMap[key].indicesOfAdjacentCells;
}


tarch::la::Vector<TWO_POWER_D,int> multiscalelinkedcell::HangingVertexBookkeeper::updateCellIndicesInMergeWithNeighbour(
  const tarch::la::Vector<TWO_POWER_D,int>&  adjacentRanks,
  const tarch::la::Vector<TWO_POWER_D,int>&  oldAdjacencyEntries
) {
  tarch::la::Vector<TWO_POWER_D,int> result;
  for (int i=0; i<TWO_POWER_D; i++) {
    if (
        adjacentRanks(i)==tarch::parallel::Node::getInstance().getGlobalMasterRank()
    ) {
      result(i) = DomainBoundaryAdjacencyIndex;
    }
    else if (
        adjacentRanks(i)!=tarch::parallel::Node::getInstance().getRank()
    ) {
      result(i) = RemoteAdjacencyIndex;
    }
    else {
      result(i) = oldAdjacencyEntries(i);
    }
  }
  return result;
}
