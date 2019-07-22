#include "exahype/repositories/RepositoryFactory.h"

#include "exahype/repositories/RepositorySTDStack.h"

#include "exahype/records/RepositoryState.h"

#ifdef Parallel
#include "tarch/parallel/NodePool.h"
#include "peano/parallel/Partitioner.h"
#endif


exahype::repositories::RepositoryFactory::RepositoryFactory() {
  #ifdef Parallel
  peano::parallel::Partitioner::initDatatypes();

  exahype::State::initDatatype();
  exahype::Vertex::initDatatype();
  exahype::Cell::initDatatype();

  if (exahype::records::RepositoryState::Datatype==0) {
    exahype::records::RepositoryState::initDatatype();
  }
  #endif
}


exahype::repositories::RepositoryFactory::~RepositoryFactory() {
}


void exahype::repositories::RepositoryFactory::shutdownAllParallelDatatypes() {
  #ifdef Parallel
  peano::parallel::Partitioner::shutdownDatatypes();

  exahype::State::shutdownDatatype();
  exahype::Vertex::shutdownDatatype();
  exahype::Cell::shutdownDatatype();

  if (exahype::records::RepositoryState::Datatype!=0) {
    exahype::records::RepositoryState::shutdownDatatype();
    exahype::records::RepositoryState::Datatype = 0;
  }
  #endif
}


exahype::repositories::RepositoryFactory& exahype::repositories::RepositoryFactory::getInstance() {
  static exahype::repositories::RepositoryFactory singleton;
  return singleton;
}

    
exahype::repositories::Repository* 
exahype::repositories::RepositoryFactory::createWithSTDStackImplementation(
  peano::geometry::Geometry&                   geometry,
  const tarch::la::Vector<DIMENSIONS,double>&  domainSize,
  const tarch::la::Vector<DIMENSIONS,double>&  computationalDomainOffset
) {
  #ifdef Parallel
  if (!tarch::parallel::Node::getInstance().isGlobalMaster()) {
    return new exahype::repositories::RepositorySTDStack(geometry);
  }
  else
  #endif
  return new exahype::repositories::RepositorySTDStack(geometry, domainSize, computationalDomainOffset);
}
