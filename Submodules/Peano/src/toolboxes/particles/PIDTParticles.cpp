#include "particles/PIDTParticles.h"


double  particles::PIDTParticles::numberOfLifts                  = 0.0;
double  particles::PIDTParticles::numberOfDrops                  = 0.0;
double  particles::PIDTParticles::numberOfParticlesSentToMaster  = 0.0;
double  particles::PIDTParticles::numberOfParticlesSentToWorkers = 0.0;
double  particles::PIDTParticles::numberOfReductionSkipsInRaPIDT = 0.0;


particles::PIDTParticles::MoveState  particles::PIDTParticles::adaptersMoveParticlesWithFlag       = particles::PIDTParticles::LastMovedInOddTraversal;
particles::PIDTParticles::MoveState  particles::PIDTParticles::adaptersSetMovedParticlesToThisFlag = particles::PIDTParticles::LastMovedInEvenTraversal;


void      particles::PIDTParticles::toggleMoveStateOfAdapters() {
  if ( adaptersMoveParticlesWithFlag == LastMovedInOddTraversal ) {
    adaptersMoveParticlesWithFlag = LastMovedInEvenTraversal;
  }
  else {
    adaptersMoveParticlesWithFlag = LastMovedInOddTraversal;
  }

  if ( adaptersSetMovedParticlesToThisFlag == LastMovedInOddTraversal ) {
      adaptersSetMovedParticlesToThisFlag = LastMovedInEvenTraversal;
  }
  else {
      adaptersSetMovedParticlesToThisFlag = LastMovedInOddTraversal;
  }
}


/*
particles::PIDTParticles::MoveState particles::PIDTParticles::getMoveStateOfAdapters() {
  return mappingMoveStateOfAdapters;
}
*/


bool particles::PIDTParticles::isContainedInDualCell(
  const tarch::la::Vector<DIMENSIONS,double>&  x,
  const tarch::la::Vector<DIMENSIONS,double>&  h,
  const tarch::la::Vector<DIMENSIONS,double>&  testPoint
) {
  bool result = true;
  for (int d=0; d<DIMENSIONS; d++) {
    assertion3(h(d)>=0.0,x,h,testPoint);
    result &= testPoint(d) >= x(d)-h(d)/2;
    result &= testPoint(d) <= x(d)+h(d)/2;
  }
  return result;
}


tarch::la::Vector<DIMENSIONS,int> particles::PIDTParticles::getDualCellOfParticle(
  const peano::grid::VertexEnumerator&         verticesEnumerator,
  const tarch::la::Vector<DIMENSIONS,double>&  p
) {
  return getDualCellOfParticle(verticesEnumerator.getCellCenter(),p);
}


tarch::la::Vector<DIMENSIONS,int> particles::PIDTParticles::getDualCellOfParticle(
  const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
  const tarch::la::Vector<DIMENSIONS,double>&  p
) {
  tarch::la::Vector<DIMENSIONS,int> result;
  for (int d=0; d<DIMENSIONS; d++) {
    result(d) = p(d) < cellCentre(d) ? 0 : 1;
  }
  return result;
}



tarch::la::Vector<DIMENSIONS,int> particles::PIDTParticles::getDualCellOfParticleWithinNextFinerLevel(
  const peano::grid::VertexEnumerator&         coarseGridVerticesEnumerator,
  const tarch::la::Vector<DIMENSIONS,double>&  p
) {
  tarch::la::Vector<DIMENSIONS,int> result;
  for (int d=0; d<DIMENSIONS; d++) {
    const double threshold0 = coarseGridVerticesEnumerator.getVertexPosition()(d) + 0.5/3.0 * coarseGridVerticesEnumerator.getCellSize()(d);
    const double threshold1 = coarseGridVerticesEnumerator.getVertexPosition()(d) + 1.5/3.0 * coarseGridVerticesEnumerator.getCellSize()(d);
    const double threshold2 = coarseGridVerticesEnumerator.getVertexPosition()(d) + 2.5/3.0 * coarseGridVerticesEnumerator.getCellSize()(d);
    if ( p(d)<threshold0 ) {
      result(d) = 0;
    }
    else if ( p(d)<threshold1 ) {
      result(d) = 1;
    }
    else if ( p(d)<threshold2 ) {
      result(d) = 2;
    }
    else {
      result(d) = 3;
    }
  }
  return result;
}
