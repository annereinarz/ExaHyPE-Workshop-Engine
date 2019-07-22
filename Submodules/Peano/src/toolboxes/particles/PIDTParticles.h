// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _PARTICLES_PIDT_PARTICLES_H_
#define _PARTICLES_PIDT_PARTICLES_H_


#include "tarch/la/Vector.h"

#include "peano/utils/Globals.h"
#include "peano/grid/VertexEnumerator.h"


namespace particles {
  class PIDTParticles {
    public:
      /**
       * We do not hold these fields in the states, as states might be
       */
      static double  numberOfLifts;
      static double  numberOfDrops;
      static double  numberOfParticlesSentToMaster;
      static double  numberOfParticlesSentToWorkers;
      static double  numberOfReductionSkipsInRaPIDT;


      enum MoveState {
        New = 0,
        /**
         * In my original implementation equal to NotMovedYet
         */
        LastMovedInOddTraversal = 1,
        /**
         * In my original implementation equal to Moved
         */
        LastMovedInEvenTraversal = 2
      };

    private:
      static MoveState  adaptersMoveParticlesWithFlag;
      static MoveState  adaptersSetMovedParticlesToThisFlag;

    public:
      static void      toggleMoveStateOfAdapters();

      template <class P>
      static void initParticle(
        P& particle
      ) {
        particle._persistentRecords._maxDx = std::numeric_limits<double>::max();
        particle.setMovedParticle( P::New );
      }


      /**
       * Moves the particle relative to its current position and resets the
       * moved flag.
       */
      template <class P>
      static void moveParticle(
        P&                                           particle,
        const tarch::la::Vector<DIMENSIONS,double>&  displacement
      ) {
        assertion1( mayMoveParticle(particle), particle.toString() );
        particle._persistentRecords._x += displacement;

        assertion2( particle._persistentRecords._maxDx <= tarch::la::max(  tarch::la::abs(particle._persistentRecords._maxDx) ), particle.toString(), displacement );

        particle.setMovedParticle( adaptersSetMovedParticlesToThisFlag );
      }


      template <class P>
      static bool mayMoveParticle(
        P&  particle
      ) {
        return particle.getMovedParticle() == adaptersMoveParticlesWithFlag;
      }

      /**
       * Test whether test point is in dual cell.
       *
       * @param x Center of dual cell
       * @param h Size of dual cell
       */
      static bool isContainedInDualCell(
        const tarch::la::Vector<DIMENSIONS,double>&  x,
        const tarch::la::Vector<DIMENSIONS,double>&  h,
        const tarch::la::Vector<DIMENSIONS,double>&  testPoint
      );

      /**
       * Get the dual cell of a particle
       *
       * Assumes that the particle is held by a cell identified by
       * verticesEnumerator. The operation returns a d-dimensional integer
       * vector holding zeros and ones that says which of the cell's adjacent
       * vertices identifies the containing dual cell.
       */
      static tarch::la::Vector<DIMENSIONS,int> getDualCellOfParticle(
        const peano::grid::VertexEnumerator&         verticesEnumerator,
        const tarch::la::Vector<DIMENSIONS,double>&  p
      );

      static tarch::la::Vector<DIMENSIONS,int> getDualCellOfParticle(
        const tarch::la::Vector<DIMENSIONS,double>&  cellCentre,
        const tarch::la::Vector<DIMENSIONS,double>&  p
      );

      static tarch::la::Vector<DIMENSIONS,int> getDualCellOfParticleWithinNextFinerLevel(
        const peano::grid::VertexEnumerator&         coarseGridVerticesEnumerator,
        const tarch::la::Vector<DIMENSIONS,double>&  p
      );
    };
}



#endif
