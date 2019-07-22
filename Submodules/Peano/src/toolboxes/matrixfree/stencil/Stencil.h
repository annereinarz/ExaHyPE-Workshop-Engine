// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_STENCIL_H_
#define _MATRIXFREE_SOLVER_STENCIL_H_

#include "peano/utils/Globals.h"

#include "tarch/la/Vector.h"
#include "tarch/la/Matrix.h"

#include <complex>


namespace matrixfree {
    namespace stencil {
      /**
       * Element-wise Matrix
       */
      typedef tarch::la::Matrix<NUMBER_OF_VERTICES_PER_ELEMENT,NUMBER_OF_VERTICES_PER_ELEMENT, double> ElementWiseAssemblyMatrix;

      typedef tarch::la::Matrix<NUMBER_OF_VERTICES_PER_ELEMENT,NUMBER_OF_VERTICES_PER_ELEMENT, std::complex<double> > ComplexElementWiseAssemblyMatrix;

      typedef tarch::la::Matrix<TWO_POWER_D,TWO_POWER_D, double> InterGridTransferMatrix;

      typedef tarch::la::Matrix<NUMBER_OF_VERTICES_PER_ELEMENT/2,NUMBER_OF_VERTICES_PER_ELEMENT/2, double> ElementWiseAssemblyMatrixOnSpaceTimeGrid;

      /**
       * Stencil
       */
      typedef tarch::la::Vector<THREE_POWER_D,double>  Stencil;
      typedef tarch::la::Vector<THREE_POWER_D,std::complex<double> >  ComplexStencil;

      /**
       * Vectors.
       */
      typedef tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D,double>  VectorOfStencils;
    typedef tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D,std::complex<double> >  VectorOfComplexStencils;

      /**
       * Vector of Element (Lexicographic Ordering of Unknowns)
       */
      typedef tarch::la::Vector<NUMBER_OF_VERTICES_PER_ELEMENT,double> ElementWiseVector;

      int getStencilEntryLinearisedIndex( const tarch::la::Vector<DIMENSIONS,int>  stencilEntry);


    }
}

#endif
