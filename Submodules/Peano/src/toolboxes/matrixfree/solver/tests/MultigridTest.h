// Copyright (C) 2009 Technische Universitaet Muenchen
// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www5.in.tum.de/peano
#ifndef _MATRIXFREE_SOLVER_TESTS_MULTIGRID_TEST_H_
#define _MATRIXFREE_SOLVER_TESTS_MULTIGRID_TEST_H_


#include "tarch/tests/TestCaseFactory.h"
#include "tarch/tests/TestCase.h"


namespace matrixfree {
  namespace solver {
    namespace tests {
      class MultigridTest;
    }
  }
}


/**
 * Test case for the matrix-free Galerkin components
 *
 * @author Marion Weinzierl
 */
class matrixfree::solver::tests::MultigridTest: public tarch::tests::TestCase{
  private:
    void testCalculateCellInterGridTransferOperator();
    void testCalculatePetrovGalerkinCoarseGridOperatorForBilinearInterGridTransferOperators();
    void testReconstructStencilFragments();

    /**
     * Example from the solver that tests positivitiy.
     */
    void testReconstruction0();
  public:
    MultigridTest();
    virtual ~MultigridTest();

    virtual void run();
};

#endif
