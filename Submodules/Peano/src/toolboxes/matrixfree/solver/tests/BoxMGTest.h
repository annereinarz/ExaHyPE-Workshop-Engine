// Copyright (C) 2009 Technische Universitaet Muenchen
// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www5.in.tum.de/peano
#ifndef _MATRIXFREE_SOLVER_TESTS_BOXMG_TEST_H_
#define _MATRIXFREE_SOLVER_TESTS_BOXMG_TEST_H_


#include "tarch/tests/TestCaseFactory.h"
#include "tarch/tests/TestCase.h"


namespace matrixfree {
  namespace solver {
    namespace tests {
      class BoxMGTest;
    }
  }
}


/**
 * Test case for the BoxMG routine. This test case compares the output for
 * some input vectors to the original 2d implementation of Marion Weinzierl.
 * This way, we are able to exchange this implementation that basically
 * follows the original description by Dendy et al by a description that is
 * based upon large matrix-vector products and matrix inversions.
 *
 * @author Marion Weinzierl
 */
class matrixfree::solver::tests::BoxMGTest: public tarch::tests::TestCase{
  private:
    /**
     * Example from the solver that tests positivitiy.
     */
    void runPoisson();
    void runPoissonWithSymmetricP();
    void run2dPoissonComparisonToLegacyCode();
    void run2dConvectionComparisonToLegacyCode();
    void run2dConvectionWithSymmetricP();
  public:
    BoxMGTest();
    virtual ~BoxMGTest();

    virtual void run();
};

#endif
