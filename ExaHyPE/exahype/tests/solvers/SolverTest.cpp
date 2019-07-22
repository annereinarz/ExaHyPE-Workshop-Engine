/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon 
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/
 
#include "exahype/tests/solvers/SolverTest.h"

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "tarch/tests/TestCaseFactory.h"

#include "exahype/solvers/Solver.h"

#include <limits>

registerTest(exahype::tests::solvers::SolverTest)
#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", off)
#endif

    exahype::tests::solvers::SolverTest::SolverTest()
    : tarch::tests::TestCase("exahype::tests::solvers::SolverTest") {
}

exahype::tests::solvers::SolverTest::~SolverTest() {}

void exahype::tests::solvers::SolverTest::run() { testMethod(testSolve); }

void exahype::tests::solvers::SolverTest::testSolve() {
  /*  exahype::solvers::Solve solve(
        0, // solverNumber
        true, // corrector time lagging
        false  // active
    );

    validateEquals(solve.getSolverNumber()       ,0);
    validateEquals(solve.isCorrectorTimeLagging(),true);
    validateEquals(solve.isActive()              ,false);

    validateNumericalEqualsWithEps(solve.getCorrectorTimeStepSize(),std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStepSize(),std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStamp(),std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStamp(),std::numeric_limits<double>::max(),1e-10);

    //
    // check the bucket chain shifting of the time step sizes and stamps
    //
    // initialise predictor time stamp
    solve.setPredictorTimeStamp(0.);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStepSize(),std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStepSize(),std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStamp()
    ,std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStamp()   ,0. ,1e-10);

    // step 1
    solve.updateNextPredictorTimeStepSize(5.);
    solve.startNewTimeStep();

    validateNumericalEqualsWithEps(solve.getCorrectorTimeStepSize(),std::numeric_limits<double>::max(),1e-10);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStamp()   ,0. ,1e-10);

    validateNumericalEqualsWithEps(solve.getPredictorTimeStepSize(),5. ,1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStamp()   ,5. ,1e-10);

    // step 2
    solve.updateNextPredictorTimeStepSize(20.);
    solve.startNewTimeStep();

    validateNumericalEqualsWithEps(solve.getCorrectorTimeStepSize(),5. ,1e-10);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStamp()   ,5. ,1e-10);

    validateNumericalEqualsWithEps(solve.getPredictorTimeStepSize(),20.,1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStamp()   ,25.,1e-10);

    //
    //  copy the solver
    //
    exahype::solvers::Solve solveCopy(solve);
    solveCopy.setParentSolve(0);

    // check everything again for new solver
    validateEquals(solveCopy.getSolverNumber()       ,0);
    validateEquals(solveCopy.getParentSolve()        ,0);
    validateEquals(solveCopy.isCorrectorTimeLagging(),true);
    validateEquals(solveCopy.isActive()              ,false);

    validateNumericalEqualsWithEps(solveCopy.getCorrectorTimeStepSize(),5.
    ,1e-10);
    validateNumericalEqualsWithEps(solveCopy.getCorrectorTimeStamp()   ,5.
    ,1e-10);

    validateNumericalEqualsWithEps(solveCopy.getPredictorTimeStepSize(),20.,1e-10);
    validateNumericalEqualsWithEps(solveCopy.getPredictorTimeStamp()
    ,25.,1e-10);

    //merge two solvers
    //
    // create an additional solver
    exahype::solvers::Solve otherSolve(
        0, // solverNumber
        true, // corrector time lagging
        true  // active
    );

    otherSolve.setCorrectorTimeStamp(0.1);
    otherSolve.setCorrectorTimeStepSize(0.2);

    otherSolve.setPredictorTimeStamp(0.3);
    otherSolve.setPredictorTimeStepSize(0.4);
    otherSolve.updateNextPredictorTimeStepSize(0.5);

    // merge two solvers
    solve.merge(otherSolve);

    // check properties of solve again
    validate(solve.isActive()==false);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStamp() ,0.1,1e-10);
    validateNumericalEqualsWithEps(solve.getCorrectorTimeStepSize() ,0.2,1e-10);

    validateNumericalEqualsWithEps(solve.getPredictorTimeStamp() ,0.3,1e-10);
    validateNumericalEqualsWithEps(solve.getPredictorTimeStepSize() ,0.4,1e-10);

    validateNumericalEqualsWithEps(solve.getNextPredictorTimeStepSize(),0.5,1e-10);*/
}

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", on)
#endif
