// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _PEANO_TOOLBOX_TIME_STEPPER_H_
#define _PEANO_TOOLBOX_TIME_STEPPER_H_


#include "tarch/logging/Log.h"


namespace matrixfree {
  class TimeStepper;
}


/**
 * Time Stepping Algorithm
 *
 * Manages the time step sizes and keeps track how often to write snapshots to
 * files.
 *
 *
 * @author Tobias Weinzierl
 */
class matrixfree::TimeStepper {
  private:
    static tarch::logging::Log _log;

    double  _time;
    double  _timeStepSize;
    double  _maxDifferenceFromTimeStepToTimeStepInMaxNorm;
    double  _maxDifferenceFromTimeStepToTimeStepInHNorm;
    double  _timeInBetweenTwoSnapshots;
    double  _nextSnapshotIsDue;
    bool    _printInitialCondition;
  public:
    /**
     * @param maxDifferenceFromTimeStepToTimeStepInMaxNorm           0.0 to switch it off
     * @param deltaInBetweenTwoSnapshots                             Pass a negative value if you are not interested in snapshot information, or 0 if you wanna have snapshots always.
     * @param maximumNumberOfIterationsOfLinearEquationSystemSolver  Set it to 0 if you are using an explicit scheme.
     */
    TimeStepper(
      double  initialTimeStepSize,
      double  maxDifferenceFromTimeStepToTimeStepInMaxNorm,
      double  maxDifferenceFromTimeStepToTimeStepInHNorm,
      double  deltaInBetweenTwoSnapshots,
      bool    printInitialCondition
    );

    double getTime() const;
    double getTimeStepSize() const;

    void switchToNextTimeStep();

    /**
     * Shall we write snapshot
     *
     * This operation usually tells the application to write a snapshot
     * whenever the time since the last snapshot has overran the given time
     * interval in-between two snapshots. It also tells the application to
     * write a snapshot if the grid has changed, i.e. if the domain has moved
     * or if a refinement criterion has decided to refine the grid further.
     */
    bool shallWriteSnapshot() const;
    void wroteSnapshot();

    void setMaxDifferenceFromTimeStepToTimeStepInMaxNorm( double maxDifferenceFromTimeStepToTimeStepInMaxNorm );
    void setMaxDifferenceFromTimeStepToTimeStepInHNorm( double maxDifferenceFromTimeStepToTimeStepInHNorm );
    void setDeltaInBetweenTwoSnapshots( double deltaInBetweenTwoSnapshots );

    /**
     * Analyse whether the chosen time step size is valid or whether one should
     * give a try with a smaller time step size before we continue. If the
     * operation returns false, you should call computeNewTimeStepSize(). And
     * you should also clear your number of iteration of the equation system
     * solver.
     */
    bool isTimeStepSizeWellSuitedToProceed(
      double updateInHNorm,
      double updateInMaxNorm,
      bool   linearEquationSystemHasTerminatedWithLessIterationsThanNecessary
    ) const;

    /**
     * @param numberOfIterationsOfPreviousTimeStep Pass 0 if you are using an explicit scheme.
     */
    void computeNewTimeStepSize(
      double updateInHNorm,
      double updateInMaxNorm,
      bool   linearEquationSystemHasTerminatedWithLessIterationsThanNecessary
    );
};


#endif
