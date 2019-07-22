// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_JACOBI_SMOOTHER_H_
#define _MATRIXFREE_SOLVER_JACOBI_SMOOTHER_H_


#include "peano/utils/Globals.h"
#include "tarch/la/Vector.h"


#include <complex>


namespace matrixfree {
  namespace solver{
    class JacobiSmoother;
  }
}


/**
 * Jacobi smoother
 *
 * Class for an Jacobi smoother we provide for matrix-free methods. Can also be
 * used for explicit time stepping methods where no solver is needed. It enables
 * you to compute the updates to the residuals due to stencils, and it keeps
 * track of the solution, the residual, and the update of the solution due to
 * the Jacobi step in different norms. Finally, it also bookkeeps how many
 * stencil updates have been conducted.
 *
 * @author Tobias Weinzierl
 */
class matrixfree::solver::JacobiSmoother {
  protected:
    /**
     * Relaxation factor.
     */
    double                                             _omega;

    std::complex<double>                               _complexOmega;

    /**
     * Norm of the solution in maximum norm.
     */
    double                                             _normOfSolutionMax;

    /**
     * Norm of the solution in h-norm (i.e. L2 if we use equidistant meshes).
     */
    double                                             _normOfSolutionH;

   /**
    * Norm of solution update in max norm.
    */
    double                                             _measureUpdateUInMaxNorm;

    /**
     * Norm of solution update in h norm.
     */
    double                                             _measureUpdateUInHNorm;

    double                                             _residualInMaxNorm;
    double                                             _residualInEukledianNorm;

    int                                                _numberOfStencilUpdates;
  public:
    JacobiSmoother();

    /**
     * Copy constructor
     *
     * Used implicitly by the shared memory parallelisation. This operation
     * copies the smoother's settings such as the relaxation factor. But it
     * does not copy the smoother's statistics variables such as the number
     * of stencil updates.
     */
    JacobiSmoother(const JacobiSmoother& smoother);
    JacobiSmoother(double newOmega);
    virtual ~JacobiSmoother();

    /**
     * Tell the smoother that we've updated the solution. Residual is
     * usually left blank if an explicit scheme is used where no equation
     * system is solved.
     *
     * This operation is intended to be used if you do not use
     * getNewValueOfJacobiStep(). The other way round: If you use
     * getNewValueOfJacobiStep() to compute updates to your equation
     * system, there is no need to call this one.
     */
    void informAboutSolutionUpdate(
      double update,
      double hVolume,
      double residual = 0.0
    );

    void informAboutSolutionUpdate(
      const std::complex<double>& update,
      double                      hVolume,
      const std::complex<double>& residual = 0.0
    );

    /**
     * Inform code about initial values. If you don't want to evaluate a
     * residual in the beginning, pass zero.
     */
    void informAboutInitialValues(
      double u,
      double initialResidual,
      double hVolume = 1.0
    );

    /**
     * Compute the new value according to a Jacobi step.
     *
     * @param hVolume Is the volume of the cells adjacent to the unknown's
     *                vertex. If you set it to 1, the h norm equals the
     *                Eukledian norm (see getSolutionInHNorm()).
     */
    double getNewValueOfJacobiStep(
      double u,
      double residual,
      double diag,
      double hVolume,
      double omega
    );

    double getNewValueOfJacobiStep(
      double u,
      double residual,
      double diag,
      double hVolume
    );

    std::complex<double> getNewValueOfJacobiStep(
      const std::complex<double>&  u,
      const std::complex<double>&  residual,
      const std::complex<double>&  diag,
      double                       hVolume,
      const std::complex<double>&  omega
    );

    /**
     * Use getOmega() to invoke other variant of getNewValueOfJacobiStep().
     */
    std::complex<double> getNewValueOfJacobiStep(
      const std::complex<double>&  u,
      const std::complex<double>&  residual,
      const std::complex<double>&  diag,
      double                       hVolume
    );

    /**
     * Neglects internal statistics on solution and residual.
     *
     * Usually only used by multigrid-type solvers for inner levels, i.e. the
     * not fine grid.
     */
    double getNewValueOfJacobiStep(
      double u,
      double residual,
      double diag
    );

    /**
     * Neglects internal statistics on solution and residual.
     *
     * Usually only used by multigrid-type solvers for inner levels, i.e. the
     * not fine grid.
     */
    std::complex<double> getNewValueOfJacobiStep(
      const std::complex<double>&  u,
      const std::complex<double>&  residual,
      const std::complex<double>&  diag
    );

    /**
     * Omega is the relaxation factor
     */
    void setOmega(double value);

    /**
     * Omega is the relaxation factor
     */
    void setOmega(const std::complex<double>& value);

    /**
     * The solver internally tracks different norms of the solution and of the
     * solution update. For this, you have to call clearMeasurements at the
     * begin of the iteration. At the end of the traversal, you then can read
     * the global values.
     *
     * Before the operation clears all the fields, it copies them to the so-far
     * fields. These fields are important for time-dependent problems where the
     * norms of the solutions have to be tracked
     */
    void clearMeasurements();

    double getSolutionInMaximumNorm() const;
    double getSolutionInHNorm() const;

    double getSolutionUpdateInMaximumNorm() const;
    double getSolutionUpdateInHNorm() const;

    double getResidualInEukledianNorm() const;
    /**
     * If we work in a parallel setting, we need access to the squared value as
     * well to merge different residuals before we apply the square root.
     */
    double getResidualInEukledianNormSquared() const;
    double getResidualInMaxNorm() const;

    int getNumberOfStencilUpdates() const;

    /**
     * Merge with a smoother object from another thread
     *
     * You may copy a smoother bit-wise and use it in another thread. However,
     * when that other thread terminates, we have to merge the statistics. That
     * can be done by this operation.
     */
    void mergeWithJacobiSmootherFromOtherThread(const JacobiSmoother& smoother);
};

#endif
