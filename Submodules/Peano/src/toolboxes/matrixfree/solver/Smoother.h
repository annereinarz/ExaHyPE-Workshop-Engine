// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_SOLVER_SMOOTHER_H_
#define _MATRIXFREE_SOLVER_SMOOTHER_H_


#include "peano/utils/Globals.h"
#include "tarch/la/Vector.h"


namespace matrixfree {
  namespace solver{
    class Smoother;
  }
}


/**
 * Smoother Collection
 *
 * Class for an Jacobi smoother we provide for matrix-free methods. Can also be
 * used for explicit time stepping methods where no solver is needed. It enables
 * you to compute the updates to the residuals due to stencils, and it keeps
 * track of the solution, the residual, and the update of the solution due to
 * the Jacobi step in different norms. Finally, it also bookkeeps how many
 * stencil updates have been conducted.
 *
 * !!! Multithreading
 *
 * This class is thread-safe.
 *
 * @author Tobias Weinzierl
 */
class matrixfree::solver::Smoother {
  protected:
	/**
	 * Relaxation factor.
	 */
    double                                             _omega;

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
    Smoother();

    /**
     * Copy constructor
     *
     * Used implicitly by the shared memory parallelisation. This operation
     * copies the smoother's settings such as the relaxation factor. But it
     * does not copy the smoother's statistics variables such as the number
     * of stencil updates.
     */
    Smoother(const Smoother& smoother);
    Smoother(double newOmega);
    virtual ~Smoother();

    /**
     * Tell the smoother that we've updated the solution. Residual is
     * usually left blank if an explicit scheme is used where no equation
     * system is solved.
     */
    void informAboutSolutionUpdate(
      double update,
      double hVolume,
      double residual = 0.0
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
     *
     * !!! Thread Safety
     *
     * This operation is not thread safe. If you use the spacetree and if you
     * trigger this operation within touchVertexLastTime(), there's no need to
     * make the implementation thread-safe as the access to the output stack
     * already is serialised. However, if you use the regular grid, you have
     * to make the whole stuff thread safe.
     *
     * @param hVolume Is the volume of the cells adjacent to the unknown's
     *                vertex. If you set it to 1, the h norm equals the
     *                Eukledian norm (see getSolutionInHNorm()).
     */
    double getNewValueOfJacobiStep(
      double u,
      double residual,
      double diag,
      double hVolume = 1.0
    );

    /**
     * Omega is the relaxation factor
     */
    double getOmega();

    /**
     * Omega is the relaxation factor
     */
    void setOmega(double value);

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
    double getResidualInMaxNorm() const;

    int getNumberOfStencilUpdates() const;

    /**
     * Merge with a smoother object from another thread
     *
     * You may copy a smoother bit-wise and use it in another thread. However,
     * when that other thread terminates, we have to merge the statistics. That
     * can be done by this operation.
     */
    void mergeWithSmootherFromOtherThread(const Smoother& smoother);
};

#endif
