#include "matrixfree/solver/Smoother.h"
#include "tarch/Assertions.h"

#include <cmath>


matrixfree::solver::Smoother::Smoother():
  _omega(1.0),
  _normOfSolutionMax(0.0),
  _normOfSolutionH(0.0),
  _measureUpdateUInMaxNorm(0.0),
  _measureUpdateUInHNorm(0.0),
  _residualInMaxNorm(0.0),
  _residualInEukledianNorm(0.0),
  _numberOfStencilUpdates(0) {
}


matrixfree::solver::Smoother::Smoother(const Smoother& smoother):
  _omega(smoother._omega),
  _normOfSolutionMax(0.0),
  _normOfSolutionH(0.0),
  _measureUpdateUInMaxNorm(0.0),
  _measureUpdateUInHNorm(0.0),
  _residualInMaxNorm(0.0),
  _residualInEukledianNorm(0.0),
  _numberOfStencilUpdates(0) {
}


matrixfree::solver::Smoother::Smoother(double newOmega):
  _omega(newOmega) {
}


matrixfree::solver::Smoother::~Smoother() {
}


void matrixfree::solver::Smoother::informAboutInitialValues(
  double u,
  double initialResidual,
  double hVolume
) {
  if (std::abs(u)>_normOfSolutionMax) {
    _normOfSolutionMax = std::abs(u);
  }
  _normOfSolutionH += std::abs(u)*hVolume;

  if (std::abs(initialResidual)>_residualInMaxNorm) {
    _residualInMaxNorm = std::abs(initialResidual);
  }
  _residualInEukledianNorm      += initialResidual * initialResidual;
}


void matrixfree::solver::Smoother::informAboutSolutionUpdate(
  double update,
  double hVolume,
  double residual
) {
  if (std::abs(update)>_measureUpdateUInMaxNorm) {
    _measureUpdateUInMaxNorm = std::abs(update);
  }
  if (std::abs(residual)>_residualInMaxNorm) {
    _residualInMaxNorm = std::abs(residual);
  }
  _measureUpdateUInHNorm += std::abs(update)*hVolume;
  _residualInEukledianNorm      += residual * residual;
}


double matrixfree::solver::Smoother::getNewValueOfJacobiStep(double u, double residual, double diag, double hVolume) {
  assertion( _omega!=0.0 );
  assertion( diag!=0.0 );

  double update   = _omega*residual/diag;
  double result = u + update;

  informAboutSolutionUpdate( update, hVolume, residual );

  if (std::abs(result)>_normOfSolutionMax) {
    _normOfSolutionMax = std::abs(result);
  }
  _normOfSolutionH += std::abs(result)*hVolume;

  _numberOfStencilUpdates++;

  return result;
}


double matrixfree::solver::Smoother::getOmega() {
  return _omega;
}

void matrixfree::solver::Smoother::setOmega(double value){
  assertion( value>0.0 );
  assertion( value<2.0 );
  _omega = value;
}


void matrixfree::solver::Smoother::clearMeasurements() {
  _normOfSolutionMax       = 0.0;
  _normOfSolutionH         = 0.0;
  _measureUpdateUInMaxNorm = 0.0;
  _measureUpdateUInHNorm   = 0.0;
  _numberOfStencilUpdates  = 0;
  _residualInMaxNorm       = 0.0;
  _residualInEukledianNorm        = 0.0;
}


void matrixfree::solver::Smoother::mergeWithSmootherFromOtherThread(const matrixfree::solver::Smoother& smoother) {
  assertionEquals( _omega, smoother._omega );

  _normOfSolutionMax        = _normOfSolutionMax > smoother._normOfSolutionMax ? _normOfSolutionMax : smoother._normOfSolutionMax;
  _normOfSolutionH         += smoother._normOfSolutionH;
  _measureUpdateUInMaxNorm  = _measureUpdateUInMaxNorm > smoother._measureUpdateUInMaxNorm ? _measureUpdateUInMaxNorm : smoother._measureUpdateUInMaxNorm;
  _measureUpdateUInHNorm   += smoother._measureUpdateUInHNorm;
  _numberOfStencilUpdates  += smoother._numberOfStencilUpdates;
  _residualInMaxNorm        = _residualInMaxNorm > smoother._residualInMaxNorm ? _residualInMaxNorm : smoother._residualInMaxNorm;
  _residualInEukledianNorm        += smoother._residualInEukledianNorm;

}


double matrixfree::solver::Smoother::getSolutionInMaximumNorm() const {
  return _normOfSolutionMax;
}


double matrixfree::solver::Smoother::getSolutionInHNorm() const {
  return _normOfSolutionH;
}


double matrixfree::solver::Smoother::getSolutionUpdateInMaximumNorm() const {
  return _measureUpdateUInMaxNorm;
}


double matrixfree::solver::Smoother::getSolutionUpdateInHNorm() const {
  return _measureUpdateUInHNorm;
}


int matrixfree::solver::Smoother::getNumberOfStencilUpdates() const {
  return _numberOfStencilUpdates;
}


double matrixfree::solver::Smoother::getResidualInEukledianNorm() const {
  return tarch::la::equals(_residualInEukledianNorm,0.0) ? 0.0 : std::sqrt(_residualInEukledianNorm);
}


double matrixfree::solver::Smoother::getResidualInMaxNorm() const {
  return _residualInMaxNorm;
}
