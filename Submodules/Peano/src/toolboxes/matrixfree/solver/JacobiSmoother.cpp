#include "matrixfree/solver/JacobiSmoother.h"
#include "tarch/Assertions.h"
#include "tarch/la/ScalarOperations.h"


#include <cmath>
#include <iostream>


matrixfree::solver::JacobiSmoother::JacobiSmoother():
  _omega(1.0),
  _complexOmega(1.0,0.0),
  _normOfSolutionMax(0.0),
  _normOfSolutionH(0.0),
  _measureUpdateUInMaxNorm(0.0),
  _measureUpdateUInHNorm(0.0),
  _residualInMaxNorm(0.0),
  _residualInEukledianNorm(0.0),
  _numberOfStencilUpdates(0) {
}


matrixfree::solver::JacobiSmoother::JacobiSmoother(const JacobiSmoother& smoother):
  _omega(smoother._omega),
  _complexOmega(smoother._complexOmega),
  _normOfSolutionMax(0.0),
  _normOfSolutionH(0.0),
  _measureUpdateUInMaxNorm(0.0),
  _measureUpdateUInHNorm(0.0),
  _residualInMaxNorm(0.0),
  _residualInEukledianNorm(0.0),
  _numberOfStencilUpdates(0) {
}


matrixfree::solver::JacobiSmoother::JacobiSmoother(double newOmega):
  _omega(newOmega),
  _complexOmega(newOmega,0.0) {
}


matrixfree::solver::JacobiSmoother::~JacobiSmoother() {
}


void matrixfree::solver::JacobiSmoother::informAboutInitialValues(
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


void matrixfree::solver::JacobiSmoother::informAboutSolutionUpdate(
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
  _measureUpdateUInHNorm    += std::abs(update)*hVolume;
  _residualInEukledianNorm  += residual * residual;
}


void matrixfree::solver::JacobiSmoother::informAboutSolutionUpdate(
  const std::complex<double>&  update,
  double                       hVolume,
  const std::complex<double>&  residual
) {
  if (tarch::la::abs(update)>_measureUpdateUInMaxNorm) {
    _measureUpdateUInMaxNorm = tarch::la::abs(update);
  }
  if (tarch::la::abs(residual)>_residualInMaxNorm) {
    _residualInMaxNorm = tarch::la::abs(residual);
  }
  _measureUpdateUInHNorm    += tarch::la::abs(update)*hVolume;
  _residualInEukledianNorm  += tarch::la::abs(residual) * tarch::la::abs(residual);
}


double matrixfree::solver::JacobiSmoother::getNewValueOfJacobiStep(double u, double residual, double diag, double hVolume, double omega) {
  assertion( omega>=0.0 );
  assertion( diag!=0.0 );

  double update   = omega*residual/diag;
  double result = u + update;

  informAboutSolutionUpdate( update, hVolume, residual );

  if (std::abs(result)>_normOfSolutionMax) {
    _normOfSolutionMax = std::abs(result);
  }
  _normOfSolutionH += std::abs(result)*hVolume;

  _numberOfStencilUpdates++;

  return result;
}


double matrixfree::solver::JacobiSmoother::getNewValueOfJacobiStep(double u, double residual, double diag, double hVolume) {
  return getNewValueOfJacobiStep(u,residual,diag,_omega);
}


std::complex<double> matrixfree::solver::JacobiSmoother::getNewValueOfJacobiStep(
  const std::complex<double>&  u,
  const std::complex<double>&  residual,
  const std::complex<double>&  diag,
  double                       hVolume
) {
  return getNewValueOfJacobiStep( u, residual, diag, hVolume, _complexOmega );
}


std::complex<double> matrixfree::solver::JacobiSmoother::getNewValueOfJacobiStep(
  const std::complex<double>&  u,
  const std::complex<double>&  residual,
  const std::complex<double>&  diag,
  double                       hVolume,
  const std::complex<double>&  omega
) {
  assertion4( diag.real()*diag.real() + diag.imag()*diag.imag()!=0.0, u, residual, diag, hVolume );

  std::complex<double> update = omega*residual/diag;
  std::complex<double> result = u + update;

  informAboutSolutionUpdate( update, hVolume, residual );

  if (std::abs(result)>_normOfSolutionMax) {
    _normOfSolutionMax = std::abs(result);
  }
  _normOfSolutionH += std::abs(result)*hVolume;

  _numberOfStencilUpdates++;

  return result;
}


double matrixfree::solver::JacobiSmoother::getNewValueOfJacobiStep(double u, double residual, double diag) {
  assertion( _omega!=0.0 );
  assertion( diag!=0.0 );

  double update   = _omega*residual/diag;
  double result = u + update;

  _numberOfStencilUpdates++;

  return result;
}


std::complex<double> matrixfree::solver::JacobiSmoother::getNewValueOfJacobiStep(
  const std::complex<double>&  u,
  const std::complex<double>&  residual,
  const std::complex<double>&  diag
) {
  assertion( _omega!=0.0 );
  assertion3( diag.real()*diag.real() + diag.imag()*diag.imag()!=0.0, u, residual, diag );

  std::complex<double> update = _omega*residual/diag;
  std::complex<double> result = u + update;

  _numberOfStencilUpdates++;

  return result;
}


void matrixfree::solver::JacobiSmoother::setOmega(double  value){
  assertion1( value>0.0,value );
  assertion1( value<2.0,value );
  _omega        = value;
  _complexOmega = value;
}


void matrixfree::solver::JacobiSmoother::setOmega(const std::complex<double>&  value){
  _omega = std::numeric_limits<double>::infinity();
  _complexOmega = value;
}


void matrixfree::solver::JacobiSmoother::clearMeasurements() {
  _normOfSolutionMax       = 0.0;
  _normOfSolutionH         = 0.0;
  _measureUpdateUInMaxNorm = 0.0;
  _measureUpdateUInHNorm   = 0.0;
  _numberOfStencilUpdates  = 0;
  _residualInMaxNorm       = 0.0;
  _residualInEukledianNorm = 0.0;
}


void matrixfree::solver::JacobiSmoother::mergeWithJacobiSmootherFromOtherThread(const matrixfree::solver::JacobiSmoother& smoother) {
  assertionEquals( _omega, smoother._omega );

  _normOfSolutionMax        = _normOfSolutionMax > smoother._normOfSolutionMax ? _normOfSolutionMax : smoother._normOfSolutionMax;
  _normOfSolutionH         += smoother._normOfSolutionH;
  _measureUpdateUInMaxNorm  = _measureUpdateUInMaxNorm > smoother._measureUpdateUInMaxNorm ? _measureUpdateUInMaxNorm : smoother._measureUpdateUInMaxNorm;
  _measureUpdateUInHNorm   += smoother._measureUpdateUInHNorm;
  _numberOfStencilUpdates  += smoother._numberOfStencilUpdates;
  _residualInMaxNorm        = _residualInMaxNorm > smoother._residualInMaxNorm ? _residualInMaxNorm : smoother._residualInMaxNorm;
  _residualInEukledianNorm += smoother._residualInEukledianNorm;

}


double matrixfree::solver::JacobiSmoother::getSolutionInMaximumNorm() const {
  return _normOfSolutionMax;
}


double matrixfree::solver::JacobiSmoother::getSolutionInHNorm() const {
  return _normOfSolutionH;
}


double matrixfree::solver::JacobiSmoother::getSolutionUpdateInMaximumNorm() const {
  return _measureUpdateUInMaxNorm;
}


double matrixfree::solver::JacobiSmoother::getSolutionUpdateInHNorm() const {
  return _measureUpdateUInHNorm;
}


int matrixfree::solver::JacobiSmoother::getNumberOfStencilUpdates() const {
  return _numberOfStencilUpdates;
}


double matrixfree::solver::JacobiSmoother::getResidualInEukledianNorm() const {
  return tarch::la::equals(_residualInEukledianNorm,0.0) ? 0.0 : std::sqrt(_residualInEukledianNorm);
}


double matrixfree::solver::JacobiSmoother::getResidualInEukledianNormSquared() const {
  return _residualInEukledianNorm;
}


double matrixfree::solver::JacobiSmoother::getResidualInMaxNorm() const {
  return _residualInMaxNorm;
}
