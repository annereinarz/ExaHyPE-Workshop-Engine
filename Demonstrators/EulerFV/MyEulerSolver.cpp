#include "MyEulerSolver.h"

#include "MyEulerSolver_Variables.h"

#include "Logo.h"


tarch::logging::Log EulerFV::MyEulerSolver::_log( "EulerFV::MyEulerSolver" );


void EulerFV::MyEulerSolver::init(const std::vector<std::string>& cmdlineargs,const exahype::parser::ParserView& constants) {
  // @todo Please implement/augment if required
}

void EulerFV::MyEulerSolver::adjustSolution(const double* const x,const double t,const double dt, double* const Q) {
  if ( tarch::la::equals( t,0.0 ) ) {
  Variables vars(Q);
  
  tarch::la::Vector<DIMENSIONS,double> myX( x[0] - 0.06, 1.0-x[1] - 0.25 ); // translate
  myX *= static_cast<double>(Image.width);
  tarch::la::Vector<DIMENSIONS,int>    myIntX( 1.2*myX(0) , 1.2*myX(1) );  // scale

  double Energy = 0.1;

  if (
    myIntX(0) > 0 && myIntX(0) < static_cast<int>(Image.width)
    &&
    myIntX(1) > 0 && myIntX(1) < static_cast<int>(Image.height)
  ) {
    Energy += 1.0-Image.pixel_data[myIntX(1)*Image.width+myIntX(0)];
  }

  vars.rho() = 1.0;
  vars.E()   = Energy;
  vars.j(0,0,0);
  }
}


void EulerFV::MyEulerSolver::eigenvalues(const double* const Q, const int normalNonZeroIndex, double* const lambda) {
  ReadOnlyVariables vars(Q);
  Variables eigs(lambda);

  const double GAMMA = 1.4;
  const double irho = 1./vars.rho();
  const double p = (GAMMA-1) * (vars.E() - 0.5 * irho * vars.j()*vars.j() );

  double u_n = Q[normalNonZeroIndex + 1] * irho;
  double c  = std::sqrt(GAMMA * p * irho);

  eigs.rho()=u_n - c;
  eigs.E()  =u_n + c;
  eigs.j(u_n,u_n,u_n);
}


void EulerFV::MyEulerSolver::flux(const double* const Q, double** const F) {
  ReadOnlyVariables vars(Q);
  Fluxes fluxes(F);

  tarch::la::Matrix<3,3,double> I;
  I = 1, 0, 0,
      0, 1, 0,
      0, 0, 1;

  const double GAMMA = 1.4;
  const double irho = 1./vars.rho();
  const double p = (GAMMA-1) * (vars.E() - 0.5 * irho * vars.j()*vars.j() );

  fluxes.rho ( vars.j()                                 );
  fluxes.j   ( irho * outerDot(vars.j(),vars.j()) + p*I );
  fluxes.E   ( irho * (vars.E() + p) * vars.j()         );
}


void EulerFV::MyEulerSolver::boundaryValues(
    const double* const x,
    const double t,const double dt,
    const int faceIndex,
    const int normalNonZero,
    const double* const stateInside,
    double* const stateOutside) {
  ReadOnlyVariables varsInside(stateInside);
  Variables         varsOutside(stateOutside);

  varsOutside = varsInside;
}
