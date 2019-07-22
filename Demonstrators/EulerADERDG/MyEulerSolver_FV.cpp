#include "MyEulerSolver_FV.h"
#include "MyEulerSolver_ADERDG.h"

#include "MyEulerSolver_FV_Variables.h"


tarch::logging::Log EulerADERDG::MyEulerSolver_FV::_log( "EulerADERDG::MyEulerSolver_FV" );


void EulerADERDG::MyEulerSolver_FV::init(const std::vector<std::string>& cmdlineargs,const exahype::parser::ParserView& constants) {
  // @todo Please implement/augment if required
}

void EulerADERDG::MyEulerSolver_FV::adjustSolution(const double* const x,const double t,const double dt, double* const Q) {
  Variables vars(Q);
 
  if ( tarch::la::equals( t,0.0 ) ) {
    vars.rho() = 1.0;
    vars.E()   = 1.0;
    vars.j(0,0,0);
  }
  double energy = vars.E();
  MyEulerSolver_ADERDG::getInitialProfile(x,energy,t,dt);
  vars.E() = energy;

  double density = vars.rho();
  MyEulerSolver_ADERDG::getInitialProfile(x,density,t,dt);
  vars.rho() = density;
}

void EulerADERDG::MyEulerSolver_FV::eigenvalues(const double* const Q, const int normalNonZeroIndex, double* const lambda) {
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

void EulerADERDG::MyEulerSolver_FV::boundaryValues(
    const double* const x,
    const double t,const double dt,
    const int faceIndex,
    const int d,
    const double* const stateInside,
    double* const stateOutside) {
/*
  ReadOnlyVariables varsInside(stateInside);
  Variables         varsOutside(stateOutside);

  varsOutside = varsInside;
*/
  std::copy_n(stateInside, NumberOfVariables, stateOutside);
  stateOutside[1+d] =  -stateOutside[1+d];
}

//***********************************************************
//*********************** PDE *******************************
//***********************************************************

//to add new PDEs specify them in the specification file, delete this file and its header and rerun the toolkit


void EulerADERDG::MyEulerSolver_FV::flux(const double* const Q,double** const F) {
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
