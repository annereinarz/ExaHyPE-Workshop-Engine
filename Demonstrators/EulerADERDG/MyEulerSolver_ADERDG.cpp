#include "MyEulerSolver_ADERDG.h"

#include "MyEulerSolver_ADERDG_Variables.h"

#include "LogoDurhamUniversity.h"
#include "LogoExaHyPE.h"


tarch::logging::Log EulerADERDG::MyEulerSolver_ADERDG::_log( "EulerADERDG::MyEulerSolver_ADERDG" );


void EulerADERDG::MyEulerSolver_ADERDG::init(const std::vector<std::string>& cmdlineargs,const exahype::parser::ParserView& constants) {
  // @todo Please implement/augment if required
}

void EulerADERDG::MyEulerSolver_ADERDG::getInitialProfile(const double* const x, double& E, double t, double dt) {
  const double blendIn = 0.4;
  if (tarch::la::equals( t,0.0 )) {
    tarch::la::Vector<DIMENSIONS,double> myX( x[0]-0.146 , 1.0-x[1] - 0.12 ); // translate
    myX *= static_cast<double>(LogoDurhamUniversity.width);
    tarch::la::Vector<DIMENSIONS,int>    myIntX( 1.4*myX(0), 1.4*myX(1) );  // scale

    E = 0.1;
    if (
      myIntX(0) > 0 && myIntX(0) < static_cast<int>(LogoDurhamUniversity.width)
      &&
      myIntX(1) > 0 && myIntX(1) < static_cast<int>(LogoDurhamUniversity.height)
    ) {
      E += 1.0-LogoDurhamUniversity.pixel_data[myIntX(1)*LogoDurhamUniversity.width+myIntX(0)];
    }
  }
  else if (t<blendIn and t+dt>blendIn) {
    tarch::la::Vector<DIMENSIONS,double> myX( x[0] - 0.06, 1.0-x[1] - 0.25 ); // translate
    myX *= static_cast<double>(LogoExaHyPE.width);
    tarch::la::Vector<DIMENSIONS,int>    myIntX( 1.2*myX(0) , 1.2*myX(1) );  // scale

    if (
      myIntX(0) > 0 && myIntX(0) < static_cast<int>(LogoExaHyPE.width)
      &&
      myIntX(1) > 0 && myIntX(1) < static_cast<int>(LogoExaHyPE.height)
    ) {
      E+= (1.0-LogoExaHyPE.pixel_data[myIntX(1)*LogoExaHyPE.width+myIntX(0)]);
    }
  }
}

void EulerADERDG::MyEulerSolver_ADERDG::adjustPointSolution(const double* const x,const double t,const double dt,double* const Q) {
  Variables vars(Q);
  if ( tarch::la::equals( t,0.0 ) ) {
    vars.rho() = 1.0;
    vars.E()   = 0.0;
    vars.j(0,0,0);
  }
  double energy = vars.E();
  getInitialProfile(x,energy,t,dt);
  vars.E() = energy;

  double density = vars.rho();
  getInitialProfile(x,density,t,dt);
  vars.rho() = density;
}

void EulerADERDG::MyEulerSolver_ADERDG::boundaryValues(const double* const x,const double t,const double dt,const int faceIndex,const int normalNonZero,const double* const fluxIn,const double* const stateIn,const double* const gradStateIn,double* const fluxOut,double* const stateOut) {
/*
  stateOut[0] = stateIn[0];
  stateOut[1] = stateIn[1];
  stateOut[2] = stateIn[2];
  stateOut[3] = stateIn[3];
  stateOut[4] = stateIn[4];

  // Compute flux and
  // extract normal flux in a lazy fashion.
  double fi[DIMENSIONS][NumberOfVariables], *F[DIMENSIONS];
  for(int d=0; d<DIMENSIONS; d++) F[d] = fi[d];
  // it could also be done "more effective" with something like
  // fi[normalNonZero] = reinterpret_cast<double[5]>(fluxOut);
  F[normalNonZero] = fluxOut; // This replaces the double pointer at pos normalNonZero by fluxOut.
  flux(stateOut, F);
*/
  std::copy_n(stateIn, NumberOfVariables, stateOut);
  stateOut[1+normalNonZero] =  -stateOut[1+normalNonZero];
  double _F[3][NumberOfVariables]={0.0};
  double* F[3] = {_F[0], _F[1], _F[2]};
  flux(stateOut,F);
  std::copy_n(F[normalNonZero], NumberOfVariables, fluxOut);
}

exahype::solvers::Solver::RefinementControl EulerADERDG::MyEulerSolver_ADERDG::refinementCriterion(const double* const luh,const tarch::la::Vector<DIMENSIONS,double>& center,const tarch::la::Vector<DIMENSIONS,double>& dx,double t,const int level) {
  // @todo Please implement/augment if required
  return exahype::solvers::Solver::RefinementControl::Keep;
}

//*****************************************************************************
//******************************** PDE ****************************************
// To use other PDE terms, specify them in the specification file, delete this 
// file and its header and rerun the toolkit
//*****************************************************************************


void EulerADERDG::MyEulerSolver_ADERDG::eigenvalues(const double* const Q,const int normalNonZeroIndex,double* const lambda) {
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


void EulerADERDG::MyEulerSolver_ADERDG::flux(const double* const Q,double** const F) {
  ReadOnlyVariables vars(Q);
  Fluxes f(F);

  tarch::la::Matrix<3,3,double> I;
  I = 1, 0, 0,
      0, 1, 0,
      0, 0, 1;

  const double GAMMA = 1.4;
  const double irho = 1./vars.rho();
  const double p = (GAMMA-1) * (vars.E() - 0.5 * irho * vars.j()*vars.j() );

  f.rho ( vars.j()                                 );
  f.j   ( irho * outerDot(vars.j(),vars.j()) + p*I );
  f.E   ( irho * (vars.E() + p) * vars.j()         );
}


void EulerADERDG::MyEulerSolver_ADERDG::mapDiscreteMaximumPrincipleObservables(double* const observables, const double* const Q) const {
  for (int i=0; i<NumberOfVariables; ++i) {
    observables[i] = Q[i];
  }
}


bool EulerADERDG::MyEulerSolver_ADERDG::isPhysicallyAdmissible(
      const double* const solution,
      const double* const observablesMin,const double* const observablesMax,
      const bool wasTroubledInPreviousTimeStep,
      const tarch::la::Vector<DIMENSIONS,double>& center,
      const tarch::la::Vector<DIMENSIONS,double>& dx,
      const double t) const {
  // This is an example for the compressible Euler equations.
  // Modify it according to your needs.
  if (observablesMin[0] <= 0.0) return false;
  if (observablesMin[4] <= 0.0) return false;

  return true;
}

