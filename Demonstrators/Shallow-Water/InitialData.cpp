#include "InitialData.h"
#include "MySWESolver_ADERDG_Variables.h"
#include "MySWESolver_ADERDG.h"

#include <cmath>

using namespace std;

extern double grav_DG;
extern int scenario_DG;
///// 2D /////

#ifdef Dim2
/*
* Constant water height with both sides smashing together
*/
void SWE::ShockShockProblem(const double * const x, double* Q){
  MySWESolver_ADERDG::Variables vars(Q);

  if(x[0] < 5) {
      vars.h() = 4.0;
      vars.hu()= 2.0;
      vars.hv()= 0.0;
      vars.b() = 0;
  } else {
      vars.h() = 4.0;
      vars.hu()= -2.0;
      vars.hv()= 0.0;
      vars.b() = 0.0;
  }
}

/*
* Constant water height with both sides moving away from each other
*/
void SWE::RareRareProblem(const double * const x, double* Q){
  MySWESolver_ADERDG::Variables vars(Q);

  if(x[0] < 5) {
      vars.h() = 4.0;
      vars.hu()= -2.0;
      vars.hv()= 0.0;
      vars.b() = 0;
  } else {
      vars.h() = 4.0;
      vars.hu()= 2.0;
      vars.hv()= 0.0;
      vars.b() = 0.0;
  }
}

/*
* Gaussfunktion
*/
void SWE::GaussFunctionProblem(const double* const x, double* Q){
  MySWESolver_ADERDG::Variables vars(Q);

  vars.h() = exp(-pow(x[0] - 5, 2)) + 1;
  vars.hu() = 0.0;
  vars.hv() = 0.0;
  vars.b() = 0.0;
}


/*
 * Constant water height with "exponential" hump in bathymetry.
 */
void SWE::ExpBreakProblem(const double* const x,double* Q) {
  MySWESolver_ADERDG::Variables vars(Q);

  if((x[0] -5) *(x[0] -5) + (x[1] -5) *(x[1] -5) < 2) {
    vars.h() = 4.0;
    vars.hu()= 0.0;
    vars.hv()= 0.0;
    vars.b() = exp(-pow((x[0]-5),2)-pow((x[1]-5),2));
  } else {
    vars.h() = 4.0;
    vars.hu()= 0.0;
    vars.hv()= 0.0;
    vars.b() = 0.0;
  }
}

/*
 * Constant water height with discontinuous jump in bathymetry.
 */
void SWE::DamBreakProblem(const double* const x,double* Q) {
  MySWESolver_ADERDG::Variables vars(Q);

  if((x[0] -5) *(x[0] -5) + (x[1] -5) *(x[1] -5) < 2) {
    vars.h() = 4.0;
    vars.hu()= 0.0;
    vars.hv()= 0.0;
    vars.b() = 1;
  } else {
    vars.h() = 4.0;
    vars.hu()= 0.0;
    vars.hv()= 0.0;
    vars.b() = 0.0;
  }
}

/*
 * Sea at rest (steady state).
 */
void SWE::SeaAtRestProblem(const double* const x,double* Q) {
  MySWESolver_ADERDG::Variables vars(Q);

  if(x[0] >= (10.0/3.0) && x[0]<= (20.0/3.0)) {
    vars.h() = 2 - x[0]*(3.0/10.0) + 1;
    vars.hu()= 0.0;
    vars.hv()= 0.0;
    vars.b() = x[0]*(3.0/10.0) - 1;
  } else {
    vars.h() = 2.0;
    vars.hu()= 0.0;
    vars.hv()= 0.0;
    vars.b() = 0.0;
  }
}

/*
* Simulates a channel with a linearly rising ramp at the left end of the channel.
* To be used with WALL boundaries.
*/
void SWE::SteadyRunUpLinear(const double* const x, double* Q) {
   MySWESolver_ADERDG::Variables vars(Q);

   const double d =1;
   const double xr=19.85;

   vars.b()  = (x[0]>xr) ? 0 : (-d/xr)*x[0] +d;
   vars.h()  = (x[0]>xr) ? d : (d/xr)*x[0];
   vars.hu() = 0.0;
   vars.hv() = 0.0;

}

/*
* Simulates a wave which approaches a linearly rising ramp at the left end of the channel.
* To be used with WALL boundaries.
*/
void SWE::RunUpLinear(const double* const x, double* Q) {
   MySWESolver_ADERDG::Variables vars(Q);

   const double as=0.3;
   const double d =1;
   const double xr=19.85;
   const double alpha=atan(d/xr);
   const double xs= d/tan(alpha) + sqrt(4.0/(3.0*as))*acosh(sqrt(20.0));

   vars.b()  = (x[0]>xr) ? 0 : (-d/xr)*x[0] +d;
   vars.h()  = (x[0]>xr) ? d : (d/xr)*x[0];
   vars.h()  += as*(1.0/pow(cosh(sqrt(3.0*as/4.0)*(x[0]-xs)),2));
   vars.hu() = -vars.h()* as*(1.0/pow(cosh(sqrt(3*as/4)*(x[0]-xs)),2))*sqrt(grav_DG/d);
   vars.hv() = 0.0;

}

/*
* Steady-state test case for artificial continental shelf.
*/
void SWE::SteadyRunUpShelf(const double* const x, double* Q) {
   MySWESolver_ADERDG::Variables vars(Q);

   const double d1 = 200.0;
   const double d2 = 2000.0;
   const double xb = 10.0;
   const double xe = 50.0;

   if(x[0]>xe)
   {
      vars.h()  = d2;
      vars.b()  = 0.0;
   }
   else if ( x[0]>xb)
   {
      vars.h()  = ((d1-d2)/(xb-xe))*(x[0]-xb) +d1;
      vars.b()  = d2 - vars.h();
   }
   else
   {
      vars.h()  = d1;
      vars.b()  = d2-d1;
   }

   vars.hu() = 0.0;
   vars.hv() = 0.0;
}

/*
* Run-up on artificial continental shelf.
*/
void SWE::RunUpShelf(const double* const x, double* Q) {
   MySWESolver_ADERDG::Variables vars(Q);

   const double d1 = 200.0;
   const double d2 = 2000.0;
   const double xb = 10.0;
   const double xe = 50.0;
   const double xw = 500.0;
   const double aw = 1.0;

   vars.hu()=0.0;
   vars.hv()=0.0;

   if(x[0]>xe)
   {
      vars.h()  = d2;
      vars.b()  = 0.0;
   }
   else if (x[0]>xb)
   {
      vars.h()  = ((d1-d2)/(xb-xe))*(x[0]-xb) +d1;
      vars.b()  = d2 - vars.h();
   }
   else
   {
      vars.h()  = d1;
      vars.b()  = d2-d1;
   }

   vars.h()  += aw*(1.0/pow(cosh(x[0]-xw),2));
   vars.hu() +=-vars.h()*aw*(1.0/pow(cosh(x[0]-xw),2))*sqrt(grav_DG/d2);
   //vars.hu() = -vars.h()* aw*(1.0/pow(cosh(sqrt(x[0]-xw),2))*sqrt(grav_DG/d);
   //vars.hu() = -vars.h()* sqrt(grav_DG*vars.h());
}

// width = 10.0,10.0
// offset = 0.0, 0.0
void SWE::WettingDryingProblem(const double* const x, double* Q){
  MySWESolver_ADERDG::Variables vars(Q);

  if(x[0] < -5) {
      vars.h() = 2.0;
  } else {
      vars.h() = 0.0;
  }
  vars.hu() = 0.0;
  vars.hv() = 0.0;
  vars.b() = -0.1*x[0] + exp((-x[0]*x[0])/(0.1*0.1));
}

void SWE::OscillatingLake(const double* const x, double* Q){
    MySWESolver_ADERDG::Variables vars(Q);

    double omega = sqrt(0.2*grav_DG);

    vars.b() = 0.1 * (pow(x[0], 2) + pow(x[1], 2));
    vars.h() = max(0.0, 0.05 * (2 * x[0] * cos(omega * 0) + 2 * x[1] * sin(omega * 0)) + 0.075 - vars.b());
    vars.hu() = 0.5 * omega * sin(omega * 0) * vars.h();
    vars.hv() = 0.5 * omega * cos(omega * 0) * vars.h();
}

// width = 10.0,10.0
// offset = 0.0, 0.0
void SWE::RunUpTest(const double* const x, double* Q){
    MySWESolver_ADERDG::Variables vars(Q);

    if(x[0] < 7){
        vars.h() = 0.5*exp(-pow(x[0] - 3.5, 2)) + 0.5;
        vars.hu() = 2.0;
        vars.b() = 0.0;
    }
    else {
        vars.h() = 0.0;
        vars.hu() = 0.0;
        vars.b() = (x[0] - 7) * 1;
    }
    vars.hv() = 0.0;
}

//width = 70.0,1.0
//offset = -10.0,0.0
void SWE::SolitaryWaveOnSimpleBeach(const double*const x, double* Q, double Hd, double d){
  MySWESolver_ADERDG::Variables vars(Q);

  const double H = Hd * d;
  const double beta = std::atan(1/19.85);

  double gamma = std::sqrt((3*H)/(4*d));
  double x0 = d * cos(beta)/sin(beta);
  double L = d * std::log(std::sqrt(20) + std::sqrt(20 - 1)) / gamma;
  double eta = H * (1/(std::cosh(gamma*(x[0]-(x0 + L))/d))) * (1/(std::cosh(gamma*(x[0]-(x0 + L))/d)));

  if (x[0] < 0){
      vars.h() = 0;
      vars.b() = -x[0] * std::sin(beta)/std::cos(beta) + d;
  }
  else if (x[0] <= x0){
      vars.h() = x[0] * std::sin(beta)/std::cos(beta);
      vars.b() = d - vars.h();
  }
  else{
       vars.h() =  H * (1/(std::cosh(gamma*(x[0]-(x0 + L))/d))) * (1/(std::cosh(gamma*(x[0]-(x0 + L))/d))) + d;
       vars.b() = 0;
  }
  vars.hu() = -eta * std::sqrt(grav_DG/d) * vars.h();
  vars.hv() = 0.0;
}



#endif

void SWE::initialData(const double* const x,double* Q) {
  switch (scenario_DG)
  {
      case 0:
        ShockShockProblem(x, Q);
        break;
    case 1:
      RareRareProblem(x, Q);
          break;
    case 2:
      GaussFunctionProblem(x, Q);
          break;
    case 3:
      ExpBreakProblem(x, Q);
          break;
    case 4:
      DamBreakProblem(x, Q);
          break;
    case 5:
      SeaAtRestProblem(x, Q);
          break;
    case 6:
      SteadyRunUpLinear(x, Q);
          break;
    case 7:
      RunUpLinear(x, Q);
          break;
    case 8:
      SteadyRunUpShelf(x, Q);
          break;
    case 9:
      RunUpShelf(x, Q);
          break;
    case 10:
      WettingDryingProblem(x, Q);
          break;
    case 11:
      OscillatingLake(x, Q);
          break;
    case 12:
      RunUpTest(x, Q);
          break;
    case 13:
      SolitaryWaveOnSimpleBeach(x, Q, 0.0185, 0.3);
          break;
    case 14:
      SolitaryWaveOnSimpleBeach(x, Q, 0.3, 0.15);
          break;
    default:
      GaussFunctionProblem(x, Q);
  }
}
