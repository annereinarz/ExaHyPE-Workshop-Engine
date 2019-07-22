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
 
#include "kernels/DGBasisFunctions.h"
//
// Makes the kernels depend on Peano, but it just seems to be stupid not to reuse stuff that is already there.
//
#include "peano/utils/Loop.h"


kernels::UnivariateFunction** kernels::basisFunctions;

kernels::UnivariateFunction** kernels::basisFunctionFirstDerivatives;

kernels::UnivariateFunction** kernels::basisFunctionSecondDerivatives;


double kernels::interpolate(
    const double* offsetOfPatch,
    const double* sizeOfPatch,
    const double* x,
    int           numberOfUnknowns,
    int           unknown,
    int           order,
    const double* u
) {
  double result = 0.0;

  double xRef[DIMENSIONS];
  xRef[0] =  (x[0] - offsetOfPatch[0]) / sizeOfPatch[0];
  xRef[1] =  (x[1] - offsetOfPatch[1]) / sizeOfPatch[1];
  #if DIMENSIONS==3
  xRef[2] =  (x[2] - offsetOfPatch[2]) / sizeOfPatch[2];
  #endif



  // The code below evaluates the basis functions at the reference coordinates
  // and multiplies them with their respective coefficient.
  dfor(ii,order+1) { // Gauss-Legendre node indices
    int iGauss = peano::utils::dLinearisedWithoutLookup(ii,order + 1);
    result += kernels::basisFunctions[order][ii(0)](xRef[0]) *
             kernels::basisFunctions[order][ii(1)](xRef[1]) *
             #if DIMENSIONS==3
             kernels::basisFunctions[order][ii(2)](xRef[2]) *
             #endif
             u[iGauss * numberOfUnknowns + unknown];
    assertion6(std::isfinite(result), result, unknown, iGauss, numberOfUnknowns, offsetOfPatch[0], sizeOfPatch[0]);
  }

  return result;
}


void kernels::freeBasisFunctions(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    delete [] basisFunctions[i];
    delete [] basisFunctionFirstDerivatives[i];
    delete [] basisFunctionSecondDerivatives[i];
  }

  delete [] basisFunctions;
  delete [] basisFunctionFirstDerivatives;
  delete [] basisFunctionSecondDerivatives;
}

void kernels::initBasisFunctions(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  basisFunctions                 = new UnivariateFunction* [MAX_ORDER + 1];
  basisFunctionFirstDerivatives  = new UnivariateFunction* [MAX_ORDER + 1];
  basisFunctionSecondDerivatives = new UnivariateFunction* [MAX_ORDER + 1];

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    basisFunctions[i]                 = new UnivariateFunction[i + 1];
    basisFunctionFirstDerivatives [i] = new UnivariateFunction[i + 1];
    basisFunctionSecondDerivatives[i] = new UnivariateFunction[i + 1];
  }

  // Basis functions in interval (0,1).
  basisFunctions[0][0] = (UnivariateFunction) basisFunction_0_0;
  basisFunctions[1][0] = (UnivariateFunction) basisFunction_1_0;
  basisFunctions[1][1] = (UnivariateFunction) basisFunction_1_1;
  basisFunctions[2][0] = (UnivariateFunction) basisFunction_2_0;
  basisFunctions[2][1] = (UnivariateFunction) basisFunction_2_1;
  basisFunctions[2][2] = (UnivariateFunction) basisFunction_2_2;
  basisFunctions[3][0] = (UnivariateFunction) basisFunction_3_0;
  basisFunctions[3][1] = (UnivariateFunction) basisFunction_3_1;
  basisFunctions[3][2] = (UnivariateFunction) basisFunction_3_2;
  basisFunctions[3][3] = (UnivariateFunction) basisFunction_3_3;
  basisFunctions[4][0] = (UnivariateFunction) basisFunction_4_0;
  basisFunctions[4][1] = (UnivariateFunction) basisFunction_4_1;
  basisFunctions[4][2] = (UnivariateFunction) basisFunction_4_2;
  basisFunctions[4][3] = (UnivariateFunction) basisFunction_4_3;
  basisFunctions[4][4] = (UnivariateFunction) basisFunction_4_4;
  basisFunctions[5][0] = (UnivariateFunction) basisFunction_5_0;
  basisFunctions[5][1] = (UnivariateFunction) basisFunction_5_1;
  basisFunctions[5][2] = (UnivariateFunction) basisFunction_5_2;
  basisFunctions[5][3] = (UnivariateFunction) basisFunction_5_3;
  basisFunctions[5][4] = (UnivariateFunction) basisFunction_5_4;
  basisFunctions[5][5] = (UnivariateFunction) basisFunction_5_5;
  basisFunctions[6][0] = (UnivariateFunction) basisFunction_6_0;
  basisFunctions[6][1] = (UnivariateFunction) basisFunction_6_1;
  basisFunctions[6][2] = (UnivariateFunction) basisFunction_6_2;
  basisFunctions[6][3] = (UnivariateFunction) basisFunction_6_3;
  basisFunctions[6][4] = (UnivariateFunction) basisFunction_6_4;
  basisFunctions[6][5] = (UnivariateFunction) basisFunction_6_5;
  basisFunctions[6][6] = (UnivariateFunction) basisFunction_6_6;
  basisFunctions[7][0] = (UnivariateFunction) basisFunction_7_0;
  basisFunctions[7][1] = (UnivariateFunction) basisFunction_7_1;
  basisFunctions[7][2] = (UnivariateFunction) basisFunction_7_2;
  basisFunctions[7][3] = (UnivariateFunction) basisFunction_7_3;
  basisFunctions[7][4] = (UnivariateFunction) basisFunction_7_4;
  basisFunctions[7][5] = (UnivariateFunction) basisFunction_7_5;
  basisFunctions[7][6] = (UnivariateFunction) basisFunction_7_6;
  basisFunctions[7][7] = (UnivariateFunction) basisFunction_7_7;
  basisFunctions[8][0] = (UnivariateFunction) basisFunction_8_0;
  basisFunctions[8][1] = (UnivariateFunction) basisFunction_8_1;
  basisFunctions[8][2] = (UnivariateFunction) basisFunction_8_2;
  basisFunctions[8][3] = (UnivariateFunction) basisFunction_8_3;
  basisFunctions[8][4] = (UnivariateFunction) basisFunction_8_4;
  basisFunctions[8][5] = (UnivariateFunction) basisFunction_8_5;
  basisFunctions[8][6] = (UnivariateFunction) basisFunction_8_6;
  basisFunctions[8][7] = (UnivariateFunction) basisFunction_8_7;
  basisFunctions[8][8] = (UnivariateFunction) basisFunction_8_8;
  basisFunctions[9][0] = (UnivariateFunction) basisFunction_9_0;
  basisFunctions[9][1] = (UnivariateFunction) basisFunction_9_1;
  basisFunctions[9][2] = (UnivariateFunction) basisFunction_9_2;
  basisFunctions[9][3] = (UnivariateFunction) basisFunction_9_3;
  basisFunctions[9][4] = (UnivariateFunction) basisFunction_9_4;
  basisFunctions[9][5] = (UnivariateFunction) basisFunction_9_5;
  basisFunctions[9][6] = (UnivariateFunction) basisFunction_9_6;
  basisFunctions[9][7] = (UnivariateFunction) basisFunction_9_7;
  basisFunctions[9][8] = (UnivariateFunction) basisFunction_9_8;
  basisFunctions[9][9] = (UnivariateFunction) basisFunction_9_9;

  // First derivatives of basis functions in interval (0,1).
  basisFunctionFirstDerivatives[0][0] = (UnivariateFunction) basisFunctionFirstDerivative_0_0;
  basisFunctionFirstDerivatives[1][0] = (UnivariateFunction) basisFunctionFirstDerivative_1_0;
  basisFunctionFirstDerivatives[1][1] = (UnivariateFunction) basisFunctionFirstDerivative_1_1;
  basisFunctionFirstDerivatives[2][0] = (UnivariateFunction) basisFunctionFirstDerivative_2_0;
  basisFunctionFirstDerivatives[2][1] = (UnivariateFunction) basisFunctionFirstDerivative_2_1;
  basisFunctionFirstDerivatives[2][2] = (UnivariateFunction) basisFunctionFirstDerivative_2_2;
  basisFunctionFirstDerivatives[3][0] = (UnivariateFunction) basisFunctionFirstDerivative_3_0;
  basisFunctionFirstDerivatives[3][1] = (UnivariateFunction) basisFunctionFirstDerivative_3_1;
  basisFunctionFirstDerivatives[3][2] = (UnivariateFunction) basisFunctionFirstDerivative_3_2;
  basisFunctionFirstDerivatives[3][3] = (UnivariateFunction) basisFunctionFirstDerivative_3_3;
  basisFunctionFirstDerivatives[4][0] = (UnivariateFunction) basisFunctionFirstDerivative_4_0;
  basisFunctionFirstDerivatives[4][1] = (UnivariateFunction) basisFunctionFirstDerivative_4_1;
  basisFunctionFirstDerivatives[4][2] = (UnivariateFunction) basisFunctionFirstDerivative_4_2;
  basisFunctionFirstDerivatives[4][3] = (UnivariateFunction) basisFunctionFirstDerivative_4_3;
  basisFunctionFirstDerivatives[4][4] = (UnivariateFunction) basisFunctionFirstDerivative_4_4;
  basisFunctionFirstDerivatives[5][0] = (UnivariateFunction) basisFunctionFirstDerivative_5_0;
  basisFunctionFirstDerivatives[5][1] = (UnivariateFunction) basisFunctionFirstDerivative_5_1;
  basisFunctionFirstDerivatives[5][2] = (UnivariateFunction) basisFunctionFirstDerivative_5_2;
  basisFunctionFirstDerivatives[5][3] = (UnivariateFunction) basisFunctionFirstDerivative_5_3;
  basisFunctionFirstDerivatives[5][4] = (UnivariateFunction) basisFunctionFirstDerivative_5_4;
  basisFunctionFirstDerivatives[5][5] = (UnivariateFunction) basisFunctionFirstDerivative_5_5;
  basisFunctionFirstDerivatives[6][0] = (UnivariateFunction) basisFunctionFirstDerivative_6_0;
  basisFunctionFirstDerivatives[6][1] = (UnivariateFunction) basisFunctionFirstDerivative_6_1;
  basisFunctionFirstDerivatives[6][2] = (UnivariateFunction) basisFunctionFirstDerivative_6_2;
  basisFunctionFirstDerivatives[6][3] = (UnivariateFunction) basisFunctionFirstDerivative_6_3;
  basisFunctionFirstDerivatives[6][4] = (UnivariateFunction) basisFunctionFirstDerivative_6_4;
  basisFunctionFirstDerivatives[6][5] = (UnivariateFunction) basisFunctionFirstDerivative_6_5;
  basisFunctionFirstDerivatives[6][6] = (UnivariateFunction) basisFunctionFirstDerivative_6_6;
  basisFunctionFirstDerivatives[7][0] = (UnivariateFunction) basisFunctionFirstDerivative_7_0;
  basisFunctionFirstDerivatives[7][1] = (UnivariateFunction) basisFunctionFirstDerivative_7_1;
  basisFunctionFirstDerivatives[7][2] = (UnivariateFunction) basisFunctionFirstDerivative_7_2;
  basisFunctionFirstDerivatives[7][3] = (UnivariateFunction) basisFunctionFirstDerivative_7_3;
  basisFunctionFirstDerivatives[7][4] = (UnivariateFunction) basisFunctionFirstDerivative_7_4;
  basisFunctionFirstDerivatives[7][5] = (UnivariateFunction) basisFunctionFirstDerivative_7_5;
  basisFunctionFirstDerivatives[7][6] = (UnivariateFunction) basisFunctionFirstDerivative_7_6;
  basisFunctionFirstDerivatives[7][7] = (UnivariateFunction) basisFunctionFirstDerivative_7_7;
  basisFunctionFirstDerivatives[8][0] = (UnivariateFunction) basisFunctionFirstDerivative_8_0;
  basisFunctionFirstDerivatives[8][1] = (UnivariateFunction) basisFunctionFirstDerivative_8_1;
  basisFunctionFirstDerivatives[8][2] = (UnivariateFunction) basisFunctionFirstDerivative_8_2;
  basisFunctionFirstDerivatives[8][3] = (UnivariateFunction) basisFunctionFirstDerivative_8_3;
  basisFunctionFirstDerivatives[8][4] = (UnivariateFunction) basisFunctionFirstDerivative_8_4;
  basisFunctionFirstDerivatives[8][5] = (UnivariateFunction) basisFunctionFirstDerivative_8_5;
  basisFunctionFirstDerivatives[8][6] = (UnivariateFunction) basisFunctionFirstDerivative_8_6;
  basisFunctionFirstDerivatives[8][7] = (UnivariateFunction) basisFunctionFirstDerivative_8_7;
  basisFunctionFirstDerivatives[8][8] = (UnivariateFunction) basisFunctionFirstDerivative_8_8;
  basisFunctionFirstDerivatives[9][0] = (UnivariateFunction) basisFunctionFirstDerivative_9_0;
  basisFunctionFirstDerivatives[9][1] = (UnivariateFunction) basisFunctionFirstDerivative_9_1;
  basisFunctionFirstDerivatives[9][2] = (UnivariateFunction) basisFunctionFirstDerivative_9_2;
  basisFunctionFirstDerivatives[9][3] = (UnivariateFunction) basisFunctionFirstDerivative_9_3;
  basisFunctionFirstDerivatives[9][4] = (UnivariateFunction) basisFunctionFirstDerivative_9_4;
  basisFunctionFirstDerivatives[9][5] = (UnivariateFunction) basisFunctionFirstDerivative_9_5;
  basisFunctionFirstDerivatives[9][6] = (UnivariateFunction) basisFunctionFirstDerivative_9_6;
  basisFunctionFirstDerivatives[9][7] = (UnivariateFunction) basisFunctionFirstDerivative_9_7;
  basisFunctionFirstDerivatives[9][8] = (UnivariateFunction) basisFunctionFirstDerivative_9_8;
  basisFunctionFirstDerivatives[9][9] = (UnivariateFunction) basisFunctionFirstDerivative_9_9;

  // Second derivatives of basis functions in interval (0,1).
  basisFunctionSecondDerivatives[0][0] = (UnivariateFunction) basisFunctionSecondDerivative_0_0;
  basisFunctionSecondDerivatives[1][0] = (UnivariateFunction) basisFunctionSecondDerivative_1_0;
  basisFunctionSecondDerivatives[1][1] = (UnivariateFunction) basisFunctionSecondDerivative_1_1;
  basisFunctionSecondDerivatives[2][0] = (UnivariateFunction) basisFunctionSecondDerivative_2_0;
  basisFunctionSecondDerivatives[2][1] = (UnivariateFunction) basisFunctionSecondDerivative_2_1;
  basisFunctionSecondDerivatives[2][2] = (UnivariateFunction) basisFunctionSecondDerivative_2_2;
  basisFunctionSecondDerivatives[3][0] = (UnivariateFunction) basisFunctionSecondDerivative_3_0;
  basisFunctionSecondDerivatives[3][1] = (UnivariateFunction) basisFunctionSecondDerivative_3_1;
  basisFunctionSecondDerivatives[3][2] = (UnivariateFunction) basisFunctionSecondDerivative_3_2;
  basisFunctionSecondDerivatives[3][3] = (UnivariateFunction) basisFunctionSecondDerivative_3_3;
  basisFunctionSecondDerivatives[4][0] = (UnivariateFunction) basisFunctionSecondDerivative_4_0;
  basisFunctionSecondDerivatives[4][1] = (UnivariateFunction) basisFunctionSecondDerivative_4_1;
  basisFunctionSecondDerivatives[4][2] = (UnivariateFunction) basisFunctionSecondDerivative_4_2;
  basisFunctionSecondDerivatives[4][3] = (UnivariateFunction) basisFunctionSecondDerivative_4_3;
  basisFunctionSecondDerivatives[4][4] = (UnivariateFunction) basisFunctionSecondDerivative_4_4;
  basisFunctionSecondDerivatives[5][0] = (UnivariateFunction) basisFunctionSecondDerivative_5_0;
  basisFunctionSecondDerivatives[5][1] = (UnivariateFunction) basisFunctionSecondDerivative_5_1;
  basisFunctionSecondDerivatives[5][2] = (UnivariateFunction) basisFunctionSecondDerivative_5_2;
  basisFunctionSecondDerivatives[5][3] = (UnivariateFunction) basisFunctionSecondDerivative_5_3;
  basisFunctionSecondDerivatives[5][4] = (UnivariateFunction) basisFunctionSecondDerivative_5_4;
  basisFunctionSecondDerivatives[5][5] = (UnivariateFunction) basisFunctionSecondDerivative_5_5;
  basisFunctionSecondDerivatives[6][0] = (UnivariateFunction) basisFunctionSecondDerivative_6_0;
  basisFunctionSecondDerivatives[6][1] = (UnivariateFunction) basisFunctionSecondDerivative_6_1;
  basisFunctionSecondDerivatives[6][2] = (UnivariateFunction) basisFunctionSecondDerivative_6_2;
  basisFunctionSecondDerivatives[6][3] = (UnivariateFunction) basisFunctionSecondDerivative_6_3;
  basisFunctionSecondDerivatives[6][4] = (UnivariateFunction) basisFunctionSecondDerivative_6_4;
  basisFunctionSecondDerivatives[6][5] = (UnivariateFunction) basisFunctionSecondDerivative_6_5;
  basisFunctionSecondDerivatives[6][6] = (UnivariateFunction) basisFunctionSecondDerivative_6_6;
  basisFunctionSecondDerivatives[7][0] = (UnivariateFunction) basisFunctionSecondDerivative_7_0;
  basisFunctionSecondDerivatives[7][1] = (UnivariateFunction) basisFunctionSecondDerivative_7_1;
  basisFunctionSecondDerivatives[7][2] = (UnivariateFunction) basisFunctionSecondDerivative_7_2;
  basisFunctionSecondDerivatives[7][3] = (UnivariateFunction) basisFunctionSecondDerivative_7_3;
  basisFunctionSecondDerivatives[7][4] = (UnivariateFunction) basisFunctionSecondDerivative_7_4;
  basisFunctionSecondDerivatives[7][5] = (UnivariateFunction) basisFunctionSecondDerivative_7_5;
  basisFunctionSecondDerivatives[7][6] = (UnivariateFunction) basisFunctionSecondDerivative_7_6;
  basisFunctionSecondDerivatives[7][7] = (UnivariateFunction) basisFunctionSecondDerivative_7_7;
  basisFunctionSecondDerivatives[8][0] = (UnivariateFunction) basisFunctionSecondDerivative_8_0;
  basisFunctionSecondDerivatives[8][1] = (UnivariateFunction) basisFunctionSecondDerivative_8_1;
  basisFunctionSecondDerivatives[8][2] = (UnivariateFunction) basisFunctionSecondDerivative_8_2;
  basisFunctionSecondDerivatives[8][3] = (UnivariateFunction) basisFunctionSecondDerivative_8_3;
  basisFunctionSecondDerivatives[8][4] = (UnivariateFunction) basisFunctionSecondDerivative_8_4;
  basisFunctionSecondDerivatives[8][5] = (UnivariateFunction) basisFunctionSecondDerivative_8_5;
  basisFunctionSecondDerivatives[8][6] = (UnivariateFunction) basisFunctionSecondDerivative_8_6;
  basisFunctionSecondDerivatives[8][7] = (UnivariateFunction) basisFunctionSecondDerivative_8_7;
  basisFunctionSecondDerivatives[8][8] = (UnivariateFunction) basisFunctionSecondDerivative_8_8;
  basisFunctionSecondDerivatives[9][0] = (UnivariateFunction) basisFunctionSecondDerivative_9_0;
  basisFunctionSecondDerivatives[9][1] = (UnivariateFunction) basisFunctionSecondDerivative_9_1;
  basisFunctionSecondDerivatives[9][2] = (UnivariateFunction) basisFunctionSecondDerivative_9_2;
  basisFunctionSecondDerivatives[9][3] = (UnivariateFunction) basisFunctionSecondDerivative_9_3;
  basisFunctionSecondDerivatives[9][4] = (UnivariateFunction) basisFunctionSecondDerivative_9_4;
  basisFunctionSecondDerivatives[9][5] = (UnivariateFunction) basisFunctionSecondDerivative_9_5;
  basisFunctionSecondDerivatives[9][6] = (UnivariateFunction) basisFunctionSecondDerivative_9_6;
  basisFunctionSecondDerivatives[9][7] = (UnivariateFunction) basisFunctionSecondDerivative_9_7;
  basisFunctionSecondDerivatives[9][8] = (UnivariateFunction) basisFunctionSecondDerivative_9_8;
  basisFunctionSecondDerivatives[9][9] = (UnivariateFunction) basisFunctionSecondDerivative_9_9;
}


// Basis functions in interval (0,1).
double basisFunction_0_0(const double s) {
  return 1;
}

double basisFunctionFirstDerivative_0_0(const double s) {
  return 0;
}

double basisFunctionSecondDerivative_0_0(const double s) {
  return 0;
}



#if !defined(_GLL)
double basisFunction_1_0(const double s) {
  return -1.73205080756888*s + 1.36602540378444;
}

double basisFunction_1_1(const double s) {
  return 1.73205080756888*s - 0.366025403784439;
}

double basisFunction_2_0(const double s) {
  return (1.29099444873581*s - 1.1454972243679)*(2.58198889747161*s - 1.29099444873581);
}

double basisFunction_2_1(const double s) {
  return -6.66666666666667*pow_2(s) + 6.66666666666667*s - 0.666666666666667;
}

double basisFunction_2_2(const double s) {
  return (1.29099444873581*s - 0.145497224367903)*(2.58198889747161*s - 1.29099444873581);
}

double basisFunction_3_0(const double s) {
  return -7.42054006803942*pow_3(s) + 14.3258583541719*pow_2(s) - 8.54602360787259*s + 1.52678812545812;
}

double basisFunction_3_1(const double s) {
  return (1.66511622813251*s - 1.54950413760097)*(2.94134046256143*s - 1.97067023128072)*(3.83762790624753*s - 0.266453582895562);
}

double basisFunction_3_2(const double s) {
  return -18.7954494075556*pow_3(s) + 24.9981258592215*pow_2(s) - 7.41707042146291*s + 0.400761520311451;
}

double basisFunction_3_3(const double s) {
  return (1.16125633832453*s - 0.0806281691622644)*(1.66511622813251*s - 0.54950413760097)*(3.83762790624753*s - 2.57117432335196);
}

double basisFunction_4_0(const double s) {
  return (1.10353370192663*s - 1.05176685096332)*(1.38441918000096*s - 1.06494321037658)*(2.20706740385327*s - 1.10353370192663)*(5.43906090552822*s - 1.25514676605282);
}

double basisFunction_4_1(const double s) {
  return -51.9397211144315*pow_4(s) + 117.86341512665*pow_3(s) - 88.2228108281618*pow_2(s) + 22.9243335557241*s - 0.893158392001382;
}

double basisFunction_4_2(const double s) {
  return (2.20706740385327*s - 0.103533701926633)*(2.20706740385327*s - 2.10353370192663)*(3.71423210657534*s - 0.857116053287669)*(3.71423210657534*s - 2.85711605328767);
}

double basisFunction_4_3(const double s) {
  return -51.9397211144315*pow_4(s) + 89.8954693310793*pow_3(s) - 46.2708921348155*pow_2(s) + 7.68992717838791*s - 0.267941652222863;
}

double basisFunction_4_4(const double s) {
  return (1.10353370192663*s - 0.0517668509633166)*(1.38441918000096*s - 0.319475969624385)*(2.20706740385327*s - 1.10353370192663)*(5.43906090552822*s - 4.1839141394754);
}


double basisFunction_5_0(const double s) {
  return -48.8475703740572*pow_5(s) + 144.89336104348*pow_4(s) - 161.633448563357*pow_3(s) + 83.3561716652067*pow_2(s) - 19.3888996957563*s + 1.56567320015126;
}

double basisFunction_5_1(const double s) {
  return (1.25495794614581*s - 1.21258398626689)*(1.51238022397769*s - 1.25619011198885)*(2.22264558051702*s - 1.3765057299457)*(4.73271741319853*s - 1.80170011804966)*(7.37299660177086*s - 0.248951021148047);
}

double basisFunction_5_2(const double s) {
  return -217.010042972834*pow_5(s) + 568.41648734511*pow_4(s) - 523.416260790838*pow_3(s) + 195.304165166948*pow_2(s) - 24.2905065059373*s + 0.616930055433303;
}

double basisFunction_5_3(const double s) {
  return (1.70781256749465*s - 0.0576647061664379)*(2.22264558051702*s - 0.376505729945704)*(2.88246602897655*s - 2.7851388633617)*(4.19077785158206*s - 1.59538892579103)*(4.73271741319853*s - 3.93101729514887);
}

double basisFunction_5_4(const double s) {
  return -147.202432444171*pow_5(s) + 319.340266089055*pow_4(s) - 236.580950489613*pow_3(s) + 71.1355384559057*pow_2(s) - 7.82468446839186*s + 0.191800014037773;
}

double basisFunction_5_5(const double s) {
  return (1.07242111915536*s - 0.0362105595776804)*(1.25495794614581*s - 0.212583986266887)*(1.70781256749465*s - 0.65014786132821)*(2.88246602897655*s - 1.7851388633617)*(7.37299660177086*s - 6.12404558062281);
}

double basisFunction_6_0(const double s) {
  return (1.05362097080365*s - 1.02681048540182)*(1.18298458993076*s - 1.03010227772396)*(1.47606589006766*s - 1.03756003733259)*(2.10724194160729*s - 1.05362097080365)*(3.68145977177979*s - 1.09367858670601)*(9.63499151074237*s - 1.24517241627074);
}

double basisFunction_6_1(const double s) {
  return -430.548824453669*pow_6(s) + 1451.27916348878*pow_5(s) - 1898.95063920754*pow_4(s) + 1203.55262982769*pow_3(s) - 371.355003891051*pow_2(s) + 46.8493308286903*s - 0.97072669650486;
}

double basisFunction_6_2(const double s) {
  return (1.47606589006766*s - 1.43850585273507)*(1.74310724000965*s - 1.51783780916045)*(2.46399395583784*s - 1.73199697791892)*(3.68145977177979*s - 0.0936785867060128)*(4.92798791167568*s - 2.46399395583784)*(5.95794818999635*s - 0.769971902464295);
}

double basisFunction_6_3(const double s) {
  return -784.457142857143*pow_6(s) + 2353.37142857143*pow_5(s) - 2624.91428571429*pow_4(s) + 1327.54285714286*pow_3(s) - 296.228571428571*pow_2(s) + 24.6857142857143*s - 0.457142857142857;
}

double basisFunction_6_4(const double s) {
  return (1.47606589006766*s - 0.0375600373325939)*(1.74310724000965*s - 0.225269430849203)*(2.46399395583784*s - 0.73199697791892)*(3.68145977177979*s - 3.58778118507378)*(4.92798791167568*s - 2.46399395583784)*(5.95794818999634*s - 5.18797628753205);
}

double basisFunction_6_5(const double s) {
  return -430.548824453669*pow_6(s) + 1132.01378323323*pow_5(s) - 1100.78718856864*pow_4(s) + 490.434781188012*pow_3(s) - 99.8416815704339*pow_2(s) + 7.90247357862299*s - 0.144070103613311;
}

double basisFunction_6_6(const double s) {
  return (1.05362097080365*s - 0.0268104854018236)*(1.18298458993076*s - 0.152882312206794)*(1.47606589006766*s - 0.43850585273507)*(2.10724194160729*s - 1.05362097080365)*(3.68145977177979*s - 2.58778118507378)*(9.63499151074237*s - 8.38981909447163);
}

double basisFunction_7_0(const double s) {
  return -403.920510528552*pow_7(s) + 1607.66217139587*pow_6(s) - 2607.02708768841*pow_5(s) + 2210.19214907644*pow_4(s) - 1043.59477387295*pow_3(s) + 269.273563572708*pow_2(s) - 34.1982194409867*s + 1.58068706302981;
}

double basisFunction_7_1(const double s) {
  return (1.13833221770959*s - 1.11573054985022)*(1.25523042371068*s - 1.12761521185534)*(1.51263173730148*s - 1.15378436976073)*(2.04060576952012*s - 1.2074617796633)*(3.26140928457752*s - 1.33157691921453)*(7.37642458013851*s - 1.74993719698614)*(12.2231921330408*s - 0.242692356830618);
}

double basisFunction_7_2(const double s) {
  return -2168.25026630443*pow_7(s) + 8158.61882594182*pow_6(s) - 12230.4016334776*pow_5(s) + 9240.73689690901*pow_4(s) - 3645.38178778276*pow_3(s) + 691.884742993132*pow_2(s) - 48.1378065361807*s + 0.710156890316616;
}

double basisFunction_7_3(const double s) {
  return (1.74867286812566*s - 1.71395284285959)*(2.04060576952012*s - 1.83314398985682)*(2.57448230243215*s - 0.051116530837067)*(2.82100556463914*s - 2.15176770870566)*(3.26140928457752*s - 0.331576919214525)*(5.45153296233952*s - 3.22576648116976)*(5.84628194179528*s - 1.38693565193665);
}

double basisFunction_7_4(const double s) {
  return -2693.79930678766*pow_7(s) + 9181.22951735901*pow_6(s) - 12166.7962685729*pow_5(s) + 7885.9720218029*pow_4(s) - 2586.27035502104*pow_3(s) + 403.668792223634*pow_2(s) - 24.8707870558158*s + 0.3537304181068;
}

double basisFunction_7_5(const double s) {
  return (1.34605601572195*s - 0.0267260387733365)*(1.51263173730148*s - 0.153784369760728)*(1.90283221573187*s - 0.451416107865937)*(2.82100556463914*s - 1.15176770870566)*(4.60026623057835*s - 4.50892761449545)*(5.84628194179529*s - 3.45934628985864)*(7.37642458013851*s - 6.62648738315237);
}

double basisFunction_7_6(const double s) {
  return -1296.94228807432*pow_7(s) + 4022.68278623565*pow_6(s) - 4859.64662643633*pow_5(s) + 2897.294720319*pow_4(s) - 889.031549069314*pow_3(s) + 132.491462188462*pow_2(s) - 7.95188653165957*s + 0.112177210206021;
}

double basisFunction_7_7(const double s) {
  return (1.04135224717181*s - 0.0206761235859027)*(1.13833221770959*s - 0.115730549850224)*(1.34605601572195*s - 0.31932997694861)*(1.74867286812566*s - 0.713952842859594)*(2.57448230243215*s - 1.52336577159509)*(4.60026623057835*s - 3.50892761449545)*(12.2231921330408*s - 10.9804997762102);
}

double basisFunction_8_0(const double s) {
  return (1.03288687057482*s - 1.01644343528741)*(1.10852987046486*s - 1.01764766278712)*(1.26459686843166*s - 1.02013223070514)*(1.54749215161853*s - 1.02463588973568)*(2.06577374114964*s - 1.03288687057482)*(3.10603949202106*s - 1.0494477767529)*(5.63715641989734*s - 1.08974285513379)*(15.1367073028256*s - 1.24097456758257);
}

double basisFunction_8_1(const double s) {
  return -4008.73624216047*pow_8(s) + 17710.6590683988*pow_7(s) - 32504.3554732612*pow_6(s) + 31998.9265069698*pow_5(s) - 18174.8525833434*pow_4(s) + 5937.89722241289*pow_3(s) - 1036.86733739771*pow_2(s) + 78.2447075882907*s - 1.00568288639355;
}

double basisFunction_8_2(const double s) {
  return (1.26459686843166*s - 1.24446463772653)*(1.37987891201186*s - 1.26675030339891)*(1.6303335086819*s - 1.31516675434095)*(2.13304925416494*s - 1.41234888855843)*(3.26066701736381*s - 1.6303335086819)*(5.63715641989734*s - 0.0897428551337897)*(6.91759051905683*s - 2.33726905577346)*(8.98231798532437*s - 0.73641036684684);
}

double basisFunction_8_3(const double s) {
  return -9087.95490442456*pow_8(s) + 37825.2198624478*pow_7(s) - 64200.1924807785*pow_6(s) + 56892.5505012905*pow_5(s) - 27927.9752689486*pow_4(s) + 7403.32901268855*pow_3(s) - 952.858185169427*pow_2(s) + 48.1523827349633*s - 0.553219335035512;
}

double basisFunction_8_4(const double s) {
  return (2.06577374114964*s - 2.03288687057482)*(2.06577374114964*s - 0.0328868705748199)*(2.39225548244894*s - 2.19612774122447)*(2.39225548244894*s - 0.196127741224468)*(3.26066701736381*s - 2.6303335086819)*(3.26066701736381*s - 0.630333508681904)*(6.16801506366611*s - 4.08400753183306)*(6.16801506366611*s - 2.08400753183306);
}

double basisFunction_8_5(const double s) {
  return -9087.95490442456*pow_8(s) + 34878.4193729487*pow_7(s) - 53886.3907675316*pow_6(s) + 42904.4619197522*pow_5(s) - 18742.25809822*pow_4(s) + 4429.69612787332*pow_3(s) - 521.225172051791*pow_2(s) + 24.9806018130092*s - 0.282299494305024;
}

double basisFunction_8_6(const double s) {
  return (1.26459686843166*s - 0.0201322307051351)*(1.37987891201186*s - 0.113128608612956)*(1.6303335086819*s - 0.315166754340952)*(2.13304925416494*s - 0.720700365606509)*(3.26066701736381*s - 1.6303335086819)*(5.63715641989734*s - 5.54741356476355)*(6.91759051905683*s - 4.58032146328337)*(8.98231798532437*s - 8.24590761847753);
}

double basisFunction_8_7(const double s) {
  return -4008.73624216047*pow_8(s) + 14359.230868885*pow_7(s) - 20774.3567749628*pow_6(s) + 15592.5954572091*pow_5(s) - 6484.02170468759*pow_4(s) + 1475.51967351534*pow_3(s) - 169.132543558034*pow_2(s) + 7.98539655251807*s - 0.0898136794151058;
}

double basisFunction_8_8(const double s) {
  return (1.03288687057482*s - 0.0164434352874099)*(1.10852987046486*s - 0.0908822076777352)*(1.26459686843166*s - 0.244464637726527)*(1.54749215161853*s - 0.522856261882847)*(2.06577374114964*s - 1.03288687057482)*(3.10603949202106*s - 2.05659171526816)*(5.63715641989734*s - 4.54741356476355)*(15.1367073028256*s - 13.895732735243);
}

double basisFunction_9_0(const double s) {
  return -3827.82774641725*pow_9(s) + 19089.1980750151*pow_8(s) - 40547.5334692784*pow_7(s) + 47822.4954216571*pow_6(s) - 34217.6006340629*pow_5(s) + 15232.259649268*pow_4(s) - 4156.45974540193*pow_3(s) + 656.823361843635*pow_2(s) - 52.9639101678501*s + 1.58800537867501;
}

double basisFunction_9_1(const double s) {
  return (1.08756538386714*s - 1.07337620570232)*(1.15598468101531*s - 1.07799234050766)*(1.29494014086788*s - 1.087367431474)*(1.54028765514254*s - 1.10392061525773)*(1.97250776730628*s - 1.13308177865007)*(2.79255883939237*s - 1.18840924405526)*(4.63319061671462*s - 1.31259357165383)*(10.7727394609997*s - 1.72681859720201)*(18.3750634068962*s - 0.239734596501503);
}

double basisFunction_9_2(const double s) {
  return -22434.4186400316*pow_9(s) + 108575.963221774*pow_8(s) - 221699.464886896*pow_7(s) + 247844.76661189*pow_6(s) - 164473.498745941*pow_5(s) + 65526.508115168*pow_4(s) - 15021.6428761764*pow_3(s) + 1759.4867802325*pow_2(s) - 78.6017099254307*s + 0.757522798651748;
}

double basisFunction_9_3(const double s) {
  return (1.42115914702868*s - 1.4026176591909)*(1.54028765514254*s - 1.43636703988481)*(1.79726013769364*s - 1.50916793598256)*(2.30736185373899*s - 1.65368092686949)*(3.43483421216942*s - 1.97309644250881)*(3.70020129606544*s - 0.0482755484998039)*(4.63319061671462*s - 0.312593571653829)*(7.0293567516905*s - 2.99143295590826)*(8.12961288409251*s - 1.30313805203652);
}

double basisFunction_9_4(const double s) {
  return -35114.2347185061*pow_9(s) + 160627.860474559*pow_8(s) - 305886.570432416*pow_7(s) + 313374.273295268*pow_6(s) - 186255.192146802*pow_5(s) + 64563.7489375677*pow_4(s) - 12476.0226162313*pow_3(s) + 1213.43656110524*pow_2(s) - 48.0768152250963*s + 0.446602312880397;
}

double basisFunction_9_5(const double s) {
  return (1.78129148607186*s - 0.0232400392972105)*(1.97250776730628*s - 0.133081778650069)*(2.41463100081977*s - 0.387053797475685)*(2.42414783976601*s - 2.39252062350246)*(2.79255883939237*s - 2.60414959533711)*(3.43483421216942*s - 0.973096442508815)*(3.76977793269879*s - 3.16550056526843)*(6.71707432483301*s - 2.8585371624165)*(7.0293567516905*s - 5.03792379578224);
}

double basisFunction_9_6(const double s) {
  return -30546.9567297263*pow_9(s) + 130841.850108104*pow_8(s) - 231792.196704141*pow_7(s) + 219731.361957977*pow_6(s) - 120562.545987372*pow_5(s) + 38712.67912259*pow_4(s) - 7010.15041007892*pow_3(s) + 650.198632275479*pow_2(s) - 25.0542874722612*s + 0.230692454393378;
}

double basisFunction_9_7(const double s) {
  return (1.20969003075178*s - 0.0157825061602416)*(1.29494014086788*s - 0.0873674314740019)*(1.47186623012038*s - 0.23593311506019)*(1.79726013769364*s - 0.509167935982559)*(2.41463100081977*s - 1.02757720334409)*(3.76977793269879*s - 2.16550056526843)*(6.79124157518811*s - 6.70263804100052)*(8.12961288409251*s - 5.826474832056)*(10.7727394609997*s - 10.0459208637977);
}

double basisFunction_9_8(const double s) {
  return -12668.2086755305*pow_9(s) + 51527.5374165006*pow_8(s) - 86965.373362548*pow_7(s) + 78921.5119922414*pow_6(s) - 41711.3428393168*pow_5(s) + 12991.5202187831*pow_4(s) - 2298.51493986601*pow_3(s) + 209.789611679887*pow_2(s) - 8.00923796148983*s + 0.0735280521875994;
}

double basisFunction_9_9(const double s) {
  return (1.02679258298284*s - 0.0133962914914212)*(1.08756538386714*s - 0.0733762057023171)*(1.20969003075178*s - 0.19390752459154)*(1.42115914702868*s - 0.402617659190899)*(1.78129148607186*s - 0.758051446774654)*(2.42414783976601*s - 1.39252062350246)*(3.70020129606544*s - 2.65192574756564)*(6.79124157518811*s - 5.70263804100052)*(18.3750634068962*s - 17.1353288103947);
}



// First derivative of basis functions in interval (0,1).
double basisFunctionFirstDerivative_1_0(const double s) {
  return -1.73205080756888;
}

double basisFunctionFirstDerivative_1_1(const double s) {
  return 1.73205080756888;
}

double basisFunctionFirstDerivative_2_0(const double s) {
  return 6.66666666666667*s - 4.62432778206914;
}

double basisFunctionFirstDerivative_2_1(const double s) {
  return -13.3333333333333*s + 6.66666666666667;
}

double basisFunctionFirstDerivative_2_2(const double s) {
  return 6.66666666666667*s - 2.04233888459753;
}

double basisFunctionFirstDerivative_3_0(const double s) {
  return -22.2616202041156*pow_2(s) + 28.6517167083437*s - 8.54602360787259;
}

double basisFunctionFirstDerivative_3_1(const double s) {
  return 56.3863482226667*pow_2(s) - 62.7764447268922*s + 13.8071669256901;
}

double basisFunctionFirstDerivative_3_2(const double s) {
  return -56.3863482226667*pow_2(s) + 49.9962517184392*s - 7.41707042146291;
}

double basisFunctionFirstDerivative_3_3(const double s) {
  return 22.2616202041156*pow_2(s) - 15.8715236998902*s + 2.15592710364439;
}

double basisFunctionFirstDerivative_4_0(const double s) {
  return 73.3588844577245*pow_3(s) - 134.966955167636*pow_2(s) + 77.2889981106907*s - 13.4702845011948;
}

double basisFunctionFirstDerivative_4_1(const double s) {
  return -207.758884457726*pow_3(s) + 353.590245379942*pow_2(s) - 176.445621656326*s + 22.9243335557241;
}

double basisFunctionFirstDerivative_4_2(const double s) {
  return 268.8*pow_3(s) - 403.2*pow_2(s) + 164.266666666667*s - 14.9333333333333;
}

double basisFunctionFirstDerivative_4_3(const double s) {
  return -207.758884457726*pow_3(s) + 269.686407993234*pow_2(s) - 92.5417842696176*s + 7.68992717838791;
}

double basisFunctionFirstDerivative_4_4(const double s) {
  return 73.3588844577245*pow_3(s) - 85.1096982055377*pow_2(s) + 27.4317411485878*s - 2.21064289957879;
}

double basisFunctionFirstDerivative_5_0(const double s) {
  return -244.23785187026*pow_4(s) + 579.573444173913*pow_3(s) - 484.900345690071*pow_2(s) + 166.712343330413*s - 19.3888996957563;
}

double basisFunctionFirstDerivative_5_1(const double s) {
  return 736.012162220866*pow_4(s) - 1666.68758452724*pow_3(s) + 1293.73263172535*pow_2(s) - 389.180081840647*s + 33.9475568900566;
}

double basisFunctionFirstDerivative_5_2(const double s) {
  return -1085.05021486417*pow_4(s) + 2273.66594938044*pow_3(s) - 1570.24878237251*pow_2(s) + 390.608330333902*s - 24.2905065059373;
}

double basisFunctionFirstDerivative_5_3(const double s) {
  return 1085.05021486417*pow_4(s) - 2066.53491007621*pow_3(s) + 1259.55222341617*pow_2(s) - 269.092245726446*s + 15.3152240282669;
}

double basisFunctionFirstDerivative_5_4(const double s) {
  return -736.012162220866*pow_4(s) + 1277.36106435622*pow_3(s) - 709.742851468836*pow_2(s) + 142.271076911811*s - 7.82468446839186;
}

double basisFunctionFirstDerivative_5_5(const double s) {
  return 244.23785187026*pow_4(s) - 397.377963307127*pow_3(s) + 211.607124389893*pow_2(s) - 41.3194230090281*s + 2.24130975176039;
}

double basisFunctionFirstDerivative_6_0(const double s) {
  return 825.099426800292*pow_5(s) - 2389.04373135308*pow_4(s) + 2617.07247123451*pow_3(s) - 1338.4428529539*pow_2(s) + 313.873636590949*s - 26.2986906664171;
}

double basisFunctionFirstDerivative_6_1(const double s) {
  return -2583.29294672202*pow_5(s) + 7256.39581744392*pow_4(s) - 7595.80255683016*pow_3(s) + 3610.65788948306*pow_2(s) - 742.710007782101*s + 46.8493308286903;
}

double basisFunctionFirstDerivative_6_2(const double s) {
  return 4111.56494849315*pow_5(s) - 10974.1868291159*pow_4(s) + 10675.3710002171*pow_3(s) - 4539.93992117269*pow_2(s) + 778.069369952515*s - 35.3756406138212;
}

double basisFunctionFirstDerivative_6_3(const double s) {
  return -4706.74285714286*pow_5(s) + 11766.8571428571*pow_4(s) - 10499.6571428571*pow_3(s) + 3982.62857142857*pow_2(s) - 592.457142857143*s + 24.6857142857143;
}

double basisFunctionFirstDerivative_6_4(const double s) {
  return 4111.56494849315*pow_5(s) - 9583.63791334987*pow_4(s) + 7894.27316868515*pow_3(s) - 2756.70158971499*pow_2(s) + 385.379954260818*s - 15.5029277604244;
}

double basisFunctionFirstDerivative_6_5(const double s) {
  return -2583.29294672202*pow_5(s) + 5660.06891616615*pow_4(s) - 4403.14875427455*pow_3(s) + 1471.30434356404*pow_2(s) - 199.683363140866*s + 7.90247357862299;
}

double basisFunctionFirstDerivative_6_6(const double s) {
  return 825.099426800293*pow_5(s) - 1736.45340264839*pow_4(s) + 1311.89181382513*pow_3(s) - 429.506440634091*pow_2(s) + 57.5275529758323*s - 2.26025965235975;
}

double basisFunctionFirstDerivative_7_0(const double s) {
  return -2827.44357369987*pow_6(s) + 9645.97302837523*pow_5(s) - 13035.1354384421*pow_4(s) + 8840.76859630575*pow_3(s) - 3130.78432161885*pow_2(s) + 538.547127145416*s - 34.1982194409867;
}

double basisFunctionFirstDerivative_7_1(const double s) {
  return 9078.59601652024*pow_6(s) - 30335.4793817075*pow_5(s) + 39796.6897929157*pow_4(s) - 25814.7068037165*pow_3(s) + 8506.92987013461*pow_2(s) - 1285.69476698963*s + 61.6171593747048;
}

double basisFunctionFirstDerivative_7_2(const double s) {
  return -15177.751864131*pow_6(s) + 48951.7129556509*pow_5(s) - 61152.0081673881*pow_4(s) + 36962.947587636*pow_3(s) - 10936.1453633483*pow_2(s) + 1383.76948598626*s - 48.1378065361807;
}

double basisFunctionFirstDerivative_7_3(const double s) {
  return 18856.5951475136*pow_6(s) - 58052.1937809278*pow_5(s) + 68246.023034799*pow_4(s) - 38050.1691929793*pow_3(s) + 10106.1910317803*pow_2(s) - 1117.23101981437*s + 35.6555666843119;
}

double basisFunctionFirstDerivative_7_4(const double s) {
  return -18856.5951475136*pow_6(s) + 55087.377104154*pow_5(s) - 60833.9813428646*pow_4(s) + 31543.8880872116*pow_3(s) - 7758.81106506313*pow_2(s) + 807.337584447268*s - 24.8707870558158;
}

double basisFunctionFirstDerivative_7_5(const double s) {
  return 15177.751864131*pow_6(s) - 42114.7982291348*pow_5(s) + 44059.7213510978*pow_4(s) - 21682.9928080262*pow_3(s) + 5108.50001022382*pow_2(s) - 515.657553885703*s + 15.6131721302905;
}

double basisFunctionFirstDerivative_7_6(const double s) {
  return -9078.59601652023*pow_6(s) + 24136.0967174139*pow_5(s) - 24298.2331321817*pow_4(s) + 11589.178881276*pow_3(s) - 2667.09464720794*pow_2(s) + 264.982924376924*s - 7.95188653165957;
}

double basisFunctionFirstDerivative_7_7(const double s) {
  return 2827.44357369987*pow_6(s) - 7318.68841382397*pow_5(s) + 7216.92390206388*pow_4(s) - 3388.91434770739*pow_3(s) + 771.214485099453*pow_2(s) - 76.053781266189*s + 2.2728013753287;
}

double basisFunctionFirstDerivative_8_0(const double s) {
  return 9814.13270251095*pow_7(s) - 38506.4376759608*pow_6(s) + 61823.1638629783*pow_5(s) - 52219.4331002191*pow_4(s) + 24793.9531369774*pow_3(s) - 6523.34139985824*pow_2(s) + 863.330770012799*s - 43.0867590824862;
}

double basisFunctionFirstDerivative_8_1(const double s) {
  return -32069.8899372838*pow_7(s) + 123974.613478792*pow_6(s) - 195026.132839567*pow_5(s) + 159994.632534849*pow_4(s) - 72699.4103333737*pow_3(s) + 17813.6916672387*pow_2(s) - 2073.73467479543*s + 78.2447075882907;
}

double basisFunctionFirstDerivative_8_2(const double s) {
  return 55445.9996447724*pow_7(s) - 208939.932861542*pow_6(s) + 317624.254116466*pow_5(s) - 248485.258490839*pow_4(s) + 105404.846343187*pow_3(s) - 23244.6191566094*pow_2(s) + 2272.94646665438*s - 62.5512267605649;
}

double basisFunctionFirstDerivative_8_3(const double s) {
  return -72703.6392353964*pow_7(s) + 264776.539037134*pow_6(s) - 385201.154884671*pow_5(s) + 284462.752506452*pow_4(s) - 111711.901075794*pow_3(s) + 22209.9870380656*pow_2(s) - 1905.71637033885*s + 48.1523827349633;
}

double basisFunctionFirstDerivative_8_4(const double s) {
  return 79026.7936507937*pow_7(s) - 276593.777777778*pow_6(s) + 383512.380952381*pow_5(s) - 267296.507936508*pow_4(s) + 98086.1968253968*pow_3(s) - 18129.6761904762*pow_2(s) + 1466.10793650794*s - 35.7587301587302;
}

double basisFunctionFirstDerivative_8_5(const double s) {
  return -72703.6392353964*pow_7(s) + 244148.935610641*pow_6(s) - 323318.34460519*pow_5(s) + 214522.309598761*pow_4(s) - 74969.0323928799*pow_3(s) + 13289.08838362*pow_2(s) - 1042.45034410356*s + 24.9806018130092;
}

double basisFunctionFirstDerivative_8_6(const double s) {
  return 55445.9996447725*pow_7(s) - 179182.064651865*pow_6(s) + 228350.649487436*pow_5(s) - 146147.006735397*pow_4(s) + 49517.6838806854*pow_3(s) - 8567.90970966607*pow_2(s) + 660.884146123406*s - 15.6848353282207;
}

double basisFunctionFirstDerivative_8_7(const double s) {
  return -32069.8899372838*pow_7(s) + 100514.616082195*pow_6(s) - 124646.140649777*pow_5(s) + 77962.9772860454*pow_4(s) - 25936.0868187504*pow_3(s) + 4426.55902054609*pow_2(s) - 338.265087116068*s + 7.98539655251807;
}

double basisFunctionFirstDerivative_8_8(const double s) {
  return 9814.13270251097*pow_7(s) - 30192.491241616*pow_6(s) + 36881.3245599439*pow_5(s) - 22794.4656631443*pow_4(s) + 7513.7504345518*pow_3(s) - 1273.77965286043*pow_2(s) + 96.8971570553815*s - 2.28153735878081;
}

double basisFunctionFirstDerivative_9_0(const double s) {
  return -34450.4497177553*pow_8(s) + 152713.584600121*pow_7(s) - 283832.734284949*pow_6(s) + 286934.972529942*pow_5(s) - 171088.003170314*pow_4(s) + 60929.0385970722*pow_3(s) - 12469.3792362058*pow_2(s) + 1313.64672368727*s - 52.9639101678501;
}

double basisFunctionFirstDerivative_9_1(const double s) {
  return 114013.878079774*pow_8(s) - 499890.725306189*pow_7(s) + 915604.10444748*pow_6(s) - 907167.495768823*pow_5(s) + 525536.546460909*pow_4(s) - 179189.022980629*pow_3(s) + 34173.3711683393*pow_2(s) - 3169.37535071005*s + 96.7284878085901;
}

double basisFunctionFirstDerivative_9_2(const double s) {
  return -201909.767760284*pow_8(s) + 868607.70577419*pow_7(s) - 1551896.25420827*pow_6(s) + 1487068.59967134*pow_5(s) - 822367.493729707*pow_4(s) + 262106.032460672*pow_3(s) - 45064.9286285292*pow_2(s) + 3518.973560465*s - 78.6017099254307;
}

double basisFunctionFirstDerivative_9_3(const double s) {
  return 274922.610567537*pow_8(s) - 1152646.08367546*pow_7(s) + 1993234.86676619*pow_6(s) - 1831119.46344664*pow_5(s) + 957917.234590785*pow_4(s) - 283374.185871104*pow_3(s) + 44055.8955660319*pow_2(s) - 3027.95679335315*s + 62.1365834857804;
}

double basisFunctionFirstDerivative_9_4(const double s) {
  return -316028.112466555*pow_8(s) + 1285022.88379647*pow_7(s) - 2141205.99302692*pow_6(s) + 1880245.63977161*pow_5(s) - 931275.960734011*pow_4(s) + 258254.995750271*pow_3(s) - 37428.0678486938*pow_2(s) + 2426.87312221048*s - 48.0768152250963;
}

double basisFunctionFirstDerivative_9_5(const double s) {
  return 316028.112466555*pow_8(s) - 1243202.01593596*pow_7(s) + 1994832.95551514*pow_6(s) - 1679084.05679102*pow_5(s) + 794304.597061985*pow_4(s) - 210285.675258508*pow_3(s) + 29258.9320271891*pow_2(s) - 1840.59072098202*s + 35.8184508363869;
}

double basisFunctionFirstDerivative_9_6(const double s) {
  return -274922.610567537*pow_8(s) + 1046734.80086483*pow_7(s) - 1622545.37692899*pow_6(s) + 1318388.17174786*pow_5(s) - 602812.72993686*pow_4(s) + 154850.71649036*pow_3(s) - 21030.4512302367*pow_2(s) + 1300.39726455096*s - 25.0542874722612;
}

double basisFunctionFirstDerivative_9_7(const double s) {
  return 201909.767760284*pow_8(s) - 746670.436308082*pow_7(s) + 1125115.81107689*pow_6(s) - 890494.098896209*pow_5(s) + 397882.349620326*pow_4(s) - 100280.322389465*pow_3(s) + 13421.2860654111*pow_2(s) - 821.489789286292*s + 15.7345700520389;
}

double basisFunctionFirstDerivative_9_8(const double s) {
  return -114013.878079774*pow_8(s) + 412220.299332005*pow_7(s) - 608757.613537836*pow_6(s) + 473529.071953449*pow_5(s) - 208556.714196584*pow_4(s) + 51966.0808751322*pow_3(s) - 6895.54481959802*pow_2(s) + 419.579223359774*s - 8.00923796148983;
}

double basisFunctionFirstDerivative_9_9(const double s) {
  return 34450.4497177553*pow_8(s) - 122890.013141922*pow_7(s) + 179450.234181251*pow_6(s) - 138301.340771508*pow_5(s) + 60460.1740334726*pow_4(s) - 14977.6576738013*pow_3(s) + 1978.88693629221*pow_2(s) - 120.057239941923*s + 2.28786856933239;
}



// Second derivative of basis functions in interval (0,1).
double basisFunctionSecondDerivative_1_0(const double s) {
  return 0;
}

double basisFunctionSecondDerivative_1_1(const double s) {
  return 0;
}

double basisFunctionSecondDerivative_2_0(const double s) {
  return 6.66666666666667;
}

double basisFunctionSecondDerivative_2_1(const double s) {
  return -13.3333333333333;
}

double basisFunctionSecondDerivative_2_2(const double s) {
  return 6.66666666666667;
}

double basisFunctionSecondDerivative_3_0(const double s) {
  return -44.5232404082337*s + 28.6517167083438;
}

double basisFunctionSecondDerivative_3_1(const double s) {
  return 112.77269644533*s - 62.7764447268921;
}

double basisFunctionSecondDerivative_3_2(const double s) {
  return -112.77269644533*s + 49.9962517184382;
}

double basisFunctionSecondDerivative_3_3(const double s) {
  return 44.5232404082337*s - 15.8715236998899;
}

double basisFunctionSecondDerivative_4_0(const double s) {
  return 220.076653373174*pow_2(s) - 269.933910335275*s + 77.2889981106907;
}

double basisFunctionSecondDerivative_4_1(const double s) {
  return -623.276653373176*pow_2(s) + 707.180490759883*s - 176.445621656326;
}

double basisFunctionSecondDerivative_4_2(const double s) {
  return 806.4*pow_2(s) - 806.4*s + 164.266666666667;
}

double basisFunctionSecondDerivative_4_3(const double s) {
  return -623.276653373176*pow_2(s) + 539.372815986466*s - 92.5417842696176;
}

double basisFunctionSecondDerivative_4_4(const double s) {
  return 220.076653373174*pow_2(s) - 170.219396411074*s + 27.4317411485878;
}

double basisFunctionSecondDerivative_5_0(const double s) {
  return -976.95140748104*pow_3(s) + 1738.72033252174*pow_2(s) - 969.800691380144*s + 166.712343330413;
}

double basisFunctionSecondDerivative_5_1(const double s) {
  return 2944.04864888346*pow_3(s) - 5000.06275358172*pow_2(s) + 2587.46526345071*s - 389.180081840647;
}

double basisFunctionSecondDerivative_5_2(const double s) {
  return -4340.20085945665*pow_3(s) + 6820.99784814132*pow_2(s) - 3140.49756474502*s + 390.608330333902;
}

double basisFunctionSecondDerivative_5_3(const double s) {
  return 4340.20085945665*pow_3(s) - 6199.60473022864*pow_2(s) + 2519.10444683233*s - 269.092245726446;
}

double basisFunctionSecondDerivative_5_4(const double s) {
  return -2944.04864888346*pow_3(s) + 3832.08319306867*pow_2(s) - 1419.48570293767*s + 142.271076911811;
}

double basisFunctionSecondDerivative_5_5(const double s) {
  return 976.95140748104*pow_3(s) - 1192.13388992138*pow_2(s) + 423.214248779783*s - 41.3194230090281;
}

double basisFunctionSecondDerivative_6_0(const double s) {
  return 4125.49713400146*pow_4(s) - 9556.17492541231*pow_3(s) + 7851.21741370354*pow_2(s) - 2676.88570590781*s + 313.873636590949;
}

double basisFunctionSecondDerivative_6_1(const double s) {
  return -12916.4647336101*pow_4(s) + 29025.5832697757*pow_3(s) - 22787.4076704905*pow_2(s) + 7221.31577896612*s - 742.710007782101;
}

double basisFunctionSecondDerivative_6_2(const double s) {
  return 20557.8247424658*pow_4(s) - 43896.7473164635*pow_3(s) + 32026.1130006514*pow_2(s) - 9079.87984234538*s + 778.069369952515;
}

double basisFunctionSecondDerivative_6_3(const double s) {
  return -23533.7142857143*pow_4(s) + 47067.4285714286*pow_3(s) - 31498.9714285714*pow_2(s) + 7965.25714285714*s - 592.457142857143;
}

double basisFunctionSecondDerivative_6_4(const double s) {
  return 20557.8247424658*pow_4(s) - 38334.5516533995*pow_3(s) + 23682.8195060554*pow_2(s) - 5513.40317942997*s + 385.379954260818;
}

double basisFunctionSecondDerivative_6_5(const double s) {
  return -12916.4647336101*pow_4(s) + 22640.2756646646*pow_3(s) - 13209.4462628239*pow_2(s) + 2942.60868712808*s - 199.683363140866;
}

double basisFunctionSecondDerivative_6_6(const double s) {
  return 4125.49713400147*pow_4(s) - 6945.81361059354*pow_3(s) + 3935.67544147538*pow_2(s) - 859.012881268187*s + 57.5275529758323;
}

double basisFunctionSecondDerivative_7_0(const double s) {
  return -16964.6614421992*pow_5(s) + 48229.8651418761*pow_4(s) - 52140.5417537681*pow_3(s) + 26522.3057889172*pow_2(s) - 6261.5686432377*s + 538.547127145416;
}

double basisFunctionSecondDerivative_7_1(const double s) {
  return 54471.5760991214*pow_5(s) - 151677.396908538*pow_4(s) + 159186.759171663*pow_3(s) - 77444.1204111494*pow_2(s) + 17013.8597402692*s - 1285.69476698963;
}

double basisFunctionSecondDerivative_7_2(const double s) {
  return -91066.5111847858*pow_5(s) + 244758.564778255*pow_4(s) - 244608.032669552*pow_3(s) + 110888.842762908*pow_2(s) - 21872.2907266966*s + 1383.76948598626;
}

double basisFunctionSecondDerivative_7_3(const double s) {
  return 113139.570885082*pow_5(s) - 290260.968904639*pow_4(s) + 272984.092139196*pow_3(s) - 114150.507578938*pow_2(s) + 20212.3820635607*s - 1117.23101981437;
}

double basisFunctionSecondDerivative_7_4(const double s) {
  return -113139.570885082*pow_5(s) + 275436.88552077*pow_4(s) - 243335.925371459*pow_3(s) + 94631.6642616349*pow_2(s) - 15517.6221301263*s + 807.337584447268;
}

double basisFunctionSecondDerivative_7_5(const double s) {
  return 91066.5111847858*pow_5(s) - 210573.991145674*pow_4(s) + 176238.885404391*pow_3(s) - 65048.9784240786*pow_2(s) + 10217.0000204476*s - 515.657553885703;
}

double basisFunctionSecondDerivative_7_6(const double s) {
  return -54471.5760991214*pow_5(s) + 120680.483587069*pow_4(s) - 97192.9325287267*pow_3(s) + 34767.536643828*pow_2(s) - 5334.18929441588*s + 264.982924376924;
}

double basisFunctionSecondDerivative_7_7(const double s) {
  return 16964.6614421992*pow_5(s) - 36593.4420691198*pow_4(s) + 28867.6956082555*pow_3(s) - 10166.7430431222*pow_2(s) + 1542.42897019891*s - 76.053781266189;
}

double basisFunctionSecondDerivative_8_0(const double s) {
  return 68698.9289175767*pow_6(s) - 231038.626055765*pow_5(s) + 309115.819314891*pow_4(s) - 208877.732400877*pow_3(s) + 74381.8594109322*pow_2(s) - 13046.6827997165*s + 863.330770012796;
}

double basisFunctionSecondDerivative_8_1(const double s) {
  return -224489.229560987*pow_6(s) + 743847.68087275*pow_5(s) - 975130.664197837*pow_4(s) + 639978.530139397*pow_3(s) - 218098.231000121*pow_2(s) + 35627.3833344773*s - 2073.73467479543;
}

double basisFunctionSecondDerivative_8_2(const double s) {
  return 388121.997513407*pow_6(s) - 1253639.59716925*pow_5(s) + 1588121.27058233*pow_4(s) - 993941.033963357*pow_3(s) + 316214.53902956*pow_2(s) - 46489.2383132188*s + 2272.94646665438;
}

double basisFunctionSecondDerivative_8_3(const double s) {
  return -508925.474647775*pow_6(s) + 1588659.23422281*pow_5(s) - 1926005.77442335*pow_4(s) + 1137851.01002581*pow_3(s) - 335135.703227383*pow_2(s) + 44419.9740761313*s - 1905.71637033885;
}

double basisFunctionSecondDerivative_8_4(const double s) {
  return 553187.555555556*pow_6(s) - 1659562.66666667*pow_5(s) + 1917561.9047619*pow_4(s) - 1069186.03174603*pow_3(s) + 294258.59047619*pow_2(s) - 36259.3523809524*s + 1466.10793650794;
}

double basisFunctionSecondDerivative_8_5(const double s) {
  return -508925.474647775*pow_6(s) + 1464893.61366384*pow_5(s) - 1616591.72302595*pow_4(s) + 858089.238395044*pow_3(s) - 224907.097178639*pow_2(s) + 26578.1767672399*s - 1042.45034410356;
}

double basisFunctionSecondDerivative_8_6(const double s) {
  return 388121.997513407*pow_6(s) - 1075092.38791119*pow_5(s) + 1141753.24743718*pow_4(s) - 584588.026941588*pow_3(s) + 148553.051642056*pow_2(s) - 17135.8194193321*s + 660.884146123406;
}

double basisFunctionSecondDerivative_8_7(const double s) {
  return -224489.229560987*pow_6(s) + 603087.696493169*pow_5(s) - 623230.703248885*pow_4(s) + 311851.909144182*pow_3(s) - 77808.2604562511*pow_2(s) + 8853.11804109218*s - 338.265087116068;
}

double basisFunctionSecondDerivative_8_8(const double s) {
  return 68698.9289175768*pow_6(s) - 181154.947449696*pow_5(s) + 184406.622799719*pow_4(s) - 91177.8626525773*pow_3(s) + 22541.2513036554*pow_2(s) - 2547.55930572086*s + 96.8971570553815;
}

double basisFunctionSecondDerivative_9_0(const double s) {
  return -275603.597742042*pow_7(s) + 1068995.09220085*pow_6(s) - 1702996.40570969*pow_5(s) + 1434674.86264971*pow_4(s) - 684352.012681258*pow_3(s) + 182787.115791216*pow_2(s) - 24938.7584724115*s + 1313.64672368727;
}

double basisFunctionSecondDerivative_9_1(const double s) {
  return 912111.024638193*pow_7(s) - 3499235.07714332*pow_6(s) + 5493624.62668488*pow_5(s) - 4535837.47884411*pow_4(s) + 2102146.18584364*pow_3(s) - 537567.068941886*pow_2(s) + 68346.7423366786*s - 3169.37535071005;
}

double basisFunctionSecondDerivative_9_2(const double s) {
  return -1615278.14208227*pow_7(s) + 6080253.94041933*pow_6(s) - 9311377.52524962*pow_5(s) + 7435342.99835669*pow_4(s) - 3289469.97491883*pow_3(s) + 786318.097382016*pow_2(s) - 90129.8572570584*s + 3518.973560465;
}

double basisFunctionSecondDerivative_9_3(const double s) {
  return 2199380.88454029*pow_7(s) - 8068522.58572824*pow_6(s) + 11959409.2005972*pow_5(s) - 9155597.3172332*pow_4(s) + 3831668.93836314*pow_3(s) - 850122.557613313*pow_2(s) + 88111.7911320637*s - 3027.95679335315;
}

double basisFunctionSecondDerivative_9_4(const double s) {
  return -2528224.89973244*pow_7(s) + 8995160.1865753*pow_6(s) - 12847235.9581615*pow_5(s) + 9401228.19885805*pow_4(s) - 3725103.84293605*pow_3(s) + 774764.987250813*pow_2(s) - 74856.1356973875*s + 2426.87312221048;
}

double basisFunctionSecondDerivative_9_5(const double s) {
  return 2528224.89973244*pow_7(s) - 8702414.11155175*pow_6(s) + 11968997.7330908*pow_5(s) - 8395420.28395511*pow_4(s) + 3177218.38824794*pow_3(s) - 630857.025775525*pow_2(s) + 58517.8640543778*s - 1840.59072098202;
}

double basisFunctionSecondDerivative_9_6(const double s) {
  return -2199380.88454029*pow_7(s) + 7327143.60605383*pow_6(s) - 9735272.26157391*pow_5(s) + 6591940.85873932*pow_4(s) - 2411250.91974744*pow_3(s) + 464552.14947108*pow_2(s) - 42060.9024604735*s + 1300.39726455096;
}

double basisFunctionSecondDerivative_9_7(const double s) {
  return 1615278.14208227*pow_7(s) - 5226693.05415657*pow_6(s) + 6750694.86646137*pow_5(s) - 4452470.49448105*pow_4(s) + 1591529.3984813*pow_3(s) - 300840.967168395*pow_2(s) + 26842.5721308223*s - 821.489789286291;
}

double basisFunctionSecondDerivative_9_8(const double s) {
  return -912111.024638193*pow_7(s) + 2885542.09532403*pow_6(s) - 3652545.68122702*pow_5(s) + 2367645.35976724*pow_4(s) - 834226.856786336*pow_3(s) + 155898.242625397*pow_2(s) - 13791.089639196*s + 419.579223359774;
}

double basisFunctionSecondDerivative_9_9(const double s) {
  return 275603.597742043*pow_7(s) - 860230.091993451*pow_6(s) + 1076701.4050875*pow_5(s) - 691506.703857538*pow_4(s) + 241840.69613389*pow_3(s) - 44932.9730214039*pow_2(s) + 3957.77387258443*s - 120.057239941923;
}
#else

double basisFunction_1_0(const double s){
return -s+1.000000000;
}
double basisFunction_1_1(const double s){
return 1.*s
;
}
double basisFunction_2_0(const double s){
return (2.*s-1.)*(s-1.)
;
}
double basisFunction_2_1(const double s){
return (-4.*s+4.)*s
;
}
double basisFunction_2_2(const double s){
return (2.*s-1.0)*s
;
}
double basisFunction_3_0(const double s){
return -1.381966011*(3.618033988*s-1.)*(s-.7236067977)*(s-1.)
;
}
double basisFunction_3_1(const double s){
return 11.18033989*s*(s-.7236067977)*(s-1.)
;
}
double basisFunction_3_2(const double s){
return -11.18033989*s*(s-.2763932022500211)*(s-1.000000000000000)
;
}
double basisFunction_3_3(const double s){
return 4.999999998*s*(s-.2763932022500211)*(s-.7236067977499789)
;
}
double basisFunction_4_0(const double s){
return 2.417424305*(5.791287849*s-1.)*(s-.5)*(s-.8273268354)*(s-1.)
;
}
double basisFunction_4_1(const double s){
return -32.66666666*s*(s-.5)*(s-.8273268354)*(s-1.)
;
}
double basisFunction_4_2(const double s){
return 37.33333332*s*(s-.1726731646)*(s-.8273268354)*(s-1.)
;
}
double basisFunction_4_3(const double s){
return -32.66666666*s*(s-.1726731646)*(s-.5)*(s-1.)
;
}
double basisFunction_4_4(const double s){
return 14.*s*(s-.1726731646)*(s-.5)*(s-.8273268354)
;
}
double basisFunction_5_0(const double s){
return -4.933838196*(8.512642355*s-1.)*(s-.3573842418)*(s-.6426157582)*(s-.882527662)*(s-1.)
;
}
double basisFunction_5_1(const double s){
return 100.0722106*s*(s-.3573842418)*(s-.6426157582)*(s-.882527662)*(s-1.)
;
}
double basisFunction_5_2(const double s){
return -121.167457*s*(s-.117472338)*(s-.6426157582)*(s-.882527662)*(s-1.)
;
}
double basisFunction_5_3(const double s){
return 121.167457*s*(s-.117472338)*(s-.3573842418)*(s-.882527662)*(s-1.)
;
}
double basisFunction_5_4(const double s){
return -100.0722106*s*(s-.117472338)*(s-.3573842418)*(s-.6426157582)*(s-1.)
;
}
double basisFunction_5_5(const double s){
return 42.00000003*s*(s-.117472338)*(s-.3573842418)*(s-.6426157582)*(s-.882527662)
;
}
double basisFunction_6_0(const double s){
return 11.20522285*(11.78022087*s-1.)*(s-.2655756033)*(s-.5)*(s-.7344243967)*(s-.9151119481)*(s-1.)
;
}
double basisFunction_6_1(const double s){
return -318.2636613*s*(s-.2655756033)*(s-.5)*(s-.7344243967)*(s-.9151119481)*(s-1.)
;
}
double basisFunction_6_2(const double s){
return 397.4636614*s*(s-0.8488805186e-1)*(s-.5)*(s-.7344243967)*(s-.9151119481)*(s-1.)
;
}
double basisFunction_6_3(const double s){
return -422.4000002*s*(s-0.8488805186e-1)*(s-.2655756033)*(s-.7344243967)*(s-.9151119481)*(s-1.)
;
}
double basisFunction_6_4(const double s){
return 397.4636612*s*(s-0.8488805186e-1)*(s-.2655756033)*(s-.5)*(s-.9151119481)*(s-1.)
;
}
double basisFunction_6_5(const double s){
return -318.2636612*s*(s-0.8488805186e-1)*(s-.2655756033)*(s-.5)*(s-.7344243967)*(s-1.)
;
}
double basisFunction_6_6(const double s){
return 132.*s*(s-0.8488805186e-1)*(s-.2655756033)*(s-.5)*(s-.7344243967)*(s-.9151119481)
;
}
double basisFunction_7_0(const double s){
return -27.51173814*(15.59334411*s-1.)*(s-.2041499093)*(s-.395350391)*(s-.604649609)*(s-.7958500907)*(s-.9358700743)*(s-1.)
;
}
double basisFunction_7_1(const double s){
return 1042.012507*s*(s-.2041499093)*(s-.395350391)*(s-.604649609)*(s-.7958500907)*(s-.9358700743)*(s-1.)
;
}
double basisFunction_7_2(const double s){
return -1325.841514*s*(s-0.6412992575e-1)*(s-.395350391)*(s-.604649609)*(s-.7958500907)*(s-.9358700743)*(s-1.)
;
}
double basisFunction_7_3(const double s){
return 1457.89616*s*(s-0.6412992575e-1)*(s-.2041499093)*(s-.604649609)*(s-.7958500907)*(s-.9358700743)*(s-1.)
;
}
double basisFunction_7_4(const double s){
return -1457.896161*s*(s-0.6412992575e-1)*(s-.2041499093)*(s-.395350391)*(s-.7958500907)*(s-.9358700743)*(s-1.)
;
}
double basisFunction_7_5(const double s){
return 1325.841514*s*(s-0.6412992575e-1)*(s-.2041499093)*(s-.395350391)*(s-.604649609)*(s-.9358700743)*(s-1.)
;
}
double basisFunction_7_6(const double s){
return -1042.012508*s*(s-0.6412992575e-1)*(s-.2041499093)*(s-.395350391)*(s-.604649609)*(s-.7958500907)*(s-1.)
;
}
double basisFunction_7_7(const double s){
return 429.0000002*s*(s-0.6412992575e-1)*(s-.2041499093)*(s-.395350391)*(s-.604649609)*(s-.7958500907)*(s-.9358700743)
;
}
double basisFunction_8_0(const double s){
return 71.67303331*(19.95171593*s-.9999999998)*(s-.1614068602)*(s-.3184412681)*(s-.5)*(s-.6815587319)*(s-.8385931398)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_1(const double s){
return -3490.440193*s*(s-.1614068602)*(s-.3184412681)*(s-.5)*(s-.6815587319)*(s-.8385931398)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_2(const double s){
return 4495.614715*s*(s-0.5012100229e-1)*(s-.3184412681)*(s-.5)*(s-.6815587319)*(s-.8385931398)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_3(const double s){
return -5050.031668*s*(s-0.5012100229e-1)*(s-.1614068602)*(s-.5)*(s-.6815587319)*(s-.8385931398)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_4(const double s){
return 5229.714284*s*(s-0.5012100229e-1)*(s-.1614068602)*(s-.3184412681)*(s-.6815587319)*(s-.8385931398)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_5(const double s){
return -5050.031667*s*(s-0.5012100229e-1)*(s-.1614068602)*(s-.3184412681)*(s-.5)*(s-.8385931398)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_6(const double s){
return 4495.614714*s*(s-0.5012100229e-1)*(s-.1614068602)*(s-.3184412681)*(s-.5)*(s-.6815587319)*(s-.9498789977)*(s-1.)
;
}
double basisFunction_8_7(const double s){
return -3490.440194*s*(s-0.5012100229e-1)*(s-.1614068602)*(s-.3184412681)*(s-.5)*(s-.6815587319)*(s-.8385931398)*(s-1.)
;
}
double basisFunction_8_8(const double s){
return 1430.000001*s*(s-0.5012100229e-1)*(s-.1614068602)*(s-.3184412681)*(s-.5)*(s-.6815587319)*(s-.8385931398)*(s-.9498789977)
;
}
double basisFunction_9_0(const double s){
return -195.6130694*(24.85518998*s-.9999999998)*(s-.1306130674472474)*(s-.2610375250947777)*(s-.4173605211668065)*(s-.5826394788331934)*(s-.7389624749052223)*(s-.8693869325527526)*(s-.9597669540832294)*(s-1.000000000000000)
;
}
double basisFunction_9_1(const double s){
return 11908.19794*s*(s-.1306130674)*(s-.2610375251)*(s-.4173605212)*(s-.5826394788)*(s-.7389624749)*(s-.8693869326)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_2(const double s){
return -15466.98443*s*(s-0.4023304592e-1)*(s-.2610375251)*(s-.4173605212)*(s-.5826394788)*(s-.7389624749)*(s-.8693869326)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_3(const double s){
return 17625.61894*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.4173605212)*(s-.5826394788)*(s-.7389624749)*(s-.8693869326)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_4(const double s){
return -18666.08441*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.2610375251)*(s-.5826394788)*(s-.7389624749)*(s-.8693869326)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_5(const double s){
return 18666.0844*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.2610375251)*(s-.4173605212)*(s-.7389624749)*(s-.8693869326)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_6(const double s){
return -17625.61894*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.2610375251)*(s-.4173605212)*(s-.5826394788)*(s-.8693869326)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_7(const double s){
return 15466.98442*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.2610375251)*(s-.4173605212)*(s-.5826394788)*(s-.7389624749)*(s-.9597669541)*(s-1.)
;
}
double basisFunction_9_8(const double s){
return -11908.19795*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.2610375251)*(s-.4173605212)*(s-.5826394788)*(s-.7389624749)*(s-.8693869326)*(s-1.)
;
}
double basisFunction_9_9(const double s){
return 4862.000006*s*(s-0.4023304592e-1)*(s-.1306130674)*(s-.2610375251)*(s-.4173605212)*(s-.5826394788)*(s-.7389624749)*(s-.8693869326)*(s-.9597669541)
;
}


double basisFunctionFirstDerivative_1_0(const double s){
return -1
;
}
double basisFunctionFirstDerivative_1_1(const double s){
return 1.
;
}
double basisFunctionFirstDerivative_2_0(const double s){
return 4.*s-3.
;
}
double basisFunctionFirstDerivative_2_1(const double s){
return -8.*s+4.
;
}
double basisFunctionFirstDerivative_2_2(const double s){
return 4.*s-1.0
;
}
double basisFunctionFirstDerivative_3_0(const double s){
return -14.99999999*pow_2(s)+19.99999999*s-5.999999998
;
}
double basisFunctionFirstDerivative_3_1(const double s){
return 33.54101967*pow_2(s)-38.54101967*s+8.090169945
;
}
double basisFunctionFirstDerivative_3_2(const double s){
return -33.54101967*pow_2(s)+28.54101967*s-3.090169945
;
}
double basisFunctionFirstDerivative_3_3(const double s){
return 14.99999999*pow_2(s)-9.999999996*s+.9999999996
;
}
double basisFunctionFirstDerivative_4_0(const double s){
return 56.00000000*pow_3(s)-105.0000000*pow_2(s)+60.00000000*s-10.00000000
;
}
double basisFunctionFirstDerivative_4_1(const double s){
return -130.6666666*pow_3(s)+228.0780298*pow_2(s)-113.7446965*s+13.51300498
;
}
double basisFunctionFirstDerivative_4_2(const double s){
return 149.3333333*pow_3(s)-223.9999999*pow_2(s)+85.33333331*s-5.333333329
;
}
double basisFunctionFirstDerivative_4_3(const double s){
return -130.6666666*pow_3(s)+163.9219701*pow_2(s)-49.58863678*s+2.820328355
;
}
double basisFunctionFirstDerivative_4_4(const double s){
return 56.*pow_3(s)-62.99999999*pow_2(s)+18.00000000*s-.9999999997
;
}
double basisFunctionFirstDerivative_5_0(const double s){
return -210.0000000*pow_4(s)+504.0000000*pow_3(s)-420.0000000*pow_2(s)+140.0000000*s-15.00000001
;
}
double basisFunctionFirstDerivative_5_1(const double s){
return 500.3610530*pow_4(s)-1153.843661*pow_3(s)+899.0635716*pow_2(s)-263.1639690*s+20.28283187
;
}
double basisFunctionFirstDerivative_5_2(const double s){
return -605.837285*pow_4(s)+1280.796125*pow_3(s)-868.3723060*pow_2(s)+196.9964711*s-8.072374533
;
}
double basisFunctionFirstDerivative_5_3(const double s){
return 605.837285*pow_4(s)-1142.553015*pow_3(s)+661.0076411*pow_2(s)-120.7089057*s+4.489369293
;
}
double basisFunctionFirstDerivative_5_4(const double s){
return -500.3610530*pow_4(s)+847.6005509*pow_3(s)-439.6989065*pow_2(s)+74.87640333*s-2.699826626
;
}
double basisFunctionFirstDerivative_5_5(const double s){
return 210.0000001*pow_4(s)-336.0000003*pow_3(s)+168.0000002*pow_2(s)-28.00000002*s+1.000000000
;
}
double basisFunctionFirstDerivative_6_0(const double s){
return -1260.000001*pow_2(s)+792.0000006*pow_5(s)-2310.000001*pow_4(s)+2520.000001*pow_3(s)-21.00000000+280.0000001*s
;
}
double basisFunctionFirstDerivative_6_1(const double s){
return 2674.636787*pow_2(s)+28.40315321-1909.581967*pow_5(s)+5434.530162*pow_4(s)-5706.881280*pow_3(s)-523.7415985*s
;
}
double basisFunctionFirstDerivative_6_2(const double s){
return -2554.606475*pow_2(s)+2384.781967*pow_5(s)-6427.830816*pow_4(s)+6222.282591*pow_3(s)+390.8106327*s-11.33797046
;
}
double basisFunctionFirstDerivative_6_3(const double s){
return 1958.400000*pow_2(s)-2534.400001*pow_5(s)+6336.000003*pow_4(s)-5529.600002*pow_3(s)+6.400000007-243.2000002*s
;
}
double basisFunctionFirstDerivative_6_4(const double s){
return -1393.076080*pow_2(s)+2384.781967*pow_5(s)-5496.079022*pow_4(s)+4358.779004*pow_3(s)-4.099929630+161.0320296*s
;
}
double basisFunctionFirstDerivative_6_5(const double s){
return 934.6457669*pow_2(s)-1909.581967*pow_5(s)+4113.379677*pow_4(s)-3064.580312*pow_3(s)+2.634746873-104.9010636*s
;
}
double basisFunctionFirstDerivative_6_6(const double s){
return -360.0000000*pow_2(s)+792.*pow_5(s)-1650.000001*pow_4(s)+1200.000000*pow_3(s)-1.000000001+39.99999999*s
;
}
double basisFunctionFirstDerivative_7_0(const double s){
return -3003.000000*pow_6(s)-3150.000000*pow_2(s)+10296.00000*pow_5(s)-13860.00000*pow_4(s)+9239.999998*pow_3(s)-27.99999999+503.9999998*s
;
}
double basisFunctionFirstDerivative_7_1(const double s){
return 7294.087549*pow_6(s)+6651.499582*pow_2(s)-24607.35506*pow_5(s)+32349.96292*pow_4(s)-20783.66574*pow_3(s)+37.87519718-939.8090762*s
;
}
double basisFunctionFirstDerivative_7_2(const double s){
return -9280.890598*pow_6(s)-6305.513395*pow_2(s)+30196.17380*pow_5(s)-37697.75790*pow_4(s)+22399.79138*pow_3(s)-15.13857964+699.4519965*s
;
}
double basisFunctionFirstDerivative_7_3(const double s){
return 10205.27312*pow_6(s)+4837.071012*pow_2(s)-31531.22893*pow_5(s)+36713.02415*pow_4(s)-19789.23361*pow_3(s)+8.595816335-437.8811675*s
;
}
double basisFunctionFirstDerivative_7_4(const double s){
return -10205.27313*pow_6(s)-3514.322474*pow_2(s)+29700.40980*pow_5(s)-32135.97628*pow_4(s)+15856.03599*pow_3(s)-5.620377986+296.1506383*s
;
}
double basisFunctionFirstDerivative_7_5(const double s){
return 9280.890598*pow_6(s)+2544.307783*pow_2(s)-25489.16979*pow_5(s)+25930.24791*pow_4(s)-12047.31432*pow_3(s)+3.883318853-207.7069353*s
;
}
double basisFunctionFirstDerivative_7_6(const double s){
return -7294.087556*pow_6(s)-1738.042510*pow_2(s)+19157.17025*pow_5(s)-18724.50087*pow_4(s)+8424.386313*pow_3(s)-2.595374778+139.7945450*s
;
}
double basisFunctionFirstDerivative_7_7(const double s){
return 3003.000001*pow_6(s)+675.0000004*pow_2(s)-7722.000004*pow_5(s)+7425.000004*pow_4(s)-3300.000002*pow_3(s)+1.000000001-54.00000004*s
;
}
double basisFunctionFirstDerivative_8_0(const double s){
return -45045.00002*pow_6(s)-6930.000002*pow_2(s)+72072.00004*pow_5(s)-60060.00003*pow_4(s)+27720.00002*pow_3(s)-35.99999999+11440.00000*pow_7(s)+839.9999998*s
;
}
double basisFunctionFirstDerivative_8_1(const double s){
return 108724.2556*pow_6(s)+14581.97080*pow_2(s)-171247.2984*pow_5(s)+139445.9162*pow_4(s)-62069.50539*pow_3(s)+48.69949034-27923.52153*pow_7(s)-1563.086510*s
;
}
double basisFunctionFirstDerivative_8_2(const double s){
return -136532.5022*pow_6(s)-13751.76728*pow_2(s)+207689.8823*pow_5(s)-160880.3414*pow_4(s)+66372.00952*pow_3(s)-19.47740332+35964.91774*pow_7(s)+1161.027617*s
;
}
double basisFunctionFirstDerivative_8_3(const double s){
return 147819.0282*pow_6(s)+10540.22631*pow_2(s)-214174.4428*pow_5(s)+155266.3458*pow_4(s)-58338.35193*pow_3(s)+11.08992782-40400.25336*pow_7(s)-728.8234795*s
;
}
double basisFunctionFirstDerivative_8_4(const double s){
return -146432.0000*pow_6(s)-7723.885712*pow_2(s)+200821.0286*pow_5(s)-135972.5715*pow_4(s)+46986.97141*pow_3(s)+41837.71425*pow_7(s)-7.314285709+497.3714282*s
;
}
double basisFunctionFirstDerivative_8_5(const double s){
return 134982.7453*pow_6(s)+5741.081792*pow_2(s)-175665.5943*pow_5(s)+112329.3136*pow_4(s)-36645.70191*pow_3(s)-40400.25335*pow_7(s)+5.181491354-357.8625503*s
;
}
double basisFunctionFirstDerivative_8_6(const double s){
return -115221.9218*pow_6(s)-4256.775942*pow_2(s)+143758.1416*pow_5(s)-88353.65850*pow_4(s)+27871.54464*pow_3(s)+35964.91769*pow_7(s)-3.748881746+260.9786022*s
;
}
double basisFunctionFirstDerivative_8_7(const double s){
return 86740.39529*pow_6(s)+2954.150055*pow_2(s)-105295.7174*pow_5(s)+63249.99601*pow_4(s)-19596.96641*pow_3(s)-27923.52153*pow_7(s)+2.569661267-179.6051089*s
;
}
double basisFunctionFirstDerivative_8_8(const double s){
return -35035.00003*pow_6(s)-1155.000001*pow_2(s)+42042.00004*pow_5(s)-25025.00000*pow_4(s)+7700.000004*pow_3(s)+11440.00001*pow_7(s)-1.000000001+70.00000004*s
;
}
double basisFunctionFirstDerivative_9_0(const double s){
return -360360.0001*pow_6(s)-13860.00001*pow_2(s)+360360.0002*pow_5(s)-210210.0001*pow_4(s)+72072.00004*pow_3(s)-43758.00001*pow_8(s)-45.00000001+194480.0001*pow_7(s)+1320.000001*s
;
}
double basisFunctionFirstDerivative_9_1(const double s){
return 865973.9347*pow_6(s)+29092.21651*pow_2(s)-852744.0956*pow_5(s)+486264.0301*pow_4(s)-160870.4160*pow_3(s)+107173.7815*pow_8(s)+60.87629002-472495.0930*pow_7(s)-2452.682594*s
;
}
double basisFunctionFirstDerivative_9_2(const double s){
return -1077516.876*pow_6(s)-27333.76761*pow_2(s)+1025744.107*pow_5(s)-557073.3006*pow_4(s)+171066.4565*pow_3(s)-139202.8598*pow_8(s)-24.35589342+602517.8550*pow_7(s)+1819.083138*s
;
}
double basisFunctionFirstDerivative_9_3(const double s){
return 1153743.366*pow_6(s)+20926.62864*pow_2(s)-1048223.421*pow_5(s)+534027.4458*pow_4(s)-149752.9186*pow_3(s)+158630.5703*pow_8(s)+13.88757696-668217.1740*pow_7(s)-1143.479010*s
;
}
double basisFunctionFirstDerivative_9_4(const double s){
return -1133579.313*pow_6(s)-15396.44343*pow_2(s)+977962.3255*pow_5(s)-466897.9280*pow_4(s)+120805.4413*pow_3(s)-167994.7597*pow_8(s)-9.198709517+684319.4827*pow_7(s)+783.8034559*s
;
}
double basisFunctionFirstDerivative_9_5(const double s){
return 1047196.204*pow_6(s)+11578.25305*pow_2(s)-860510.9547*pow_5(s)+389227.2705*pow_4(s)-95273.91186*pow_3(s)+6.589286061+167994.7596*pow_8(s)-659638.5944*pow_7(s)-570.4170033*s
;
}
double basisFunctionFirstDerivative_9_6(const double s){
return -917879.1207*pow_6(s)-8844.145045*pow_2(s)+724988.0663*pow_5(s)-315599.6722*pow_4(s)+74700.82880*pow_3(s)-4.905768347-158630.5703*pow_8(s)+600827.3898*pow_7(s)+428.2417019*s
;
}
double basisFunctionFirstDerivative_9_7(const double s){
return 757571.9672*pow_6(s)+6691.400610*pow_2(s)-581842.3476*pow_5(s)+247181.1755*pow_4(s)-57358.43120*pow_3(s)+3.659127859+139202.8597*pow_8(s)-511105.0236*pow_7(s)-320.9037874*s
;
}
double basisFunctionFirstDerivative_9_8(const double s){
return -559374.1652*pow_6(s)-4702.142748*pow_2(s)+422434.3224*pow_5(s)-176989.0217*pow_4(s)+40626.95118*pow_3(s)-2.551909673-107173.7816*pow_8(s)+384895.1591*pow_7(s)+224.3541010*s
;
}
double basisFunctionFirstDerivative_9_9(const double s){
return 224224.0003*pow_6(s)+1848.000001*pow_2(s)-168168.0001*pow_5(s)+70070.00008*pow_4(s)-16016.00000*pow_3(s)+43758.00008*pow_8(s)-155584.0003*pow_7(s)+1.000000001-88.00000006*s
;
}

double basisFunctionSecondDerivative_1_0(const double s){
return 0
;
}
double basisFunctionSecondDerivative_1_1(const double s){
return 0
;
}
double basisFunctionSecondDerivative_2_0(const double s){
return 4.
;
}
double basisFunctionSecondDerivative_2_1(const double s){
return -8.
;
}
double basisFunctionSecondDerivative_2_2(const double s){
return 4.
;
}
double basisFunctionSecondDerivative_3_0(const double s){
return -29.99999998*s+19.99999999
;
}
double basisFunctionSecondDerivative_3_1(const double s){
return 67.08203934*s-38.54101967
;
}
double basisFunctionSecondDerivative_3_2(const double s){
return -67.08203934*s+28.54101967
;
}
double basisFunctionSecondDerivative_3_3(const double s){
return 29.99999998*s-9.999999996
;
}
double basisFunctionSecondDerivative_4_0(const double s){
return 168.0000000*pow_2(s)-210.0000000*s+60.00000000
;
}
double basisFunctionSecondDerivative_4_1(const double s){
return -391.9999998*pow_2(s)+456.1560596*s-113.7446965
;
}
double basisFunctionSecondDerivative_4_2(const double s){
return 447.9999999*pow_2(s)-447.9999998*s+85.33333331
;
}
double basisFunctionSecondDerivative_4_3(const double s){
return -391.9999998*pow_2(s)+327.8439402*s-49.58863678
;
}
double basisFunctionSecondDerivative_4_4(const double s){
return 168.*pow_2(s)-126.0000000*s+18.00000000
;
}
double basisFunctionSecondDerivative_5_0(const double s){
return -840.0000000*pow_3(s)+1512.000000*pow_2(s)-840.0000000*s+140.0000000
;
}
double basisFunctionSecondDerivative_5_1(const double s){
return 2001.444212*pow_3(s)-3461.530983*pow_2(s)+1798.127143*s-263.1639690
;
}
double basisFunctionSecondDerivative_5_2(const double s){
return -2423.349140*pow_3(s)+3842.388375*pow_2(s)-1736.744612*s+196.9964711
;
}
double basisFunctionSecondDerivative_5_3(const double s){
return 2423.349140*pow_3(s)-3427.659045*pow_2(s)+1322.015282*s-120.7089057
;
}
double basisFunctionSecondDerivative_5_4(const double s){
return -2001.444212*pow_3(s)+2542.801653*pow_2(s)-879.3978130*s+74.87640333
;
}
double basisFunctionSecondDerivative_5_5(const double s){
return 840.0000004*pow_3(s)-1008.000001*pow_2(s)+336.0000004*s-28.00000002
;
}
double basisFunctionSecondDerivative_6_0(const double s){
return -2520.000002*s+3960.000003*pow_4(s)-9240.000004*pow_3(s)+7560.000003*pow_2(s)+280.0000001
;
}
double basisFunctionSecondDerivative_6_1(const double s){
return 5349.273574*s-9547.909835*pow_4(s)+21738.12065*pow_3(s)-17120.64384*pow_2(s)-523.7415985
;
}
double basisFunctionSecondDerivative_6_2(const double s){
return -5109.212950*s+11923.90984*pow_4(s)-25711.32326*pow_3(s)+18666.84777*pow_2(s)+390.8106327
;
}
double basisFunctionSecondDerivative_6_3(const double s){
return 3916.800000*s-12672.00000*pow_4(s)+25344.00001*pow_3(s)-16588.80001*pow_2(s)-243.2000002
;
}
double basisFunctionSecondDerivative_6_4(const double s){
return -2786.152160*s+11923.90984*pow_4(s)-21984.31609*pow_3(s)+13076.33701*pow_2(s)+161.0320296
;
}
double basisFunctionSecondDerivative_6_5(const double s){
return 1869.291534*s-9547.909835*pow_4(s)+16453.51871*pow_3(s)-9193.740936*pow_2(s)-104.9010636
;
}
double basisFunctionSecondDerivative_6_6(const double s){
return -720.0000000*s+3960.*pow_4(s)-6600.000004*pow_3(s)+3600.000000*pow_2(s)+39.99999999
;
}
double basisFunctionSecondDerivative_7_0(const double s){
return -18018.00000*pow_5(s)-6300.000000*s+51480.00000*pow_4(s)-55440.00000*pow_3(s)+27719.99999*pow_2(s)+503.9999998
;
}
double basisFunctionSecondDerivative_7_1(const double s){
return 43764.52529*pow_5(s)+13302.99916*s-123036.7753*pow_4(s)+129399.8517*pow_3(s)-62350.99722*pow_2(s)-939.8090762
;
}
double basisFunctionSecondDerivative_7_2(const double s){
return -55685.34359*pow_5(s)-12611.02679*s+150980.8690*pow_4(s)-150791.0316*pow_3(s)+67199.37414*pow_2(s)+699.4519965
;
}
double basisFunctionSecondDerivative_7_3(const double s){
return 61231.63872*pow_5(s)+9674.142024*s-157656.1446*pow_4(s)+146852.0966*pow_3(s)-59367.70083*pow_2(s)-437.8811675
;
}
double basisFunctionSecondDerivative_7_4(const double s){
return -61231.63878*pow_5(s)-7028.644948*s+148502.0490*pow_4(s)-128543.9051*pow_3(s)+47568.10797*pow_2(s)+296.1506383
;
}
double basisFunctionSecondDerivative_7_5(const double s){
return 55685.34359*pow_5(s)+5088.615566*s-127445.8490*pow_4(s)+103720.9916*pow_3(s)-36141.94296*pow_2(s)-207.7069353
;
}
double basisFunctionSecondDerivative_7_6(const double s){
return -43764.52534*pow_5(s)-3476.085020*s+95785.85125*pow_4(s)-74898.00348*pow_3(s)+25273.15894*pow_2(s)+139.7945450
;
}
double basisFunctionSecondDerivative_7_7(const double s){
return 18018.00001*pow_5(s)+1350.000001*s-38610.00002*pow_4(s)+29700.00002*pow_3(s)-9900.000006*pow_2(s)-54.00000004
;
}
double basisFunctionSecondDerivative_8_0(const double s){
return -270270.0001*pow_5(s)-13860.00000*s+360360.0002*pow_4(s)-240240.0001*pow_3(s)+83160.00006*pow_2(s)+80080.00000*pow_6(s)+839.9999998
;
}
double basisFunctionSecondDerivative_8_1(const double s){
return 652345.5336*pow_5(s)+29163.94160*s-856236.4920*pow_4(s)+557783.6648*pow_3(s)-186208.5162*pow_2(s)-195464.6507*pow_6(s)-1563.086510
;
}
double basisFunctionSecondDerivative_8_2(const double s){
return -819195.0132*pow_5(s)-27503.53456*s+1038449.412*pow_4(s)-643521.3656*pow_3(s)+199116.0286*pow_2(s)+251754.4242*pow_6(s)+1161.027617
;
}
double basisFunctionSecondDerivative_8_3(const double s){
return 886914.1692*pow_5(s)+21080.45262*s-1070872.214*pow_4(s)+621065.3832*pow_3(s)-175015.0558*pow_2(s)-282801.7735*pow_6(s)-728.8234795
;
}
double basisFunctionSecondDerivative_8_4(const double s){
return -878592.0000*pow_5(s)-15447.77142*s+1004105.143*pow_4(s)-543890.2860*pow_3(s)+140960.9142*pow_2(s)+292863.9998*pow_6(s)+497.3714282
;
}
double basisFunctionSecondDerivative_8_5(const double s){
return 809896.4718*pow_5(s)+11482.16358*s-878327.9715*pow_4(s)+449317.2544*pow_3(s)-109937.1057*pow_2(s)-282801.7734*pow_6(s)-357.8625503
;
}
double basisFunctionSecondDerivative_8_6(const double s){
return -691331.5308*pow_5(s)-8513.551884*s+718790.7080*pow_4(s)-353414.6340*pow_3(s)+83614.63392*pow_2(s)+251754.4238*pow_6(s)+260.9786022
;
}
double basisFunctionSecondDerivative_8_7(const double s){
return 520442.3717*pow_5(s)+5908.300110*s-526478.5870*pow_4(s)+252999.9840*pow_3(s)-58790.89923*pow_2(s)-195464.6507*pow_6(s)-179.6051089
;
}
double basisFunctionSecondDerivative_8_8(const double s){
return -210210.0002*pow_5(s)-2310.000002*s+210210.0002*pow_4(s)-100100.0000*pow_3(s)+23100.00001*pow_2(s)+80080.00007*pow_6(s)+70.00000004
;
}
double basisFunctionSecondDerivative_9_0(const double s){
return -2162160.001*pow_5(s)-27720.00002*s+1801800.001*pow_4(s)-840840.0004*pow_3(s)+216216.0001*pow_2(s)-350064.0001*pow_7(s)+1361360.001*pow_6(s)+1320.000001
;
}
double basisFunctionSecondDerivative_9_1(const double s){
return 5195843.608*pow_5(s)+58184.43302*s-4263720.478*pow_4(s)+1945056.120*pow_3(s)-482611.2480*pow_2(s)+857390.2520*pow_7(s)-3307465.651*pow_6(s)-2452.682594
;
}
double basisFunctionSecondDerivative_9_2(const double s){
return -6465101.256*pow_5(s)-54667.53522*s+5128720.535*pow_4(s)-2228293.202*pow_3(s)+513199.3695*pow_2(s)-1113622.878*pow_7(s)+4217624.985*pow_6(s)+1819.083138
;
}
double basisFunctionSecondDerivative_9_3(const double s){
return 6922460.196*pow_5(s)+41853.25728*s-5241117.105*pow_4(s)+2136109.783*pow_3(s)-449258.7558*pow_2(s)+1269044.562*pow_7(s)-4677520.218*pow_6(s)-1143.479010
;
}
double basisFunctionSecondDerivative_9_4(const double s){
return -6801475.878*pow_5(s)-30792.88686*s+4889811.628*pow_4(s)-1867591.712*pow_3(s)+362416.3239*pow_2(s)-1343958.078*pow_7(s)+4790236.379*pow_6(s)+783.8034559
;
}
double basisFunctionSecondDerivative_9_5(const double s){
return 6283177.224*pow_5(s)+23156.50610*s-4302554.774*pow_4(s)+1556909.082*pow_3(s)-285821.7356*pow_2(s)+1343958.077*pow_7(s)-4617470.161*pow_6(s)-570.4170033
;
}
double basisFunctionSecondDerivative_9_6(const double s){
return -5507274.724*pow_5(s)-17688.29009*s+3624940.332*pow_4(s)-1262398.689*pow_3(s)+224102.4864*pow_2(s)-1269044.562*pow_7(s)+4205791.729*pow_6(s)+428.2417019
;
}
double basisFunctionSecondDerivative_9_7(const double s){
return 4545431.803*pow_5(s)+13382.80122*s-2909211.738*pow_4(s)+988724.7020*pow_3(s)-172075.2936*pow_2(s)+1113622.878*pow_7(s)-3577735.165*pow_6(s)-320.9037874
;
}
double basisFunctionSecondDerivative_9_8(const double s){
return -3356244.991*pow_5(s)-9404.285496*s+2112171.612*pow_4(s)-707956.0868*pow_3(s)+121880.8535*pow_2(s)-857390.2528*pow_7(s)+2694266.114*pow_6(s)+224.3541010
;
}
double basisFunctionSecondDerivative_9_9(const double s){
return 1345344.002*pow_5(s)+3696.000002*s-840840.0005*pow_4(s)+280280.0003*pow_3(s)-48048.00000*pow_2(s)+350064.0006*pow_7(s)-1089088.002*pow_6(s)-88.00000006
;
}


#endif  

