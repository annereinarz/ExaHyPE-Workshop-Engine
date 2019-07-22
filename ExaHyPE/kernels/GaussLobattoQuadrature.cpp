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
 
#include "kernels/GaussLobattoQuadrature.h"

double** kernels::gaussLobattoWeights;

double** kernels::gaussLobattoNodes;

void kernels::freeGaussLobattoNodesAndWeights(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    delete [] gaussLobattoNodes[i];
    delete [] gaussLobattoWeights[i];
  }

  delete [] gaussLobattoNodes;
  delete [] gaussLobattoWeights;
}

void kernels::initGaussLobattoNodesAndWeights(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  gaussLobattoNodes = new double* [MAX_ORDER + 1];
  gaussLobattoWeights = new double* [MAX_ORDER + 1];

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    gaussLobattoNodes[i] = new double[i + 1];
    gaussLobattoWeights[i] = new double[i + 1];
  }

  //Gauss-Lobatto isn't defined for n=1, using Gauss-Legendre instead
  gaussLobattoWeights[0][0] = 1;   //should not be used
  gaussLobattoNodes  [0][0] = 0.5; //should not be used

  gaussLobattoWeights[1][0] = 0.5;
  gaussLobattoWeights[1][1] = 0.5;
  gaussLobattoNodes  [1][0] = 1;
  gaussLobattoNodes  [1][1] = 0;

  gaussLobattoWeights[2][0] = 0.1666666666666667;
  gaussLobattoWeights[2][1] = 0.6666666666666666;
  gaussLobattoWeights[2][2] = 0.1666666666666667;
  gaussLobattoNodes  [2][0] = 1;
  gaussLobattoNodes  [2][1] = 0.5;
  gaussLobattoNodes  [2][2] = 0;

  gaussLobattoWeights[3][0] = 0.08333333333333333;
  gaussLobattoWeights[3][1] = 0.4166666666666667;
  gaussLobattoWeights[3][2] = 0.4166666666666667;
  gaussLobattoWeights[3][3] = 0.08333333333333333;
  gaussLobattoNodes  [3][0] = 1;
  gaussLobattoNodes  [3][1] = 0.7236067977499789;
  gaussLobattoNodes  [3][2] = 0.2763932022500211;
  gaussLobattoNodes  [3][3] = 0;

  gaussLobattoWeights[4][0] = 0.05;
  gaussLobattoWeights[4][1] = 0.2722222222222221;
  gaussLobattoWeights[4][2] = 0.3555555555555556;
  gaussLobattoWeights[4][3] = 0.2722222222222221;
  gaussLobattoWeights[4][4] = 0.05;
  gaussLobattoNodes  [4][0] = 1;
  gaussLobattoNodes  [4][1] = 0.8273268353539885;
  gaussLobattoNodes  [4][2] = 0.5;
  gaussLobattoNodes  [4][3] = 0.1726731646460115;
  gaussLobattoNodes  [4][4] = 0;

  gaussLobattoWeights[5][0] = 0.03333333333333333;
  gaussLobattoWeights[5][1] = 0.1892374781489235;
  gaussLobattoWeights[5][2] = 0.2774291885177432;
  gaussLobattoWeights[5][3] = 0.2774291885177432;
  gaussLobattoWeights[5][4] = 0.1892374781489235;
  gaussLobattoWeights[5][5] = 0.03333333333333333;
  gaussLobattoNodes  [5][0] = 1;
  gaussLobattoNodes  [5][1] = 0.8825276619647324;
  gaussLobattoNodes  [5][2] = 0.6426157582403226;
  gaussLobattoNodes  [5][3] = 0.3573842417596774;
  gaussLobattoNodes  [5][4] = 0.1174723380352676;
  gaussLobattoNodes  [5][5] = 0;

  gaussLobattoWeights[6][0] = 0.02380952380952381;
  gaussLobattoWeights[6][1] = 0.138413023680783;
  gaussLobattoWeights[6][2] = 0.2158726906049313;
  gaussLobattoWeights[6][3] = 0.2438095238095238;
  gaussLobattoWeights[6][4] = 0.2158726906049313;
  gaussLobattoWeights[6][5] = 0.138413023680783;
  gaussLobattoWeights[6][6] = 0.02380952380952381;
  gaussLobattoNodes  [6][0] = 1;
  gaussLobattoNodes  [6][1] = 0.9151119481392835;
  gaussLobattoNodes  [6][2] = 0.7344243967353571;
  gaussLobattoNodes  [6][3] = 0.5;
  gaussLobattoNodes  [6][4] = 0.2655756032646429;
  gaussLobattoNodes  [6][5] = 0.08488805186071652;
  gaussLobattoNodes  [6][6] = 0;

  gaussLobattoWeights[7][0] = 0.01785714285714286;
  gaussLobattoWeights[7][1] = 0.1053521135717531;
  gaussLobattoWeights[7][2] = 0.1705613462417522;
  gaussLobattoWeights[7][3] = 0.2062293973293519;
  gaussLobattoWeights[7][4] = 0.2062293973293519;
  gaussLobattoWeights[7][5] = 0.1705613462417522;
  gaussLobattoWeights[7][6] = 0.1053521135717531;
  gaussLobattoWeights[7][7] = 0.01785714285714286;
  gaussLobattoNodes  [7][0] = 1;
  gaussLobattoNodes  [7][1] = 0.9358700742548033;
  gaussLobattoNodes  [7][2] = 0.7958500907165711;
  gaussLobattoNodes  [7][3] = 0.6046496089512394;
  gaussLobattoNodes  [7][4] = 0.3953503910487606;
  gaussLobattoNodes  [7][5] = 0.2041499092834289;
  gaussLobattoNodes  [7][6] = 0.06412992574519671;
  gaussLobattoNodes  [7][7] = 0;

  gaussLobattoWeights[8][0] = 0.01388888888888889;
  gaussLobattoWeights[8][1] = 0.08274768078040276;
  gaussLobattoWeights[8][2] = 0.1372693562500808;
  gaussLobattoWeights[8][3] = 0.1732142554865232;
  gaussLobattoWeights[8][4] = 0.1857596371882086;
  gaussLobattoWeights[8][5] = 0.1732142554865232;
  gaussLobattoWeights[8][6] = 0.1372693562500808;
  gaussLobattoWeights[8][7] = 0.08274768078040276;
  gaussLobattoWeights[8][8] = 0.01388888888888889;
  gaussLobattoNodes  [8][0] = 1;
  gaussLobattoNodes  [8][1] = 0.94987899770573;
  gaussLobattoNodes  [8][2] = 0.8385931397553689;
  gaussLobattoNodes  [8][3] = 0.6815587319130891;
  gaussLobattoNodes  [8][4] = 0.5;
  gaussLobattoNodes  [8][5] = 0.3184412680869109;
  gaussLobattoNodes  [8][6] = 0.1614068602446311;
  gaussLobattoNodes  [8][7] = 0.05012100229426991;
  gaussLobattoNodes  [8][8] = 0;

  gaussLobattoWeights[9][0] = 0.01111111111111111;
  gaussLobattoWeights[9][1] = 0.06665299542553503;
  gaussLobattoWeights[9][2] = 0.1124446710315632;
  gaussLobattoWeights[9][3] = 0.1460213418398419;
  gaussLobattoWeights[9][4] = 0.1637698805919487;
  gaussLobattoWeights[9][5] = 0.1637698805919487;
  gaussLobattoWeights[9][6] = 0.1460213418398419;
  gaussLobattoWeights[9][7] = 0.1124446710315632;
  gaussLobattoWeights[9][8] = 0.06665299542553503;
  gaussLobattoWeights[9][9] = 0.01111111111111111;
  gaussLobattoNodes  [9][0] = 1;
  gaussLobattoNodes  [9][1] = 0.9597669540832294;
  gaussLobattoNodes  [9][2] = 0.8693869325527526;
  gaussLobattoNodes  [9][3] = 0.7389624749052223;
  gaussLobattoNodes  [9][4] = 0.5826394788331934;
  gaussLobattoNodes  [9][5] = 0.4173605211668065;
  gaussLobattoNodes  [9][6] = 0.2610375250947777;
  gaussLobattoNodes  [9][7] = 0.1306130674472474;
  gaussLobattoNodes  [9][8] = 0.04023304591677057;
  gaussLobattoNodes  [9][9] = 0;

}
