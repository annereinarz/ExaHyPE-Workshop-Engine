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
 
#include "kernels/GaussLegendreQuadrature.h"

double** kernels::gaussLegendreWeights;

double** kernels::gaussLegendreNodes;

void kernels::freeGaussLegendreNodesAndWeights(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    delete [] gaussLegendreNodes[i];
    delete [] gaussLegendreWeights[i];
  }

  delete [] gaussLegendreNodes;
  delete [] gaussLegendreWeights;
}

void kernels::initGaussLegendreNodesAndWeights(const std::set<int>& orders) {
  // @todo The argument is not used yet.
  constexpr int MAX_ORDER=9;

  gaussLegendreNodes = new double* [MAX_ORDER + 1];
  gaussLegendreWeights = new double* [MAX_ORDER + 1];

  for (int i = 0; i < MAX_ORDER + 1; i++) {
    gaussLegendreNodes[i] = new double[i + 1];
    gaussLegendreWeights[i] = new double[i + 1];
  }

  gaussLegendreWeights[0][0] = 1.0000000000000000;
  gaussLegendreNodes[0][0] = 0.5000000000000000;

  gaussLegendreWeights[1][0] = 0.5000000000000000;
  gaussLegendreWeights[1][1] = 0.5000000000000000;
  gaussLegendreNodes[1][0] = 0.2113248654051871;
  gaussLegendreNodes[1][1] = 0.7886751345948129;

  gaussLegendreWeights[2][0] = 0.2777777777777778;
  gaussLegendreWeights[2][1] = 0.4444444444444444;
  gaussLegendreWeights[2][2] = 0.2777777777777778;
  gaussLegendreNodes[2][0] = 0.1127016653792583;
  gaussLegendreNodes[2][1] = 0.5000000000000000;
  gaussLegendreNodes[2][2] = 0.8872983346207417;

  gaussLegendreWeights[3][0] = 0.1739274225687273;
  gaussLegendreWeights[3][1] = 0.3260725774312732;
  gaussLegendreWeights[3][2] = 0.3260725774312732;
  gaussLegendreWeights[3][3] = 0.1739274225687273;
  gaussLegendreNodes[3][0] = 0.0694318442029737;
  gaussLegendreNodes[3][1] = 0.3300094782075719;
  gaussLegendreNodes[3][2] = 0.6699905217924281;
  gaussLegendreNodes[3][3] = 0.9305681557970262;

  gaussLegendreWeights[4][0] = 0.1184634425280948;
  gaussLegendreWeights[4][1] = 0.239314335249683;
  gaussLegendreWeights[4][2] = 0.2844444444444443;
  gaussLegendreWeights[4][3] = 0.239314335249683;
  gaussLegendreWeights[4][4] = 0.1184634425280948;
  gaussLegendreNodes[4][0] = 0.04691007703066802;
  gaussLegendreNodes[4][1] = 0.2307653449471584;
  gaussLegendreNodes[4][2] = 0.5000000000000000;
  gaussLegendreNodes[4][3] = 0.7692346550528415;
  gaussLegendreNodes[4][4] = 0.9530899229693319;

  gaussLegendreWeights[5][0] = 0.0856622461895845;
  gaussLegendreWeights[5][1] = 0.1803807865240695;
  gaussLegendreWeights[5][2] = 0.2339569672863459;
  gaussLegendreWeights[5][3] = 0.2339569672863459;
  gaussLegendreWeights[5][4] = 0.1803807865240695;
  gaussLegendreWeights[5][5] = 0.0856622461895845;
  gaussLegendreNodes[5][0] = 0.03376524289842397;
  gaussLegendreNodes[5][1] = 0.1693953067668678;
  gaussLegendreNodes[5][2] = 0.3806904069584015;
  gaussLegendreNodes[5][3] = 0.6193095930415985;
  gaussLegendreNodes[5][4] = 0.8306046932331322;
  gaussLegendreNodes[5][5] = 0.966234757101576;

  gaussLegendreWeights[6][0] = 0.06474248308443538;
  gaussLegendreWeights[6][1] = 0.1398526957446382;
  gaussLegendreWeights[6][2] = 0.1909150252525592;
  gaussLegendreWeights[6][3] = 0.2089795918367344;
  gaussLegendreWeights[6][4] = 0.1909150252525592;
  gaussLegendreWeights[6][5] = 0.1398526957446382;
  gaussLegendreWeights[6][6] = 0.06474248308443538;
  gaussLegendreNodes[6][0] = 0.02544604382862076;
  gaussLegendreNodes[6][1] = 0.1292344072003028;
  gaussLegendreNodes[6][2] = 0.2970774243113014;
  gaussLegendreNodes[6][3] = 0.5000000000000000;
  gaussLegendreNodes[6][4] = 0.7029225756886985;
  gaussLegendreNodes[6][5] = 0.8707655927996972;
  gaussLegendreNodes[6][6] = 0.9745539561713792;

  gaussLegendreWeights[7][0] = 0.05061426814518821;
  gaussLegendreWeights[7][1] = 0.1111905172266871;
  gaussLegendreWeights[7][2] = 0.1568533229389437;
  gaussLegendreWeights[7][3] = 0.1813418916891809;
  gaussLegendreWeights[7][4] = 0.1813418916891809;
  gaussLegendreWeights[7][5] = 0.1568533229389437;
  gaussLegendreWeights[7][6] = 0.1111905172266871;
  gaussLegendreWeights[7][7] = 0.05061426814518821;
  gaussLegendreNodes[7][0] = 0.01985507175123186;
  gaussLegendreNodes[7][1] = 0.1016667612931866;
  gaussLegendreNodes[7][2] = 0.2372337950418355;
  gaussLegendreNodes[7][3] = 0.4082826787521751;
  gaussLegendreNodes[7][4] = 0.5917173212478249;
  gaussLegendreNodes[7][5] = 0.7627662049581645;
  gaussLegendreNodes[7][6] = 0.8983332387068134;
  gaussLegendreNodes[7][7] = 0.9801449282487682;

  gaussLegendreWeights[8][0] = 0.04063719418078751;
  gaussLegendreWeights[8][1] = 0.09032408034742861;
  gaussLegendreWeights[8][2] = 0.1303053482014677;
  gaussLegendreWeights[8][3] = 0.1561735385200013;
  gaussLegendreWeights[8][4] = 0.1651196775006297;
  gaussLegendreWeights[8][5] = 0.1561735385200013;
  gaussLegendreWeights[8][6] = 0.1303053482014677;
  gaussLegendreWeights[8][7] = 0.09032408034742861;
  gaussLegendreWeights[8][8] = 0.04063719418078751;
  gaussLegendreNodes[8][0] = 0.01591988024618696;
  gaussLegendreNodes[8][1] = 0.08198444633668212;
  gaussLegendreNodes[8][2] = 0.1933142836497048;
  gaussLegendreNodes[8][3] = 0.3378732882980955;
  gaussLegendreNodes[8][4] = 0.5000000000000000;
  gaussLegendreNodes[8][5] = 0.6621267117019045;
  gaussLegendreNodes[8][6] = 0.8066857163502952;
  gaussLegendreNodes[8][7] = 0.9180155536633179;
  gaussLegendreNodes[8][8] = 0.984080119753813;

  gaussLegendreWeights[9][0] = 0.03333567215434358;
  gaussLegendreWeights[9][1] = 0.07472567457529024;
  gaussLegendreWeights[9][2] = 0.1095431812579912;
  gaussLegendreWeights[9][3] = 0.1346333596549983;
  gaussLegendreWeights[9][4] = 0.1477621123573766;
  gaussLegendreWeights[9][5] = 0.1477621123573766;
  gaussLegendreWeights[9][6] = 0.1346333596549983;
  gaussLegendreWeights[9][7] = 0.1095431812579912;
  gaussLegendreWeights[9][8] = 0.07472567457529024;
  gaussLegendreWeights[9][9] = 0.03333567215434358;
  gaussLegendreNodes[9][0] = 0.01304673574141413;
  gaussLegendreNodes[9][1] = 0.06746831665550773;
  gaussLegendreNodes[9][2] = 0.1602952158504878;
  gaussLegendreNodes[9][3] = 0.2833023029353764;
  gaussLegendreNodes[9][4] = 0.4255628305091844;
  gaussLegendreNodes[9][5] = 0.5744371694908156;
  gaussLegendreNodes[9][6] = 0.7166976970646236;
  gaussLegendreNodes[9][7] = 0.8397047841495122;
  gaussLegendreNodes[9][8] = 0.9325316833444923;
  gaussLegendreNodes[9][9] = 0.9869532642585859;
}
