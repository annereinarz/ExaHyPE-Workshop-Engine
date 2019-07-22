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

#include "exahype/tests/kernels/c/LimiterKernelTest.h"

// TODO(Dominic): JM, please fix the segfaults before you register
// the test again.

#ifndef ALIGNMENT
registerTest(exahype::tests::c::LimiterKernelTest)
#endif

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", off)
#endif

tarch::logging::Log exahype::tests::c::LimiterKernelTest::_log( "exahype::tests::c::LimiterKernelTest" );

namespace exahype {
namespace tests {
namespace c {

const double LimiterKernelTest::eps = 1.0e-13;
const int LimiterKernelTest::numberOfVariables = 5;
const int LimiterKernelTest::basisSize = 4;
const int LimiterKernelTest::basisSizeLim = 7;
#ifdef Dim2
const std::string LimiterKernelTest::dim = "2";
#endif
#ifdef Dim3
const std::string LimiterKernelTest::dim = "3";
#endif

LimiterKernelTest::LimiterKernelTest()
    : tarch::tests::TestCase("exahype::tests::c::LimiterKernelTest") {}

LimiterKernelTest::~LimiterKernelTest() {}



void LimiterKernelTest::run() {
  _log.info("LimiterKernelTest::run()", "LimiterKernelTest is active");
  
  testMethod(testGetGaussLobattoData);  
  testMethod(testGetFVMData);
  testMethod(testUpdateSubcellWithLimiterData);
  //testMethod(testFindCellLocallocalMinlocalMax)
  //testMethod(testIsTroubledCell)
}

void LimiterKernelTest::testGetGaussLobattoData() {
  logInfo("LimiterKernelTest::testGetGaussLobattoData()",
          "Test luh -> lob, ORDER=4, DIM="+dim);

  // TODO(Dominic): Assess

//  double* lob = kernels::limiter::generic::c::getGaussLobattoData(exahype::tests::testdata::limiter::testFromLuhConversion::luh_in, numberOfVariables, basisSize);
//
//  for(int i=0; i<exahype::tests::testdata::limiter::sizeLuh; i++) {
//    validateNumericalEqualsWithEps(lob[i], exahype::tests::testdata::limiter::testFromLuhConversion::lob_out[i], eps);
//  }

//  delete[] lob;
}

void LimiterKernelTest::testGetFVMData() {
  logInfo("LimiterKernelTest::testGetFVMData()",
          "Test luh -> lim, ORDER=4, DIM="+dim);

  // TODO(Dominic): Assess

//  const int calcBasisSizeLim = kernels::limiter::generic::c::getBasisSizeLim(basisSize);
//#ifdef Dim2
//  const int size = calcBasisSizeLim*calcBasisSizeLim*numberOfVariables;
//#endif
//#ifdef Dim3
//  const int size = calcBasisSizeLim*calcBasisSizeLim*calcBasisSizeLim*numberOfVariables;
//#endif
//  double* lim = new double[size];
//
//  kernels::limiter::generic::c::projectOnFVLimiterSpace(exahype::tests::testdata::limiter::testFromLuhConversion::luh_in, numberOfVariables, basisSize, calcBasisSizeLim, lim);
//
//  validateEquals(calcBasisSizeLim, basisSizeLim)
//  for(int i=0; i<exahype::tests::testdata::limiter::sizeLim; i++) {
//    validateNumericalEqualsWithEps(lim[i], exahype::tests::testdata::limiter::testFromLuhConversion::lim_out[i], eps);
//  }
//
//  delete[] lim;
}

void LimiterKernelTest::testUpdateSubcellWithLimiterData(){
  logInfo("LimiterKernelTest::testUpdateSubcellWithLimiterData()",
          "Test lim -> luh, ORDER=4, DIM="+dim);

  // TODO(Dominic): Assess

//  double* luh = new double[exahype::tests::testdata::limiter::sizeLuh]();
//  kernels::limiter::generic::c::projectOnADERDGSpace(exahype::tests::testdata::limiter::testToLuhConversion::lim_in, numberOfVariables, basisSizeLim, basisSize, luh);
//
//  for(int i=0; i<exahype::tests::testdata::limiter::sizeLuh; i++) {
//    validateNumericalEqualsWithEps(luh[i], exahype::tests::testdata::limiter::testToLuhConversion::luh_out[i], eps);
//  }
  
//  delete[] luh;
}

void LimiterKernelTest::testFindCellLocallocalMinlocalMax(){
  
}

void LimiterKernelTest::testIsTroubledCell(){
  
}

}  // namespace c
}  // namespace tests
}  // namespace exahype

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", on)
#endif

