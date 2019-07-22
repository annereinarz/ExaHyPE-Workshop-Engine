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

#include "exahype/tests/kernels/finitevolumes/riemannsolvers/c/RusanovTest.h"

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "tarch/tests/TestCaseFactory.h"

#include "kernels/finitevolumes/riemannsolvers/c/riemannsolvers.h"

#include <algorithm>

#include "tarch/la/ScalarOperations.h"

#ifndef ALIGNMENT
registerTest(exahype::tests::kernels::finitevolumes::riemannsolvers::c::RusanovTest)
#endif

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", off)
#endif

tarch::logging::Log exahype::tests::kernels::finitevolumes::riemannsolvers::c::RusanovTest::_log( "exahype::tests::c::RusanovTest" );

// ZeroFluxZeroNCP
exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::ZeroFluxZeroNCP() {
  // do nothing
}

exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::~ZeroFluxZeroNCP() {
  // do nothing
}

void
exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::flux(const double* const Q, double** F) {
// do nothing
}

void
exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::viscousFlux(const double* const Q,const double* const gradQ, double** F) {
// do nothing
}

void
exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::eigenvalues(
    const double* const Q,const int normalNonZeroIndex, double* lambda) {
  // always return 1.0
  std::fill_n(lambda, NumberOfVariables, 1.0);
}

void
exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::viscousEigenvalues(
    const double* const Q,const int normalNonZeroIndex, double* lambda) {
  // always return 0.0
  std::fill_n(lambda, NumberOfVariables, 0.0);
}

bool
exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::useNonConservativeProduct() {
  return true;
}

void exahype::tests::kernels::finitevolumes::riemannsolvers::c::ZeroFluxZeroNCP::nonConservativeProduct(
       const double* const Q, const double* const gradQ, double* BgradQ) {
  // do nothing
}


// RusanovTest
exahype::tests::kernels::finitevolumes::riemannsolvers::c::RusanovTest::RusanovTest()
    : tarch::tests::TestCase("exahype::tests::c::RusanovTest") {}

exahype::tests::kernels::finitevolumes::riemannsolvers::c::RusanovTest::~RusanovTest() {}

void exahype::tests::kernels::finitevolumes::riemannsolvers::c::RusanovTest::run() {
  #if defined(Debug) || defined(Asserts)
  _log.info("RusanovTest::run()", "RusanovTest is active");

  // ZeroFluxZeroNCP
  ZeroFluxZeroNCP mockupSolver;
  double qL[ZeroFluxZeroNCP::NumberOfVariables+ZeroFluxZeroNCP::NumberOfParameters] = {0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8};
  double qR[ZeroFluxZeroNCP::NumberOfVariables+ZeroFluxZeroNCP::NumberOfParameters] = {0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2};

  const double output_fL[ZeroFluxZeroNCP::NumberOfVariables] = {0.3, 0.3, 0.3, 0.3};
  const double output_fR[ZeroFluxZeroNCP::NumberOfVariables] = {0.3, 0.3, 0.3, 0.3};

  double fL[ZeroFluxZeroNCP::NumberOfVariables];
  double fR[ZeroFluxZeroNCP::NumberOfVariables];
  double cellSize[2] = {1.0, 1.0};
  double gradQ[ZeroFluxZeroNCP::NumberOfVariables * DIMENSIONS] = {0.0};
  ::kernels::finitevolumes::riemannsolvers::c::rusanov<false,true,false,ZeroFluxZeroNCP>(
      mockupSolver,
      fL,fR,
      qL,
      qR,
      gradQ,
      gradQ,
      cellSize,
      0 // this test is independent of the dimension since non-conservative product and flux are zero
  );
  #endif

  for (int i=0; i<ZeroFluxZeroNCP::NumberOfVariables; i++) {
    assertionNumericalEquals2(fL[i],output_fL[i],fL[i],output_fL[i]);
    assertionNumericalEquals2(fR[i],output_fR[i],fR[i],output_fR[i]);
  }
}

#ifdef UseTestSpecificCompilerSettings
#pragma optimize("", on)
#endif
