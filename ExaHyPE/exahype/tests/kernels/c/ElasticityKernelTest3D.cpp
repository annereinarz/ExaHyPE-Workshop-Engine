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

#include "exahype/tests/kernels/c/ElasticityKernelTest.h"

#include "../testdata/elasticity_testdata.h"
#include "kernels/KernelUtils.h"
#include "kernels/aderdg/generic/Kernels.h"

#if DIMENSIONS == 3

namespace exahype {
namespace tests {
namespace c {

// This test does not exist.

void ElasticityKernelTest::testRiemannSolverLinear() {}

void ElasticityKernelTest::testSpaceTimePredictorLinear() {}

void ElasticityKernelTest::testVolumeIntegralLinear() {}

void ElasticityKernelTest::testSurfaceIntegralLinear() {}

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // DIMENSIONS==3
