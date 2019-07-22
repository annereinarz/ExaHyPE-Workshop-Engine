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

#ifndef _EXAHYPE_TESTS_TESTDATA_ELASTICITY_TESTDATA_H_
#define _EXAHYPE_TESTS_TESTDATA_ELASTICITY_TESTDATA_H_

namespace exahype {
namespace tests {
namespace testdata {
namespace elasticity {

#ifdef Dim2

namespace testSurfaceIntegralLinear {
extern const double lFbnd_IN[4 * 5 * 9];
extern const double lduh_IN[5 * 5 * 9];
extern const double lduh_OUT[5 * 5 * 9];
}  // namespace testSurfaceIntegralLinear

namespace testVolumeIntegralLinear {
extern const double lFhi_IN[2 * 5 * 5 * 9];
extern const double lduh_OUT[5 * 5 * 9];
}  // namespace testVolumeIntegralLinear

namespace testSpaceTimePredictorLinear {
extern const double luh_IN[9 * 5 * 5];
extern const double param_IN[3 * 5 * 5];
// extern const double lQi_OUT[];
// extern const double lFi_OUT[];
extern const double lQhi_OUT[9 * 5 * 5];
extern const double lFhi_OUT[9 * 5 * 5 * 2];
extern const double lQbnd_OUT[9 * 5 * 4];  // 2 * DIMENSIONS = 4
extern const double lFbnd_OUT[9 * 5 * 4];
}  // namespace testSpaceTimePredictorLinear

namespace testRiemannSolverLinear {
extern const double qL_IN[9 * 5];
extern const double qR_IN[9 * 5];
extern const double paramL_IN[3 * 5];
extern const double paramR_IN[3 * 5];
extern const double FL_IN[9 * 5];
extern const double FR_IN[9 * 5];
extern const double FL_OUT[9 * 5];
extern const double FR_OUT[9 * 5];
}  // namespace testRiemannSolverLinear

#endif  // Dim2

}  // namespace elasticity
}  // namespace testdata
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_TESTDATA_ELASTICITY_TESTDATA_H_
