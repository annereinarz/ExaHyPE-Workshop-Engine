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

#ifndef _EXAHYPE_TESTS_TESTDATA_GENERIC_EULER_TESTDATA_H_
#define _EXAHYPE_TESTS_TESTDATA_GENERIC_EULER_TESTDATA_H_

namespace exahype {
namespace tests {
namespace testdata {
namespace generic_euler {

#ifdef Dim2

namespace testPDEFluxes {
extern const double f[5];
extern const double g[5];
}  // namespace testPDEFluxes

namespace testSolutionUpdate {
extern const double lduh[80];
extern const double luh[80];
}  // namespace testSolutionUpdate

namespace testSurfaceIntegral {
extern const double lduh_in[80];
extern const double lFhbnd_in[80];
}  // namespace testSurfaceIntegral

namespace testSurfaceIntegralLinear {
extern const double lduh_out_1[80];
extern const double lduh_out_2[80];
}  // namespace testSurfaceIntegralLinear

namespace testSurfaceIntegralNonlinear {
extern const double lduh_1[80];
extern const double lduh_2[80];
}  // namespace testSurfaceIntegralNonlinear

namespace testVolumeIntegral {
extern const double lFhi[240];
}  // namespace testVolumeIntegral

namespace testVolumeIntegralLinear {
extern const double lduh_1[80];
extern const double lduh_2[80];
}  // namespace testVolumeIntegralLinear

namespace testVolumeIntegralNonlinear {
extern const double lduh_1[80];
extern const double lduh_2[80];
}  // namespace testVolumeIntegralNonlinear

namespace testSpaceTimePredictorLinear {
extern const double luh[80];
extern const double lFhi[160];
extern const double lQhi[80];
extern const double lQbnd[80];
}  // namespace testSpaceTimePredictorLinear

namespace testSpaceTimePredictorNonlinear {
extern const double luh[80];
extern const double lQi[320];
extern const double lFi[640];
extern const double lQhi[80];
extern const double lFhi[160];
extern const double lQhbnd[80];
extern const double lFhbnd[80];
}  // namespace testSpaceTimePredictorNonlinear

namespace testRiemannSolver {
extern const double QL[20];
extern const double QR[20];
}  // namespace testRiemannSolver

namespace testRiemannSolverLinear {
extern const double FL_1[20];
extern const double FR_1[20];
extern const double FL_2[20];
extern const double FR_2[20];
}  // namespace testRiemannSolverLinear

namespace testRiemannSolverNonlinear {
extern const double QL_1_in[20];
extern const double QR_1_in[20];
extern const double QL_2_in[20];
extern const double QR_2_in[20];

extern const double FL_1_in[20];
extern const double FR_1_in[20];
extern const double FL_2_in[20];
extern const double FR_2_in[20];

extern const double F_1_out[20];
extern const double F_2_out[20];
}  // namespace testRiemannSolverNonlinear

#endif  // Dim2

#ifdef Dim3

namespace testPDEFluxes {
extern const double f[5];
extern const double g[5];
extern const double h[5];
}  // namespace testPDEFluxes

namespace testVolumeIntegralLinear {
extern const double lduh[320];
}  // namespace testVolumeIntegralLinear

namespace testVolumeIntegralNonlinear {
extern const double lduh[320];
}  // namespace testVolumeIntegralNonlinear

namespace testSurfaceIntegralLinear {
extern const double lduh[320];
}  // namespace testSurfaceIntegralLinear

namespace testSurfaceIntegralNonlinear {
extern const double lduh[320];
}  // namesapce testSurfaceIntegral

namespace testRiemannSolver {
extern const double QL[80];
extern const double QR[80];
}  // testRiemannSolver

namespace testRiemannSolverLinear {
extern const double FL[80];
extern const double FR[80];
}  // namespace testRiemannSolverLinear

namespace testRiemannSolverNonlinear {
extern const double FL[80];
extern const double FR[80];
}  // namespace testRiemannSolverNonlinear

namespace testSolutionUpdate {
extern const double luh[320];
}  // namespace testSolutionUpdate

namespace testSpaceTimePredictor {
extern const double luh[320];  // nVar * nDOFx * nDOFy * nDOFz
}  // namespace testSpaceTimePredictor

namespace testSpaceTimePredictorLinear {
extern const double lQhi[320];    // nVar * nDOFx * nDOFy * nDOFz
extern const double lFhi[960];    // nVar * nDOFx * nDOFy * nDOFz * dim
extern const double lQhbnd[480];  // nVar * nDOFy * nDOF_z * 6
extern const double lFhbnd[480];  // nVar * nDOFy * nDOF_z * 6
}  // namespace testSpaceTimePredictorLinear

namespace testSpaceTimePredictorNonlinear {
extern const double luh[320];     // nVar * nDOFx * nDOFy * nDOFz
extern const double lQi[1280];    // nVar * nDOFt * nDOFx * nDOFy * nDOFz
extern const double lFi[3840];    // nVar * nDOFx * nDOFy * nDOFz * nDOFt * dim
extern const double lQhi[320];    // nVar * nDOFx * nDOFy * nDOFz
extern const double lFhi[960];    // nVar * nDOFx * nDOFy * nDOFz * dim
extern const double lQhbnd[480];  // nVar * nDOFy * nDOF_z * 6
extern const double lFhbnd[480];  // nVar * nDOFy * nDOF_z * 6
}  // namespace testSpaceTimePredictorNonlinear

#endif  // Dim3

}  // namespace generic_euler
}  // namespace testdata
}  // namespace tests
}  // namespace exahype

#endif  // _EXAHYPE_TESTS_TESTDATA_GENERIC_EULER_TESTDATA_H_
