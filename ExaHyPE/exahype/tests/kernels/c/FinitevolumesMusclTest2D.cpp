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

#include "exahype/tests/kernels/c/FinitevolumesMusclTest.h"

#include <algorithm>
#include <cassert>
#include <iostream>

#if DIMENSIONS == 2

//#include "kernels/finitevolumes/musclhancock/c/2d/musclhancock.cpph"

namespace exahype {
namespace tests {
namespace c {

namespace {
// Will be set inside testSolutionUpdate
static double a;  // linear advection coefficient (x direction)
static double b;  // linear advection coefficient (y direction)
}  // namespace

void FinitevolumesMusclTest::testFlux(const double *const Q, double **F) {
  double *f = F[0];
  double *g = F[1];

  f[0] = a * Q[0];
  f[1] = a * Q[1];
  f[2] = a * Q[1];
  f[3] = a * Q[1];
  f[4] = a * Q[1];

  g[0] = b * Q[0];
  g[1] = b * Q[1];
  g[2] = b * Q[1];
  g[3] = b * Q[1];
  g[4] = b * Q[1];
}

void FinitevolumesMusclTest::testSource(const double *const Q, double *S) {
  S[0] = 0.0;
  S[1] = 0.0;
  S[2] = 0.0;
  S[3] = 0.0;
  S[4] = 0.0;
}

void FinitevolumesMusclTest::testEigenvalues(const double *const Q,
                                             const int normalNonZeroIndex,
                                             double *lambda) {
  switch (normalNonZeroIndex) {
    case 0:
      std::fill(lambda, lambda + 5, a);
      break;
    case 1:
      std::fill(lambda, lambda + 5, b);
      break;
    default:
      assert(false);
  }
}

void FinitevolumesMusclTest::testSolutionUpdate() {
  logInfo("testSolutionUpdate()", "Test FVM MUSCL solutionUpdate");

//  // linear advection x
//  {
//    a = 1.23;
//    b = 0.0;
//    const int basisSize = 4;  // 4 points per dimension in cell
//    const double dt = 0.234;
//    const double cfl = 1.0;
//    const double dx_scalar = basisSize * a * dt / cfl;
//    const tarch::la::Vector<DIMENSIONS, double> dx(dx_scalar, dx_scalar);
//
//    const int basisSize2 = basisSize * basisSize;
//    const int numberOfVariables = 5;
//    double *luh[3 * 3];  // 9 cells
//    for (int i = 0; i < 3 * 3; i++) {
//      luh[i] = new double[basisSize2 * numberOfVariables];
//    }
//
//    // initialize (lower half -1.0, upper half +1.0, center cell middle 0.0)
//    for (int i = 0; i < 3; i++) {
//      for (int j = 0; j < 3; j++) {
//        if (j == 0) {
//          // set complete cell to -1.0
//          std::fill(luh[i * 3 + j],
//                    luh[i * 3 + j] + basisSize2 * numberOfVariables, -1.0);
//        } else if (j == 2) {
//          // set complete cell to +1.0
//          std::fill(luh[i * 3 + j],
//                    luh[i * 3 + j] + basisSize2 * numberOfVariables, +1.0);
//        } else {  // j == 1 (middle)
//          for (int ii = 0; ii < basisSize; ii++) {
//            for (int jj = 0; jj < basisSize; jj++) {
//              double value;
//              if (jj < 1) {
//                value = -1.0;
//              } else if (jj < 2) {
//                value = 0.0;
//              } else {
//                value = +1.0;
//              }
//              for (int l = 0; l < numberOfVariables; l++) {
//                luh[i * 3 + j][ii * basisSize * numberOfVariables +
//                               jj * numberOfVariables + l] = value;
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // do one time step
//    kernels::finitevolumes::muscl::c::solutionUpdate<testFlux, testSource,
//                                                     testEigenvalues>(
//        luh, dx, dt, numberOfVariables, basisSize);
//
//    // compute reference solution (shift by one in x direction)
//    double *luh_expected[3 * 3];  // 9 cells
//    for (int i = 0; i < 3 * 3; i++) {
//      luh_expected[i] = new double[basisSize2 * numberOfVariables];
//    }
//
//    for (int i = 0; i < 3; i++) {
//      for (int j = 0; j < 3; j++) {
//        if (j == 0) {
//          // set complete cell to -1.0
//          std::fill(luh_expected[i * 3 + j],
//                    luh_expected[i * 3 + j] + basisSize2 * numberOfVariables,
//                    -1.0);
//        } else if (j == 2) {
//          // set complete cell to +1.0
//          std::fill(luh_expected[i * 3 + j],
//                    luh_expected[i * 3 + j] + basisSize2 * numberOfVariables,
//                    +1.0);
//        } else {         // j == 1 (middle)
//          if (i == 1) {  // center cell (evolved cell)
//            for (int ii = 0; ii < basisSize; ii++) {
//              for (int jj = 0; jj < basisSize; jj++) {
//                double value;
//                if (jj < 2) {
//                  value = -1.0;
//                } else if (jj < 3) {
//                  value = 0.0;
//                } else {
//                  value = +1.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[i * 3 + j][ii * basisSize * numberOfVariables +
//                                          jj * numberOfVariables + l] = value;
//                }
//              }
//            }
//          } else {
//            for (int ii = 0; ii < basisSize; ii++) {
//              for (int jj = 0; jj < basisSize; jj++) {
//                double value;
//                if (jj < 1) {
//                  value = -1.0;
//                } else if (jj < 2) {
//                  value = 0.0;
//                } else {
//                  value = +1.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[i * 3 + j][ii * basisSize * numberOfVariables +
//                                          jj * numberOfVariables + l] = value;
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // compare
//    for (int i = 0; i < 3; i++) {
//      for (int j = 0; j < 3; j++) {
//        for (int ii = 0; ii < basisSize; ii++) {
//          for (int jj = 0; jj < basisSize; jj++) {
//            for (int l = 0; l < numberOfVariables; l++) {
//              validateNumericalEqualsWithParams4(
//                  luh[i * 3 + j][ii * basisSize * numberOfVariables +
//                                 jj * numberOfVariables + l],
//                  luh_expected[i * 3 + j][ii * basisSize * numberOfVariables +
//                                          jj * numberOfVariables + l],
//                  i * 3 + j, ii, jj, l);
//            }
//          }
//        }
//      }
//    }
//
//    // do one time step
//    kernels::finitevolumes::muscl::c::solutionUpdate<testFlux, testSource,
//                                                     testEigenvalues>(
//        luh, dx, dt, numberOfVariables, basisSize);
//
//    // compute reference solution (shift by two in x direction)
//    for (int i = 0; i < 3; i++) {
//      for (int j = 0; j < 3; j++) {
//        if (j == 0) {
//          // set complete cell to -1.0
//          std::fill(luh_expected[i * 3 + j],
//                    luh_expected[i * 3 + j] + basisSize2 * numberOfVariables,
//                    -1.0);
//        } else if (j == 2) {
//          // set complete cell to +1.0
//          std::fill(luh_expected[i * 3 + j],
//                    luh_expected[i * 3 + j] + basisSize2 * numberOfVariables,
//                    +1.0);
//        } else {         // j == 1 (middle)
//          if (i == 1) {  // center cell (evolved cell)
//            for (int ii = 0; ii < basisSize; ii++) {
//              for (int jj = 0; jj < basisSize; jj++) {
//                double value;
//                if (jj < 3) {
//                  value = -1.0;
//                } else {
//                  value = 0.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[i * 3 + j][ii * basisSize * numberOfVariables +
//                                          jj * numberOfVariables + l] = value;
//                }
//              }
//            }
//          } else {
//            for (int ii = 0; ii < basisSize; ii++) {
//              for (int jj = 0; jj < basisSize; jj++) {
//                double value;
//                if (jj < 1) {
//                  value = -1.0;
//                } else if (jj < 2) {
//                  value = 0.0;
//                } else {
//                  value = +1.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[i * 3 + j][ii * basisSize * numberOfVariables +
//                                          jj * numberOfVariables + l] = value;
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // compare
//    for (int i = 0; i < 3; i++) {
//      for (int j = 0; j < 3; j++) {
//        for (int ii = 0; ii < basisSize; ii++) {
//          for (int jj = 0; jj < basisSize; jj++) {
//            for (int l = 0; l < numberOfVariables; l++) {
//              validateNumericalEqualsWithParams4(
//                  luh[i * 3 + j][ii * basisSize * numberOfVariables +
//                                 jj * numberOfVariables + l],
//                  luh_expected[i * 3 + j][ii * basisSize * numberOfVariables +
//                                          jj * numberOfVariables + l],
//                  i * 3 + j, ii, jj, l);
//            }
//          }
//        }
//      }
//    }
//
//    // cleanup
//    for (int i = 0; i < 3 * 3; i++) {
//      delete[] luh_expected[i];
//    }
//
//    for (int i = 0; i < 3 * 3; i++) {
//      delete[] luh[i];
//    }
//  }
//
//  // linear advection y
//  {
//    a = 0.0;
//    b = 5.43;
//    const int basisSize = 4;  // 4 points per dimension in cell
//    const double dt = 0.345;
//    const double cfl = 1.0;
//    const double dx_scalar = basisSize * b * dt / cfl;
//    const tarch::la::Vector<DIMENSIONS, double> dx(dx_scalar, dx_scalar);
//
//    const int basisSize2 = basisSize * basisSize;
//    const int numberOfVariables = 5;
//    double *luh[3 * 3];  // 9 cells
//    for (int i = 0; i < 3 * 3; i++) {
//      luh[i] = new double[basisSize2 * numberOfVariables];
//    }
//
//    // initialize (left half -1.0, right half +1.0, center cell middle 0.0)
//    for (int j = 0; j < 3; j++) {
//      for (int k = 0; k < 3; k++) {
//        if (j == 0) {
//          // set complete cell to -1.0
//          std::fill(luh[j * 3 + k],
//                    luh[j * 3 + k] + basisSize2 * numberOfVariables, -1.0);
//        } else if (j == 2) {
//          // set complete cell to +1.0
//          std::fill(luh[j * 3 + k],
//                    luh[j * 3 + k] + basisSize2 * numberOfVariables, +1.0);
//        } else {  // j == 1 (middle)
//          for (int jj = 0; jj < basisSize; jj++) {
//            for (int kk = 0; kk < basisSize; kk++) {
//              double value;
//              if (jj < 1) {
//                value = -1.0;
//              } else if (jj < 2) {
//                value = 0.0;
//              } else {
//                value = +1.0;
//              }
//              for (int l = 0; l < numberOfVariables; l++) {
//                luh[j * 3 + k][jj * basisSize * numberOfVariables +
//                               kk * numberOfVariables + l] = value;
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // do time step
//    kernels::finitevolumes::muscl::c::solutionUpdate<testFlux, testSource,
//                                                     testEigenvalues>(
//        luh, dx, dt, numberOfVariables, basisSize);
//
//    // check
//    double *luh_expected[3 * 3];  // 9 cells
//    for (int i = 0; i < 3 * 3; i++) {
//      luh_expected[i] = new double[basisSize2 * numberOfVariables];
//    }
//    for (int j = 0; j < 3; j++) {
//      for (int k = 0; k < 3; k++) {
//        if (j == 0) {
//          // set complete cell to -1.0
//          std::fill(luh_expected[j * 3 + k],
//                    luh_expected[j * 3 + k] + basisSize2 * numberOfVariables,
//                    -1.0);
//        } else if (j == 2) {
//          // set complete cell to +1.0
//          std::fill(luh_expected[j * 3 + k],
//                    luh_expected[j * 3 + k] + basisSize2 * numberOfVariables,
//                    +1.0);
//        } else {         // j == 1 (middle)
//          if (k == 1) {  // center cell (evolved cell)
//            for (int jj = 0; jj < basisSize; jj++) {
//              for (int kk = 0; kk < basisSize; kk++) {
//                double value;
//                if (jj < 2) {
//                  value = -1.0;
//                } else if (jj < 3) {
//                  value = 0.0;
//                } else {
//                  value = +1.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[j * 3 + k][jj * basisSize * numberOfVariables +
//                                          kk * numberOfVariables + l] = value;
//                }
//              }
//            }
//          } else {
//            for (int jj = 0; jj < basisSize; jj++) {
//              for (int kk = 0; kk < basisSize; kk++) {
//                double value;
//                if (jj < 1) {
//                  value = -1.0;
//                } else if (jj < 2) {
//                  value = 0.0;
//                } else {
//                  value = +1.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[j * 3 + k][jj * basisSize * numberOfVariables +
//                                          kk * numberOfVariables + l] = value;
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // compare
//    for (int j = 0; j < 3; j++) {
//      for (int k = 0; k < 3; k++) {
//        for (int ii = 0; ii < basisSize; ii++) {
//          for (int jj = 0; jj < basisSize; jj++) {
//            for (int kk = 0; kk < basisSize; kk++) {
//              for (int l = 0; l < numberOfVariables; l++) {
//                validateNumericalEqualsWithParams4(
//                    luh[j * 3 + k][jj * basisSize * numberOfVariables +
//                                   kk * numberOfVariables + l],
//                    luh_expected[j * 3 + k][jj * basisSize * numberOfVariables +
//                                            kk * numberOfVariables + l],
//                    j * 3 + k, jj, kk, l);
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // do second time step
//    kernels::finitevolumes::muscl::c::solutionUpdate<testFlux, testSource,
//                                                     testEigenvalues>(
//        luh, dx, dt, numberOfVariables, basisSize);
//
//    // initialize reference solution (shifted by two in y direction)
//    for (int j = 0; j < 3; j++) {
//      for (int k = 0; k < 3; k++) {
//        if (j == 0) {
//          // set complete cell to -1.0
//          std::fill(luh_expected[j * 3 + k],
//                    luh_expected[j * 3 + k] + basisSize2 * numberOfVariables,
//                    -1.0);
//        } else if (j == 2) {
//          // set complete cell to +1.0
//          std::fill(luh_expected[j * 3 + k],
//                    luh_expected[j * 3 + k] + basisSize2 * numberOfVariables,
//                    +1.0);
//        } else {         // j == 1 (middle)
//          if (k == 1) {  // center cell (evolved cell)
//            for (int jj = 0; jj < basisSize; jj++) {
//              for (int kk = 0; kk < basisSize; kk++) {
//                double value;
//                if (jj < 3) {
//                  value = -1.0;
//                } else {
//                  value = 0.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[j * 3 + k][jj * basisSize * numberOfVariables +
//                                          kk * numberOfVariables + l] = value;
//                }
//              }
//            }
//          } else {
//            for (int jj = 0; jj < basisSize; jj++) {
//              for (int kk = 0; kk < basisSize; kk++) {
//                double value;
//                if (jj < 1) {
//                  value = -1.0;
//                } else if (jj < 2) {
//                  value = 0.0;
//                } else {
//                  value = +1.0;
//                }
//                for (int l = 0; l < numberOfVariables; l++) {
//                  luh_expected[j * 3 + k][jj * basisSize * numberOfVariables +
//                                          kk * numberOfVariables + l] = value;
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//
//    // compare
//    for (int j = 0; j < 3; j++) {
//      for (int k = 0; k < 3; k++) {
//        for (int jj = 0; jj < basisSize; jj++) {
//          for (int kk = 0; kk < basisSize; kk++) {
//            for (int l = 0; l < numberOfVariables; l++) {
//              validateNumericalEqualsWithParams4(
//                  luh[j * 3 + k][jj * basisSize * numberOfVariables +
//                                 kk * numberOfVariables + l],
//                  luh_expected[j * 3 + k][jj * basisSize * numberOfVariables +
//                                          kk * numberOfVariables + l],
//                  j * 3 + k, jj, kk, l);
//            }
//          }
//        }
//      }
//    }
//
//    // cleanup
//    for (int i = 0; i < 3 * 3; i++) {
//      delete[] luh_expected[i];
//    }
//
//    for (int i = 0; i < 3 * 3; i++) {
//      delete[] luh[i];
//    }
//  }
}

}  // namespace c
}  // namespace tests
}  // namespace exahype

#endif  // DIMENSIONS == 2
