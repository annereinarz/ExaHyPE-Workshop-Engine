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

#ifndef _EXAHYPE_TESTS_TESTDATA_LIMITER_TESTDATA_H_
#define _EXAHYPE_TESTS_TESTDATA_LIMITER_TESTDATA_H_

namespace exahype {
namespace tests {
namespace testdata {
namespace limiter {

// nVar = 5
// basisSize = 4 (<=> N+1)
// basisSizeLim = 7 (<=> 2N+1)


// Declared in cpp for each dim
extern const int sizeLuh;
extern const int sizeLim;


#ifdef Dim2
namespace testFromLuhConversion {
  extern const double luh_in[80];   // nVar * basisSize * basisSize
  extern const double lob_out[80];  // nVar * basisSize * basisSize  
  extern const double lim_out[245]; // nVar * basisSizeLim * basisSizeLim
}

namespace testToLuhConversion {
  extern const double lim_in[245]; // nVar * basisSizeLim * basisSizeLim
  extern const double luh_out[80]; // nVar * basisSize * basisSize
}

namespace testLocalMinMax {
  extern const double luh_in[80]; // nVar * basisSize * basisSize
  extern const double min_out[5]; // nVar
  extern const double max_out[5]; // nVar
}
#endif // Dim2

#ifdef Dim3
namespace testFromLuhConversion {
  extern const double luh_in[320];   // nVar * basisSize * basisSize * basisSize
  extern const double lob_out[320];  // nVar * basisSize * basisSize * basisSize
  extern const double lim_out[1715]; // nVar * basisSizeLim * basisSizeLim * basisSizeLim
}

namespace testToLuhConversion {
  extern const double lim_in[1715]; // nVar * basisSizeLim * basisSizeLim
  extern const double luh_out[320]; // nVar * basisSize * basisSize
}

namespace testLocalMinMax {
  extern const double luh_in[320]; // nVar * basisSize * basisSize * basisSize
  extern const double min_out[5];  // nVar
  extern const double max_out[5];  // nVar
}
#endif // Dim3


}  // namespace limiter
}  // namespace testdata
}  // namespace tests
}  // namespace exahype

#endif // _EXAHYPE_TESTS_TESTDATA_LIMITER_TESTDATA_H_