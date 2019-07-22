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
 *
 * @authors: Dominic E. Charrier
 **/

#ifndef FV_KERNEL_SLOPE_LIMITERS_H_
#define FV_KERNEL_SLOPE_LIMITERS_H_

namespace kernels {
namespace finitevolumes {
namespace commons {
namespace c {
	
	/**
	 * We use Finite volume slope limiters and flux limiters as synomyms.
	 * If you want to implement another limiter, add it here in the enum.
	 * Name the enum option the same as the methods you implement.
	 **/

	
	typedef double (*slope_limiter)(double a, double b);
	
	double minmod(double a, double b);
	double koren(double a, double b);
  double vanalbada(double a, double b);
  double superbee(double a, double b);
  double mclim(double a,double b);

} // ns c
} // ns commons
} // ns finitevolumes
} // ns kernels

#include "slope-limiters.cpph"
	
#endif /* FV_KERNEL_SLOPE_LIMITERS_H_ */
