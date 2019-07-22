
#include "exahype/plotters/ascii/TimeSeriesReductions.h"

#include "tarch/logging/Log.h"
#include <limits>
#include <cmath>


/**
 * static constexpr need to declared again when following a
 * C++ standard before C++17.
 */
constexpr int exahype::plotters::ascii::TimeSeriesReductions::LEN;

exahype::plotters::ascii::TimeSeriesReductions::TimeSeriesReductions() {
  data[index::tidx] = -1.0; // to start with 0 at first startRow
  // to debug whether strange things happen
  data[index::l1] = 42.123;
}


void exahype::plotters::ascii::TimeSeriesReductions::startRow(double current_time) {
  data[index::tidx] += 1.0;
  data[index::time] = current_time;
  data[index::l1] = 0.0;
  data[index::l2] = 0.0;
  data[index::max] = 0.0;
  data[index::min] = std::numeric_limits<double>::infinity();
  data[index::avg] = 0.0;
  data[index::nelem] = 0.0;
  data[index::nnan] = 0.0;
}


void exahype::plotters::ascii::TimeSeriesReductions::addValue(double val, double dx) {
  // dx is the scaling (volume form) as computed by peano::la::volume,
        // compare the calculation in MyEulerSolver_Plotter1.cpp
  if(std::isfinite(val)) {
    data[index::l1] += std::abs(val) * dx;
    data[index::l2] += val * val * dx;
    data[index::max] = std::max( data[max], val);
    data[index::min] = std::min( data[min], val);
    data[index::avg] += val;
    data[index::nelem] += 1.0; // double, unfortunately
  } else {
    data[index::nnan] += 1.0;
  }
    }


void exahype::plotters::ascii::TimeSeriesReductions::addValues(double input[LEN]) {
  // we don't do a NaN check here as they should be filtered out
  // already at addValue().
  data[index::l1] += input[index::l1];
  data[index::l2] += input[index::l2];
  data[index::max] = std::max( data[index::max], input[index::max] );
  data[index::min] = std::min( data[index::min], input[index::min] );
  data[index::avg] += input[index::avg]; // this is wrong. It should be weighted with input[nelem] vs. data[nelem].
  data[index::nelem] += input[index::nelem];
}

void exahype::plotters::ascii::TimeSeriesReductions::addValues(TimeSeriesReductions& input) {
  addValues(input.data);
}

void exahype::plotters::ascii::TimeSeriesReductions::finishRow() {
  data[index::l2] = std::sqrt(data[index::l2]);
  if(data[index::nelem] >= 1.0) // avoid writing NaNs
    data[index::avg] = data[index::avg] / data[index::nelem];
}

