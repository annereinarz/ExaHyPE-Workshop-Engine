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

#ifndef _EXAHYPE_PROFILERS_PROFILER_UTILS_H_
#define _EXAHYPE_PROFILERS_PROFILER_UTILS_H_

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <numeric>
#include <thread>

namespace exahype {
namespace profilers {
namespace utils {

using mean_sec = double;
using median_sec = double;
using std_sec = double;
using min_sec = double;
using max_sec = double;

void escape(void* p);

void clobber();

void noop();

template <int64_t milli>
void sleep() {
  std::this_thread::sleep_for(std::chrono::milliseconds(milli));
}

template <typename type, uint64_t size>
static double mean(const std::array<type, size>& array) {
  double sum = std::accumulate(array.begin(), array.end(), type(0));
  return sum / static_cast<double>(size);
}

template <typename type, uint64_t size>
static double median(const std::array<type, size>& array) {
  std::array<type, size> copy = array;
  std::sort(copy.begin(), copy.end());

  if (size & 1) {  // odd
    return static_cast<double>(copy[size / 2]);
  } else {  // even
    return 0.5 * (copy[size / 2 - 1] + copy[size / 2]);
  }
}

template <typename type, uint64_t size>
static double std(const std::array<type, size>& array, const double mean) {
  return std::sqrt(std::accumulate(array.begin(), array.end(), 0.0,
                                   [mean](const double& sum, const double xi) {
                                     return sum + (xi - mean) * (xi - mean);
                                   }) /
                   (size - 1));
}

template <typename type, uint64_t size>
static double std(const std::array<type, size>& array) {
  return std<type, size>(array, mean<type, size>(array));
}

template <typename type, uint64_t size>
static double meanDuration(const std::array<type, size>& array) {
  double sum = static_cast<std::chrono::duration<double, std::ratio<1>>>(
                   std::accumulate(array.begin(), array.end(), type(0)))
                   .count();
  return sum / size;
}

template <typename type, uint64_t size>
static double medianDuration(const std::array<type, size>& array) {
  std::array<type, size> copy = array;
  std::sort(copy.begin(), copy.end());

  if (size & 1) {  // odd
    return static_cast<std::chrono::duration<double, std::ratio<1>>>(
               copy[size / 2])
        .count();
  } else {  // even
    return 0.5 *
           static_cast<std::chrono::duration<double, std::ratio<1>>>(
               copy[size / 2 - 1] + copy[size / 2])
               .count();
  }
}

template <typename type, uint64_t size>
static double stdDuration(const std::array<type, size>& array,
                          const double mean_sec) {
  return std::sqrt(
      std::accumulate(
          array.begin(), array.end(), 0.0,
          [mean_sec](const double& sum, const type& xi) {
            double xi_sec =
                static_cast<std::chrono::duration<double, std::ratio<1>>>(xi)
                    .count() *
                std::chrono::steady_clock::period::num /
                std::chrono::steady_clock::period::den;
            return sum + (xi_sec - mean_sec) * (xi_sec - mean_sec);
          }) /
      (size - 1));
}

template <typename type, uint64_t size>
static double stdDuration(const std::array<type, size>& array) {
  return std<type, size>(array, mean<type, size>(array));
}

template <void (*f)()>
static std::chrono::steady_clock::duration durationOf() {
  auto start = std::chrono::steady_clock::now();
  f();
  auto stop = std::chrono::steady_clock::now();
  escape(&stop);
  return stop - start;
}

template <uint64_t n, void (*f)()>
static std::array<std::chrono::steady_clock::duration, n> nTimesDurationOf() {
  std::array<std::chrono::steady_clock::duration, n> durations;
  std::generate(durations.begin(), durations.end(), durationOf<f>);
  return durations;
}

template <uint64_t n>
static std::tuple<mean_sec, median_sec, std_sec, min_sec, max_sec>
meanMedianStdMinMaxOfDurations(
    const std::array<std::chrono::steady_clock::duration, n> durations) {
  const double mean_sec =
      meanDuration<std::chrono::steady_clock::duration, n>(durations);
  const double median_sec =
      medianDuration<std::chrono::steady_clock::duration, n>(durations);
  const double std_sec =
      stdDuration<std::chrono::steady_clock::duration, n>(durations, mean_sec);
  const double min_sec =
      static_cast<std::chrono::duration<double, std::ratio<1>>>(
          *std::min_element(durations.begin(), durations.end()))
          .count();
  const double max_sec =
      static_cast<std::chrono::duration<double, std::ratio<1>>>(
          *std::max_element(durations.begin(), durations.end()))
          .count();

  return std::make_tuple(mean_sec, median_sec, std_sec, min_sec, max_sec);
}

}  // namespace utils
}  // namespace profilers
}  // namespace exahype

#endif  // _EXAHYPE_PROFILERS_PROFILER_UTILS_H_
