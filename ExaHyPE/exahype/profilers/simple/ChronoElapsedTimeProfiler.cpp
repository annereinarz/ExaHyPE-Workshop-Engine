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

#include "ChronoElapsedTimeProfiler.h"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <thread>

#include "../ProfilerUtils.h"

namespace {
using namespace exahype::profilers::utils;

const uint64_t kNumberOfSamples1 = 1000000;
const uint64_t kNumberOfSamples2 = 10;

std::unordered_map<std::string,
                   std::chrono::time_point<clockType>>
    overhead_time_points;

void initOverheadMeasurementMap() {
  static const int kNumberOfKeys = 15;
  std::array<std::string, kNumberOfKeys> keys = {"boundaryConditions",
                                                 "volumeUnknownsRestriction",
                                                 "riemannSolver",
                                                 "volumeIntegral",
                                                 "surfaceIntegral",
                                                 "solutionUpdate",
                                                 "spaceTimePredictor",
                                                 "spaceTimePredictor_PDEflux",
                                                 "spaceTimePredictor_PDEsource",
                                                 "spaceTimePredictor_PDEncp",
                                                 "stableTimeStepSize",
                                                 "faceUnknownsRestriction",
                                                 "solutionAdjustment",
                                                 "faceUnknownsProlongation",
                                                 "volumeUnknownsProlongation"};
  overhead_time_points.reserve(kNumberOfKeys);
  std::for_each(keys.begin(), keys.end(),
                [](const std::string& key) { overhead_time_points[key]; });
}

void storeInMap() {
  overhead_time_points["solutionUpdate"] = clockType::now();
  escape(&overhead_time_points);
}

void getCurrentTime() {
  auto now = clockType::now();
  escape(&now);
}

//static void estimateOverhead() {
//  std::cout << "clock::period = "
//            << static_cast<double>(clockType::period::num) /
//                   clockType::period::den
//            << "sec" << std::endl;
//
//  // noop
//  std::tuple<mean_sec, median_sec, std_sec, min_sec, max_sec> duration_noop =
//      meanMedianStdMinMaxOfDurations<kNumberOfSamples1>(
//          nTimesDurationOf<kNumberOfSamples1, &noop>());
//  std::cout << "duration_noop" << std::endl;
//  std::cout << "  mean_sec = " << std::get<0>(duration_noop) << std::endl;
//  std::cout << "  median_sec = " << std::get<1>(duration_noop) << std::endl;
//  std::cout << "  std_sec = " << std::get<2>(duration_noop) << std::endl;
//  std::cout << "  min_sec = " << std::get<3>(duration_noop) << std::endl;
//  std::cout << "  max_sec = " << std::get<4>(duration_noop) << std::endl;
//
//  // storeInMap
//  initOverheadMeasurementMap();
//  std::tuple<mean_sec, median_sec, std_sec, min_sec, max_sec>
//      duration_storeInMap = meanMedianStdMinMaxOfDurations<kNumberOfSamples1>(
//          nTimesDurationOf<kNumberOfSamples1, &storeInMap>());
//  std::cout << "duration_storeInMap" << std::endl;
//  std::cout << "  mean_sec = " << std::get<0>(duration_storeInMap) << std::endl;
//  std::cout << "  median_sec = " << std::get<1>(duration_storeInMap)
//            << std::endl;
//  std::cout << "  std_sec = " << std::get<2>(duration_storeInMap) << std::endl;
//  std::cout << "  min_sec = " << std::get<3>(duration_storeInMap) << std::endl;
//  std::cout << "  max_sec = " << std::get<4>(duration_storeInMap) << std::endl;
//
//  // getCurrentTime
//  std::tuple<mean_sec, median_sec, std_sec, min_sec, max_sec>
//      duration_getCurrentTime =
//          meanMedianStdMinMaxOfDurations<kNumberOfSamples1>(
//              nTimesDurationOf<kNumberOfSamples1, &getCurrentTime>());
//  std::cout << "getCurrentTime" << std::endl;
//  std::cout << "  mean_sec = " << std::get<0>(duration_getCurrentTime)
//            << std::endl;
//  std::cout << "  median_sec = " << std::get<1>(duration_getCurrentTime)
//            << std::endl;
//  std::cout << "  std_sec = " << std::get<2>(duration_getCurrentTime)
//            << std::endl;
//  std::cout << "  min_sec = " << std::get<3>(duration_getCurrentTime)
//            << std::endl;
//  std::cout << "  max_sec = " << std::get<4>(duration_getCurrentTime)
//            << std::endl;
//}

}  // namespace

namespace exahype {
namespace profilers {
namespace simple {

ChronoElapsedTimeProfiler::ChronoElapsedTimeProfiler(const std::string& output)
    : Profiler(output) {
  // estimateOverhead();
}

void ChronoElapsedTimeProfiler::setNumberOfTags(int n) {
  time_points_.reserve(n);
  counts_and_durations_.reserve(n);
}

void ChronoElapsedTimeProfiler::registerTag(const std::string& tag) {
  time_points_[tag];
  counts_and_durations_[tag];
}

void ChronoElapsedTimeProfiler::start(const std::string& tag) {
  time_points_[tag] = clockType::now();
}

void ChronoElapsedTimeProfiler::stop(const std::string& tag) {
  auto end = clockType::now();
  escape(&end);

  auto start = time_points_[tag];
  auto& pair = counts_and_durations_[tag];
  pair.first++;                  // count
  pair.second += (end - start);  // total elapsed time
}

void ChronoElapsedTimeProfiler::writeToOstream(std::ostream* os) const {
  for (const auto& kv_pair : counts_and_durations_) {
    *os << "ChronoElapsedTimeProfiler: " << kv_pair.first << " count "
        << kv_pair.second.first << std::endl;
    *os << "ChronoElapsedTimeProfiler: " << kv_pair.first << " time_sec "
        << static_cast<std::chrono::duration<double, std::ratio<1>>>(
               kv_pair.second.second)
               .count()
        << std::endl;
    *os << "ChronoElapsedTimeProfiler: " << kv_pair.first
        << " time_sec / count "
        << static_cast<std::chrono::duration<double, std::ratio<1>>>(
               kv_pair.second.second)
                   .count() /
               kv_pair.second.first
        << std::endl;
  }
}

}  // namespace simple
}  // namespace profilers
}  // namespace exahype
