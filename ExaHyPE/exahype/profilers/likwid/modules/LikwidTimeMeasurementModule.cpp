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

#include "LikwidTimeMeasurementModule.h"

#ifdef LIKWID_AVAILABLE

#include "../../ProfilerUtils.h"
#include <cassert>
#include <utility>

namespace {
const int kNumberOfSamples = 1000000;
}

namespace exahype {
namespace profilers {
namespace likwid {

LikwidTimeMeasurementModule::LikwidTimeMeasurementModule(
    const LikwidProfilerState& state)
    : LikwidModule(state) {
  timer_init();

  // Overhead
  /*
  {
    uint64_t total_time_ns, total_cycles;

    TimerData timer_data;
    for (int i = 0; i < kNumberOfSamples; i++) {
      timer_start(&timer_data);
      utils::escape(&timer_data);
      timer_stop(&timer_data);
      utils::escape(&timer_data);

      total_time_ns += static_cast<uint64_t>(timer_print(&timer_data) * 1e9);
      total_cycles += static_cast<uint64_t>(timer_printCycles(&timer_data));
    }

    std::cout << "LikwidTimeMeasurementModule: overhead seconds = "
              << static_cast<double>(total_time_ns) / 1e9 / kNumberOfSamples
              << std::endl;
    std::cout << "LikwidTimeMeasurementModule: overhead cycles = "
              << static_cast<double>(total_cycles) / kNumberOfSamples
              << std::endl;
  }
  */
}

LikwidTimeMeasurementModule::~LikwidTimeMeasurementModule() {
  timer_finalize();
}

void LikwidTimeMeasurementModule::setNumberOfTags(int n) {
  aggregates_.reserve(n);
}

void LikwidTimeMeasurementModule::registerTag(const std::string& tag) {
  assert((aggregates_.count(tag) == 0) &&
         "At least one tag has been registered twice");
  aggregates_[tag] = std::make_tuple(0, 0, 0.0 /* count, cycles, seconds */);
}

void LikwidTimeMeasurementModule::start(const std::string& tag) {
  assert((aggregates_.count(tag) == 1) &&
         "At least one tag has not been registered prior to starting the "
         "corresponding measurement");
  timer_start(&timer_data_);
}

void LikwidTimeMeasurementModule::stop(const std::string& tag) {
  timer_stop(&timer_data_);
  utils::escape(&timer_data_);
  auto& tuple_count_cycles_seconds = aggregates_.at(tag);
  std::get<0>(tuple_count_cycles_seconds)++;  // increment count
  std::get<1>(tuple_count_cycles_seconds) += timer_printCycles(&timer_data_);
  std::get<2>(tuple_count_cycles_seconds) += timer_print(&timer_data_);
}

void LikwidTimeMeasurementModule::writeToOstream(std::ostream* os) const {
  for (const auto& pair_tag_tuple_count_cycles_seconds : aggregates_) {
    *os << "TimeMeasurementModule: "
        << pair_tag_tuple_count_cycles_seconds.first << " count "
        << std::get<0>(pair_tag_tuple_count_cycles_seconds.second) << std::endl;

    *os << "TimeMeasurementModule: "
        << pair_tag_tuple_count_cycles_seconds.first << " cycles "
        << std::get<1>(pair_tag_tuple_count_cycles_seconds.second) << std::endl;
    *os << "TimeMeasurementModule: "
        << pair_tag_tuple_count_cycles_seconds.first << " time_sec "
        << std::get<2>(pair_tag_tuple_count_cycles_seconds.second) << std::endl;

    *os << "TimeMeasurementModule: "
        << pair_tag_tuple_count_cycles_seconds.first << " cycles / count "
        << std::get<1>(pair_tag_tuple_count_cycles_seconds.second) /
               static_cast<double>(
                   std::get<0>(pair_tag_tuple_count_cycles_seconds.second))

        << std::endl;
    *os << "TimeMeasurementModule: "
        << pair_tag_tuple_count_cycles_seconds.first << " time_sec / count "
        << std::get<2>(pair_tag_tuple_count_cycles_seconds.second) /
               static_cast<double>(
                   std::get<0>(pair_tag_tuple_count_cycles_seconds.second))
        << std::endl;
  }
}

}  // namespace likwid
}  // namespace profilers
}  // namespace exahype

#endif  // LIKWID_AVAILABLE
