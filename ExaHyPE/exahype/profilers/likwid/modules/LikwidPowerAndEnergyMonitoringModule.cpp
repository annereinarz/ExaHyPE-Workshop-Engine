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

#include "LikwidPowerAndEnergyMonitoringModule.h"

#ifdef LIKWID_AVAILABLE

#include "../../ProfilerUtils.h"
#include "../LikwidProfiler.h"
#include <array>
#include <cassert>
#include <chrono>
#include <cstdlib>
#include <limits>
#include <map>

namespace {
using namespace exahype::profilers::utils;

// TODO: Remove once the public function power_read does not depend on private
// knowledge any more.
static const uint64_t MSR_PKG_ENERGY_STATUS = 0x611;
static const uint64_t MSR_PP0_ENERGY_STATUS = 0x639;
static const uint64_t MSR_PP1_ENERGY_STATUS = 0x641;
static const uint64_t MSR_DRAM_ENERGY_STATUS = 0x619;

static const std::map<const PowerType, uint64_t> powertype_to_register = {
    {PowerType::PKG, MSR_PKG_ENERGY_STATUS},
    {PowerType::PP0, MSR_PP0_ENERGY_STATUS},
    {PowerType::PP1, MSR_PP1_ENERGY_STATUS},
    {PowerType::DRAM, MSR_DRAM_ENERGY_STATUS}};

static const int kNumberOfSamples = 1000000;
static const int kNumberOfSamples2 = 1000;

std::chrono::steady_clock::duration singleRaplRead(int cpu_id) {
  PowerData powerdata;
  uint32_t value;

  power_start(&powerdata, cpu_id, PowerType::PKG);
  auto start = std::chrono::steady_clock::now();
  power_read(cpu_id, MSR_PKG_ENERGY_STATUS, &value);
  escape(&value);
  auto stop = std::chrono::steady_clock::now();
  power_stop(&powerdata, cpu_id, PowerType::PKG);
  return stop - start;
}

uint64_t singleRaplReadCycles(int cpu_id) {
  timer_init();

  PowerData powerdata;
  uint32_t value;

  TimerData td;
  timer_start(&td);

  power_start(&powerdata, cpu_id, PowerType::PKG);
  timer_start(&td);
  power_read(cpu_id, MSR_PKG_ENERGY_STATUS, &value);
  escape(&value);
  timer_stop(&td);
  power_stop(&powerdata, cpu_id, PowerType::PKG);

  return timer_printCycles(&td);
}

template <int kNumberOfProfiledPowerTypes, const PowerType* kProfiledPowerTypes>
int countUntilChange(int cpu_id) {
  uint32_t old_values[kNumberOfProfiledPowerTypes],
      new_values[kNumberOfProfiledPowerTypes];

  int i = 1;
  for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
    power_read(cpu_id, powertype_to_register.at(kProfiledPowerTypes[j]),
               &old_values[j]);
  }
  while (true) {
    i++;
    for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
      power_read(cpu_id, powertype_to_register.at(kProfiledPowerTypes[j]),
                 &new_values[j]);
    }

    bool all_updated = true;
    for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
      all_updated &= (old_values[j] != new_values[j]);
    }
    if (all_updated) {
      break;
    }
  }
  return i;
}

}  // namespace

namespace exahype {
namespace profilers {
namespace likwid {

// todo: check
constexpr PowerType LikwidPowerAndEnergyMonitoringModule::kProfiledPowerTypes
    [LikwidPowerAndEnergyMonitoringModule::kNumberOfProfiledPowerTypes];

LikwidPowerAndEnergyMonitoringModule::LikwidPowerAndEnergyMonitoringModule(
    const LikwidProfilerState& state)
    : LikwidModule(state), number_of_rapl_reads_(-1) {
  assert(state_.cpu_ == affinity_threadGetProcessorId());

  int has_rapl = power_init(state_.cpu_);
  if (has_rapl == 0) {
    std::cerr << "LikwidPowerAndEnergyMonitoringModule: has_rapl == 0"
              << std::endl;
    std::exit(EXIT_FAILURE);
  }

  HPMaddThread(state_.cpu_);
  power_init(state_.cpu_);

  power_info_ = get_powerInfo();

  // calibration
  // time likwid rapl read with likwid cycle count
  {
    std::array<uint64_t, kNumberOfSamples> single_read_cycles;
    std::generate(single_read_cycles.begin(), single_read_cycles.end(),
                  [this]() { return singleRaplReadCycles(state_.cpu_); });
    std::sort(single_read_cycles.begin(), single_read_cycles.end());
    std::cout << "single rapl read in cycles median = "
              << median<uint64_t, kNumberOfSamples>(single_read_cycles)
              << std::endl;
  }

  // measure energy of likwid rapl
  {
    for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
      PowerData powerdata;
      uint32_t value;

      power_start(&powerdata, state_.cpu_, kProfiledPowerTypes[i]);
      for (int j = 0; j < kNumberOfSamples; j++) {
        power_read(state_.cpu_,
                   powertype_to_register.at(kProfiledPowerTypes[i]), &value);
        escape(&value);
      }
      power_stop(&powerdata, state_.cpu_, kProfiledPowerTypes[i]);
      penality_per_rapl_read_[i] =
          power_printEnergy(&powerdata) / kNumberOfSamples;
      std::cout << powerTypeToString(kProfiledPowerTypes[i])
                << " power per rapl poll in J mean: "
                << penality_per_rapl_read_[i] << std::endl;
    }
  }

  // overhead
  /*
  // time likwid rapl read with chrono
  {
    std::array<std::chrono::steady_clock::duration, kNumberOfSamples>
        single_read;
    std::generate(single_read.begin(), single_read.end(),
                  [this]() { return singleRaplRead(state_.cpu_); });
    std::cout << "single rapl read duration in sec median = "
              << meanDuration(single_read) << std::endl;
  }

  // rapl reads until value changed
  {
    std::array<int, kNumberOfSamples2> single_read_cycles;
    std::generate(single_read_cycles.begin(), single_read_cycles.end(),
                  [this]() {
                    return countUntilChange<kNumberOfProfiledPowerTypes,
                                            kProfiledPowerTypes>(state_.cpu_);
                  });
    std::sort(single_read_cycles.begin(), single_read_cycles.end());
    std::cout << "number of reads until change median = "
              << median<int, kNumberOfSamples2>(single_read_cycles)
              << std::endl;
  }
  */
}

LikwidPowerAndEnergyMonitoringModule::~LikwidPowerAndEnergyMonitoringModule() {
  power_finalize();
  power_info_ = nullptr;
}

void LikwidPowerAndEnergyMonitoringModule::setNumberOfTags(int n) {
  aggregates_.reserve(n);
}

void LikwidPowerAndEnergyMonitoringModule::registerTag(const std::string& tag) {
  assert((aggregates_.count(tag) == 0) &&
         "At least one tag has been registered twice");

  auto& pair_count_array = aggregates_[tag];
  pair_count_array.first = 0;  // init count
  std::fill(pair_count_array.second.begin(), pair_count_array.second.end(),
            0.0);  // init aggregates for all power types
}

void LikwidPowerAndEnergyMonitoringModule::start(const std::string& tag) {
  number_of_rapl_reads_ = 1;

  // For all profiled power types...
  for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
    // start power measurement
    power_start(&power_data_[j], state_.cpu_, kProfiledPowerTypes[j]);
  }

  // poll until rapl counter update occurs
  uint32_t old_values[kNumberOfProfiledPowerTypes],
      new_values[kNumberOfProfiledPowerTypes];
  for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
    power_read(state_.cpu_, powertype_to_register.at(kProfiledPowerTypes[i]),
               &old_values[i]);
  }
  while (true) {
    number_of_rapl_reads_++;
    for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
      power_read(state_.cpu_, powertype_to_register.at(kProfiledPowerTypes[i]),
                 &new_values[i]);
    }

    bool all_updated = true;
    for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
      all_updated &= (old_values[i] != new_values[i]);
    }
    if (all_updated) {
      break;
    }
  }
}

void LikwidPowerAndEnergyMonitoringModule::stop(const std::string& tag) {
  assert(aggregates_.count(tag) && "Unregistered tag encountered");

  // poll until rapl counter update occurs
  uint32_t old_values[kNumberOfProfiledPowerTypes],
      new_values[kNumberOfProfiledPowerTypes];
  for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
    power_read(state_.cpu_, powertype_to_register.at(kProfiledPowerTypes[i]),
               &old_values[i]);
  }
  while (true) {
    number_of_rapl_reads_++;
    for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
      power_read(state_.cpu_, powertype_to_register.at(kProfiledPowerTypes[i]),
                 &new_values[i]);
    }

    bool all_updated = true;
    for (int i = 0; i < kNumberOfProfiledPowerTypes; i++) {
      all_updated &= (old_values[i] != new_values[i]);
    }
    if (all_updated) {
      break;
    }
  }

  // For all profiled power types
  for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
    // stop power measurement
    power_stop(&power_data_[j], state_.cpu_, kProfiledPowerTypes[j]);
  }

  // then update all aggregates
  auto& pair_count_array = aggregates_[tag];

  pair_count_array.first++;  // increase count

  // for all power types
  for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
    pair_count_array.second[j] +=
        power_printEnergy(&power_data_[j]) -
        number_of_rapl_reads_ * penality_per_rapl_read_[j];
  }
}

void LikwidPowerAndEnergyMonitoringModule::writeToOstream(
    std::ostream* os) const {
  // For all tags
  for (const auto& pair_tag_pair_count_array : aggregates_) {
    // print count
    *os << "PowerAndEnergyMonitoringModule: " << pair_tag_pair_count_array.first
        << " count " << pair_tag_pair_count_array.second.first << std::endl;

    // for all power types
    for (int j = 0; j < kNumberOfProfiledPowerTypes; j++) {
      *os << "PowerAndEnergyMonitoringModule: "
          << pair_tag_pair_count_array.first << " "
          << powerTypeToString(kProfiledPowerTypes[j]) << " "
          << pair_tag_pair_count_array.second.second[j] << std::endl;

      *os << "PowerAndEnergyMonitoringModule: "
          << pair_tag_pair_count_array.first << " "
          << powerTypeToString(kProfiledPowerTypes[j]) << " / count "
          << pair_tag_pair_count_array.second.second[j] /
                 pair_tag_pair_count_array.second.first
          << std::endl;
    }
  }
}

}  // namespace likwid
}  // namespace profilers
}  // namespace exahype

#endif  // LIKWID_AVAILABLE
