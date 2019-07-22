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

#include <exahype/profilers/simple/IbmAemProfiler.h>

#include "../ProfilerUtils.h"
#include <cassert>
#include <chrono>
#include <limits>
#include <thread>

namespace {
static const int kNumberOfSamples = 10000;

static const char* sysfs_value = "/sys/devices/platform/aem.0/energy1_input";
static const char* sysfs_name = "/sys/devices/platform/aem.0/name";

static inline std::string readString(const char* file) {
  std::ifstream ifs(file);
  if (!ifs.good()) {
    std::cerr << "HwmonIbmAemProfiler: Error opening file " << file
              << std::endl;
  }
  std::string s;
  ifs >> s;
  return s;
}

static inline uint64_t readValue(std::ifstream* ifs) {
  ifs->seekg(0, ifs->beg);  // clear() not needed with C++11
  uint64_t value;
  (*ifs) >> value;
  return value;
}

}  // namespace

namespace exahype {
namespace profilers {
namespace simple {

IbmAemProfiler::IbmAemProfiler(const std::string& profiling_output)
    : Profiler(profiling_output), sysfs_file_(sysfs_value), penality_count_(0) {
  std::cout << "HwmonIbmAemProfiler: " << sysfs_value << std::endl;
  std::cout << "HwmonIbmAemProfiler: " << sysfs_name << " = "
            << readString(sysfs_name) << std::endl;

  // Test if sysfs file can be read
  if (!sysfs_file_.good()) {
    std::cerr << "HwmonIbmAemProfiler: Error opening file " << sysfs_value
              << std::endl;
    std::exit(EXIT_FAILURE);
  }

  // Calibrate
  {
    uint64_t start, stop;

    start = readValue(&sysfs_file_);
    for (int i = 0; i < kNumberOfSamples; i++) {
      uint64_t value = readValue(&sysfs_file_);
      utils::escape(&value);
    }
    stop = readValue(&sysfs_file_);
    utils::escape(&stop);

    penalty_per_poll_ = (stop - start) / 1e6 / (kNumberOfSamples + 2);
    std::cout << "HwmonIbmAemProfiler: penalty_per_poll_ [J] = "
              << penalty_per_poll_ << std::endl;
  }

  // Overhead
  /*
  {
    auto start = std::chrono::steady_clock::now();
    for (int i = 0; i < kNumberOfSamples; i++) {
      uint64_t value = readValue(&sysfs_file_);
      utils::escape(&value);
    }
    auto stop = std::chrono::steady_clock::now();

    std::cout << "HwmonIbmAemProfiler: Overhead read (ns): "
              << static_cast<double>((stop - start).count()) / kNumberOfSamples
              << std::endl;

    uint64_t sum = 0;
    for (int i = 0; i < kNumberOfSamples / 10; i++) {
      penality_count_ = 1;
      energy_at_start_ = readValue(&sysfs_file_);
      while (true) {
        penality_count_++;
        if (readValue(&sysfs_file_) != energy_at_start_) {
          break;
        }
      }
      sum += penality_count_;
      std::cout << i << " / " << kNumberOfSamples / 10 << std::endl;
    }

    std::cout << "HwmonIbmAemProfiler: Average reads = "
              << static_cast<double>(sum) / kNumberOfSamples * 10 << std::endl;
  }
  */

  // Test
  /*
  this->registerTag("test");
  this->start("test");
  std::this_thread::sleep_for(std::chrono::milliseconds(1000));
  this->stop("test");
  std::cout << "IbmAemProfiler: test count "
            << this->counts_and_energy_["test"].first << std::endl;
  std::cout << "IbmAemProfiler: test energy [J] "
            << this->counts_and_energy_["test"].second << std::endl;
  std::cout << "IbmAemProfiler: test energy [J] / count "
            << this->counts_and_energy_["test"].second /
                   this->counts_and_energy_["test"].first
            << std::endl;
  std::exit(0);
  */
}

void IbmAemProfiler::setNumberOfTags(int n) { counts_and_energy_.reserve(n); }

void IbmAemProfiler::registerTag(const std::string& tag) {
  assert(counts_and_energy_.count(tag) == 0 && "Tag registered twice.");

  counts_and_energy_[tag] = std::make_pair(0, 0.0);
}

void IbmAemProfiler::start(const std::string& tag) {
  assert(counts_and_energy_.count(tag) && "Unregistered tag encountered");

  penality_count_ = 1;
  energy_at_start_ = readValue(&sysfs_file_);
  while (true) {
    penality_count_++;
    if (readValue(&sysfs_file_) != energy_at_start_) {
      break;
    }
  }
}

void IbmAemProfiler::stop(const std::string& tag) {
  penality_count_++;
  uint64_t tmp = readValue(&sysfs_file_);

  uint64_t energy_at_stop;
  while (true) {
    penality_count_++;
    energy_at_stop = readValue(&sysfs_file_);
    if (energy_at_stop != tmp) {
      break;
    }
  }

  assert(counts_and_energy_.count(tag) && "Unregistered tag encountered");

  std::get<0>(counts_and_energy_[tag])++;   // increment count
  if (energy_at_start_ < energy_at_stop) {  // no wrap around
    std::get<1>(counts_and_energy_[tag]) +=
        (energy_at_stop - energy_at_start_) / 1e6 -
        penality_count_ * penalty_per_poll_;
  } else {  // wrap around
    std::get<1>(counts_and_energy_[tag]) +=
        ((std::numeric_limits<uint64_t>::max() - energy_at_start_) +
         energy_at_stop) /
            1e6 -
        penality_count_ * penalty_per_poll_;
  }
}

void IbmAemProfiler::writeToOstream(std::ostream* os) const {
  for (auto pair_tag_pair_count_joules : counts_and_energy_) {
    *os << "IbmAemProfiler: " << pair_tag_pair_count_joules.first << " count "
        << pair_tag_pair_count_joules.second.first << std::endl;
    *os << "IbmAemProfiler: " << pair_tag_pair_count_joules.first
        << " energy [J] " << pair_tag_pair_count_joules.second.second
        << std::endl;
    *os << "IbmAemProfiler: " << pair_tag_pair_count_joules.first
        << " energy [J] / count "
        << pair_tag_pair_count_joules.second.second /
               pair_tag_pair_count_joules.second.first
        << std::endl;
  }
}

}  // namespace simple
}  // namespace profilers
}  // namespace exahype
