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

#include "IpcmProfiler.h"

#ifdef IPCM_AVAILABLE

#include <algorithm>
#include <cassert>
#include <cstdlib>

#include "metrics/IpcmMetric.h"

// Hack
#define SUPPRESS_OUTPUT(expr)                                        \
  {                                                                  \
    std::cout.setstate(std::ios_base::failbit);                      \
    std::cerr.setstate(std::ios_base::failbit);                      \
    expr;                                                            \
    std::cout.clear(); /* TODO(guera): Make copy of state instead */ \
    std::cerr.clear();                                               \
  }

namespace exahype {
namespace profilers {
namespace ipcm {

IpcmProfiler::IpcmProfiler(const std::string& output) : Profiler(output) {
  SUPPRESS_OUTPUT(pcm_ = PCM::getInstance());

  // program counters and exit on failure
  switch (pcm_->program(PCM::DEFAULT_EVENTS, nullptr)) {
    case PCM::ErrorCode::Success:
      break;
    case PCM::ErrorCode::PMUBusy:
      std::cerr << "IpcmProfiler: PMUBusy" << std::endl;
      std::exit(EXIT_FAILURE);
      break;
    case PCM::ErrorCode::MSRAccessDenied:
      std::cerr << "IpcmProfiler: MSRAccessDenied (root or daemon required)"
                << std::endl;
      std::exit(EXIT_FAILURE);
      break;
    case PCM::ErrorCode::UnknownError:
      std::cerr << "IpcmProfiler: UnknownError" << std::endl;
      std::exit(EXIT_FAILURE);
      break;
  }
}

IpcmProfiler::~IpcmProfiler() {
  metrics_.clear();
  SUPPRESS_OUTPUT(pcm_->cleanup());
}

void IpcmProfiler::addMetric(std::unique_ptr<IpcmMetric> metric) {
  metrics_.emplace_back(std::move(metric));
}

void IpcmProfiler::setNumberOfTags(int n) {
  for (auto& metric : metrics_) {
    metric->setNumberOfTags(n);
  }
}

void IpcmProfiler::registerTag(const std::string& tag) {
  assert((system_counter_states_.count(tag) == 0) &&
         "At least one tag has registered more than once");
  system_counter_states_[tag];
  for (auto& metric : metrics_) {
    metric->registerTag(tag);
  }
}

void IpcmProfiler::start(const std::string& tag) {
  assert(system_counter_states_.count(tag) && "Unregistered tag encountered");
  system_counter_states_[tag] = pcm_->getSystemCounterState();
}

void IpcmProfiler::stop(const std::string& tag) {
  const SystemCounterState after_state = pcm_->getSystemCounterState();
  assert(system_counter_states_.count(tag) && "Unregistered tag encountered");
  const SystemCounterState& before_state = system_counter_states_[tag];
  for (auto& metric : metrics_) {
    metric->stop(tag, before_state, after_state);
  }
}

void IpcmProfiler::writeToOstream(std::ostream* os) const {
  for (const auto& metric : metrics_) {
    metric->writeToOstream(os);
  }
}

}  // namespace ipcm
}  // namespace profilers
}  // namespace exahype

#endif  // IPCM_AVAILABLE
