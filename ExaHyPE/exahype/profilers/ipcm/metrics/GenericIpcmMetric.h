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
 
#ifndef _EXAHYPE_PROFILERS_IPCM_METRICS_GENERIC_IPCM_METRIC_H_
#define _EXAHYPE_PROFILERS_IPCM_METRICS_GENERIC_IPCM_METRIC_H_

#ifdef IPCM_AVAILABLE

#include <cassert>
#include <iostream>
#include <string>
#include <unordered_map>

#include "IpcmMetric.h"

namespace exahype {
namespace profilers {
namespace ipcm {

template <typename T>
class GenericIpcmMetric : public IpcmMetric {
 public:
  GenericIpcmMetric() {}
  virtual ~GenericIpcmMetric() {}

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void stop(const std::string& tag, const SystemCounterState& before_state,
            const SystemCounterState& after_state) override;
  void writeToOstream(std::ostream* os) const override;

 private:
  std::unordered_map<std::string, typename T::method_return_t> aggregates_;
};

template <typename T>
void GenericIpcmMetric<T>::setNumberOfTags(int n) {
  aggregates_.reserve(n);
}

template <typename T>
void GenericIpcmMetric<T>::registerTag(const std::string& tag) {
  assert((aggregates_.count(tag) == 0) &&
         "At least one tag has been registered more than once");
  aggregates_[tag];
}

template <typename T>
void GenericIpcmMetric<T>::stop(const std::string& tag,
                                const SystemCounterState& before_state,
                                const SystemCounterState& after_state) {
  aggregates_[tag] += T::method(before_state, after_state);
}

template <typename T>
void GenericIpcmMetric<T>::writeToOstream(std::ostream* os) const {
  for (const auto& aggregate : aggregates_) {
    *os << T::method_tag() << " " << aggregate.first << " " << aggregate.second
        << std::endl;
  }
}

}  // namespace ipcm
}  // namespace profilers
}  // namespace exahype

#endif  // IPCM_AVAILABLE

#endif  // _EXAHYPE_PROFILERS_IPCM_METRICS_GENERIC_IPCM_METRIC_H_
