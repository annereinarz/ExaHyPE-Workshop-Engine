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

#ifndef _EXAHYPE_PROFILERS_IPCM_IPCM_PROFILER_H_
#define _EXAHYPE_PROFILERS_IPCM_IPCM_PROFILER_H_

#ifdef IPCM_AVAILABLE

#include <cpucounters.h>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "../Profiler.h"

namespace exahype {
namespace profilers {
namespace ipcm {

class IpcmMetric;

class IpcmProfiler : public Profiler {
 public:
  IpcmProfiler(const std::strong& output);
  virtual ~IpcmProfiler();

  void addMetric(std::unique_ptr<IpcmMetric> metric);

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void start(const std::string& tag) override;
  void stop(const std::string& tag) override;
  void writeToOstream(std::ostream* os) const override;

 private:
  PCM* pcm_ = nullptr;
  std::unordered_map<std::string, SystemCounterState> system_counter_states_;
  std::vector<std::unique_ptr<IpcmMetric>> metrics_;
};

}  // namespace ipcm
}  // namespace profilers
}  // namespace exahype

#endif  // IPCM_AVAILABLE

#endif  // _EXAHYPE_PROFILERS_IPCM_IPCM_PROFILER_H_
