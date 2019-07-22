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
 
#ifndef _EXAHYPE_PROFILERS_IPCM_METRICS_IPCM_METRIC_H_
#define _EXAHYPE_PROFILERS_IPCM_METRICS_IPCM_METRIC_H_

#ifdef IPCM_AVAILABLE

#include <iostream>
#include <string>

class SystemCounterState;

namespace exahype {
namespace profilers {
namespace ipcm {

class IpcmMetric {
 public:
  IpcmMetric() {}
  virtual ~IpcmMetric() {}

  // Disallow copy and assign
  IpcmMetric(const IpcmMetric& other) = delete;
  IpcmMetric& operator=(const IpcmMetric& other) = delete;

  virtual void setNumberOfTags(int n) = 0;
  virtual void registerTag(const std::string& tag) = 0;
  virtual void stop(const std::string& tag,
                    const SystemCounterState& before_state,
                    const SystemCounterState& after_state) = 0;
  virtual void writeToOstream(std::ostream* os) const = 0;
};

}  // namespace ipcm
}  // namespace profilers
}  // namespace exahype

#endif  // IPCM_AVAILABLE

#endif  // _EXAHYPE_PROFILERS_IPCM_METRICS_IPCM_METRIC_H_
