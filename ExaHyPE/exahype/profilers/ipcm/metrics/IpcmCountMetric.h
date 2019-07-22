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
 
#ifndef _EXAHYPE_PROFILERS_IPCM_METRICS_IPCM_COUNT_METRIC_H_
#define _EXAHYPE_PROFILERS_IPCM_METRICS_IPCM_COUNT_METRIC_H_

#ifdef IPCM_AVAILABLE

#include <iostream>
#include <string>
#include <types.h>
#include <unordered_map>

#include "IpcmMetric.h"

namespace exahype {
namespace profilers {
namespace ipcm {

class IpcmCountMetric : public IpcmMetric {
 public:
  IpcmCountMetric() {}
  virtual ~IpcmCountMetric() {}

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void stop(const std::string& tag, const SystemCounterState& before_state,
            const SystemCounterState& after_state) override;
  void writeToOstream(std::ostream* os) const override;

 private:
  std::unordered_map<std::string, uint64> counts_;
};

}  // namespace ipcm
}  // namespace profilers
}  // namespace exahype

#endif  // IPCM_AVAILABLE

#endif
