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

#ifndef _EXAHYPE_PROFILERS_SIMPLE_IBM_AEM_PROFILER_H_
#define _EXAHYPE_PROFILERS_SIMPLE_IBM_AEM_PROFILER_H_

#include <exahype/profilers/Profiler.h>

#include <cstdint>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>

namespace exahype {
namespace profilers {
namespace simple {

/*
 * A profiler based on the the IBM AEM sysfs interface.
 * Updates happen every ~300ms; unit seems to be uJ.
 * Sampling is based on the polling approach described by Haehnel (2012), i.e.
 * code paths <300ms can in theory be profiled.
 */

class IbmAemProfiler : public Profiler {
 public:
  IbmAemProfiler(const std::string& profiling_output);
  virtual ~IbmAemProfiler() {}

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void start(const std::string& tag) override;
  void stop(const std::string& tag) override;
  void writeToOstream(std::ostream* os) const override;

 private:
  std::ifstream sysfs_file_;
  uint64_t energy_at_start_ = 0;
  std::unordered_map<std::string, std::pair<int, double>> counts_and_energy_;

  double penalty_per_poll_;
  int penality_count_ = 0;
};

}  // namespace simple
}  // namespace profilers
}  // namespace exahype

#endif  // _EXAHYPE_PROFILERS_SIMPLE_IBM_AEM_PROFILER_H_
