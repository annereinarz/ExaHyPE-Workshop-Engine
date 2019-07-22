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

#ifndef _EXAHYPE_PROFILERS_SIMPLE_CHRONO_ELAPSED_TIME_PROFILER_H_
#define _EXAHYPE_PROFILERS_SIMPLE_CHRONO_ELAPSED_TIME_PROFILER_H_

#include <chrono>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "../Profiler.h"

//typedef std::chrono::steady_clock clockType;
typedef std::chrono::high_resolution_clock clockType;

namespace exahype {
namespace profilers {
namespace simple {

class ChronoElapsedTimeProfiler : public Profiler {
 public:
  ChronoElapsedTimeProfiler(const std::string& output);

  virtual ~ChronoElapsedTimeProfiler() {}

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void start(const std::string& tag) override;
  void stop(const std::string& tag) override;
  void writeToOstream(std::ostream* os) const override;

 private:
  std::unordered_map<std::string,
                     std::chrono::time_point<clockType>>
      time_points_;
  std::unordered_map<std::string,
                     std::pair<long long, clockType::duration>>
      counts_and_durations_;
};

}  // namespace simple
}  // namespace profilers
}  // namespace exahype

#endif  // _EXAHYPE_PROFILERS_SIMPLE_CHRONO_ELAPSED_TIME_PROFILER_H_
