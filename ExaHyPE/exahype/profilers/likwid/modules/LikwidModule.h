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
 
#ifndef _EXAHYPE_PROFILERS_PROFILER_LIKWID_MODULES_LIKWID_MODULE_H_
#define _EXAHYPE_PROFILERS_PROFILER_LIKWID_MODULES_LIKWID_MODULE_H_

#include <iostream>
#include <string>

namespace exahype {
namespace profilers {
namespace likwid {

struct LikwidProfilerState;

class LikwidModule {
 public:
  explicit LikwidModule(const LikwidProfilerState& state) : state_(state) {}
  virtual ~LikwidModule() {}

  // Disallow copy and assign
  LikwidModule(const LikwidModule& other) = delete;
  const LikwidModule& operator=(const LikwidModule& other) = delete;

  virtual void setNumberOfTags(int n) = 0;
  virtual void registerTag(const std::string& tag) = 0;
  virtual void start(const std::string& tag) = 0;
  virtual void stop(const std::string& tag) = 0;
  virtual void writeToOstream(std::ostream* os) const = 0;

 protected:
  const LikwidProfilerState& state_;
};

}  // namespace likwid
}  // namespace profilers
}  // namespace exahype

#endif  // _EXAHYPE_PROFILERS_PROFILER_LIKWID_MODULES_LIKWID_MODULE_H_
