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

#ifndef _EXAHYPE_PROFILERS_SIMPLE_NO_OP_PROFILER_H_
#define _EXAHYPE_PROFILERS_SIMPLE_NO_OP_PROFILER_H_

#include <iostream>
#include <string>

#include "../Profiler.h"

namespace exahype {
namespace profilers {
namespace simple {

class NoOpProfiler : public Profiler {
 public:
  NoOpProfiler(const std::string& output) : Profiler(output) {}

  virtual ~NoOpProfiler() {}

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void start(const std::string& tag) override;
  void stop(const std::string& tag) override;
  virtual void writeToOstream(std::ostream* os) const;
};

}  // namespace simple
}  // namespace profilers
}  // namespace exahype

#endif  // _EXAHYPE_PROFILERS_SIMPLE_NO_OP_PROFILER_H_
