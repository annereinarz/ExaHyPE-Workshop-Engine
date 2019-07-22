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

#ifndef _EXAHYPE_PROFILERS_PROFILER_FACTORY_H_
#define _EXAHYPE_PROFILERS_PROFILER_FACTORY_H_

#include <memory>
#include <string>
#include <vector>

namespace exahype {
namespace profilers {
class Profiler;
}  // namespace profilers
}  // namespace exahype

namespace exahype {
namespace profilers {

class ProfilerFactory {
 public:
  virtual ~ProfilerFactory() {}

  static ProfilerFactory& getInstance();

  std::unique_ptr<Profiler> create(const std::string& profiler_name,
                                   const std::vector<std::string>& modules,
                                   const std::string& profiling_output);

 private:
  ProfilerFactory() {}
};

}  // namespace profilers
}  // namespace exahype

#endif  // _EXAHYPE_PROFILERS_PROFILER_FACTORY_H_
