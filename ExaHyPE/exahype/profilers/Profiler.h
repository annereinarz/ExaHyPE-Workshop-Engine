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

#ifndef _EXAHYPE_PROFILERS_PROFILER_H_
#define _EXAHYPE_PROFILERS_PROFILER_H_

#include <iostream>
#include <string>

namespace exahype {
namespace profilers {

class Profiler {
 public:
  Profiler(const std::string& output) : output_(output) {}

  virtual ~Profiler() {}

  // Disallow copy and assignment
  Profiler(const Profiler& other) = delete;
  Profiler& operator=(const Profiler& other) = delete;

  virtual void setNumberOfTags(int n) = 0;
  virtual void registerTag(const std::string& tag) = 0;
  virtual void start(const std::string& tag) = 0;
  virtual void stop(const std::string& tag) = 0;
  virtual void writeToOstream(std::ostream* os) const = 0;

  void writeToCout() const;
  void writeToFile(const std::string& path) const;
  void writeToConfiguredOutput() const;

 private:
  // Either "cout" or path to output file (format is determined based on
  // extension)
  const std::string output_;
};

}  // namespace profilers
}  // namespace exahype

#endif  // EXAHYPE_PROFILERS_PROFILER_H_
