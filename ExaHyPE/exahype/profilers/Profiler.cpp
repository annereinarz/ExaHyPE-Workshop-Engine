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

#include "Profiler.h"

#include <fstream>
#include <iostream>

namespace {

bool hasSuffix(const std::string& str, const std::string& suffix) {
  return str.size() >= suffix.size() &&
         str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
}

}  // namespace

namespace exahype {
namespace profilers {

void Profiler::writeToCout() const { writeToOstream(&std::cout); }

void Profiler::writeToFile(const std::string& path) const {
  std::ofstream ofs;
  ofs.open(path, std::ios::out | std::ios::trunc);

  if (ofs.is_open()) {
    writeToOstream(&ofs);
  } else {
    std::cerr << "Profiler: Could not write to file '" << path << "'"
              << std::endl;
  }
}

void Profiler::writeToConfiguredOutput() const {
  if (output_ == "") {
    // do nothing
  } else if (output_ == "std.cout") {
    writeToCout();
  } else if (hasSuffix(output_, ".txt")) {
    writeToFile(output_);
  } else if (hasSuffix(output_, ".json")) {
    // Not yet pushed due to dependencies
    writeToFile(output_);
  } else if (hasSuffix(output_, ".csv")) {
    // Not yet pushed due to dependencies
    writeToFile(output_);
  } else {
    std::cerr << "Profiler: Unknown file extension in '" << output_
              << "'. Writing to cout instead." << std::endl;
  }
}

}  // namespace profilers
}  // namespace exahype
