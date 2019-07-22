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

#ifdef LIKWID_AVAILABLE

#include "LikwidCountModule.h"

#include <cassert>
#include <utility>

namespace exahype {
namespace profilers {
namespace likwid {

void LikwidCountModule::setNumberOfTags(int n) { counts_.reserve(n); }

void LikwidCountModule::registerTag(const std::string& tag) {
  assert((counts_.count(tag) == 0) &&
         "At least one tag has been registered more than once");
  counts_[tag];
}

void LikwidCountModule::start(const std::string& tag) {}

void LikwidCountModule::stop(const std::string& tag) {
  assert(counts_.count(tag) && "Unregistered tag encountered");
  counts_[tag]++;
}

void LikwidCountModule::writeToOstream(std::ostream* os) const {
  for (const auto& count : counts_) {
    *os << "LikwidCountModule " << count.first << " count " << count.second
        << std::endl;
  }
}

}  // namespace likwid
}  // namespace profilers
}  // namespace exahype

#endif  // LIKWID_AVAILABLE
