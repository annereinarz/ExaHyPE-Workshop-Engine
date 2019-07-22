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
 
#include "NoOpProfiler.h"

void exahype::profilers::simple::NoOpProfiler::setNumberOfTags(int n) {}

void exahype::profilers::simple::NoOpProfiler::registerTag(
    const std::string& tag) {}

void exahype::profilers::simple::NoOpProfiler::start(const std::string& tag) {}

void exahype::profilers::simple::NoOpProfiler::stop(const std::string& tag) {}

void exahype::profilers::simple::NoOpProfiler::writeToOstream(
    std::ostream* os) const {}
