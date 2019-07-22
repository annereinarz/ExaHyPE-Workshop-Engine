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

#ifndef _EXAHYPE_PROFILERS_LIKWID_MODULES_LIKWID_POWER_AND_ENERGY_MODULE_H_
#define _EXAHYPE_PROFILERS_LIKWID_MODULES_LIKWID_POWER_AND_ENERGY_MODULE_H_

#ifdef LIKWID_AVAILABLE

#include <array>
#include <iostream>
#include <likwid.h>
#include <string>
#include <unordered_map>
#include <vector>

#include "LikwidModule.h"

namespace exahype {
namespace profilers {
namespace likwid {

struct LikwidProfilerState;

class LikwidPowerAndEnergyMonitoringModule : public LikwidModule {
 public:
  explicit LikwidPowerAndEnergyMonitoringModule(
      const LikwidProfilerState& state);
  virtual ~LikwidPowerAndEnergyMonitoringModule();

  void setNumberOfTags(int n) override;
  void registerTag(const std::string& tag) override;
  void start(const std::string& tag) override;
  void stop(const std::string& tag) override;
  void writeToOstream(std::ostream* os) const override;

 private:
  // PowerType:
  //   * PKG: PKG domain, mostly one CPU socket/package
  //   * PP0: PP0 domain, not clearly defined by Intel
  //   * PP1: PP1 domain, not clearly defined by Intel
  //   * DRAM: DRAM domain, the memory modules

  // TODO(guera): constexpr in C++14?
  static const char* powerTypeToString(PowerType p) {
    switch (p) {
      case PowerType::PKG:
        return "PKG_J";
      case PowerType::PP0:
        return "PP0_J";
      case PowerType::PP1:
        return "PP1_J";
      case PowerType::DRAM:
        return "DRAM_J";
      default:
        return "Unknown PowerType";
    }
  }

  static constexpr int kNumberOfProfiledPowerTypes = 2;
  static constexpr PowerType kProfiledPowerTypes[kNumberOfProfiledPowerTypes] =
      {PowerType::PKG, PowerType::DRAM};

  PowerInfo* power_info_ = nullptr;

  std::array<PowerData, kNumberOfProfiledPowerTypes> power_data_;

  std::unordered_map<std::string,
                     std::pair<int /* count */,
                               std::array<double, kNumberOfProfiledPowerTypes>>>
      aggregates_;

  double penality_per_rapl_read_[kNumberOfProfiledPowerTypes];
  int number_of_rapl_reads_;
};

}  // namespace likwid
}  // namespace profilers
}  // namespace exahype

#endif  // LIKWID_AVAILABLE

#endif  // _EXAHYPE_PROFILERS_LIKWID_MODULES_LIKWID_POWER_AND_ENERGY_MODULE_H_
