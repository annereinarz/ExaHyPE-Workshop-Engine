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

#include "ProfilerFactory.h"

#include <cstdlib>
#include <functional>
#include <iostream>
#include <unordered_map>

#include "simple/ChronoElapsedTimeProfiler.h"
#include "simple/IbmAemProfiler.h"
#include "simple/NoOpProfiler.h"

#ifdef LIKWID_AVAILABLE
#include "likwid/LikwidProfiler.h"
#include "likwid/modules/LikwidCountModule.h"
#include "likwid/modules/LikwidPerformanceMonitoringModule.h"
#include "likwid/modules/LikwidPowerAndEnergyMonitoringModule.h"
#include "likwid/modules/LikwidTimeMeasurementModule.h"
#endif  // LIKWID_AVAILABLE

#ifdef IPCM_AVAILABLE
#include "ipcm/IpcmProfiler.h"
#include "ipcm/metrics/IpcmBytesReadDramMetric.h"
#include "ipcm/metrics/IpcmBytesWrittenDramMetric.h"
#include "ipcm/metrics/IpcmConsumedJoulesMetric.h"
#include "ipcm/metrics/IpcmCountMetric.h"
#include "ipcm/metrics/IpcmCyclesLostL2MissesMetric.h"
#include "ipcm/metrics/IpcmCyclesLostL3MissesMetric.h"
#include "ipcm/metrics/IpcmCyclesMetric.h"
#include "ipcm/metrics/IpcmDramConsumedJoulesMetric."
#include "ipcm/metrics/IpcmInstructionsRetiredMetric.h"
#include "ipcm/metrics/IpcmL2CacheHitsMetric.h"
#include "ipcm/metrics/IpcmL2CacheMissesMetric.h"
#include "ipcm/metrics/IpcmL3CacheHitsMetric.h"
#include "ipcm/metrics/IpcmL3CacheMissesMetric.h"
#endif  // IPCM_AVAILABLE

namespace {

#ifdef LIKWID_AVAILABLE
const std::unordered_map<
    std::string,
    std::function<std::unique_ptr<exahype::profilers::likwid::LikwidModule>(
        const exahype::profilers::likwid::LikwidProfilerState&,
        const std::string&)>>
    likwid_module_map = {
        {"LikwidTimeMeasurementModule",
         [](const exahype::profilers::likwid::LikwidProfilerState& state,
            const std::string& suffix) {
           return std::unique_ptr<
               exahype::profilers::likwid::LikwidTimeMeasurementModule>(
               new exahype::profilers::likwid::LikwidTimeMeasurementModule(
                   state));
         }},
        {
            "LikwidPowerAndEnergyMonitoringModule",
            [](const exahype::profilers::likwid::LikwidProfilerState& state,
               const std::string& suffix) {
              return std::unique_ptr<exahype::profilers::likwid::
                                         LikwidPowerAndEnergyMonitoringModule>(
                  new exahype::profilers::likwid::
                      LikwidPowerAndEnergyMonitoringModule(state));
            },
        },
        {"LikwidPerformanceMonitoringModule",
         [](const exahype::profilers::likwid::LikwidProfilerState& state,
            const std::string& suffix) {
           return std::unique_ptr<
               exahype::profilers::likwid::LikwidPerformanceMonitoringModule>(
               new exahype::profilers::likwid::
                   LikwidPerformanceMonitoringModule(state, suffix));
         }},
        {"LikwidCountModule",
         [](const exahype::profilers::likwid::LikwidProfilerState& state,
            const std::string& suffix) {
           return std::unique_ptr<
               exahype::profilers::likwid::LikwidCountModule>(
               new exahype::profilers::likwid::LikwidCountModule(state));
         }},
};
#endif  // LIKWID_AVAILABLE

#ifdef IPCM_AVAILABLE
const std::unordered_map<
    std::string,
    std::function<std::unique_ptr<exahype::profilers::ipcm::IpcmMetric>()>>
    ipcm_metrics_map = {
        {"IpcmCyclesMetric",
         []() {
           return std::unique_ptr<exahype::profilers::ipcm::IpcmMetric>(
               new exahype::profilers::ipcm::IpcmCyclesMetric);
         }},
        {"IpcmBytesReadDramMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmBytesReadDramMetric>(
               new exahype::profilers::ipcm::IpcmBytesReadDramMetric);
         }},
        {"IpcmBytesWrittenDramMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmBytesWrittenDramMetric>(
               new exahype::profilers::ipcm::IpcmBytesWrittenDramMetric);
         }},
        {"IpcmConsumedJoulesMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmConsumedJoulesMetric>(
               new exahype::profilers::ipcm::IpcmConsumedJoulesMetric);
         }},
        {"IpcmDramConsumedJoulesMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmDramConsumedJoulesMetric>(
               new exahype::profilers::ipcm::IpcmDramConsumedJoulesMetric);
         }},
        {"IpcmCyclesLostL2MissesMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmCyclesLostL2MissesMetric>(
               new exahype::profilers::ipcm::IpcmCyclesLostL2MissesMetric);
         }},
        {"IpcmCyclesLostL3MissesMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmCyclesLostL3MissesMetric>(
               new exahype::profilers::ipcm::IpcmCyclesLostL3MissesMetric);
         }},
        {"IpcmInstructionsRetiredMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmInstructionsRetiredMetric>(
               new exahype::profilers::ipcm::IpcmInstructionsRetiredMetric);
         }},
        {"IpcmL2CacheHitsMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmL2CacheHitsMetric>(
               new exahype::profilers::ipcm::IpcmL2CacheHitsMetric);
         }},
        {"IpcmL2CacheMissesMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmL2CacheMissesMetric>(
               new exahype::profilers::ipcm::IpcmL2CacheMissesMetric);
         }},
        {"IpcmL3CacheHitsMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmL3CacheHitsMetric>(
               new exahype::profilers::ipcm::IpcmL3CacheHitsMetric);
         }},
        {"IpcmL3CacheMissesMetric",
         []() {
           return std::unique_ptr<
               exahype::profilers::ipcm::IpcmL3CacheMissesMetric>(
               new exahype::profilers::ipcm::IpcmL3CacheMissesMetric);
         }},
        {"IpcmCountMetric",
         []() {
           return std::unique_ptr<exahype::profilers::ipcm::IpcmCountMetric>(
               new exahype::profilers::ipcm::IpcmCountMetric);
         }},
};
#endif  // IPCM_AVAILABLE

const std::unordered_map<
    std::string, std::function<std::unique_ptr<exahype::profilers::Profiler>(
                     const std::vector<std::string>&, const std::string&)>>
    profiler_map = {
        {"NoOpProfiler",
         [](const std::vector<std::string>& metrics,
            const std::string& profiling_output) {
           return std::unique_ptr<exahype::profilers::simple::NoOpProfiler>(
               new exahype::profilers::simple::NoOpProfiler(profiling_output));
         }},
        {"ChronoElapsedTimeProfiler",
         [](const std::vector<std::string>& metrics,
            const std::string& profiling_output) {
           return std::unique_ptr<
               exahype::profilers::simple::ChronoElapsedTimeProfiler>(
               new exahype::profilers::simple::ChronoElapsedTimeProfiler(
                   profiling_output));
         }},
        {"IbmAemProfiler",
         [](const std::vector<std::string>& metrics,
            const std::string& profiling_output) {
           return std::unique_ptr<exahype::profilers::simple::IbmAemProfiler>(
               new exahype::profilers::simple::IbmAemProfiler(
                   profiling_output));
         }},
#ifdef LIKWID_AVAILABLE
        {"LikwidProfiler",
         [](const std::vector<std::string>& modules,
            const std::string& profiling_output) {
           std::unique_ptr<exahype::profilers::likwid::LikwidProfiler> profiler(
               new exahype::profilers::likwid::LikwidProfiler(
                   profiling_output));
           for (const auto& module : modules) {
             std::string module_identifier, suffix;
             size_t pos_underscore = module.find_first_of('_');
             if (pos_underscore != std::string::npos) {
               module_identifier = module.substr(0, pos_underscore);
               suffix = module.substr(pos_underscore + 1);
             } else {
               module_identifier = module;
               suffix = "";
             }
             if (likwid_module_map.count(module_identifier)) {
               profiler->addModule(likwid_module_map.at(module_identifier)(
                   profiler->state(), suffix));
             } else {
               std::cerr << "ProfilerFactory: Unknown likwid module name '"
                         << module_identifier << "'." << std::endl;
               std::exit(EXIT_FAILURE);
             }
           }
           return profiler;
         }},
#endif  // LIKWID_AVAILABLE
#ifdef IPCM_AVAILABLE
        {"IpcmProfiler",
         [](const std::vector<std::string>& metrics,
            const std::string& profiling_output) {
           std::unique_ptr<exahype::profilers::ipcm::IpcmProfiler> profiler(
               new exahype::profilers::ipcm::IpcmProfiler(profiling_output));
           for (const auto& metric : metrics) {
             if (ipcm_metrics_map.count(metric)) {
               profiler->addMetric(ipcm_metrics_map.at(metric)());
             } else {
               std::cerr << "ProfilerFactory: Unknown ipcm metric name '"
                         << metric << "'" << std::endl;
               std::exit(EXIT_FAILURE);
             }
           }
           return profiler;
         }},
#endif  // IPCM_AVAILABLE
};

}  // namespace

namespace exahype {
namespace profilers {

ProfilerFactory& ProfilerFactory::getInstance() {
  static ProfilerFactory singleton;
  return singleton;
}

std::unique_ptr<Profiler> ProfilerFactory::create(
    const std::string& profiler_name, const std::vector<std::string>& modules,
    const std::string& profiling_output) {
  if (profiler_map.count(profiler_name)) {  // known profiler
    return profiler_map.at(profiler_name)(modules, profiling_output);
  } else {  // unknown profiler
    std::cerr << "ProfilerFactory: Unknown profiler name '" << profiler_name
              << "'. NoOpProfiler created instead." << std::endl;
    return profiler_map.at("NoOpProfiler")(modules, profiling_output);
  }
}

}  // namespace profilers
}  // namespace exahype
