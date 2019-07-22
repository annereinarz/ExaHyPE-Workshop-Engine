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

#include "LikwidPerformanceMonitoringModule.h"

#ifdef LIKWID_AVAILABLE

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <iterator>
#include <likwid.h>
#include <sstream>
#include <utility>
#include <vector>

#include "../LikwidProfiler.h"

// TODO(guera): Remove once likwid 4.1 is available on SuperMUC. At the moment
// this section is somewhat Haswell-EP specific.

namespace {
using namespace exahype::profilers::likwid;

constexpr const int kNumberOfGroups = 20;
constexpr const char* groups[kNumberOfGroups] = {
    "BRANCH",   "CLOCK",     "DATA",       "ENERGY",         "ICACHE",
    "L2",       "L2CACHE",   "L3",         "L3CACHE",        "MEM",
    "TLB_DATA", "TLB_INSTR", "FLOPS_AVX",  "CYCLE_ACTIVITY", "PORT_USAGE",
    "UOPS",     "UOPS_EXEC", "UOPS_ISSUE", "UOPS_RETIRE",    "CACHES"};

constexpr const int kNrOfCounters[kNumberOfGroups] = {
    5, 4, 6, 7, 7, 6, 5, 5, 6, 19, 7, 5, 4, 7, 11, 7, 7, 7, 7, 55};

constexpr const char* BRANCH_CTR[kNrOfCounters[0]] = {
    "INSTR_RETIRED_ANY:FIXC0", "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "BR_INST_RETIRED_ALL_BRANCHES:PMC0",
    "BR_MISP_RETIRED_ALL_BRANCHES_1:PMC1"};

constexpr const char* CLOCK_CTR[kNrOfCounters[1]] = {
    "INSTR_RETIRED_ANY:FIXC0", "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "PWR_PKG_ENERGY:PWR0"};

constexpr const char* DATA_CTR[kNrOfCounters[2]] = {
    "INSTR_RETIRED_ANY:FIXC0",      "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",   "MEM_UOPS_RETIRED_LOADS:PMC0",
    "MEM_UOPS_RETIRED_STORES:PMC1", "UOPS_RETIRED_ALL:PMC2"};

constexpr const char* ENERGY_CTR[kNrOfCounters[3]] = {
    "INSTR_RETIRED_ANY:FIXC0",    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "TEMP_CORE:TMP0",
    "PWR_PKG_ENERGY:PWR0",        "PWR_PP0_ENERGY:PWR1",
    "PWR_DRAM_ENERGY:PWR3"};

constexpr const char* ICACHE_CTR[kNrOfCounters[4]] = {
    "INSTR_RETIRED_ANY:FIXC0",    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "ICACHE_ACCESSES:PMC0",
    "ICACHE_MISSES:PMC1",         "ICACHE_IFETCH_STALL:PMC2",
    "ILD_STALL_IQ_FULL:PMC3"};

constexpr const char* L2_CTR[kNrOfCounters[5]] = {
    "INSTR_RETIRED_ANY:FIXC0",    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "L1D_REPLACEMENT:PMC0",
    "L2_TRANS_L1D_WB:PMC1",       "ICACHE_MISSES:PMC2"};

constexpr const char* L2CACHE_CTR[kNrOfCounters[6]] = {
    "INSTR_RETIRED_ANY:FIXC0", "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "L2_TRANS_ALL_REQUESTS:PMC0",
    "L2_RQSTS_MISS:PMC1"};

constexpr const char* L3_CTR[kNrOfCounters[7]] = {
    "INSTR_RETIRED_ANY:FIXC0", "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "L2_LINES_IN_ALL:PMC0",
    "L2_LINES_OUT_DEMAND_DIRTY:PMC1"};

constexpr const char* L3CACHE_CTR[kNrOfCounters[8]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "MEM_LOAD_UOPS_RETIRED_L3_ALL:PMC0",
    "MEM_LOAD_UOPS_RETIRED_L3_MISS:PMC1",
    "UOPS_RETIRED_ALL:PMC2"};

constexpr const char* MEM_CTR[kNrOfCounters[9]] = {
    "INSTR_RETIRED_ANY:FIXC0",    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "CAS_COUNT_RD:MBOX0C0",
    "CAS_COUNT_WR:MBOX0C1",       "CAS_COUNT_RD:MBOX1C0",
    "CAS_COUNT_WR:MBOX1C1",       "CAS_COUNT_RD:MBOX2C0",
    "CAS_COUNT_WR:MBOX2C1",       "CAS_COUNT_RD:MBOX3C0",
    "CAS_COUNT_WR:MBOX3C1",       "CAS_COUNT_RD:MBOX4C0",
    "CAS_COUNT_WR:MBOX4C1",       "CAS_COUNT_RD:MBOX5C0",
    "CAS_COUNT_WR:MBOX5C1",       "CAS_COUNT_RD:MBOX6C0",
    "CAS_COUNT_WR:MBOX6C1",       "CAS_COUNT_RD:MBOX7C0",
    "CAS_COUNT_WR:MBOX7C1"};

constexpr const char* TLB_DATA_CTR[kNrOfCounters[10]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "DTLB_LOAD_MISSES_CAUSES_A_WALK:PMC0",
    "DTLB_STORE_MISSES_CAUSES_A_WALK:PMC1",
    "DTLB_LOAD_MISSES_WALK_DURATION:PMC2",
    "DTLB_STORE_MISSES_WALK_DURATION:PMC3"};

constexpr const char* TLB_INSTR_CTR[kNrOfCounters[11]] = {
    "INSTR_RETIRED_ANY:FIXC0", "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "ITLB_MISSES_CAUSES_A_WALK:PMC0",
    "ITLB_MISSES_WALK_DURATION:PMC1"};

constexpr const char* FLOPS_AVX_CTR[kNrOfCounters[12]] = {
    "INSTR_RETIRED_ANY:FIXC0", "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2", "AVX_INSTS_CALC:PMC0"};

constexpr const char* CYCLE_ACTIVITY_CTR[kNrOfCounters[13]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "CYCLE_ACTIVITY_STALLS_L2_PENDING:PMC0",
    "CYCLE_ACTIVITY_STALLS_LDM_PENDING:PMC1",
    "CYCLE_ACTIVITY_STALLS_L1D_PENDING:PMC2",
    "CYCLE_ACTIVITY_CYCLES_NO_EXECUTE:PMC3"};

constexpr const char* PORT_USAGE_CTR[kNrOfCounters[14]] = {
    "INSTR_RETIRED_ANY:FIXC0",        "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",     "UOPS_EXECUTED_PORT_PORT_0:PMC0",
    "UOPS_EXECUTED_PORT_PORT_1:PMC1", "UOPS_EXECUTED_PORT_PORT_2:PMC2",
    "UOPS_EXECUTED_PORT_PORT_3:PMC3", "UOPS_EXECUTED_PORT_PORT_4:PMC4",
    "UOPS_EXECUTED_PORT_PORT_5:PMC5", "UOPS_EXECUTED_PORT_PORT_6:PMC6",
    "UOPS_EXECUTED_PORT_PORT_7:PMC7"};

constexpr const char* UOPS_CTR[kNrOfCounters[15]] = {
    "INSTR_RETIRED_ANY:FIXC0",     "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",  "UOPS_ISSUED_ANY:PMC0",
    "UOPS_EXECUTED_THREAD:PMC1",   "UOPS_RETIRED_ALL:PMC2",
    "UOPS_ISSUED_FLAGS_MERGE:PMC3"};

constexpr const char* UOPS_EXEC_CTR[kNrOfCounters[16]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "UOPS_EXECUTED_USED_CYCLES:PMC0",
    "UOPS_EXECUTED_STALL_CYCLES:PMC1",
    "CPU_CLOCK_UNHALTED_TOTAL_CYCLES:PMC2",
    "UOPS_EXECUTED_STALL_CYCLES:PMC3:EDGEDETECT"};

constexpr const char* UOPS_ISSUE_CTR[kNrOfCounters[17]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "UOPS_ISSUED_USED_CYCLES:PMC0",
    "UOPS_ISSUED_STALL_CYCLES:PMC1",
    "CPU_CLOCK_UNHALTED_TOTAL_CYCLES:PMC2",
    "UOPS_ISSUED_STALL_CYCLES:PMC3:EDGEDETECT"};

constexpr const char* UOPS_RETIRE_CTR[kNrOfCounters[18]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "UOPS_RETIRED_USED_CYCLES:PMC0",
    "UOPS_RETIRED_STALL_CYCLES:PMC1",
    "CPU_CLOCK_UNHALTED_TOTAL_CYCLES:PMC2",
    "UOPS_RETIRED_STALL_CYCLES:PMC3:EDGEDETECT"};

constexpr const char* CACHES_CTR[kNrOfCounters[19]] = {
    "INSTR_RETIRED_ANY:FIXC0",
    "CPU_CLK_UNHALTED_CORE:FIXC1",
    "CPU_CLK_UNHALTED_REF:FIXC2",
    "L1D_REPLACEMENT:PMC0",
    "L1D_M_EVICT:PMC1",
    "L2_LINES_IN_ALL:PMC2",
    "L2_TRANS_L2_WB:PMC3",
    "LLC_LOOKUP_DATA_READ:CBOX0C0",
    "LLC_LOOKUP_DATA_READ:CBOX1C0",
    "LLC_LOOKUP_DATA_READ:CBOX2C0",
    "LLC_LOOKUP_DATA_READ:CBOX3C0",
    "LLC_LOOKUP_DATA_READ:CBOX4C0",
    "LLC_LOOKUP_DATA_READ:CBOX5C0",
    "LLC_LOOKUP_DATA_READ:CBOX6C0",
    "LLC_LOOKUP_DATA_READ:CBOX7C0",
    "LLC_LOOKUP_DATA_READ:CBOX8C0",
    "LLC_LOOKUP_DATA_READ:CBOX9C0",
    "LLC_LOOKUP_DATA_READ:CBOX10C0",
    "LLC_LOOKUP_DATA_READ:CBOX11C0",
    "LLC_LOOKUP_DATA_READ:CBOX12C0",
    "LLC_LOOKUP_DATA_READ:CBOX13C0",
    "LLC_LOOKUP_DATA_READ:CBOX14C0",
    "LLC_LOOKUP_DATA_READ:CBOX15C0",
    "LLC_LOOKUP_DATA_READ:CBOX16C0",
    "LLC_LOOKUP_DATA_READ:CBOX17C0",
    "LLC_VICTIMS_M:CBOX0C1",
    "LLC_VICTIMS_M:CBOX1C1",
    "LLC_VICTIMS_M:CBOX2C1",
    "LLC_VICTIMS_M:CBOX3C1",
    "LLC_VICTIMS_M:CBOX4C1",
    "LLC_VICTIMS_M:CBOX5C1",
    "LLC_VICTIMS_M:CBOX6C1",
    "LLC_VICTIMS_M:CBOX7C1",
    "LLC_VICTIMS_M:CBOX8C1",
    "LLC_VICTIMS_M:CBOX9C1",
    "LLC_VICTIMS_M:CBOX10C1",
    "LLC_VICTIMS_M:CBOX11C1",
    "LLC_VICTIMS_M:CBOX12C1",
    "LLC_VICTIMS_M:CBOX13C1",
    "CAS_COUNT_RD:MBOX0C0",
    "CAS_COUNT_WR:MBOX0C1",
    "CAS_COUNT_RD:MBOX1C0",
    "CAS_COUNT_WR:MBOX1C1",
    "CAS_COUNT_RD:MBOX2C0",
    "CAS_COUNT_WR:MBOX2C1",
    "CAS_COUNT_RD:MBOX3C0",
    "CAS_COUNT_WR:MBOX3C1",
    "CAS_COUNT_RD:MBOX4C0",
    "CAS_COUNT_WR:MBOX4C1",
    "CAS_COUNT_RD:MBOX5C0",
    "CAS_COUNT_WR:MBOX5C1",
    "CAS_COUNT_RD:MBOX6C0",
    "CAS_COUNT_WR:MBOX6C1",
    "CAS_COUNT_RD:MBOX7C0",
    "CAS_COUNT_WR:MBOX7C1"};

constexpr std::array<const char* const*, kNumberOfGroups> eventsets = {
    BRANCH_CTR,      CLOCK_CTR,
    DATA_CTR,        ENERGY_CTR,
    ICACHE_CTR,      L2_CTR,
    L2CACHE_CTR,     L3_CTR,
    L3CACHE_CTR,     MEM_CTR,
    TLB_DATA_CTR,    TLB_INSTR_CTR,
    FLOPS_AVX_CTR,   CYCLE_ACTIVITY_CTR,
    PORT_USAGE_CTR,  UOPS_CTR,
    UOPS_EXEC_CTR,   UOPS_ISSUE_CTR,
    UOPS_RETIRE_CTR, CACHES_CTR};

constexpr const int kNrOfMetrics[kNumberOfGroups] = {
    8, 6, 5, 11, 11, 10, 7, 10, 7, 10, 10, 7, 6, 8, 12, 8, 7, 7, 7, 28};

constexpr const char* BRANCH_METRIC_NAMES[kNrOfMetrics[0]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "Branch rate",
    "Branch misprediction rate",
    "Branch misprediction ratio",
    "Instructions per branch"};

constexpr const char* CLOCK_METRIC_NAMES[kNrOfMetrics[1]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]",
    "Clock [MHz]",         "CPI",
    "Energy [J]",          "Power [W]"};

constexpr const char* DATA_METRIC_NAMES[kNrOfMetrics[2]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]", "Clock [MHz]", "CPI",
    "Load to store ratio"};

constexpr const char* ENERGY_METRIC_NAMES[kNrOfMetrics[3]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]",
    "Clock [MHz]",         "CPI",
    "Temperature [C]",     "Energy [J]",
    "Power [W]",           "Energy PP0 [J]",
    "Power PP0 [W]",       "Energy DRAM [J]",
    "Power DRAM [W]"};

constexpr const char* ICACHE_METRIC_NAMES[kNrOfMetrics[4]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "L1I request rate",
    "L1I miss rate",
    "L1I miss ratio",
    "L1I stalls",
    "L1I stall rate",
    "L1I queue full stalls",
    "L1I queue full stall rate"};

constexpr const char* L2_METRIC_NAMES[kNrOfMetrics[5]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "L2D load bandwidth [MBytes/s]",
    "L2D load data volume [GBytes]",
    "L2D evict bandwidth [MBytes/s]",
    "L2D evict data volume [GBytes]",
    "L2 bandwidth [MBytes/s]",
    "L2 data volume [GBytes]"};

constexpr const char* L2CACHE_METRIC_NAMES[kNrOfMetrics[6]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]", "Clock [MHz]",  "CPI",
    "L2 request rate",     "L2 miss rate",         "L2 miss ratio"};

constexpr const char* L3_METRIC_NAMES[kNrOfMetrics[7]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "L3 load bandwidth [MBytes/s]",
    "L3 load data volume [GBytes]",
    "L3 evict bandwidth [MBytes/s]",
    "L3 evict data volume [GBytes]",
    "L3 bandwidth [MBytes/s]",
    "L3 data volume [GBytes]"};

constexpr const char* L3CACHE_METRIC_NAMES[kNrOfMetrics[8]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]", "Clock [MHz]",  "CPI",
    "L3 request rate",     "L3 miss rate",         "L3 miss ratio"};

constexpr const char* MEM_METRIC_NAMES[kNrOfMetrics[9]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "Memory read bandwidth [MBytes/s]",
    "Memory read data volume [GBytes]",
    "Memory write bandwidth [MBytes/s]",
    "Memory write data volume [GBytes]",
    "Memory bandwidth [MBytes/s]",
    "Memory data volume [GBytes]"};

constexpr const char* TLB_DATA_METRIC_NAMES[kNrOfMetrics[10]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "L1 DTLB load misses",
    "L1 DTLB load miss rate",
    "L1 DTLB load miss duration",
    "L1 DTLB store misses",
    "L1 DTLB store miss rate",
    "L1 DTLB store miss duration [Cyc]"};

constexpr const char* TLB_INSTR_METRIC_NAMES[kNrOfMetrics[11]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "L1 ITLB misses",
    "L1 ITLB miss rate",
    "L1 ITLB miss duration [Cyc]"};

constexpr const char* FLOPS_AVX_METRIC_NAMES[kNrOfMetrics[12]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]", "Clock [MHz]", "CPI",
    "Packed SP MFLOP/s",   "Packed DP MFLOP/s"};

constexpr const char* CYCLE_ACTIVITY_METRIC_NAMES[kNrOfMetrics[13]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "Cycles without execution [%]",
    "Cycles without execution due to L1D [%]",
    "Cycles without execution due to L2 [%]",
    "Cycles without execution due to memory [%]"};

constexpr const char* PORT_USAGE_METRIC_NAMES[kNrOfMetrics[14]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]",
    "Clock [MHz]",         "CPI",
    "Port0 usage ratio",   "Port1 usage ratio",
    "Port2 usage ratio",   "Port3 usage ratio",
    "Port4 usage ratio",   "Port5 usage ratio",
    "Port6 usage ratio",   "Port7 usage ratio"};

constexpr const char* UOPS_METRIC_NAMES[kNrOfMetrics[15]] = {
    "Runtime (RDTSC) [s]", "Runtime unhalted [s]",
    "Clock [MHz]",         "CPI",
    "Issued UOPs",         "Merged UOPs",
    "Executed UOPs",       "Retired UOPs"};

constexpr const char* UOPS_EXEC_METRIC_NAMES[kNrOfMetrics[16]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "Used cycles ratio [%]",
    "Unused cycles ratio [%]",
    "Avg stall duration [cycles]"};

constexpr const char* UOPS_ISSUE_METRIC_NAMES[kNrOfMetrics[17]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "Used cycles ratio [%]",
    "Unused cycles ratio [%]",
    "Avg stall duration [cycles]"};

constexpr const char* UOPS_RETIRE_METRIC_NAMES[kNrOfMetrics[18]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "Used cycles ratio [%]",
    "Unused cycles ratio [%]",
    "Avg stall duration [cycles]"};

constexpr const char* CACHES_METRIC_NAMES[kNrOfMetrics[19]] = {
    "Runtime (RDTSC) [s]",
    "Runtime unhalted [s]",
    "Clock [MHz]",
    "CPI",
    "L2 to L1 load bandwidth [MBytes/s]",
    "L2 to L1 load data volume [GBytes]",
    "L1 to L2 evict bandwidth [MBytes/s]",
    "L1 to L2 evict data volume [GBytes]",
    "L1 to/from L2 bandwidth [MBytes/s]",
    "L1 to/from L2 data volume [GBytes]",
    "L3 to L2 load bandwidth [MBytes/s]",
    "L3 to L2 load data volume [GBytes]",
    "L2 to L3 evict bandwidth [MBytes/s]",
    "L2 to L3 evict data volume [GBytes]",
    "L2 to/from L3 bandwidth [MBytes/s]",
    "L2 to/from L3 data volume [GBytes]",
    "System to L3 bandwidth [MBytes/s]",
    "System to L3 data volume [GBytes]",
    "L3 to system bandwidth [MBytes/s]",
    "L3 to system data volume [GBytes]",
    "L3 to/from system bandwidth [MBytes/s]",
    "L3 to/from system data volume [GBytes]",
    "Memory read bandwidth [MBytes/s]",
    "Memory read data volume [GBytes]",
    "Memory write bandwidth [MBytes/s]",
    "Memory write data volume [GBytes]",
    "Memory bandwidth [MBytes/s]",
    "Memory data volume [GBytes]"};

constexpr std::array<const char* const*, kNumberOfGroups> metric_names = {
    BRANCH_METRIC_NAMES,      CLOCK_METRIC_NAMES,
    DATA_METRIC_NAMES,        ENERGY_METRIC_NAMES,
    ICACHE_METRIC_NAMES,      L2_METRIC_NAMES,
    L2CACHE_METRIC_NAMES,     L3_METRIC_NAMES,
    L3CACHE_METRIC_NAMES,     MEM_METRIC_NAMES,
    TLB_DATA_METRIC_NAMES,    TLB_INSTR_METRIC_NAMES,
    FLOPS_AVX_METRIC_NAMES,   CYCLE_ACTIVITY_METRIC_NAMES,
    PORT_USAGE_METRIC_NAMES,  UOPS_METRIC_NAMES,
    UOPS_EXEC_METRIC_NAMES,   UOPS_ISSUE_METRIC_NAMES,
    UOPS_RETIRE_METRIC_NAMES, CACHES_METRIC_NAMES};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    BRANCH_METRIC_FUNS[kNrOfMetrics[0]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Branch rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/FIXC0
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[0]);
        },
        // Branch misprediction rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/FIXC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[0]);
        },
        // Branch misprediction ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[3]);
        },
        // Instructions per branch
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC0/PMC0
          return static_cast<double>(counter_values[0]) /
                 static_cast<double>(counter_values[3]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    CLOCK_METRIC_FUNS[kNrOfMetrics[1]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Energy [J]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR0
          return static_cast<double>(counter_values[3]);
        },
        // Power [W]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR0/time
          return static_cast<double>(counter_values[3]) /
                 perfmon_getTimeOfGroup(group_id);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    DATA_METRIC_FUNS[kNrOfMetrics[2]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);

        },
        // Load to store ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/PMC1
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[4]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    ENERGY_METRIC_FUNS[kNrOfMetrics[3]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Temperature [C]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // TMP0
          return static_cast<double>(counter_values[3]);
        },
        // Energy [J]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR0
          return static_cast<double>(counter_values[4]);
        },
        // Power [W]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR0/time
          return static_cast<double>(counter_values[4]) /
                 perfmon_getTimeOfGroup(group_id);
        },
        // Energy PP0 [J]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR1
          return static_cast<double>(counter_values[5]);
        },
        // Power PP0 [W]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR1/time
          return static_cast<double>(counter_values[5]) /
                 perfmon_getTimeOfGroup(group_id);
        },
        // Energy DRAM [J]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR3
          return static_cast<double>(counter_values[6]);
        },
        // Power DRAM [W]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PWR3/time
          return static_cast<double>(counter_values[6]) /
                 perfmon_getTimeOfGroup(group_id);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    ICACHE_METRIC_FUNS[kNrOfMetrics[4]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1I request rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/FIXC0
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1I miss rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/FIXC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1I miss ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[3]);
        },
        // L1I stalls
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC2
          return static_cast<double>(counter_values[5]);
        },
        // L1I stall rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC2/FIXC0
          return static_cast<double>(counter_values[5]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1I queue full stalls
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC3
          return static_cast<double>(counter_values[6]);
        },
        // L1I queue full stall rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC3/FIXC0
          return static_cast<double>(counter_values[6]) /
                 static_cast<double>(counter_values[0]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    L2_METRIC_FUNS[kNrOfMetrics[5]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L2D load bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC0*64.0/time
          return 1e-6 * static_cast<double>(counter_values[3]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L2D load data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC0*64.0
          return 1e-9 * static_cast<double>(counter_values[3]) * 64.0;
        },
        // L2D evict bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC1*64.0/time
          return 1e-6 * static_cast<double>(counter_values[4]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L2D evict data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC1*64.0
          return 1e-9 * static_cast<double>(counter_values[4]) * 64.0;
        },
        // L2 bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(PMC0+PMC1+PMC2)*64.0/time
          return 1e-6 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[4]) +
                         static_cast<double>(counter_values[5])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // L2 data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(PMC0+PMC1+PMC2)*64.0
          return 1e-9 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[4]) +
                         static_cast<double>(counter_values[5])) *
                 64.0;
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    L2CACHE_METRIC_FUNS[kNrOfMetrics[6]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L2 request rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/FIXC0
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[0]);
        },
        // L2 miss rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/FIXC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[0]);
        },
        // L2 miss ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[3]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    L3_METRIC_FUNS[kNrOfMetrics[7]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L3 load bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC0*64.0/time
          return 1e-6 * static_cast<double>(counter_values[3]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L3 load data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC0*64.0
          return 1e-9 * static_cast<double>(counter_values[3]) * 64.0;
        },
        // L3 evict bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC1*64.0/time
          return 1e-6 * static_cast<double>(counter_values[4]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L3 evict data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC1*64.0
          return 1e-9 * static_cast<double>(counter_values[4]) * 64.0;
        },
        // L3 bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(PMC0+PMC1)*64.0/time
          return 1e-6 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[4])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // L3 data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(PMC0+PMC1)*64.0
          return 1e-9 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[4])) *
                 64.0;
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    L3CACHE_METRIC_FUNS[kNrOfMetrics[8]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L3 request rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/PMC2
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[5]);
        },
        // L3 miss rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC2
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[5]);
        },
        // L3 miss ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[3]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    MEM_METRIC_FUNS[kNrOfMetrics[9]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[2]) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Memory read bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(MBOX0C0+MBOX1C0+MBOX2C0+
          //          MBOX3C0+MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0)*64.0/time
          return 1e-6 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[5]) +
                         static_cast<double>(counter_values[7]) +
                         static_cast<double>(counter_values[9]) +
                         static_cast<double>(counter_values[11]) +
                         static_cast<double>(counter_values[13]) +
                         static_cast<double>(counter_values[15]) +
                         static_cast<double>(counter_values[17])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // Memory read data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+
          //          MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0)*64.0
          return 1e-9 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[5]) +
                         static_cast<double>(counter_values[7]) +
                         static_cast<double>(counter_values[9]) +
                         static_cast<double>(counter_values[11]) +
                         static_cast<double>(counter_values[13]) +
                         static_cast<double>(counter_values[15]) +
                         static_cast<double>(counter_values[17])) *
                 64.0;
        },
        // Memory write bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+
          //          MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0/time
          return 1e-6 * (static_cast<double>(counter_values[4]) +
                         static_cast<double>(counter_values[6]) +
                         static_cast<double>(counter_values[8]) +
                         static_cast<double>(counter_values[10]) +
                         static_cast<double>(counter_values[12]) +
                         static_cast<double>(counter_values[14]) +
                         static_cast<double>(counter_values[16]) +
                         static_cast<double>(counter_values[18])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // Memory write data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+
          //          MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0
          return 1e-9 * (static_cast<double>(counter_values[4]) +
                         static_cast<double>(counter_values[6]) +
                         static_cast<double>(counter_values[8]) +
                         static_cast<double>(counter_values[10]) +
                         static_cast<double>(counter_values[12]) +
                         static_cast<double>(counter_values[14]) +
                         static_cast<double>(counter_values[16]) +
                         static_cast<double>(counter_values[18])) *
                 64.0;
        },
        // Memory bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+
          //          MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0+
          //          MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+
          //          MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0/time
          return 1e-6 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[4]) +
                         static_cast<double>(counter_values[5]) +
                         static_cast<double>(counter_values[6]) +
                         static_cast<double>(counter_values[7]) +
                         static_cast<double>(counter_values[9]) +
                         static_cast<double>(counter_values[10]) +
                         static_cast<double>(counter_values[11]) +
                         static_cast<double>(counter_values[12]) +
                         static_cast<double>(counter_values[13]) +
                         static_cast<double>(counter_values[14]) +
                         static_cast<double>(counter_values[15]) +
                         static_cast<double>(counter_values[16]) +
                         static_cast<double>(counter_values[17]) +
                         static_cast<double>(counter_values[18])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // Memory data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+
          //          MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0+
          //          MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+
          //          MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0
          return 1e-9 * (static_cast<double>(counter_values[3]) +
                         static_cast<double>(counter_values[4]) +
                         static_cast<double>(counter_values[5]) +
                         static_cast<double>(counter_values[6]) +
                         static_cast<double>(counter_values[7]) +
                         static_cast<double>(counter_values[8]) +
                         static_cast<double>(counter_values[9]) +
                         static_cast<double>(counter_values[10]) +
                         static_cast<double>(counter_values[11]) +
                         static_cast<double>(counter_values[12]) +
                         static_cast<double>(counter_values[13]) +
                         static_cast<double>(counter_values[14]) +
                         static_cast<double>(counter_values[15]) +
                         static_cast<double>(counter_values[16]) +
                         static_cast<double>(counter_values[17]) +
                         static_cast<double>(counter_values[18])) *
                 64.0;
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    TLB_DATA_METRIC_FUNS[kNrOfMetrics[10]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1 DTLB load misses
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0
          return static_cast<double>(counter_values[3]);
        },
        // L1 DTLB load miss rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/FIXC0
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1 DTLB load miss duration
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC2/PMC0
          return static_cast<double>(counter_values[5]) /
                 static_cast<double>(counter_values[3]);
        },
        // L1 DTLB store misses
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1
          return static_cast<double>(counter_values[4]);
        },
        // L1 DTLB store miss rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/FIXC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1 DTLB store miss duration [Cyc]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC3/PMC1
          return static_cast<double>(counter_values[6]) /
                 static_cast<double>(counter_values[4]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    TLB_INSTR_METRIC_FUNS[kNrOfMetrics[11]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1 ITLB misses
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0
          return static_cast<double>(counter_values[3]);
        },
        // L1 ITLB miss rate
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/FIXC0
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[0]);
        },
        // L1 ITLB miss duration [Cyc]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC0
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[3]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    FLOPS_AVX_METRIC_FUNS[kNrOfMetrics[12]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Packed SP MFLOP/s
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(PMC0*8.0)/time
          return 1e-6 * static_cast<double>(counter_values[3]) * 8 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // Packed DP MFLOP/s
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(PMC0*8.0)/time
          return 1e-6 * static_cast<double>(counter_values[3]) * 4 /
                 perfmon_getTimeOfGroup(group_id);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    CYCLE_ACTIVE_METRIC_FUNS[kNrOfMetrics[13]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Cycles without execution [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC3/FIXC1*100
          return static_cast<double>(counter_values[6]) /
                 static_cast<double>(counter_values[1]) * 100.0;
        },
        // Cycles without execution due to L1D [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC2/FIXC1*100
          return static_cast<double>(counter_values[5]) /
                 static_cast<double>(counter_values[1]) * 100.0;
        },
        // Cycles without execution due to L2 [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/FIXC1*100
          return static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[1]) * 100.0;
        },
        // Cycles without execution due to memory [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/FIXC1*100
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[1]) * 100.0;
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    PORT_USAGE_METRIC_FUNS[kNrOfMetrics[14]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Port0 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 0]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port1 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 1]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port2 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC2/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 2]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port3 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC3/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 3]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port4 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC4/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 4]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port5 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC5/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 5]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port6 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC6/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 6]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
        // Port7 usage ratio
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC7/(PMC0+PMC1+PMC2+PMC3+PMC4+PMC5+PMC6+PMC7)
          return static_cast<double>(counter_values[3 + 7]) /
                 static_cast<double>(std::accumulate(counter_values.begin() + 3,
                                                     counter_values.end(),
                                                     static_cast<uint64_t>(0)));
        },
};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    UOPS_METRIC_FUNS[kNrOfMetrics[15]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Issued UOPs
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC0
          return static_cast<double>(counter_values[3]);
        },
        // Merged UOPS
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC3
          return static_cast<double>(counter_values[6]);
        },
        // Executed UOPs
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1
          return static_cast<double>(counter_values[4]);
        },
        // Retired UOPs
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC2
          return static_cast<double>(counter_values[5]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    UOPS_EXEC_METRIC_FUNS[kNrOfMetrics[16]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // Used cycles ratio [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 100*PMC0/PMC2
          return 100.0 * static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[5]);
        },
        // Unused cycles ratio [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 100*PMC1/PMC2
          return 100.0 * static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[5]);
        },
        // Avg stall duration [cycles]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC3:EDGEDETECT
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[6]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    UOPS_ISSUE_METRIC_FUNS[kNrOfMetrics[17]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },  // Used cycles ratio [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 100*PMC0/PMC2
          return 100.0 * static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[5]);
        },
        // Unused cycles ratio [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 100*PMC1/PMC2
          return 100.0 * static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[5]);
        },
        // Avg stall duration [cycles]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC3:EDGEDETECT
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[6]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    UOPS_RETIRE_METRIC_FUNS[kNrOfMetrics[18]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },  // Used cycles ratio [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 100*PMC0/PMC2
          return 100.0 * static_cast<double>(counter_values[3]) /
                 static_cast<double>(counter_values[5]);
        },
        // Unused cycles ratio [%]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 100*PMC1/PMC2
          return 100.0 * static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[5]);
        },
        // Avg stall duration [cycles]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // PMC1/PMC3:EDGEDETECT
          return static_cast<double>(counter_values[4]) /
                 static_cast<double>(counter_values[6]);
        }};

static const std::function<double(int, const std::vector<uint64_t>&,
                                  const LikwidProfilerState&)>
    CACHES_FUNS[kNrOfMetrics[19]] = {
        // Runtime (RDTSC) [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // time
          return perfmon_getTimeOfGroup(group_id);
        },
        // Runtime unhalted [s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1*inverseClock
          return static_cast<double>(counter_values[1]) /
                 state.cpu_info_->clock;
        },
        // Clock [MHz]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.E-06*(FIXC1/FIXC2)/inverseClock
          return 1e-6 * (static_cast<double>(counter_values[1]) /
                         static_cast<double>(counter_values[2])) *
                 state.cpu_info_->clock;
        },
        // CPI
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // FIXC1/FIXC0
          return static_cast<double>(counter_values[1]) /
                 static_cast<double>(counter_values[0]);
        },
        // L2 to L1 load bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC0*64.0/time
          return 1.0E-06 * static_cast<double>(counter_values[3]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L2 to L1 load data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC0*64.0
          return 1.0E-09 * static_cast<double>(counter_values[3]) * 64.0;
        },
        // L1 to L2 evict bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC1*64.0/time
          return 1.0E-06 * static_cast<double>(counter_values[4]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L1 to L2 evict data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC1*64.0
          return 1.0E-09 * static_cast<double>(counter_values[4]) * 64.0;
        },
        // L1 to/from L2 bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(PMC0+PMC1)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[3]) +
                            static_cast<double>(counter_values[4])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // L1 to/from L2 data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(PMC0+PMC1)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[3]) +
                            static_cast<double>(counter_values[4])) *
                 64.0;
        },
        // L3 to L2 load bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC2*64.0/time
          return 1.0E-06 * static_cast<double>(counter_values[5]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L3 to L2 load data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*PMC2*64.0
          return 1.0E-09 * static_cast<double>(counter_values[5]) * 64.0;
        },
        // L2 to L3 evict bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC3*64.0/time
          return 1.0E-06 * static_cast<double>(counter_values[6]) * 64.0 /
                 perfmon_getTimeOfGroup(group_id);
        },
        // L2 to L3 evict data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*PMC3*64.0
          return 1.0E-06 * static_cast<double>(counter_values[6]) * 64.0;
        },
        // L2 to/from L3 bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(PMC2+PMC3)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[5]) +
                            static_cast<double>(counter_values[6])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // L2 to/from L3 data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(PMC2+PMC3)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[5]) +
                            static_cast<double>(counter_values[6])) *
                 64.0;
        },
        // System to L3 bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(CBOX0C0+CBOX1C0+CBOX2C0+CBOX3C0+CBOX4C0+CBOX5C0+CBOX6C0+CBOX7C0+CBOX8C0+CBOX9C0+CBOX10C0+CBOX11C0+CBOX12C0+CBOX13C0)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[7]) +
                            static_cast<double>(counter_values[8]) +
                            static_cast<double>(counter_values[9]) +
                            static_cast<double>(counter_values[10]) +
                            static_cast<double>(counter_values[11]) +
                            static_cast<double>(counter_values[12]) +
                            static_cast<double>(counter_values[13]) +
                            static_cast<double>(counter_values[14]) +
                            static_cast<double>(counter_values[15]) +
                            static_cast<double>(counter_values[16]) +
                            static_cast<double>(counter_values[17]) +
                            static_cast<double>(counter_values[18]) +
                            static_cast<double>(counter_values[19]) +
                            static_cast<double>(counter_values[20])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // System to L3 data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(CBOX0C0+CBOX1C0+CBOX2C0+CBOX3C0+CBOX4C0+CBOX5C0+CBOX6C0+CBOX7C0+CBOX8C0+CBOX9C0+CBOX10C0+CBOX11C0+CBOX12C0+CBOX13C0)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[7]) +
                            static_cast<double>(counter_values[8]) +
                            static_cast<double>(counter_values[9]) +
                            static_cast<double>(counter_values[10]) +
                            static_cast<double>(counter_values[11]) +
                            static_cast<double>(counter_values[12]) +
                            static_cast<double>(counter_values[13]) +
                            static_cast<double>(counter_values[14]) +
                            static_cast<double>(counter_values[15]) +
                            static_cast<double>(counter_values[16]) +
                            static_cast<double>(counter_values[17]) +
                            static_cast<double>(counter_values[18]) +
                            static_cast<double>(counter_values[19]) +
                            static_cast<double>(counter_values[20])) *
                 64.0;
        },
        // L3 to system bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(CBOX0C1+CBOX1C1+CBOX2C1+CBOX3C1+CBOX4C1+CBOX5C1+CBOX6C1+CBOX7C1+CBOX8C1+CBOX9C1+CBOX10C1+CBOX11C1+CBOX12C1+CBOX13C1)*64/time
          return 1.0E-06 * (static_cast<double>(counter_values[25]) +
                            static_cast<double>(counter_values[26]) +
                            static_cast<double>(counter_values[27]) +
                            static_cast<double>(counter_values[28]) +
                            static_cast<double>(counter_values[29]) +
                            static_cast<double>(counter_values[30]) +
                            static_cast<double>(counter_values[31]) +
                            static_cast<double>(counter_values[32]) +
                            static_cast<double>(counter_values[33]) +
                            static_cast<double>(counter_values[34]) +
                            static_cast<double>(counter_values[35]) +
                            static_cast<double>(counter_values[36]) +
                            static_cast<double>(counter_values[37]) +
                            static_cast<double>(counter_values[38])) *
                 64 / perfmon_getTimeOfGroup(group_id);
        },
        // L3 to system data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(CBOX0C1+CBOX1C1+CBOX2C1+CBOX3C1+CBOX4C1+CBOX5C1+CBOX6C1+CBOX7C1+CBOX8C1+CBOX9C1+CBOX10C1+CBOX11C1+CBOX12C1+CBOX13C1)*64
          return 1.0E-09 * (static_cast<double>(counter_values[25]) +
                            static_cast<double>(counter_values[26]) +
                            static_cast<double>(counter_values[27]) +
                            static_cast<double>(counter_values[28]) +
                            static_cast<double>(counter_values[29]) +
                            static_cast<double>(counter_values[30]) +
                            static_cast<double>(counter_values[31]) +
                            static_cast<double>(counter_values[32]) +
                            static_cast<double>(counter_values[33]) +
                            static_cast<double>(counter_values[34]) +
                            static_cast<double>(counter_values[35]) +
                            static_cast<double>(counter_values[36]) +
                            static_cast<double>(counter_values[37]) +
                            static_cast<double>(counter_values[38])) *
                 64;
        },
        // L3 to/from system bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(CBOX0C0+CBOX1C0+CBOX2C0+CBOX3C0+CBOX4C0+CBOX5C0+CBOX6C0+CBOX7C0+CBOX8C0+CBOX9C0+CBOX10C0+CBOX11C0+CBOX12C0+CBOX13C0+CBOX0C1+CBOX1C1+CBOX2C1+CBOX3C1+CBOX4C1+CBOX5C1+CBOX6C1+CBOX7C1+CBOX8C1+CBOX9C1+CBOX10C1+CBOX11C1+CBOX12C1+CBOX13C1)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[7]) +
                            static_cast<double>(counter_values[8]) +
                            static_cast<double>(counter_values[9]) +
                            static_cast<double>(counter_values[10]) +
                            static_cast<double>(counter_values[11]) +
                            static_cast<double>(counter_values[12]) +
                            static_cast<double>(counter_values[13]) +
                            static_cast<double>(counter_values[14]) +
                            static_cast<double>(counter_values[15]) +
                            static_cast<double>(counter_values[16]) +
                            static_cast<double>(counter_values[17]) +
                            static_cast<double>(counter_values[18]) +
                            static_cast<double>(counter_values[19]) +
                            static_cast<double>(counter_values[20]) +
                            static_cast<double>(counter_values[25]) +
                            static_cast<double>(counter_values[26]) +
                            static_cast<double>(counter_values[27]) +
                            static_cast<double>(counter_values[28]) +
                            static_cast<double>(counter_values[29]) +
                            static_cast<double>(counter_values[30]) +
                            static_cast<double>(counter_values[31]) +
                            static_cast<double>(counter_values[32]) +
                            static_cast<double>(counter_values[33]) +
                            static_cast<double>(counter_values[34]) +
                            static_cast<double>(counter_values[35]) +
                            static_cast<double>(counter_values[36]) +
                            static_cast<double>(counter_values[37]) +
                            static_cast<double>(counter_values[38])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // L3 to/from system data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(CBOX0C0+CBOX1C0+CBOX2C0+CBOX3C0+CBOX4C0+CBOX5C0+CBOX6C0+CBOX7C0+CBOX8C0+CBOX9C0+CBOX10C0+CBOX11C0+CBOX12C0+CBOX13C0+CBOX0C1+CBOX1C1+CBOX2C1+CBOX3C1+CBOX4C1+CBOX5C1+CBOX6C1+CBOX7C1+CBOX8C1+CBOX9C1+CBOX10C1+CBOX11C1+CBOX12C1+CBOX13C1)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[7]) +
                            static_cast<double>(counter_values[8]) +
                            static_cast<double>(counter_values[9]) +
                            static_cast<double>(counter_values[10]) +
                            static_cast<double>(counter_values[11]) +
                            static_cast<double>(counter_values[12]) +
                            static_cast<double>(counter_values[13]) +
                            static_cast<double>(counter_values[14]) +
                            static_cast<double>(counter_values[15]) +
                            static_cast<double>(counter_values[16]) +
                            static_cast<double>(counter_values[17]) +
                            static_cast<double>(counter_values[18]) +
                            static_cast<double>(counter_values[19]) +
                            static_cast<double>(counter_values[20]) +
                            static_cast<double>(counter_values[25]) +
                            static_cast<double>(counter_values[26]) +
                            static_cast<double>(counter_values[27]) +
                            static_cast<double>(counter_values[28]) +
                            static_cast<double>(counter_values[29]) +
                            static_cast<double>(counter_values[30]) +
                            static_cast<double>(counter_values[31]) +
                            static_cast<double>(counter_values[32]) +
                            static_cast<double>(counter_values[33]) +
                            static_cast<double>(counter_values[34]) +
                            static_cast<double>(counter_values[35]) +
                            static_cast<double>(counter_values[36]) +
                            static_cast<double>(counter_values[37]) +
                            static_cast<double>(counter_values[38])) *
                 64.0;
        },
        // Memory read bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[39]) +
                            static_cast<double>(counter_values[41]) +
                            static_cast<double>(counter_values[43]) +
                            static_cast<double>(counter_values[45]) +
                            static_cast<double>(counter_values[47]) +
                            static_cast<double>(counter_values[49]) +
                            static_cast<double>(counter_values[51]) +
                            static_cast<double>(counter_values[53])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // Memory read data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[39]) +
                            static_cast<double>(counter_values[41]) +
                            static_cast<double>(counter_values[43]) +
                            static_cast<double>(counter_values[45]) +
                            static_cast<double>(counter_values[47]) +
                            static_cast<double>(counter_values[49]) +
                            static_cast<double>(counter_values[51]) +
                            static_cast<double>(counter_values[53])) *
                 64.0;
        },
        // Memory write bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[40]) +
                            static_cast<double>(counter_values[42]) +
                            static_cast<double>(counter_values[44]) +
                            static_cast<double>(counter_values[46]) +
                            static_cast<double>(counter_values[48]) +
                            static_cast<double>(counter_values[50]) +
                            static_cast<double>(counter_values[52]) +
                            static_cast<double>(counter_values[54])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // Memory write data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[40]) +
                            static_cast<double>(counter_values[42]) +
                            static_cast<double>(counter_values[44]) +
                            static_cast<double>(counter_values[46]) +
                            static_cast<double>(counter_values[48]) +
                            static_cast<double>(counter_values[50]) +
                            static_cast<double>(counter_values[52]) +
                            static_cast<double>(counter_values[54])) *
                 64.0;
        },
        // Memory bandwidth [MBytes/s]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-06*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0+MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0/time
          return 1.0E-06 * (static_cast<double>(counter_values[39]) +
                            static_cast<double>(counter_values[41]) +
                            static_cast<double>(counter_values[43]) +
                            static_cast<double>(counter_values[45]) +
                            static_cast<double>(counter_values[47]) +
                            static_cast<double>(counter_values[49]) +
                            static_cast<double>(counter_values[51]) +
                            static_cast<double>(counter_values[53]) +
                            static_cast<double>(counter_values[40]) +
                            static_cast<double>(counter_values[42]) +
                            static_cast<double>(counter_values[44]) +
                            static_cast<double>(counter_values[46]) +
                            static_cast<double>(counter_values[48]) +
                            static_cast<double>(counter_values[50]) +
                            static_cast<double>(counter_values[52]) +
                            static_cast<double>(counter_values[54])) *
                 64.0 / perfmon_getTimeOfGroup(group_id);
        },
        // Memory data volume [GBytes]
        [](int group_id, const std::vector<uint64_t>& counter_values,
           const LikwidProfilerState& state) {
          // 1.0E-09*(MBOX0C0+MBOX1C0+MBOX2C0+MBOX3C0+MBOX4C0+MBOX5C0+MBOX6C0+MBOX7C0+MBOX0C1+MBOX1C1+MBOX2C1+MBOX3C1+MBOX4C1+MBOX5C1+MBOX6C1+MBOX7C1)*64.0
          return 1.0E-09 * (static_cast<double>(counter_values[39]) +
                            static_cast<double>(counter_values[41]) +
                            static_cast<double>(counter_values[43]) +
                            static_cast<double>(counter_values[45]) +
                            static_cast<double>(counter_values[47]) +
                            static_cast<double>(counter_values[49]) +
                            static_cast<double>(counter_values[51]) +
                            static_cast<double>(counter_values[53]) +
                            static_cast<double>(counter_values[40]) +
                            static_cast<double>(counter_values[42]) +
                            static_cast<double>(counter_values[44]) +
                            static_cast<double>(counter_values[46]) +
                            static_cast<double>(counter_values[48]) +
                            static_cast<double>(counter_values[50]) +
                            static_cast<double>(counter_values[52]) +
                            static_cast<double>(counter_values[54])) *
                 64.0;
        },
};

static const std::array<
    const std::function<double(int, const std::vector<uint64_t>&,
                               const LikwidProfilerState&)>*,
    kNumberOfGroups>
    metric_functions = {BRANCH_METRIC_FUNS,      CLOCK_METRIC_FUNS,
                        DATA_METRIC_FUNS,        ENERGY_METRIC_FUNS,
                        ICACHE_METRIC_FUNS,      L2_METRIC_FUNS,
                        L2CACHE_METRIC_FUNS,     L3_METRIC_FUNS,
                        L3CACHE_METRIC_FUNS,     MEM_METRIC_FUNS,
                        TLB_DATA_METRIC_FUNS,    TLB_INSTR_METRIC_FUNS,
                        FLOPS_AVX_METRIC_FUNS,   CYCLE_ACTIVE_METRIC_FUNS,
                        PORT_USAGE_METRIC_FUNS,  UOPS_METRIC_FUNS,
                        UOPS_EXEC_METRIC_FUNS,   UOPS_ISSUE_METRIC_FUNS,
                        UOPS_RETIRE_METRIC_FUNS, CACHES_FUNS};

static const int kNumberOfSamples = 100000;

}  // namespace

namespace exahype {
namespace profilers {
namespace likwid {

LikwidPerformanceMonitoringModule::LikwidPerformanceMonitoringModule(
    const LikwidProfilerState& state, const std::string& group_name)
    : LikwidModule(state),
      group_name_(group_name),
      group_index_(std::distance(
          std::begin(groups), std::find_if(std::begin(groups), std::end(groups),
                                           [group_name](const char* group) {
                                             return group_name.compare(group) ==
                                                    0;
                                           }))) {
  int errcode;

  // Initialize perfmon module for CPU state_.cpu_
  errcode = perfmon_init(1, const_cast<int*>(&state_.cpu_));
  if (errcode != 0) {
    std::cerr << "LikwidPerformanceMonitoringModule: perfmon_init returned "
              << errcode << " != 0" << std::endl;
    std::exit(EXIT_FAILURE);
  }

  if (group_index_ == kNumberOfGroups) {
    std::cerr << "LikwidPerformanceMonitoringModule: group_name_ = "
              << group_name_ << " not found." << std::endl;
    std::exit(EXIT_FAILURE);
  }

  // overhead();
}

LikwidPerformanceMonitoringModule::~LikwidPerformanceMonitoringModule() {
  perfmon_finalize();
}

void LikwidPerformanceMonitoringModule::setNumberOfTags(int n) {
  group_handles_.reserve(n);
  counter_values_.reserve(n);
}

void LikwidPerformanceMonitoringModule::registerTag(const std::string& tag) {
  // Concatenate event string
  std::stringstream eventstring;
  for (int i = 0; i < kNrOfCounters[group_index_] - 1; i++) {
    eventstring << eventsets[group_index_][i] << ",";
  }
  // Skip comma for last counter
  eventstring << eventsets[group_index_][kNrOfCounters[group_index_] - 1];

  // Register event set for tag
  int handle = perfmon_addEventSet(const_cast<char*>(
      eventstring.str()
          .c_str()));  // TODO: remove const_cast once my PR has made it to LRZ
  if (handle < 0) {
    std::cerr << "LikwidPerformanceMonitoringModule: addEventSet returned "
              << handle << " < 0 for '" << eventsets[group_index_] << "'"
              << std::endl;
    std::exit(EXIT_FAILURE);
  }
  group_handles_[tag] = handle;

  // Allocate vector elements and initialize to zero
  // (kNumberOfCounters performance counters + last one counts how often stop
  // has been called for this tag)
  counter_values_[tag] = std::vector<uint64_t>(kNrOfCounters[group_index_] + 1);
}

void LikwidPerformanceMonitoringModule::start(const std::string& tag) {
  int errcode;

  errcode = perfmon_setupCounters(group_handles_.at(tag));
  assert((errcode >= 0) &&
         "LikwidPerformanceMonitoringModule: setupCounters failed");

  errcode = perfmon_startCounters();
  assert((errcode == 0) &&
         "LikwidPerformanceMonitoringModule: startCounters failed");
}

void LikwidPerformanceMonitoringModule::stop(const std::string& tag) {
  // Note: Tagged regions may not overlap, since stopCounters stops all groups
  int errcode;

  errcode = perfmon_stopCounters();
  assert((errcode == 0) &&
         "LikwidPerformanceMonitoringModule: stopCounters failed");

  for (int i = 0; i < kNrOfCounters[group_index_]; i++) {
    counter_values_[tag][i] += static_cast<uint64_t>(
        perfmon_getResult(group_handles_[tag], i, state_.cpu_));
    // Cast is unfortunate. Likwid converts uint32_t to double, we convert it
    // back to uint64_t.
  }

  // increment call counter
  counter_values_[tag][kNrOfCounters[group_index_]]++;
}

void LikwidPerformanceMonitoringModule::writeToOstream(std::ostream* os) const {
  // For all tags
  for (const auto& pair_tag_vector : counter_values_) {
    const auto& tag = pair_tag_vector.first;
    const auto& counter_values = pair_tag_vector.second;
    // print counter values
    for (int i = 0; i < kNrOfCounters[group_index_]; i++) {
      *os << "PerformanceMonitoringModule: " << tag << " "
          << eventsets[group_index_][i] << " " << counter_values[i]
          << std::endl;
    }

    // print count
    *os << "PerformanceMonitoringModule: " << tag << " count "
        << counter_values[kNrOfCounters[group_index_]] << std::endl;

    // print metrics
    for (int i = 0; i < kNrOfMetrics[group_index_]; i++) {
      *os << "PerformanceMonitoringModule: " << tag << " "
          << metric_names[group_index_][i] << " "
          << metric_functions[group_index_]
                             [i](group_handles_.at(tag), counter_values, state_)
          << std::endl;
    }
  }
}

void LikwidPerformanceMonitoringModule::overhead() {
  // MEM
  {
    // Concatenate event string
    std::stringstream eventstring;
    for (int i = 0; i < kNrOfCounters[9 /* group_index_ */] - 1; i++) {
      eventstring << eventsets[9 /* group_index_ */][i] << ",";
    }
    // Skip comma for last counter
    eventstring << eventsets[9 /* group_index_ */]
                            [kNrOfCounters[9 /* group_index_ */] - 1];

    // Register event set for tag
    int handle = perfmon_addEventSet(
        const_cast<char*>(eventstring.str().c_str()));  // TODO: remove
                                                        // const_cast once my PR
                                                        // has made it to LRZ

    uint64_t total_cycles = 0;
    for (int i = 0; i < kNumberOfSamples; i++) {
      perfmon_setupCounters(handle);
      perfmon_startCounters();
      perfmon_stopCounters();
      total_cycles +=
          static_cast<uint64_t>(perfmon_getResult(handle, 1, state_.cpu_));
    }

    std::cout << "LikwidPerformanceMonitoringModule: MEM overhead s = "
              << perfmon_getTimeOfGroup(handle) / kNumberOfSamples << std::endl;
    std::cout << "LikwidPerformanceMonitoringModule: MEM overhead cycles = "
              << static_cast<double>(total_cycles) / kNumberOfSamples
              << std::endl;
  }
}

}  // namespace likwid
}  // namespace profilers
}  // namespace exahype

#endif  // LIKWID_AVAILABLE
