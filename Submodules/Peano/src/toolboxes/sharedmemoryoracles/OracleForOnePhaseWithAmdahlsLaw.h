// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _SHARED_MEMORY_ORAClES_ORACLE_FOR_ONE_PHASE_WITH_AMDAHLS_LAW_H_
#define _SHARED_MEMORY_ORAClES_ORACLE_FOR_ONE_PHASE_WITH_AMDAHLS_LAW_H_


#include "tarch/logging/Log.h"
#include "peano/datatraversal/autotuning/OracleForOnePhase.h"
#include "peano/performanceanalysis/SpeedupLaws.h"
#include "tarch/timing/Measurement.h"


#include <map>
#include <vector>


namespace sharedmemoryoracles {
  class OracleForOnePhaseWithAmdahlsLaw;
}


/**
 * Grain Size Sampling
 *
 * This oracle samples the grain size stochastically and allows us to get a
 * first impression about the distribution of well-suited grain size choices.
 * In practice it probably should not be used - it will not give you a fast
 * code.
 *
 * <h1>Postprocessing</h1>
 *
 * Though the sampling writes text output (on demand), it is not that easy to
 * understand. With the oracle comes a Pylab script that creates a couple of
 * plots that visualise the sampled data. It accepts the sampling output file
 * as argument and gives a new file with the additional extension .html.
 *
 * adapters=6,phases=10  fehlt noch
 *
 * There is an example sampling file in this directory called
 * sampling.example-properties. It can be used to check what the output should
 * look like.
 *
 * @author Tobias Weinzierl
 */
class sharedmemoryoracles::OracleForOnePhaseWithAmdahlsLaw: public peano::datatraversal::autotuning::OracleForOnePhase {
  private:
    static tarch::logging::Log  _log;
    static constexpr int MinSampleInterval = 2;

    struct DataBaseEntry {
      peano::performanceanalysis::SpeedupLaws   _statistics;

      int _optimalGrainSize;
      int _maxProblemSize;

      DataBaseEntry();
    };


    /**
     * Map selected method trace plus grain sizes to measurements.
     */
    typedef std::map< peano::datatraversal::autotuning::MethodTrace, DataBaseEntry >    ExecutionTimeDatabase;

    ExecutionTimeDatabase                   _executionTimeDatabase;

    int                                     _sampleEveryXQueries;
    int                                     _sampleCounter;
  public:
    /**
     */
    OracleForOnePhaseWithAmdahlsLaw();

    virtual ~OracleForOnePhaseWithAmdahlsLaw();

    peano::datatraversal::autotuning::GrainSize parallelise(int problemSize, peano::datatraversal::autotuning::MethodTrace askingMethod) override;

    /**
     * Determine grain size to sample
     *
     * If the method trace is ascend or descend, the function either returns 1
     * or 0 as these operations just distinguish between two variants.
     * Otherwise, we either use a logarithmic or an equidistant sample
     * distribution. However, if we use equidistant and the problem size is
     * smaller than the number of samples, we need a different computing rule
     * (division of integers otherwise gives a spacing of 0 between different
     * sample values).
     */
    void parallelSectionHasTerminated(int problemSize, int grainSize, peano::datatraversal::autotuning::MethodTrace askingMethod, double costPerProblemElement) override;

    void plotStatistics(std::ostream& out, int oracleNumber) const override;
    void loadStatistics(const std::string& filename, int oracleNumber) override;

    /**
     * This operation is called by the oracle (management) on the active oracle
     * before it activates another one.
     */
    void deactivateOracle() override;
    void activateOracle() override;

    /**
     * For this oracle type, the adapter number is completely irrelevant.
     */
    peano::datatraversal::autotuning::OracleForOnePhase* createNewOracle() const override;
};


#endif
