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

#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_PROBE_ASCII_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_PROBE_ASCII_H_

#include "exahype/plotters/Plotter.h"

#include <fstream>

namespace exahype {
  namespace plotters {
    class ADERDG2ProbeAscii;
  }
}


/**
 * Writes the probe into a file. All the data is first
 */
class exahype::plotters::ADERDG2ProbeAscii
    : public exahype::plotters::Plotter::Device {
 private:
  static tarch::logging::Log _log;

  std::string          _filename;
  int                  _order;
  int                  _solverUnknowns;
  int                  _writtenUnknowns;
  exahype::parser::ParserView  _plotterParameters;
  std::ofstream*       _out;
  bool                 _hasWrittenData;
  double               _time;

  tarch::la::Vector<DIMENSIONS,double> _x;

  void openOutputStream();
 public:
  ADERDG2ProbeAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
  virtual ~ADERDG2ProbeAscii();

  virtual void init(const std::string& filename, int orderPlusOne, int unknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  static std::string getIdentifier();

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};

#endif
