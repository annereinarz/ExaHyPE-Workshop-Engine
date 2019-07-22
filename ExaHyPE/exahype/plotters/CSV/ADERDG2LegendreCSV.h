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
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_CSV_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_CSV_H_

#include "exahype/plotters/Plotter.h"

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/unstructured/UnstructuredGridWriter.h"

#include <fstream> 

namespace exahype {
  namespace plotters {
    class ADERDG2LegendreCSV;

  }
}

/**
 * todo
 */
class exahype::plotters::ADERDG2LegendreCSV: public exahype::plotters::Plotter::Device {
 public:
  bool oneFilePerTimestep;
  bool allUnknownsInOneFile;
  bool writeCSVHeader; ///< write CSV header lines
  bool writeCommentHeader; ///< write comment header line
  bool writeDebugLines; ///< write comments about debugging information
  bool writeTimeColumn; ///< write a timestep for every row. Useful setting is writeTimeColumn=!oneFilePerTimestep

  std::string   comment; ///< Introducing string like "#" or "//" before a commentline
  std::string   seperator; ///< CSV seperator character, like "\t" or "," or ";"
  std::string   endl; ///< end of line, "\n" or "\r\n" if you prefer
  int           precision; ///< The digit precision to write numbers out, ie. the number of digits

  int           fileCounter; ///< Counting the output files
  std::string   basicFilename; ///< basic filename
  std::string   appendix; ///< filename appendix like ".csv" or ".txt"
  std::ofstream ofs; ///< currently open stream
  
  int           order;
  int           solverUnknowns;
  int           writtenUnknowns;
  
  char **writtenQuantitiesNames;

  static std::string getIdentifier();
  ADERDG2LegendreCSV(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing);
  virtual ~ADERDG2LegendreCSV();
  void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters) override;

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;
  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch, double* u,
      double timeStamp);
  void startPlotting( double time ) override;
  void finishPlotting() override;
  
  void openNewFile();
  void writeHeader();
  void writeDebug(const std::string& str);
  void writeRow(const double* const mappedUnknowns, const double* const coordinates, const double timeStamp);
};
#endif
