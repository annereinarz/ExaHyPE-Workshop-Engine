#ifndef _EXAHYPE_PLOTTERS_PATCH_2_CSV_H
#define _EXAHYPE_PLOTTERS_PATCH_2_CSV_H

#include "exahype/plotters/Plotter.h"

namespace exahype {
  namespace plotters {
    class Patch2CSV;
    struct Patch2CSV_Dataset; //< The line information to be written


    class Slicer; // external forward decl, #include exahype/plotters/slicing/Slicer.h
    namespace ascii { class CSVWriter; } // external forward decl, #include exahype/plotters/ascii/CSVWriter.h
  }
}

/**
 * This is a super simple variant of the Patch2VTK which only plots cell properties
 * but not cell averages, because it uses the CSVWriter infrastructure which is
 * compile time fixed. Easy to extend however to also plot cell averages as CSV,
 * if you want. Trivial to extend to plot any other information about cells as
 * CSV table.
 **/
class exahype::plotters::Patch2CSV: public exahype::plotters::Plotter::Device {
 public:
  const exahype::solvers::Solver::Type _solverType;
  const bool _isLimitingSolver; // Shorthand
  const bool    _hasMPIenabled;
  int           _fileCounter;
  std::string   _filename;
  //int           _numberOfCellsPerAxis;
  int           _solverUnknowns;
  int           _writtenUnknowns;
  exahype::parser::ParserView   _plotterParameters;
  
  // For the time being, these are constants, but it is easy to extend
  // them to be not constants...
  bool _one_file_per_timestep;

  exahype::plotters::ascii::CSVWriter* _writer;

  /**
   * To memorise the time argument from startPlotter(). We need it when we close the plotter for the time series.
   */
  double _time;

  exahype::plotters::Slicer *slicer;

  static tarch::logging::Log _log;

  static std::string getIdentifier();
  Patch2CSV(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, exahype::solvers::Solver::Type solvertype);
  virtual ~Patch2CSV();

  virtual void init(const std::string& filename, int numberOfCellsPerAxis, int unknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo) override;

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};




#endif /* _EXAHYPE_PLOTTERS_PATCH_2_CSV_H */
