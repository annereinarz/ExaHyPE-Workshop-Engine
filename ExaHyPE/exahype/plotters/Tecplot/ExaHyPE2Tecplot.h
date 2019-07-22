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
 * 
 * @authors: Sven Koeppel <koeppel@fias.uni-frankfurt.de>,
 *           Maurizio Tavelli <m.tavelli@unitn.it>
 **/
 
#ifndef _EXAHYPE_PLOTTERS_ADERDG_2_TECPLOT_H_
#define _EXAHYPE_PLOTTERS_ADERDG_2_TECPLOT_H_

#include "exahype/plotters/Plotter.h"

namespace exahype {
  namespace plotters {
    class ExaHyPE2Tecplot;
    
    class FiniteVolumes2Tecplot;
    class ADERDG2Tecplot;
    class LimitingADERDG2Tecplot;
  }
}

/**
 * <h2>The Tecplot writer from the Trento code</h2>
 *
 * This plotter is a stub to connect the Fortran plotting code to ExaHyPE.
 *
 * Note, to enable this plotter, you need
 *
 *   export PROJECT_CFLAGS="-DTECPLOT"
 * 
 */
class exahype::plotters::ExaHyPE2Tecplot : public exahype::plotters::Plotter::Device {
 public:
  static tarch::logging::Log _log;
  
  std::string _filename;
  int _ghostLayerWidth;
  int _orderPlusOne, _solverUnknowns, _writtenUnknowns;

  static std::string getIdentifier();
  
  /// If this is not an FV solver but a DG solver
  constexpr static int noGhostLayerApplicable = -1;

  ExaHyPE2Tecplot(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, int ghostLayerWidth=noGhostLayerApplicable);

  virtual ~ExaHyPE2Tecplot();

  virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

  virtual void plotPatch(const int solverNumber,solvers::Solver::CellInfo& cellInfo);

  virtual void startPlotting( double time );
  virtual void finishPlotting();
};



class exahype::plotters::FiniteVolumes2Tecplot: public exahype::plotters::ExaHyPE2Tecplot {
  public:
    FiniteVolumes2Tecplot(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth) : exahype::plotters::ExaHyPE2Tecplot(postProcessing,ghostLayerWidth) {}

    static std::string getIdentifier() { return exahype::plotters::ExaHyPE2Tecplot::getIdentifier(); }
};


class exahype::plotters::ADERDG2Tecplot: public exahype::plotters::ExaHyPE2Tecplot {
  public:
    ADERDG2Tecplot(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing) : exahype::plotters::ExaHyPE2Tecplot(postProcessing,noGhostLayerApplicable) {}

    static std::string getIdentifier() { return exahype::plotters::ExaHyPE2Tecplot::getIdentifier(); }
};


class exahype::plotters::LimitingADERDG2Tecplot: public exahype::plotters::ExaHyPE2Tecplot {
  public:
    LimitingADERDG2Tecplot(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth) : exahype::plotters::ExaHyPE2Tecplot(postProcessing,ghostLayerWidth) {}

    static std::string getIdentifier() { return exahype::plotters::ExaHyPE2Tecplot::getIdentifier(); }
};


#endif/* _EXAHYPE_PLOTTERS_ADERDG_2_TECPLOT_H_ */
