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
 * \author Dominic E. Charrier, Tobias Weinzierl, Fabian Güra
 **/

#ifndef _EXAHYPE_SOLVERS_SOLVER_COUPLING_H_
#define _EXAHYPE_SOLVERS_SOLVER_COUPLING_H_


#include <vector>


namespace exahype {
  namespace solvers {
    class SolverCoupling;

    extern std::vector<SolverCoupling*> RegisteredSolverCouplings;
  }
}



class exahype::solvers::SolverCoupling {
  public:
    enum class Type {
      CellWise
    };
  private:
    const Type   _type;
    const double _repeat;
    double       _nextSnapshot;
  public:
    SolverCoupling(Type type, double time, double repeat);
    virtual ~SolverCoupling() {};

    /**
     * Identifies whether solver coupling is active and, if this is the case,
     * updates the internal snapshot field.
     */
    virtual bool isActive(double timeStamp);

    Type getType() const;
};

#endif
