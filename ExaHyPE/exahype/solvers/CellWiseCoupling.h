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

#ifndef _EXAHYPE_SOLVERS_CELL_WISE_COUPLING_H_
#define _EXAHYPE_SOLVERS_CELL_WISE_COUPLING_H_

#include "SolverCoupling.h"

namespace exahype {
class Vertex;
} /* namespace exahype */

namespace peano {
namespace grid {
class VertexEnumerator;
} /* namespace grid */
} /* namespace peano */

namespace exahype {
  namespace solvers {
    class CellWiseCoupling;
  }
}

class exahype::solvers::CellWiseCoupling: public exahype::solvers::SolverCoupling {
  public:
    CellWiseCoupling(double time, double repeat);
    virtual ~CellWiseCoupling() {};

    /**
     * Couple the solvers before performing the
     * first time step.
     */
    virtual void coupleFirstTime(
        const int cellDescriptionsIndex,
        exahype::Vertex* const fineGridVertices,
        const peano::grid::VertexEnumerator& fineGridVerticesEnumerator) {};

    /**
     * Couple the solvers in the following iterations.
     */
    virtual void couple(
        const int cellDescriptionsIndex,
        exahype::Vertex* const fineGridVertices,
        const peano::grid::VertexEnumerator& fineGridVerticesEnumerator) {};
};


#endif
