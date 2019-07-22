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
 * \brief A collection of solver operations that do not
 * fit anywhere else.
 *
 * \author Dominic E. Charrier
 **/

#ifndef _EXAHYPE_SOLVERS_CollectiveOperations_H
#define _EXAHYPE_SOLVERS_CollectiveOperations_H

#include "exahype/solvers/FiniteVolumesSolver.h"
#include "exahype/solvers/ADERDGSolver.h"
#include "exahype/solvers/LimitingADERDGSolver.h"

namespace exahype {
namespace solvers{

/**
  * Erase all cell descriptions registered for solvers
  * of type Type::ADERDG.
  *
  * \param deleteOnlyCells deletes only cell descriptions of type Cell
  */
 static void eraseADERDGCellDescriptions(const int cellDescriptionsIndex, const bool deleteOnlyCells=false);

/**
 * Sends all the ADERDG cell descriptions at address \p
 * cellDescriptionsIndex to rank \p toRank.
 *
 * <h2>Adaptive mesh refinement</h2>
 * For adaptive meshes, we further fix the type
 * of a descendant to RemoteBoundaryDescendant
 * at both sides of master-worker boundaries.
 *
 * We further fix the type of an Ancestor
 * to RemoteBoundaryAncestor if the parent
 * of the cell description on the master side
 * is also of type RemoteBoundaryAncestor or an
 * Ancestor.
 *
 * \note The data heap indices of the cell descriptions are not
 * valid anymore on rank \p toRank.
 */
void sendADERDGCellDescriptions(
    const int                                    toRank,
    const int                                    cellDescriptionsIndex,
    const peano::heap::MessageType&              messageType,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level);

 /**
  * Sends an empty message to the rank \p toRank.
  */
 void sendEmptyADERDGCellDescriptions(
     const int                                    toRank,
     const peano::heap::MessageType&              messageType,
     const tarch::la::Vector<DIMENSIONS, double>& x,
     const int                                    level);

 /**
  * Receives cell descriptions from rank \p fromRank
  * and resets the data heap indices to -1.
  *
  * If a received cell description has the same
  * solver number as a cell description in the
  * array at address \p cellDescriptionsIndex,
  * we merge the metadata (time stamps, time step size)
  * of both cell descriptions.
  *
  * If no cell description in the array at address
  * \p cellDescriptionsIndex can be found with the
  * same solver number than a received cell description,
  * we push the received cell description to
  * the back of the array at address \p cellDescriptions
  * Index.
  *
  * This operation is intended to be used in combination
  * with the solver method mergeWithWorkerOrMasterDataDueToForkOrJoin(...).
  * Here, we would merge first the cell descriptions sent by the master and worker
  * and then merge the data that is sent out right after.
  */
 void mergeADERDGCellDescriptionsWithRemoteData(
     const int                                    fromRank,
     exahype::Cell&                               localCell,
     const peano::heap::MessageType&              messageType,
     const tarch::la::Vector<DIMENSIONS, double>& x,
     const int                                    level);

 /**
  * Drop cell descriptions received from \p fromRank.
  */
 void dropADERDGCellDescriptions(
     const int                                    fromRank,
     const peano::heap::MessageType&              messageType,
     const tarch::la::Vector<DIMENSIONS, double>& x,
     const int                                    level);

/**
 * Send all FiniteVolumes cell descriptions to rank
 * \p toRank.
 *
 * \note The data heap indices of the cell descriptions are not
 * valid anymore on rank \p toRank.
 */
static void sendFiniteVolumesCellDescriptions(
    const int                                     toRank,
    const int                                     cellDescriptionsIndex,
    const peano::heap::MessageType&               messageType,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level);

}
}



#endif /* COLLECTIVEOPERATIONS_H_ */
