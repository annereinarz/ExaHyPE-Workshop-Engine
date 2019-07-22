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
 
#ifndef _EXAHYPE_VERTEX_H_
#define _EXAHYPE_VERTEX_H_

#include "exahype/records/Vertex.h"

#include "peano/MappingSpecification.h"
#include "peano/grid/Vertex.h"
#include "peano/grid/VertexEnumerator.h"
#include "peano/utils/Globals.h"

#include "exahype/solvers/ADERDGSolver.h"
#include "exahype/solvers/FiniteVolumesSolver.h"

#include "peano/MappingSpecification.h"

namespace exahype {
  class Vertex;

  /**
   * Forward declaration
   */
  class VertexOperations;
}

/**
 * A grid vertex.
 *
 * Peano realises the neighbour communication between
 * different MPI ranks via exchanging and merging vertices.
 * It further offers routines in the mappings to prepare
 * vertices before the data exchange and routines to merge
 * the exchanged vertices. In these two routines, we plugin the
 * sending and receiving of heap data.
 * A fair share of the required heap data exchange functionality can
 * thus be found in this class.
 */
class exahype::Vertex : public peano::grid::Vertex<exahype::records::Vertex> {
public:
  /**
   * If set to true, then additional parallelism is introduced
   * per vertex where the faces are processed in parallel.
   * Concurrency level is 12 in 3D and 4 in 2D.
   */
  static bool SpawnNeighbourMergeAsThread;

  /**
   * @return a mapping specification which applies to all neighbour merges.
   */ 
  static peano::MappingSpecification getNeighbourMergeSpecification(const int level);

  /**
   * Compare if two vectors are equal up to a relative
   * tolerance.
   *
   * TODO(Dominic): Right place?
   */
  static bool equalUpToRelativeTolerance(
      const tarch::la::Vector<DIMENSIONS,double>& first,
      const tarch::la::Vector<DIMENSIONS,double>& second);

  /**
   * Express @p index as i = a*2^0 + b*2^1 + c*2^2.
   *
   * @return the coefficients as vector [a,b] or [a,b,c] in 2D or 3D, respectively.
   *
   * @param index an integer in [0,3] or [0,7] in 2D or 3D, respectively.
   */
  static tarch::la::Vector<DIMENSIONS,int> delineariseIndex2(int index);

  /**
   * Validate that a compute cell associated with index1 is not next to
   * an invalid cell description index 2 if their
   * interface is an interior face.
   */
  static void validateNeighbourhood(
      const int                                cellDescriptionsIndex1,
      const int                                cellDescriptionsIndex2,
      const exahype::Vertex&                   vertex,
      const tarch::la::Vector<DIMENSIONS,int>& pos1,
      const tarch::la::Vector<DIMENSIONS,int>& pos2);

  /**
   * @return if the neighbourMergePerformed flag is set for face @p faceindex on
   * the cell descriptions referenced by @p cellInfo.
   *
   * @note Side effects: This functions checks if the neighbourMergePerformed flag is set to false on the cell descriptions.
   * If this flag is not set, the function returns true. However, before it returns, it
   * sets the flag to true. The next call of this function will then return false.
   *
   * @param cellInfo              a struct holding referenecs to cell description arrays
   * @param faceIndex             an index in the range of 0 (inclusive) to 2*DIMENSIONS (exclusive).
   * @param prefethADERDGFaceData if ADER-DG face data should be prefetched
   *                              (if defined(SharedTBB) && !defined(noTBBPrefetchesJobData))
   */
  static bool hasToMergeAtFace(
      solvers::Solver::CellInfo& cellInfo,
      const int                  faceIndex,
      const bool                 prefetchADERDGFaceData);

private:
  typedef class peano::grid::Vertex<exahype::records::Vertex> Base;

  friend class VertexOperations;

  /**
   * The log device of this class.
   */
  static tarch::logging::Log _log;

  /**
   * Loops over the cell descriptions stored at the
   * two heap array indices and tries to merge matching
   * pairs adjacent to the common face.
   *
   * @param cellInfo1 cell descriptions found for pos1
   * @param cellInfo2 cell descriptions found for pos2
   * @param pos1 position of first cell
   * @param pos2 position of second cell
   * @param x the position of the vertex
   * @param h the mesh size at the level of the vertex
   *
   * @note Assumes a stable mesh, or at least one where no cells are deleted
   * but only added and the adjacency information is updated.
   */
  static void mergeNeighboursDataAndMetadata(
      solvers::Solver::CellInfo& cellInfo1,
      solvers::Solver::CellInfo& cellInfo2,
      const tarch::la::Vector<DIMENSIONS,int>& pos1,
      const tarch::la::Vector<DIMENSIONS,int>& pos2,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h);

  /**
   * Loops over the cell descriptions stored at the
   * two heap array indices and tries to impose boundary
   * conditions if this was not already previously.
   *
   * @param cellDescriptionsIndex1 index corresponding to pos1
   * @param cellDescriptionsIndex2 index corresponding to pos2
   * @param pos1 position of first cell
   * @param pos2 position of second cell
   * @param x the position of the vertex
   * @param h the mesh size at the level of the vertex
   *
   * @note Assumes a stable mesh, or at least one where no cells are deleted
   * but only added and the adjacency information is updated.
   */
  static void mergeWithBoundaryData(
      solvers::Solver::CellInfo& cellInfo,
      const tarch::la::Vector<DIMENSIONS,int>& posCell,
      const tarch::la::Vector<DIMENSIONS,int>& posBoundary,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h);



  /**
   * Loop body of loop in mergeNeighbours.
   *
   * @note All parameters must be copied as the function
   * might be spawned as task.
   *
   * @param pos1Scalar             linearised adjacency index
   * @param cellDescriptionsIndex1 cell description index corresponding to the adjacency index
   * @param x position of a vertex
   * @param h extent of cells adjacent to the vertex
   */
  static void mergeNeighboursLoopBody(
      const int                                   spos1Scalar,
      const int                                   spos2Scalar,
      const exahype::Vertex&                      vertex,
      const tarch::la::Vector<DIMENSIONS, double> x,
      const tarch::la::Vector<DIMENSIONS, double> h);

  /**
   * Loop body of loop in mergeNeighboursMetadata.
   *
   * @param pos1Scalar             linearised adjacency index
   * @param cellDescriptionsIndex1 cell description index corresponding to the adjacency index
   * @param x position of this vertex
   * @param h mesh size
   */
  static void mergeOnlyNeighboursMetadataLoopBody(
      const int pos1Scalar,
      const int pos2Scalar,
      const int cellDescriptionsIndex1,
      const int cellDescriptionsIndex2,
      const exahype::State::AlgorithmSection& section,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const bool                                   checkThoroughly);

  #ifdef Parallel
  static constexpr int InvalidMetadataIndex = -1;

  /**
   * TODO(Dominic): Add docu.
   * @param toRank
   * @param srcScalar
   * @param destScalar
   * @param srcCellDescriptionIndex
   * @param adjacentRanks
   * @param baryCentre of a face
   * @param h
   * @param level
   * @param checkThoroughly
   */
  static void sendOnlyMetadataToNeighbourLoopBody(
      const int                                    toRank,
      const int                                    srcScalar,
      const int                                    destScalar,
      const int                                    srcCellDescriptionIndex,
      const tarch::la::Vector<TWO_POWER_D, int>&   adjacentRanks,
      const tarch::la::Vector<DIMENSIONS, double>& baryCentre,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level,
      const bool                                   checkThoroughly);


  /**
   * Loop body for mergeOnlyWithNeighbourMetadata.
   *
   * @param fromRank                  rank we expect to receive a message from
   * @param srcScalar                 linearised position of message source
   * @param destScalar                linearised position of message destination
   * @param destCellDescriptionIndex  cell descriptions index associated with cell at destination
   * @param adjacentRanks             map holding the ranks adjacent to a vertex.
   * @param x                         coordinates of vertex
   * @param h                         mesh size of cells adjacent to vertex
   * @param level                     mesh level
   * @param checkThoroughly           compare geometry information on the cell descriptions with the parameters @p x, @p h,@p level.
   */
  static void mergeOnlyWithNeighbourMetadataLoopBody(
      const int                                    fromRank,
      const int                                    srcScalar,
      const int                                    destScalar,
      const int                                    destCellDescriptionIndex,
      const tarch::la::Vector<TWO_POWER_D, int>&   adjacentRanks,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level,
      const exahype::State::AlgorithmSection&      section,
      const bool                                   checkThoroughly);

  /*! Helper routine for sendToNeighbour
   *
   * Loop over all the solvers and check
   * if a cell description (ADERDGCellDescription,
   * FiniteVolumesCellDescription,...) is registered
   * for the solver type. If so, send
   * out data or empty messages to the rank \p toRank that
   * owns the neighbouring domain.
   *
   * If not so, send out empty messages for the particular
   * solver.
   *
   * \note Not thread-safe.
   */
  void sendSolverDataToNeighbour(
      const int                                    toRank,
      const bool                                   isLastIterationOfBatchOrNoBatch,
      const tarch::la::Vector<DIMENSIONS,int>&     src,
      const tarch::la::Vector<DIMENSIONS,int>&     dest,
      const int                                    srcCellDescriptionIndex,
      const int                                    destCellDescriptionIndex,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const;

  /**
   * TODO(Dominic): Add docu.
   *
   * @param toRank
   * @param srcScalar
   * @param destScalar
   * @param srcCellDescriptionsIndex
   * @param adjacentRanks
   * @param isLastIterationOfBatchOrNoBatch
   * @param x
   * @param level
   */
  static void sendToNeighbourLoopBody(
      const int                                    toRank,
      const int                                    srcScalar,
      const int                                    destScalar,
      const int                                    srcCellDescriptionsIndex,
      const tarch::la::Vector<TWO_POWER_D, int>&   adjacentRanks,
      const bool                                   isLastIterationOfBatchOrNoBatch,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level);

  /**
   * TODO(Dominic): Add docu.
   *
   * @param fromRank
   * @param srcScalar
   * @param destScalar
   * @param destCellDescriptionIndex
   * @param mergeWithReceivedData
   * @param receiveNeighbourMetadata
   * @param adjacentRanks
   * @param baryCentre bary centre of the face
   * @param level
   */
  static void receiveNeighbourDataLoopBody(
    const int                                    fromRank,
    const int                                    srcScalar,
    const int                                    destScalar,
    const int                                    destCellDescriptionIndex,
    const bool                                   mergeWithReceivedData,
    const bool                                   receiveNeighbourMetadata,
    const tarch::la::Vector<TWO_POWER_D, int>&   adjacentRanks,
    const tarch::la::Vector<DIMENSIONS, double>& baryCentre,
    const int                                    level);

#endif

 public:
  /**
   * Default Constructor
   *
   * This constructor is required by the framework's data container. Do not
   * remove it.
   */
  Vertex();

  /**
   * This constructor should not set any attributes. It is used by the
   * traversal algorithm whenever it allocates an array whose elements
   * will be overwritten later anyway.
   */
  Vertex(const Base::DoNotCallStandardConstructor&);

  /**
   * Constructor
   *
   * This constructor is required by the framework's data container. Do not
   * remove it. It is kind of a copy constructor that converts an object which
   * comprises solely persistent attributes into a full attribute. This very
   * functionality is implemented within the super type, i.e. this constructor
   * has to invoke the correponsing super type's constructor and not the super
   * type standard constructor.
   */
  Vertex(const Base::PersistentVertex& argument);

  /**
   * @return the cell descriptions indices of the adjacent cells.
   */
  tarch::la::Vector<TWO_POWER_D, int> getCellDescriptionsIndex() const;
 
  /**
   * @return the cell descriptions indices of an adjcacent cell.
   */
  int getCellDescriptionsIndex(const int adjacencyIndex) const;

  /**
   * @return a cell info object linking to cell descriptions associated with the cell
   * with index @p index in the adjacency map of the vertex.
   */
  exahype::solvers::Solver::CellInfo createCellInfo(int index) const;

  /**
   * Compute the face barycentre from a vertex perspective where
   * the normal direction d is known and the position of one
   * of the adjacent cells.
   *
   * The barycentre is then computed as xB[d] = x[d] and
   * xB[i] = x[i] +- h[i]/2 for i != d. The sign
   * of the second term depends on the position of any of
   * the cells adjacent to the interface.
   *
   * \param[in] cellPosition Position of one of the cells adjacent to the
   *                         face. Which one does not matter.
   */
  static tarch::la::Vector<DIMENSIONS,double> computeFaceBarycentre(
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const tarch::la::Vector<DIMENSIONS,double>& h,
      const int                                   normalDirection,
      const tarch::la::Vector<DIMENSIONS,int>&    cellPosition);

  /**
   * Evaluate if the current vertex can be erased, must be refined,
   * or should be kept.
   *
   * \note We do not evaluate any physics-based refinement criterion in
   * this function. This is done cell-wisely and usually triggered in enterCell(..).
   * Instead, we simply check here the refinement events and flags of
   * adjacent cell descriptions (which might have been modified earlier by
   * a physics-based refinement criterion.)
   */
  exahype::solvers::Solver::RefinementControl evaluateRefinementCriterion(
      const tarch::la::Vector<DIMENSIONS, double>& vertexOffset,
      const int                                    level,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize,
      const bool                                   checkThoroughly) const;

  /**
   * Loop over all neighbouring cells and merge
   * the metadata of cell descriptions in neighbouring
   * cells which are owned by the same solver.
   *
   * \note Since this function sets the
   * neighbourMergePerformed flags, do never
   * use it in combination with the Merging mapping.
   *
   * @param section the code section this routine is called from
   * @param x the position of the vertex
   * @param h the level-dependent mesh size of cells adjacent to this vertex
   * @param checkThoroughly if set to true, this routine will tell the solvers to compare
   *                        the geometry information of the vertex
   *                        with that stored on the patches before doing a merge.
   *                        This is usually only turned on during the mesh refinement iterations
   *                        where the grid changes and thus the adjacency maps stored in the vertices.
   *                        Here, it might happen that the indices are outdated and then link
   *                        to patches/cell descriptions located somewhere else.
   */
  void mergeOnlyNeighboursMetadata(
      const exahype::State::AlgorithmSection& section,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const bool checkThoroughly) const;

  /**
   * Solve Riemann problems on all interior faces that are adjacent
   * to this vertex and impose boundary conditions on faces that
   * belong to the boundary.
   *
   * This is done for all cell descriptions
   * belonging to the cells that are an interior face.
   *
   * The routine itself runs the loop over the faces. The actual
   * functionality is outsourced to solveRiemannProblemAtInterface().
   *
   * The function ensures implicitly that interior faces
   * do not align with MPI boundaries. In this case, no operation
   * is performed.
   *
   * This method sets the riemannSolvePerformed flag on a cell description
   * if boundary conditions have been imposed for this cell description.
   * This method sets the riemannSolvePerformed flag on both cell descriptions
   * (per solver) for interior faces if a Riemann solve has been performed for
   * both cell descriptions.
   *
   * \note The function itself is not thread-safe.
   * Thread-safety of this function must be ensured by setting
   * touchVertexFirstTimeSpecification()
   * to peano::MappingSpecification::AvoidFineGridRaces.
   *
   * Shared Memory
   * =============
   * 
   * All vertices can be processed race-free in parallel if each vertex only
   * performs Riemann solves/neighbour merges around a single adjacent cell.
   * This ensures a unique mapping between the vertices and the faces of the mesh
   * and prevents that two threads access the same face data simulutaneously.
   * 
   * In the sketch below, Vertex a only merges neighbours adjacent to cell 0.
   * The interfaces are annotated with A. Analgously, vertices b and c
   * only perform merges at faces annotated with B and C, respectively.
   *
   * --C--c
   *      |
   *   2  C  3   
   *      |    
   * --A--a--B--b
   *      |     |
   *   0  A  1  B
   *      |     |
   * 
   * This works as all cells are sounded by vertices. At the boundary, some vertices
   * will have less work associated with them as some of their adjacent faces are outside of the domain.
   * 
   * We let every vertex do the Riemann solves around the cell at position 0 in the vertex' local adjacency map.
   * The cell's neigbour merge partners are at positions 1,2, (2D and 3D) and 4 (only 3D).
   *
   * Background Jobs and neighbourMergePerformed flags
   * --------------------------------------------------
   * The Riemann solves wait if a background job has not completed
   * in one of the cells adjacent to a face.
   * We rely here on information if a face has already been
   * processed or not. Otherwise, we may wait
   * on a neighbour which has already advanced in time
   * and submitted the next background job.
   * This would introduce numerical errors.
   *
   *
   * Old implemtation (still used for MPI routine)
   * ---------------------------------------------
   *
   * The AvoidFineGridRaces multithreading touchVertexFirstTime
   * specification prevents that more than one threads write data for
   * the same face of the grid at the same time.
   *
   * The specification is realised by touching the vertices in
   * a red-black (X and O in the figure below) manner:
   * We might first process the X-vertices in parallel
   * and then the O-vertices.
   * In each step, faces adjacent to the vertices,
   * do not overlap and race conditions can thus
   * not occur.
   *
   *     |    |
   *     |    |
   * ----O----X-----
   *     |    |
   *     |    |
   * ----X----O-----
   *     |    |
   *     |    |
   *
   * Further parallelisation over the adjacent faces
   * -----------------------------------------------
   *
   * Further concurrency can be found in the loops over the adjacent
   * faces per vertex. In 2D, 3D, there are 4, 12 faces ,respectively,
   * which can be processed in parallel. A face is defined
   * by its adjacent cells:
   *
   * Adjacency index pairs in 2D:
   *
   * face between positions [0,0]-[1,0]: adjacency index pairs 0-1
   * face between positions [0,0]-[0,1]: adjacency index pairs 0-2
   * face between positions [1,0]-[1,1]: adjacency index pairs 1-3
   * face between positions [0,1]-[1,1]: adjacency index pairs 2-3
   *
   * Adjacency index pairs in 3D:
   *
   * face between positions [0,0,0]-[1,0,0]: adjacency index pairs 0-1
   * face between positions [0,0,0]-[0,1,0]: adjacency index pairs 0-2
   * face between positions [0,0,0]-[0,0,1]: adjacency index pairs 0-4
   * face between positions [1,0,0]-[1,1,0]: adjacency index pairs 1-3
   * face between positions [1,0,0]-[1,0,1]: adjacency index pairs 1-5
   * face between positions [0,1,0]-[1,1,0]: adjacency index pairs 2-3
   * face between positions [0,1,0]-[0,1,1]: adjacency index pairs 2-6
   * face between positions [1,1,0]-[1,1,1]: adjacency index pairs 3-7
   * face between positions [0,0,1]-[1,0,1]: adjacency index pairs 4-5
   * face between positions [0,0,1]-[0,1,1]: adjacency index pairs 4-6
   * face between positions [1,0,1]-[1,1,1]: adjacency index pairs 5-7
   * face between positions [0,1,1]-[1,1,1]: adjacency index pairs 6-7
   *
   * The following python3 code can be used to compute teh adjacency index pairs:
   *
   * @code{.py}
   * dim=3
   *
   * print("adjacency index pairs for dim{}".format(dim))
   *
   * def linearise(tup):
   *     return tup[0]+tup[1]*2+tup[2]*4
   *
   * lim3=1
   * if dim==3:
   *     lim3=2
   *
   * pairs = collections.OrderedDict() # keys are stored in set which removes duplicates
   *
   * for i2 in range(0,lim3):
   *     for i1 in range(0,2):
   *         for i0 in range(0,2):
   *             for j2 in range(0,lim3):
   *                   for j1 in range(0,2):
   *                       for j0 in range(0,2):
   *                         i = [i0,i1,i2]
   *                         j = [j0,j1,j2]
   *                         diff=0
   *                         for d in range(0,3):
   *                             if i[d]!=j[d]:
   *                                 diff+=1
   *                         if diff==1:
   *                             minIndex = min(linearise(i),linearise(j))
   *                             maxIndex = max(linearise(i),linearise(j))
   *                             minPosition = [str(x) for x in i]
   *                             maxPosition = [str(x) for x in j]
   *                             if linearise(j)==minIndex:
   *                                 minPosition = [str(x) for x in j]
   *                                 maxPosition = [str(x) for x in i]
   *                             key = "{}-{}".format(minIndex,maxIndex)
   *                             msg = "face between positions [{}]-[{}]: adjacency index pairs {}".format(",".join(minPosition),",".join(maxPosition),key)
   *                             pairs[key] = msg
   *
   * for key,value in pairs.items():
   *     print(value)
   * @endcode
   *
   * Limiter identification
   * ======================
   * 
   * Each ADER-DG solver analyses the local min and max values within a cell.
   * This information however is not stored in the cell but on the 2d faces
   * of a cell.
   */
  void mergeNeighbours(
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h) const;

#ifdef Parallel

  /**
   * Returns true if the vertex has to communicate, i.e.
   * send and receive metadata, and solver data if applicable.
   *
   * Criteria:
   * - Vertex has to be inside of the domain.
   * - Vertex must belong to a grid which is at least
   *   as fine as the coarsest solver grid.
   */
  bool hasToCommunicate( const int level ) const;

  /**
   * TODO(Dominic): Add docu.
   */
  static bool compareGeometryInformationOfCellDescriptionsAndVertex(
      const tarch::la::Vector<DIMENSIONS,int>&     src,
      const tarch::la::Vector<DIMENSIONS,int>&     dest,
      const int                                    srcCellDescriptionsIndex,
      const tarch::la::Vector<DIMENSIONS, double>& baryCentre,
      const tarch::la::Vector<DIMENSIONS, double>& h);

  /**
   * Returns if this vertex needs to send a metadata message to a remote rank \p toRank.
   *
   * We need to send a message to remote rank \p roRank if both ranks
   * share a face and the adjacent rank at position dest is the remote rank.
   *
   * It is further necessary that either holds:
   *
   * 1. The adjacent rank at position src in the vertex' adjacency information
   *    equals the rank of the MPI process that calls
   *    this function.
   *
   * 2. For the rank at position src in the vertex' adjacency information forking was triggered.
   *    Then, the domain at position src will not be owned anymore by the rank that calls
   *    this function in the next iteration. However, remote ranks still expect receiving data from it.
   *
   * @param toRank        The rank we want to send the message to.
   * @param state         The state tells us if a neighbouring rank is forking or if forking was triggered for this rank.
   * @param srcScalar     linearised position of message source relative to vertex
   * @param destScalar    linearised position of message destination relative to vertex
   * @param adjacentRanks map holding the ranks adjacent to a vertex.
   *
   * @developers:
   * TODO(Dominic): Consider joins.
   * TODO(Dominic): Potentially, there is still a bug if two neighbouring ranks are
   *                forking at the same time.
   */
  static bool hasToSendMetadata(
      const int                                 toRank,
      const int                                 srcScalar,
      const int                                 destScalar,
      const tarch::la::Vector<TWO_POWER_D,int>& adjacentRanks);

  /**
   * TODO(Dominic): Add docu
   */
  void sendOnlyMetadataToNeighbour(
      const int                                    toRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level,
      const bool                                   checkThoroughly) const;


  /**
   * Returns if this vertex needs to receive a metadata message from a remote rank \p fromRank.
   *
   * We need to receive a message from remote rank \p fromRank if both ranks
   * share a face and the adjacent rank at position src is the remote rank.
   *
   * It is further necessary that either holds:
   *
   * 1. The adjacent rank at position dest in the vertex' adjacency information
   *    equals the rank of the MPI process that calls
   *    this function.
   *
   * 2. The rank at position dest in the vertex' adjacency information is now a forking rank.
   *    Then, the domain at position dest was owned by the rank of the MPI process that calls
   *    this function in the previous iteration and remote ranks have thus sent data to it.
   *
   *
   * @param fromRank                  rank we expect to receive a message from
   * @param srcScalar                 linearised position of message source
   * @param destScalar                linearised position of message destination
   * @param adjacentRanks             map holding the ranks adjacent to a vertex.
   *
   * TODO(Dominic): Consider joins.
   * TODO(Dominic): Potentially, there is still a bug if two neighbouring ranks are
   *                forking at the same time.
   *
   */
  static bool hasToReceiveMetadata(
      const int                                  fromRank,
      const int                                  srcScalar,
      const int                                  destScalar,
      const tarch::la::Vector<TWO_POWER_D, int>& adjacentRanks);

  /**
   * Receive metadata from neighbouring ranks
   * and merge the solvers with it.
   *
   * \note Since this function sets the
   * neighbourMergePerformed flags, do never
   * use it in combination with the Merging mapping.
   */
  void mergeOnlyWithNeighbourMetadata(
      const int                                    fromRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level,
      const exahype::State::AlgorithmSection&      section,
      const bool                                   checkThoroughly) const;

  /**
   * Drops the metadata received from neighbouring ranks.
   *
   * \note Since this function sets the
   * neighbourMergePerformed flags, do never
   * use it in combination with the Merging mapping.
   */
  void dropNeighbourMetadata(
      const int                                    fromRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level) const;

  /*! Send face data to neighbouring remote ranks.
   *
   * Look up facewise neighbours. If we find a remote boundary,
   * send face data of every registered solver to the remote rank.
   *
   * <h2> Batching </h2>
   * If we are not in the last iteration of a batch or we do not run a batch at all and
   * all solvers perform static or no limiting at all,
   * we can switch off the metadata sends.
   *
   * \param[in] x     position of the vertex.
   * \param[in] h     extents of adjacent cells.
   * \param[in] level level this vertex is residing.
  */
  void sendToNeighbour(
      int toRank,
      const bool isLastIterationOfBatchOrNoBatch,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level) const;

  /*! Receive data from remote ranks at all remote boundary faces adjacent to this vertex.
   *
   * TODO(Dominic): Add docu.
   */
  void receiveNeighbourData(
      const int                                    fromRank,
      const bool                                   mergeWithReceivedData,
      const bool                                   isFirstIterationOfBatchOrNoBatch,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const tarch::la::Vector<DIMENSIONS, double>& h,
      const int                                    level) const;
  #endif
};

#endif // _EXAHYPE_VERTEX_H
