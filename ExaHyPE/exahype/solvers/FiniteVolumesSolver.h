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
 * @author Dominic E. Charrier, Tobias Weinzierl
 **/

#ifndef _EXAHYPE_SOLVERS_FINITE_VOLUMES_SOLVER_H_
#define _EXAHYPE_SOLVERS_FINITE_VOLUMES_SOLVER_H_


#include "exahype/solvers/Solver.h"

#include "exahype/records/FiniteVolumesCellDescription.h"

#include "exahype/Cell.h"
#include "exahype/Vertex.h"

#ifdef USE_ITAC
#include "VT.h"
#endif

namespace exahype {
namespace solvers {
class FiniteVolumesSolver;
}  // namespace solvers
namespace parser {
class ParserView;
}  // namespace parser
}  // namespace exahype

/**
 * Abstract base class for one-step Finite Volumes solvers.
 */
class exahype::solvers::FiniteVolumesSolver : public exahype::solvers::Solver {
  friend class LimitingADERDGSolver;
public:

  #ifdef USE_ITAC
  /**
   * These handles are used to trace solver events with Intel Trace Analyzer and Collector.
   */
  static int adjustSolutionHandle;
  static int updateBodyHandle;
  static int updateBodyHandleSkeleton;
  static int mergeNeighboursHandle;
  #endif

  typedef exahype::DataHeap DataHeap;

  /**
   * Rank-local heap that stores FiniteVolumesCellDescription instances.
   *
   * @note This heap might be shared by multiple FiniteVolumesSolver instances
   * that differ in their solver number and other attributes.
   * @see solvers::Solver::RegisteredSolvers.
   */
  typedef exahype::records::FiniteVolumesCellDescription CellDescription;
  typedef peano::heap::RLEHeap<CellDescription> Heap;

private:
  /**
   * TODO(WORKAROUND): We store these fields in order
   * to use the symmetric boundary exchanger of Peano
   * which does not yet support asymmetric send buffers.
   */
  DataHeap::HeapEntries _invalidExtrapolatedSolution;

  /**
   * Log device.
   */
  static tarch::logging::Log _log;

  /**
   * Minimum time stamp of all patches
   * in the previous iteration.
   */
  double _previousMinTimeStamp;

  /**
   * Minimum time step size of all patches
   * in the previous iteration.
   */
  double _previousMinTimeStepSize;

  /**
   * Minimum time stamps of all patches.
   */
  double _minTimeStamp;

  /**
   * Minimum time step size of all patches.
   */
  double _minTimeStepSize;

  /**
   * Minimum stable time step size of all patches for
   * the next iteration.
   */
  double _admissibleTimeStepSize;

  /**
   * Width of the ghost layer used for
   * reconstruction and Riemann solves.
   */
  const int _ghostLayerWidth;

  /**
   * The current mesh update event.
   */
  MeshUpdateEvent _meshUpdateEvent;

  /**
   * Synchronises the cell description's time stepping data with
   * the solver's time stepping data.
   */
  void synchroniseTimeStepping(CellDescription& cellDescription) const;

  /**
   * Simply adjust the solution if necessary. Do not modify the time step
   * data or anything else.
   */
  void adjustSolution(CellDescription& cellDescription);

  /**
   * Body of FiniteVolumesSolver::adjustSolutionDuringMeshRefinement(int,int).
   */
  void adjustSolutionDuringMeshRefinementBody(
      CellDescription& cellDescription,
      const bool isInitialMeshRefinement);

  /**
   * This routine is called from the update(...) and
   * fusedTimeStep(...) functions.
   *
   * @return a struct holding an admissible time step size for the next update
   *
   * @param cellDescription         a cell description
   * @param cellInfo                links to all the cell descriptions associated with a cell
   * @param neighbourMergePerformed per face a flag indicating if a neighbour merge has been performed
   * @param isFirstTimeStepOfBatch  if the current time step is the first time step of a batch of time steps
   * @param isLastTimeStepOfBatch   if the current time step is the last time step of a batch of time steps
   * @param isAtRemoteBoundary      if the cell description is at a remote boundary.
   * @param uncompressBefore        if the cell description should uncompress data before doing any PDE operations
   *
   * @note Might be called by background task. Do not synchronise time step data here.
   */
  UpdateResult updateBody(
      CellDescription&                                           cellDescription,
      CellInfo&                                                  cellInfo,
      const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed,
      const bool                                                 isFirstTimeStepOfBatch,
      const bool                                                 isLastTimeStepOfBatch,
      const bool                                                 isAtRemoteBoundary,
      const bool                                                 uncompressBefore);

#ifdef Parallel
  /**
   * Data messages per neighbour communication.
   * This information is required by the sendEmpty...(...)
   * method.
   */
  static const int DataMessagesPerNeighbourCommunication;
  /**
   * Data messages per fork/join communication.
   * This information is required by the sendEmpty...(...)
   * method.
   */
  static const int DataMessagesPerForkOrJoinCommunication;
  /**
   * Data messages per master worker communication.
   * This information is required by the sendEmpty...(...)
   * method.
   */
  static const int DataMessagesPerMasterWorkerCommunication;
#endif

  /**
   * Allocate necessary memory and deallocate unnecessary memory.
   */
  static void ensureOnlyNecessaryMemoryIsAllocated(CellDescription& cellDescription);

  /**
   * There are no prolongations and restrictions
   * for the Finite Volums Solver in ExaHyPE
   *
   * @param[in] isSkeletonCell indicates that the cell is adjacent to a remote boundary.
   *            (There is currently no AMR for the pure FVM solver.)
   */
  void compress(
      CellDescription& cellDescription,
      const bool isSkeletonCell) const;
  /**
   * \copydoc ADERDGSolver::computeHierarchicalTransform()
   *
   * We assume a ordering of degrees of freedom according to (3D):
   *
   * Solution,previousSolution: [subcell[ijk],variable[l]]
   * extrapolatedSolution:      [face,subcell[ij],variable[l]]
   */
  void computeHierarchicalTransform(CellDescription& cellDescription, double sign) const;
  /**
   * We assume a ordering of degrees of freedom according to (3D):
   *
   * Solution,previousSolution: [subcell[ijk],variable[l]]
   * extrapolatedSolution:      [face,subcell[ij],variable[l]]
   */
  void determineUnknownAverages(CellDescription& cellDescription) const;
  void pullUnknownsFromByteStream(CellDescription& cellDescription) const;
  void putUnknownsIntoByteStream(CellDescription& cellDescription) const;
  void uncompress(CellDescription& cellDescription) const;

  /**
   * Perform a solution update.
   *
   * @param cellDescription          a cell description
   * @param neighbourMergePerformed  per face, a flag indicating if ghost layers have been copied over from a neighbour
   * @param cellDescriptionsIndex    a cell descriptions index for debuggin purposes
   * @param backupPreviousSolution   if the previous solution should be backed up or not. When running batches, we only want to back up the solution in the first step.
   */
  void updateSolution(
      CellDescription&                                           cellDescription,
      const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed,
      const int                                                  cellDescriptionsIndex,
      const bool                                                 backupPreviousSolution);

  /**
   * Rolls back the solver's solution on the cell description.
   * This method is used by the ADER-DG a-posteriori
   * subcell limiter (LimitingADERDGSolver).
   *
   * <h2>Open issues</h2>
   * A rollback is of course not possible if we have adjusted the solution
   * values. Assuming the rollback is invoked by a LimitingADERDGSolver,
   * we should use the adjusted FVM solution as reference solution.
   * A similar issue occurs if we impose initial conditions that
   * include a discontinuity.
   */
  void swapSolutionAndPreviousSolution(CellDescription& cellDescription) const;

  class CompressionJob: public tarch::multicore::jobs::Job {
    private:
      const FiniteVolumesSolver& _solver;
      CellDescription&           _cellDescription;
      const bool                 _isSkeletonJob;
    public:
      CompressionJob(
        const FiniteVolumesSolver& solver,
        CellDescription&           cellDescription,
        const bool                 isSkeletonJob);

      bool run() override;
  };

  /**
   * A job which performs the Finite Volumes solution update
   * and further updates the local time stamp associated with
   * the FV cell description.
   */
  class FusedTimeStepJob: public tarch::multicore::jobs::Job {
  private:
    FiniteVolumesSolver&                                      _solver;
    CellDescription&                                          _cellDescription;
    CellInfo                                                  _cellInfo; // copy
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char> _neighbourMergePerformed; // copy
    const bool                                                _isFirstTimeStepOfBatch;
    const bool                                                _isLastTimeStepOfBatch;
    const bool                                                _isSkeletonJob;
  public:
  /**
   * Construct a FusedTimeStepJob.
   *
   * @note Job is spawned as high priority job if spawned in the last time step.
   * It further spawns a prediction job in this case in order
   * to overlap work with the reduction of time step size
   * and mesh update events.
   *
   * @note The state of the neighbourMergePerformed flags is used internally by
   * some of the kernels, e.g. in order to determine where to perform a face integral.
   * However, they have to be reset before the next iteration as they indicate on
   * which face a Riemann solve has already been performed or not (their original usage).
   * The flags are thus reset directly after spawning a FusedTimeStepJob.
   * Therefore, we need to copy the neighbourMergePerformed flags when spawning
   * a FusedTimeStep job.
     *
     * @param solver                 the spawning solver
     * @param cellDescription        a cell description
     * @param cellInfo               links to all cell descriptions associated with the cell
     * @param isFirstTimeStepOfBatch if we currently run the first time step of a batch
     * @param isLastTimeStepOfBatch  if we currently run the last time step of a batch
     * @param isSkeletonJob          if the cell is a skeleton cell
     */
    FusedTimeStepJob(
        FiniteVolumesSolver& solver,
        CellDescription&     cellDescription,
        CellInfo&            cellInfo,
        const bool           isFirstTimeStepOfBatch,
        const bool           isLastTimeStepOfBatch,
        const bool           isSkeletonJob
    );

    bool run() override;
  };

  /**
   * A job which performs the solution update and computes a new time step size.
   *
   * @note Spawning these operations as background job makes only sense if you
   * wait in endIteration(...) on the completion of the job.
   * It further important to flag this job as high priority job to
   * ensure completion before the next reduction.
   */
  class UpdateJob: public tarch::multicore::jobs::Job {
  private:
    FiniteVolumesSolver&                                      _solver; // TODO not const because of kernels
    CellDescription&                                          _cellDescription;
    CellInfo                                                  _cellInfo;
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char> _neighbourMergePerformed; // copy
    const bool                                                _isAtRemoteBoundary;
  public:
    /**
     * Construct a UpdateJob.
     *
     * @note Job is always spawned as high priority job.
     *
     * @param solver                 the spawning solver
     * @param cellDescription        a cell description
     * @param cellInfo               links to all cell descriptions associated with the cell
     * @param isSkeletonJob          if the cell is a skeleton cell
     */
    UpdateJob(
        FiniteVolumesSolver&                                       solver,
        CellDescription&                                           cellDescription,
        CellInfo&                                                  cellInfo,
        const bool                                                 isAtRemoteBoundary);

    bool run() override;
    void prefetchData() override;
  };

  /**
   * A job that calls adjustSolutionDuringMeshRefinementBody(...).
   */
  class AdjustSolutionDuringMeshRefinementJob: public tarch::multicore::jobs::Job {
  private:
    FiniteVolumesSolver& _solver;
    CellDescription&     _cellDescription;
    const bool           _isInitialMeshRefinement;
  public:
    AdjustSolutionDuringMeshRefinementJob(
        FiniteVolumesSolver& solver,
        CellDescription&     cellDescription,
        const bool           isInitialMeshRefinement);

    bool run() override;
  };

public:
  /**
    * Returns the Finite Volumes description.
    */
   static Heap::HeapEntries& getCellDescriptions(
       const int cellDescriptionsIndex) {
     assertion1(Heap::getInstance().isValidIndex(cellDescriptionsIndex),cellDescriptionsIndex);

     return Heap::getInstance().getData(cellDescriptionsIndex);
   }

   /**
    * Returns the ADERDGCellDescription.
    */
   static CellDescription& getCellDescription(
       const int cellDescriptionsIndex,
       const int element) {
     assertion2(Heap::getInstance().isValidIndex(cellDescriptionsIndex),cellDescriptionsIndex,element);
     assertion2(element>=0,cellDescriptionsIndex,element);
     assertion2(static_cast<unsigned int>(element)<Heap::getInstance().getData(cellDescriptionsIndex).size(),cellDescriptionsIndex,element);

     return Heap::getInstance().getData(cellDescriptionsIndex)[element];
   }


   /**
    * Push a new cell description to the back
    * of the Finite Volumes cell descriptions heap vector referenced
    * in @p cellInfo.
    *
    * @param solverNumber    identification number of this solver
    * @param cellInfo        references to the cell descriptions associated with a mesh cell.
    * @param cellType        the cell type the new cell description should become
    * @param refinementEvent the initial refinement event the new cell description should get
    * @param level           the mesh level the new cell description is associated with
    * @param parentIndex     the cell descriptions index of a parent cell
    * @param cellSize        the size of the mesh cell the new cell description is associated with
    * @param cellOffset      the offset of the mesh cell the new cell description is associated with
    */
   static void addNewCellDescription(
       const int solverNumber,
       CellInfo& cellInfo,
       const CellDescription::Type cellType,
       const CellDescription::RefinementEvent refinementEvent,
       const int level,
       const int parentIndex,
       const tarch::la::Vector<DIMENSIONS, double>&  cellSize,
       const tarch::la::Vector<DIMENSIONS, double>&  cellOffset);


   /**
    * Returns if a ADERDGCellDescription type holds face data.
    */
   static bool holdsFaceData(const CellDescription::Type& cellDescriptionType) { return true; }


  /**
   * Erase all cell descriptions registered for solvers
   * of type Type::ADERDG.
   */
  static void eraseCellDescriptions(const int cellDescriptionsIndex);

  FiniteVolumesSolver(
      const std::string& identifier,
      const int numberOfVariables,
      const int numberOfParameters,
      const int numberOfGlobalObservables,
      const int basisSize,
      const int ghostLayerWidth,
      const double maximumMeshSize,
      const exahype::solvers::Solver::TimeStepping timeStepping,
      std::unique_ptr<profilers::Profiler> profiler =
          std::unique_ptr<profilers::Profiler>(
              new profilers::simple::NoOpProfiler("")));

  virtual ~FiniteVolumesSolver() {}

  // Disallow copy and assignment
  FiniteVolumesSolver(const FiniteVolumesSolver& other) = delete;
  FiniteVolumesSolver& operator=(const FiniteVolumesSolver& other) = delete;

  void initSolver(
      const double                                timeStamp,
      const tarch::la::Vector<DIMENSIONS,double>& domainOffset,
      const tarch::la::Vector<DIMENSIONS,double>& domainSize,
      const double                                boundingBoxSize,
      const double                                boundingBoxMeshSize,
      const std::vector<std::string>&             cmdlineargs,
      const exahype::parser::ParserView&          parserView) final override;

  void kickOffTimeStep(const bool isFirstTimeStepOfBatchOrNoBatch) final override;
  void wrapUpTimeStep(const bool isFirstTimeStepOfBatchOrNoBatch,const bool isLastTimeStepOfBatchOrNoBatch) final override;

  void updateTimeStepSize()      override;
  void rollbackToPreviousTimeStep() final override;

  void updateMeshUpdateEvent(MeshUpdateEvent meshUpdateEvent) final override;
  void resetMeshUpdateEvent() final override;
  MeshUpdateEvent getMeshUpdateEvent() const final override;
  double getPreviousMinTimeStepSize() const;
  double getMinTimeStamp() const override;
  double getMinTimeStepSize() const override;
  void updateAdmissibleTimeStepSize( double value ) override;
  double getAdmissibleTimeStepSize() const override;
  void resetAdmissibleTimeStepSize() final override;

  /**
   * The number of unknowns per patch.
   * This number does not include ghost layer values.
   * It does take into account the unknowns and the material parameters.
   */
  int getDataPerPatch() const;

  /**
   * Get the width of the ghost layer of the patch.
   */
  int getGhostLayerWidth() const;

  /**
   * Get the total number of ghost values per patch.
   */
  int getGhostDataPerPatch() const;

  /**
   * This operation returns the number of unknowns per
   * face of a patch.
   *
   * This number does not include ghost values.
   */
  int getDataPerPatchFace() const;

  /**
   * This operation returns the combined number of data
   * of all faces of a patch (variables and material parameter coefficients).
   *
   * This number does not include ghost values.
   */
  int getDataPerPatchBoundary() const;

  /**
   * This operation returns the number of unknowns that are located
   * on or in the vicinity of the boundary of a cell.
   */
  int getUnknownsPerPatchBoundary() const;


  virtual int getTempUnknownsSize()              const {return getDataPerPatch();} // TODO function should be renamed
  virtual int getBndFaceSize()                   const {return getDataPerPatchFace();} // TODO function should be renamed

  bool isPerformingPrediction(const exahype::State::AlgorithmSection& section) const override;
  bool isMergingMetadata(const exahype::State::AlgorithmSection& section) const override;

  static bool isValidCellDescriptionIndex(const int cellDescriptionsIndex);

  int tryGetElement(
      const int cellDescriptionsIndex,
      const int solverNumber) const override;


  /**
   * Compute a load balancing weight for a cell in the mesh.
   */
  static int computeWeight(const int cellDescriptionsIndex);

  ///////////////////////////////////
  // MODIFY CELL DESCRIPTION
  ///////////////////////////////////


  /**
   * Initialise cell description of type Cell.
   * Initialise the refinement event with None.
   */
  void addNewCell(
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      const int coarseGridCellDescriptionsIndex,
      const int solverNumber);

  /**
   * Check if the heap array with index @p index could be allocated.
   */
  static void checkDataHeapIndex(const CellDescription& cellDescription, const int arrayIndex, const std::string arrayName);

  /**
   * Checks if no unnecessary memory is allocated for the cell description.
   * If this is not the case, it deallocates the unnecessarily allocated memory.
   */
  void ensureNoUnnecessaryMemoryIsAllocated(CellDescription& cellDescription) const;

  /**
   * Checks if all the necessary memory is allocated for the cell description.
   * If this is not the case, it allocates the necessary
   * memory for the cell description.
   *
   * @note Heap data creation assumes default policy
   * DataHeap::Allocation::UseRecycledEntriesIfPossibleCreateNewEntriesIfRequired.
   */
  void ensureNecessaryMemoryIsAllocated(CellDescription& cellDescription) const;

  bool progressMeshRefinementInEnterCell(
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const int  solverNumber,
      const bool stillInRefiningMode) override;

  bool progressMeshRefinementInLeaveCell(
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
      const int solverNumber,
      const bool stillInRefiningMode) override;

  exahype::solvers::Solver::RefinementControl eraseOrRefineAdjacentVertices(
          const int cellDescriptionsIndex,
          const int solverNumber,
          const tarch::la::Vector<DIMENSIONS, double>& cellOffset,
          const tarch::la::Vector<DIMENSIONS, double>& cellSize,
          const int level,
          const bool checkThoroughly) const final override;

  bool attainedStableState(
      exahype::Cell&                       fineGridCell,
      exahype::Vertex* const               fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      const int                            solverNumber,
      const bool                           stillInRefiningMode) const final override;

  void finaliseStateUpdates(
      const int solverNumber,
      CellInfo& cellInfo) final override;

  ///////////////////////////////////
  // CELL-LOCAL
  //////////////////////////////////
  double updateTimeStepSize(const int solverNumber,CellInfo& cellInfo) final override;

  double startNewTimeStep(CellDescription& cellDescription,const bool isFirstTimeStepOfBatch);

  /**
   * Set the previous time stamp and step size for the patch.
   *
   * @note the original time stamp and step size is lost (although the time stamp can be easily recalculated).
   */
  void rollbackToPreviousTimeStep(CellDescription& cellDescription) const;

  /** @copydoc: exahype::solvers::Solver::fusedTimeStepOrRestrict
   *
   * The "hasCompletedLastStep" flag must only be unset when
   * a background job is spawned.
   */
  UpdateResult fusedTimeStepOrRestrict(
      const int solverNumber,
      CellInfo& cellInfo,
      const bool isFirstTimeStepOfBatch,
      const bool isLastTimeStepOfBatch,
      const bool isAtRemoteBoundary) final override;

  UpdateResult updateOrRestrict(
        const int  solverNumber,
        CellInfo&  cellInfo,
        const bool isAtRemoteBoundary) final override;

  void adjustSolutionDuringMeshRefinement(const int solverNumber,CellInfo& cellInfo) final override;

  /**
   * Does nothing as a FV solver should never do global rollbacks;
   * no mesh refinement is performed by this solver type.
   */
  void rollbackSolutionGlobally(const int solverNumber,CellInfo& cellInfo) const final override;

  void compress(
      const int solverNumber,
      CellInfo& cellInfo,
      const bool isAtRemoteBoundary) const final override;

  ///////////////////////////////////
  // NEIGHBOUR
  ///////////////////////////////////
  void mergeNeighboursData(
      const int                                 solverNumber,
      Solver::CellInfo&                         context1,
      Solver::CellInfo&                         context2,
      const tarch::la::Vector<DIMENSIONS, int>& pos1,
      const tarch::la::Vector<DIMENSIONS, int>& pos2);

  void mergeWithBoundaryData(
      const int                                 solverNumber,
      Solver::CellInfo&                         context,
      const tarch::la::Vector<DIMENSIONS, int>& posCell,
      const tarch::la::Vector<DIMENSIONS, int>& posBoundary);


#ifdef Parallel
  ///////////////////////////////////
  // MASTER<=>WORKER
  ///////////////////////////////////

  void appendMasterWorkerCommunicationMetadata(
      exahype::MetadataHeap::HeapEntries& metadata,
      const int cellDescriptionsIndex,
      const int solverNumber) const override;

  /**
     * Sets heap indices of an FiniteVolumesCellDescription to -1,
     * and the parent index of the cell descriptions to the specified @p
     * parentIndex.
     */
   static void resetIndicesAndFlagsOfReceivedCellDescription(CellDescription& cellDescription,const int parentIndex);

  /**
   * Send all ADERDG cell descriptions to rank
   * @p toRank.
   */
  static void sendCellDescriptions(
      const int                                     toRank,
      const int                                     cellDescriptionsIndex,
      const peano::heap::MessageType&               messageType,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level);

  /**
   * Send an empty message to rank
   * @p toRank.
   */
  static void sendEmptyCellDescriptions(
      const int                                     toRank,
      const peano::heap::MessageType&               messageType,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level);

  /**
   * Receives cell descriptions from rank @p fromRank
   * and resets the data heap indices to -1.
   *
   * If a received cell description has the same
   * solver number as a cell description in the
   * array at address @p cellDescriptionsIndex,
   * we merge the metadata (time stamps, time step size)
   * of both cell descriptions.
   *
   * If no cell description in the array at address
   * @p cellDescriptionsIndex can be found with the
   * same solver number than a received cell description,
   * we push the received cell description to
   * the back of the array at address @p cellDescriptions
   * Index.
   *
   * This operation is intended to be used in combination
   * with the solver method mergeWithWorkerOrMasterDataDueToForkOrJoin(...).
   * Here, we would merge first the cell descriptions sent by the master and worker
   * and then merge the data that is sent out right after.
   */
  static void receiveCellDescriptions(
      const int                                     fromRank,
      exahype::Cell&                                localCell,
      const peano::heap::MessageType&               messageType,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level);

  /**
   * Drop cell descriptions received from @p fromRank.
   */
  static void dropCellDescriptions(
      const int                                     fromRank,
      const peano::heap::MessageType&               messageType,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level);

  ///////////////////////////////////
  // NEIGHBOUR
  ///////////////////////////////////
  void appendNeighbourCommunicationMetadata(
      exahype::MetadataHeap::HeapEntries& metadata,
      const tarch::la::Vector<DIMENSIONS,int>& src,
      const tarch::la::Vector<DIMENSIONS,int>& dest,
      const int cellDescriptionsIndex,
      const int solverNumber) const override;

  /**
   * Send boundary layers to a neighbouring rank.
   *
   * @note Assumes cell is initialised. Solver offers
   * sendEmptyDataToNeighbour for other case.
   *
   * @param toRank       the adjacent rank we want to send to
   * @param solverNumber identification number for the solver
   * @param cellInfo     links to a cells data
   * @param src          position of message source relative to vertex
   * @param dest         position of message destination relative to vertex
   * @param x            vertex' position
   * @param level        vertex' level
   */
  void sendDataToNeighbour(
      const int                                     toRank,
      const int                                     solverNumber,
      Solver::CellInfo&                             cellInfo,
      const tarch::la::Vector<DIMENSIONS, int>&     src,
      const tarch::la::Vector<DIMENSIONS, int>&     dest,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level);

  /**
   * Send zero-length message to a neighbouring rank.
   *
   * @param toRank       the adjacent rank we want to send to
   * @param x            vertex' position
   * @param level        vertex' level
   */
  void sendEmptyDataToNeighbour(
      const int                                     toRank,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level) const;

  /**
   * Receive data and merge it if the face data exchange counter
   * demands it. Otherwise, drop the data.
   *
   * @param fromRank       rank we expect to receive data from
   * @param cellInfo       information about the cell for which we want to receive data
   * @param solverNumber   identification number for this solver
   * @param src            relative position of message source      to vertex.
   * @param src            relative position of message destination to vertex.
   * @param x              vertex' position
   * @param level          vertex' level
   */
  void mergeWithNeighbourData(
      const int                                    fromRank,
      const int                                    solverNumber,
      Solver::CellInfo&                            cellInfo,
      const tarch::la::Vector<DIMENSIONS, int>&    src,
      const tarch::la::Vector<DIMENSIONS, int>&    dest,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level);

  void dropNeighbourData(
      const int                                     fromRank,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level) const;

  ///////////////////////
  // MASTER <=> WORKER
  ///////////////////////

  void sendDataToWorkerOrMasterDueToForkOrJoin(
      const int                                     toRank,
      const int                                     cellDescriptionsIndex,
      const int                                     element,
      const peano::heap::MessageType&               messageType,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level) const override;

  void mergeWithWorkerOrMasterDataDueToForkOrJoin(
      const int                                     fromRank,
      const int                                     cellDescriptionsIndex,
      const int                                     element,
      const peano::heap::MessageType&               messageType,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level) const override;

  ///////////////////////////////////
  // WORKER->MASTER
  ///////////////////////////////////

  /**
   * Nop
   */
  void progressMeshRefinementInPrepareSendToWorker(
      const int workerRank,
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const int solverNumber) final override;

  /**
   * Nop
   */
  void sendDataToWorkerIfProlongating(
      const int                                     toRank,
      const int                                     cellDescriptionsIndex,
      const int                                     element,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level) const final override;

  /**
   * Nop
   */
  void receiveDataFromMasterIfProlongating(
      const int masterRank,
      const int receivedCellDescriptionsIndex,
      const int receivedElement,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int level) const final override;

  /**
   * Nop as it does not support adaptivity
   *
   * \return false
   */
  bool progressMeshRefinementInMergeWithWorker(
      const int localCellDescriptionsIndex,
      const int receivedCellDescriptionsIndex, const int receivedElement) final override;

  /**
   * Nop
   */
  void progressMeshRefinementInPrepareSendToMaster(
      const int masterRank,
      const int cellDescriptionsIndex, const int element,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int level) const final override;

  /**
   * Nop. TODO(Dominic): As long as no multi-solver and limiter
   */
  bool progressMeshRefinementInMergeWithMaster(
        const int worker,
        const int localCellDescriptionsIndex,
        const int localElement,
        const int coarseGridCellDescriptionsIndex,
        const tarch::la::Vector<DIMENSIONS, double>& x,
        const int                                    level,
        const bool                                   stillInRefiningMode) final override;

  ///////////////////////////////////
  // WORKER->MASTER
  ///////////////////////////////////

  void sendDataToMaster(
      const int                                    masterRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const override;

  void mergeWithWorkerData(
      const int                                    workerRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) override;

  ///////////////////////////////////
  // MASTER->WORKER
  ///////////////////////////////////
  void sendDataToWorker(
      const                                        int workerRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const override;

  void mergeWithMasterData(
      const                                        int masterRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) override;

#endif

  void validateNoNansInFiniteVolumesSolution(CellDescription& cellDescription,const int cellDescriptionsIndex,const char* methodTrace) const;

  void printFiniteVolumesSolution(CellDescription& cellDescription) const;

  void printFiniteVolumesBoundaryLayer(const double* const luhbnd)  const;

  std::string toString() const override;

  void toString (std::ostream& out) const override;

  using Solver::reduceGlobalObservables;
  void reduceGlobalObservables(std::vector<double>& globalObservables,
                                   CellInfo cellInfo,
                                   int solverNumber) const override;
  
  ///////////////////////
  // PROFILING
  ///////////////////////

  CellProcessingTimes measureCellProcessingTimes(const int numberOfRuns=100) override;

protected:
  /** @name Plugin points for derived solvers.
   *
   *  These are the macro kernels and user hooks solvers derived from
   *  FiniteVolumesSolver need to implement.
   */
  ///@{
  /**
   * Initialise the solver using command line arguments and specification
   * file parameters.
   *
   * @param cmdlineargs command line arguments as passed to the applciation
   * @param parameters  user-defined parameters as specified in the specification file
   */
  virtual void init(
      const std::vector<std::string>& cmdlineargs,
      const exahype::parser::ParserView& parameters) = 0;

  /**
   * @brief Returns a stable time step size.
   *
   * @param[in] luh             Cell-local solution DoF.
   * @param[in] cellSize        Extent of the cell in each coordinate direction.
   */
  virtual double stableTimeStepSize(
      const double* const                          luh,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize) = 0;

  /**
   * Extract volume averages belonging to the boundary layer
   * of the neighbour patch and store them in the ghost layer
   * of the current patch.
   *
   * Depending on the implementation (if reconstruction is applied),
   * the boundary layer/ghost layer might not just be a single layer.
   *
   * @param[inout] luh               the solution vector (with updated ghost layers).
   * @param[in]    luhbnd            hold the boundary layers obtained from the neighbour.
   * @param[in]    neighbourPosition contains the relative position of the neighbour patch
   *                          with respect to the patch this method was invoked for. The entries of the vector are in the range
   *                          {-1,0,1}.
   *
   * @note The theoretical arithmetic intensity of this operation is zero.
   * @note This operation is invoked per vertex in touchVertexFirstTime and mergeWithNeighbour
   * in mapping Merging.
   *
   * <h2>MPI</h2>
   * No ghost layer is necessary if a patch is surrounded only
   * by local cells. However as soon as the cell is adjacent
   * to a MPI boundary this becomes necessary.
   * We thus always hold ghost layers.
   */
  virtual void ghostLayerFilling(
      double* const                                  luh,
      const double* const                            luhNeighbour,
      const tarch::la::Vector<DIMENSIONS,int>& boundaryPosition) = 0;

  /**
   * Similar to ghostLayerFilling but we do not work with
   * complete patches from a local neighbour here but with smaller arrays received
   * from a remote neighbour or containing boundary conditions.
   *
   * @note The theoretical arithmetic intensity of this operation is zero.
   * @note This operation is invoked per vertex in mergeWithNeighbour in mapping Merging.
   *
   * @param[inout] luh              the local solution vector
   * @param[in]    luhbnd           a vector with enough space to hold the method-specific boundary layers
   * @param[in]    boundaryPosition positon of the boundary w.r.t. the cell
   */
  virtual void ghostLayerFillingAtBoundary(
      double* const                                  luh,
      const double* const                            luhbnd,
      const tarch::la::Vector<DIMENSIONS,int>& boundaryPosition) = 0;

  /**
   * Extract boundary layers of @p luh before
   * sending them away via MPI, e.g.
   *
   * @note The theoretical arithmetic intensity of this operation is zero.
   * @note This operation is invoked per vertex in prepareSendToNeighbour in mapping Sending.
   *
   * @param[inout] luhbnd           a vector with enough space to hold the method-specific boundary layers
   * @param[in]    luh              the local solution vector
   * @param[in]    boundaryPosition positon of the boundary w.r.t. the cell
   */
  virtual void boundaryLayerExtraction(
      double* const                                  luhbnd,
      const double* const                            luh,
      const tarch::la::Vector<DIMENSIONS,int>& boundaryPosition) = 0;

  /**
   * Return the state variables at the boundary.
   *
   * @param[inout] luh         the solution patch
   * @param[in]    cellCentre  cell centre.
   * @param[in]    dx          cell size.
   * @param[in]    t           a time stamp.
   * @param[in]    dt          a time step size.
   * @param[in]    posCell     position of the cell w.r.t. a vertex.
   * @param[in]    posBoundary position of the boundary w.r.t. the same vertex.
   */
  virtual void boundaryConditions(
      double* const luh,
      const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>& dx,
      const double                                t,
      const double                                dt,
      const tarch::la::Vector<DIMENSIONS, int>&   posCell,
      const tarch::la::Vector<DIMENSIONS, int>&   posBoundary) = 0;


  /**
   * Compute the Riemann problem.
   *
   * This function shall implement a pointwise riemann Solver, in contrast to the ADERDGSolver::riemannSolver
   * function which implements a patch-wise riemann solver.
   *
   * In a fully conservative scheme, it is fL = fR and the Riemann solver really computes the fluxes
   * in normalNonzero direction steming from the contribution of qL and qR.
   *
   * @param[out] fL        the fluxes on the left side of the point cell (already allocated)
   * @param[out] fR        the fluxes on the right side of the point cell (already allocated).
   * @param[in]  qL        the state vector in the left neighbour cell
   * @param[in]  qR        the state vector in the right neighbour cell
   * @param[in]  direction coordinate direction the normal vector of the face points to,
   *                       i.e., 0 for e_x, 1 for e_y, and 2 for e_z.
   */
  virtual double riemannSolver(
      double* const       fL,
      double* const       fR,
      const double* const qL,
      const double* const qR,
      const double* gradQL,
      const double* gradQR,
      const double* cellSize,
      const int     direction) = 0;

  /**
   * Update the solution of all volumes on a patch.
   *
   * @param[inout] luh             the current (and then new) solution
   * @param[in]    dx              the extends of the cell holding the FV patch
   * @param[in]    dt              time step size the FV patch is marching with
   * @param[inout] maxAdmissibleDt admissible time step size obtained from the Riemann solves
   */
  virtual void solutionUpdate(
      double* const                                      luh,
      const tarch::la::Vector<DIMENSIONS, double>& center,
      const tarch::la::Vector<DIMENSIONS, double>& dx,
      const double t,
      const double dt, double&                     maxAdmissibleDt) = 0;

  /**
   * Adjust the conserved variables and parameters (together: Q) at a given time t at the (quadrature) point x.
   *
   * @note Use this function and ::useAdjustSolution to set initial conditions.
   *
   * @param[in]    x         the physical coordinate on the face.
   * @param[in]    t         the start of the time interval.
   * @param[in]    dt        the width of the time interval.
   * @param[inout] Q         the conserved variables (and parameters) associated with a quadrature point
   *                         as C array (already allocated).
   */
  virtual void adjustSolution(
      double* const                                luh,
      const tarch::la::Vector<DIMENSIONS, double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS, double>& dx,
      const double                                 t,
      const double                                 dt) = 0;

  /**
   * Pointwise solution adjustment.
   *
   * In the FV solver, we currently don't support both patchwise
   * and pointwise adjustment @TODO.
   *
   * @param[in]   x   The position (array with DIMENSIONS entries)
   * @param[in]   t   the start of the time interval
   * @param[in]   dt  the width of the time interval.
   * @param[inout] Q  the conserved variables and parameters as C array (already allocated).
   *
   **/
  virtual void adjustSolution(
      const double* const x,
      const double        t,
      const double        dt,
      double* const       Q) = 0;
  ///@}
};

#endif
