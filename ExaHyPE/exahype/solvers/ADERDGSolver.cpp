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
 * \author Dominic E. Charrier, Tobias Weinzierl, Jean-Matthieu Gallard, Fabian Gra, Leonhard Rannabauer
 **/
#include "exahype/solvers/ADERDGSolver.h"

#include <limits>
#include <iomanip>
#include <chrono>
#include <algorithm> // copy_n

#include "exahype/Cell.h"
#include "exahype/Vertex.h"
#include "exahype/VertexOperations.h"

#include "tarch/la/VectorVectorOperations.h"
#include "tarch/multicore/Lock.h"

#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

#include "exahype/amr/AdaptiveMeshRefinement.h"

#include "peano/heap/CompressedFloatingPointNumbers.h"
#include "peano/datatraversal/TaskSet.h"

#include "peano/grid/aspects/VertexStateAnalysis.h"

#include "exahype/solvers/LimitingADERDGSolver.h"

#include "kernels/KernelUtils.h"

#include "tarch/multicore/Jobs.h"
#include "tarch/la/Vector.h"

#if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
#include <immintrin.h>
#endif

#ifdef USE_ITAC
#include "VT.h"
#endif

namespace {
  constexpr const char* tags[]{"solutionUpdate",
                             "volumeIntegral",
                             "surfaceIntegral",
                             "riemannSolver",
                             "spaceTimePredictor",
                             "stableTimeStepSize",
                             "solutionAdjustment",
                             "faceUnknownsProlongation",
                             "faceUnknownsRestriction",
                             "volumeUnknownsProlongation",
                             "volumeUnknownsRestriction",
                             "boundaryConditions",
                             "deltaDistribution"
                             };
}

#ifdef USE_ITAC
int exahype::solvers::ADERDGSolver::adjustSolutionHandle                 = 0;
int exahype::solvers::ADERDGSolver::fusedTimeStepBodyHandle              = 0;
int exahype::solvers::ADERDGSolver::fusedTimeStepBodyHandleSkeleton      = 0;
int exahype::solvers::ADERDGSolver::predictorBodyHandle                  = 0;
int exahype::solvers::ADERDGSolver::predictorBodyHandleSkeleton          = 0;
int exahype::solvers::ADERDGSolver::updateBodyHandle                     = 0;
int exahype::solvers::ADERDGSolver::mergeNeighboursHandle                = 0;
int exahype::solvers::ADERDGSolver::prolongateFaceDataToDescendantHandle = 0;
int exahype::solvers::ADERDGSolver::restrictToTopMostParentHandle        = 0;
#endif

tarch::logging::Log exahype::solvers::ADERDGSolver::_log( "exahype::solvers::ADERDGSolver");

// communication status
int exahype::solvers::ADERDGSolver::CellCommunicationStatus                             = 2;
int exahype::solvers::ADERDGSolver::MinimumCommunicationStatusForNeighbourCommunication = 1;
// augmentation status
// On-the fly erasing seems to work with those values
int exahype::solvers::ADERDGSolver::MaximumAugmentationStatus                   = 4;
int exahype::solvers::ADERDGSolver::MinimumAugmentationStatusForVirtualRefining = 3;
int exahype::solvers::ADERDGSolver::MinimumAugmentationStatusForRefining        = 3;

/**
 * static constexpr need to declared again when following a
 * C++ standard before C++17.
 */
constexpr int exahype::solvers::ADERDGSolver::BoundaryStatus;
constexpr int exahype::solvers::ADERDGSolver::Pending;
constexpr int exahype::solvers::ADERDGSolver::Erase; 
constexpr int exahype::solvers::ADERDGSolver::Keep;

tarch::multicore::BooleanSemaphore exahype::solvers::ADERDGSolver::RestrictionSemaphore;

tarch::multicore::BooleanSemaphore exahype::solvers::ADERDGSolver::CoarseGridSemaphore;

int exahype::solvers::ADERDGSolver::computeWeight(const int cellDescriptionsIndex) {
  if ( ADERDGSolver::isValidCellDescriptionIndex(cellDescriptionsIndex) ) {
    int result = 0;
    for ( CellDescription& cellDescription : getCellDescriptions(cellDescriptionsIndex) ) {
      result += ( cellDescription.getType()==CellDescription::Type::Cell ) ?  1 : 0;
    }
    return result;
  }
  else return 0;
}

void exahype::solvers::ADERDGSolver::addNewCellDescription(
    const int                                    solverNumber,
    CellInfo&                                    cellInfo,
    const CellDescription::Type                  cellType,
    const CellDescription::RefinementEvent       refinementEvent,
    const int                                    level,
    const int                                    parentIndex,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize,
    const tarch::la::Vector<DIMENSIONS, double>& cellOffset) {
  assertion2(parentIndex == -1 || parentIndex != cellInfo._cellDescriptionsIndex, parentIndex, cellInfo._cellDescriptionsIndex);
  assertion2(parentIndex != cellInfo._cellDescriptionsIndex, parentIndex, cellInfo._cellDescriptionsIndex);
  logDebug("addNewCellDescription(...)","Add cell description: index="<<cellInfo._cellDescriptionsIndex<<",type="<<CellDescription::toString(cellType)<<",level="<<level<<",parentIndex="<<parentIndex << ",solver=" <<solverNumber);

  assertion2(static_cast<unsigned int>(solverNumber) < solvers::RegisteredSolvers.size(),solverNumber,exahype::solvers::RegisteredSolvers.size());

  CellDescription newCellDescription;
  newCellDescription.setSolverNumber(solverNumber);

  // Background job completion monitoring (must be initialised with true)
  newCellDescription.setHasCompletedLastStep(true);

  // Default AMR settings
  newCellDescription.setType(cellType);
  newCellDescription.setParentIndex(parentIndex);
  newCellDescription.setLevel(level);
  newCellDescription.setRefinementEvent(refinementEvent);

  newCellDescription.setHasVirtualChildren(false);
  newCellDescription.setAugmentationStatus(0);
  newCellDescription.setFacewiseAugmentationStatus(0); // implicit conversion
  newCellDescription.setCommunicationStatus(0);
  newCellDescription.setFacewiseCommunicationStatus(0); // implicit conversion
  if (cellType==CellDescription::Type::Cell) {
    newCellDescription.setCommunicationStatus(CellCommunicationStatus);
    newCellDescription.setFacewiseCommunicationStatus(CellCommunicationStatus); // implicit conversion
    // TODO(Dominic): Make sure prolongation and restriction considers this.
  }
  newCellDescription.setNeighbourMergePerformed((signed char) 0/*implicit conversion*/);

  // Pass geometry information to the cellDescription description
  newCellDescription.setSize(cellSize);
  newCellDescription.setOffset(cellOffset);

  // Default field data indices
  newCellDescription.setSolutionIndex(-1);
  newCellDescription.setSolution(nullptr);
  newCellDescription.setPreviousSolutionIndex(-1);
  newCellDescription.setPreviousSolution(nullptr);
  newCellDescription.setUpdateIndex(-1);
  newCellDescription.setUpdate(nullptr);
  newCellDescription.setExtrapolatedPredictorIndex(-1);
  newCellDescription.setExtrapolatedPredictor(nullptr);
  newCellDescription.setFluctuationIndex(-1);
  newCellDescription.setFluctuation(nullptr);

  newCellDescription.setVetoErasingChildren(false);
  // Halo/Limiter meta data (oscillations identificator)
  newCellDescription.setRefinementFlag(false);
  newCellDescription.setRefinementStatus(Pending);
  newCellDescription.setPreviousRefinementStatus(Pending); 
  newCellDescription.setFacewiseRefinementStatus(Pending);  // implicit conversion
  newCellDescription.setSolutionMinIndex(-1);
  newCellDescription.setSolutionMin(0);
  newCellDescription.setSolutionMaxIndex(-1);
  newCellDescription.setSolutionMax(0);
  newCellDescription.setIterationsToCureTroubledCell(0);

  // Compression
  newCellDescription.setCompressionState(CellDescription::CompressionState::Uncompressed);
  newCellDescription.setSolutionAveragesIndex(-1);
  newCellDescription.setSolutionAverages(nullptr);
  newCellDescription.setPreviousSolutionAveragesIndex(-1);
  newCellDescription.setPreviousSolutionAverages(nullptr);
  newCellDescription.setUpdateAveragesIndex(-1);
  newCellDescription.setUpdateAverages(nullptr);
  newCellDescription.setExtrapolatedPredictorAveragesIndex(-1);
  newCellDescription.setExtrapolatedPredictorAverages(nullptr);
  newCellDescription.setFluctuationAveragesIndex(-1);
  newCellDescription.setFluctuationAverages(nullptr);

  newCellDescription.setSolutionCompressedIndex(-1);
  newCellDescription.setSolutionCompressed(nullptr);
  newCellDescription.setPreviousSolutionAveragesIndex(-1);
  newCellDescription.setPreviousSolutionAverages(nullptr);
  newCellDescription.setUpdateCompressedIndex(-1);
  newCellDescription.setUpdateCompressed(nullptr);
  newCellDescription.setExtrapolatedPredictorCompressedIndex(-1);
  newCellDescription.setExtrapolatedPredictorCompressed(nullptr);
  newCellDescription.setFluctuationCompressedIndex(-1);
  newCellDescription.setFluctuationCompressed(nullptr);

  newCellDescription.setBytesPerDoFInExtrapolatedPredictor(-1);
  newCellDescription.setBytesPerDoFInFluctuation(-1);
  newCellDescription.setBytesPerDoFInPreviousSolution(-1);
  newCellDescription.setBytesPerDoFInSolution(-1);
  newCellDescription.setBytesPerDoFInUpdate(-1);

  #ifdef Asserts
  newCellDescription.setCreation(CellDescription::Creation::NotSpecified);
  #endif

  tarch::multicore::Lock lock(exahype::HeapSemaphore);
  cellInfo._ADERDGCellDescriptions.push_back(newCellDescription);
  lock.free();
}

/**
 * Returns the ADERDGCellDescription heap vector
 * at address \p cellDescriptionsIndex.
 */
exahype::solvers::ADERDGSolver::Heap::HeapEntries& exahype::solvers::ADERDGSolver::getCellDescriptions(
    const int cellDescriptionsIndex) {
  assertion1(Heap::getInstance().isValidIndex(cellDescriptionsIndex),cellDescriptionsIndex);

  return Heap::getInstance().getData(cellDescriptionsIndex);
}

/**
 * Returns the ADERDGCellDescription with index \p element
 * in the heap vector at address \p cellDescriptionsIndex.
 */
exahype::solvers::ADERDGSolver::CellDescription& exahype::solvers::ADERDGSolver::getCellDescription(
    const int cellDescriptionsIndex,
    const int element) {
  assertion2(Heap::getInstance().isValidIndex(cellDescriptionsIndex),cellDescriptionsIndex,element);
  assertion2(element>=0,cellDescriptionsIndex,element);
  assertion2(static_cast<unsigned int>(element)<Heap::getInstance().getData(cellDescriptionsIndex).size(),cellDescriptionsIndex,element);

  return Heap::getInstance().getData(cellDescriptionsIndex)[element];
}


bool exahype::solvers::ADERDGSolver::holdsFaceData(const CellDescription& cellDescription) {
  return cellDescription.getType() != CellDescription::Type::Ancestor &&
         cellDescription.getCommunicationStatus()>=MinimumCommunicationStatusForNeighbourCommunication;
}

bool exahype::solvers::ADERDGSolver::communicateWithNeighbour(const CellDescription& cellDescription,const int faceIndex) {
  assertion1(cellDescription.getType()!=CellDescription::Type::Cell ||
            cellDescription.getCommunicationStatus()==CellCommunicationStatus,cellDescription.toString());
  return
      (cellDescription.getCommunicationStatus()                  == CellCommunicationStatus &&
      cellDescription.getFacewiseCommunicationStatus(faceIndex)  >= MinimumCommunicationStatusForNeighbourCommunication &&
      cellDescription.getFacewiseAugmentationStatus(faceIndex)   <  MaximumAugmentationStatus)
      ||
      (cellDescription.getFacewiseCommunicationStatus(faceIndex) == CellCommunicationStatus &&
      cellDescription.getCommunicationStatus()                   >= MinimumCommunicationStatusForNeighbourCommunication &&
      cellDescription.getAugmentationStatus()                    <  MaximumAugmentationStatus);
}

void exahype::solvers::ADERDGSolver::prefetchFaceData(CellDescription& cellDescription,const int faceIndex) {
  #if defined(SharedTBB) && !defined(noTBBPrefetchesJobData)
  auto* solver = solvers::RegisteredSolvers[cellDescription.getSolverNumber()];
  int dataPerFace = 0;
  int dofPerFace  = 0;
  switch (solver->getType()) {
    case Solver::Type::ADERDG:
      dataPerFace = static_cast<ADERDGSolver*>(solver)->getBndFaceSize();
      dofPerFace  = static_cast<ADERDGSolver*>(solver)->getBndFluxSize();
      break;
    case Solver::Type::LimitingADERDG:
      dataPerFace = static_cast<LimitingADERDGSolver*>(solver)->getSolver()->getBndFaceSize();
      dofPerFace  = static_cast<LimitingADERDGSolver*>(solver)->getSolver()->getBndFluxSize();
      break;
    default:
      break;
  }

  double* lQhbnd = static_cast<double*>(cellDescription.getExtrapolatedPredictor()) + faceIndex * dataPerFace;
  double* lFhbnd = static_cast<double*>(cellDescription.getFluctuation())           + faceIndex * dofPerFace;

  _mm_prefetch(lQhbnd, _MM_HINT_NTA);
  _mm_prefetch(lFhbnd, _MM_HINT_NTA);
  #endif
}

void exahype::solvers::ADERDGSolver::ensureNoUnnecessaryMemoryIsAllocated(
    CellDescription& cellDescription) const {

  if (
      cellDescription.getType()!=CellDescription::Type::Cell &&
      DataHeap::getInstance().isValidIndex(cellDescription.getSolutionIndex())
  ) {
    tarch::multicore::Lock lock(exahype::HeapSemaphore);

    assertion(DataHeap::getInstance().isValidIndex(cellDescription.getSolutionIndex()));
    assertion(DataHeap::getInstance().isValidIndex(cellDescription.getPreviousSolutionIndex()));

    if ( cellDescription.getSolutionIndex()>=0 ) {
      DataHeap::getInstance().deleteData(cellDescription.getSolutionIndex());
      assertion(cellDescription.getSolutionCompressedIndex()==-1);
    }
    else {
      assertion(CompressionAccuracy>0.0);
      assertion(cellDescription.getSolutionIndex()==-1);
      CompressedDataHeap::getInstance().deleteData(cellDescription.getSolutionCompressedIndex());
    }

    DataHeap::getInstance().deleteData(cellDescription.getSolutionAveragesIndex());
    DataHeap::getInstance().deleteData(cellDescription.getPreviousSolutionAveragesIndex());

    cellDescription.setPreviousSolutionIndex(-1);
    cellDescription.setPreviousSolution(nullptr);
    cellDescription.setSolutionIndex(-1);
    cellDescription.setSolution(nullptr);

    cellDescription.setPreviousSolutionAveragesIndex(-1);
    cellDescription.setPreviousSolutionAverages(nullptr);
    cellDescription.setSolutionAveragesIndex(-1);
    cellDescription.setSolutionAverages(nullptr);

    cellDescription.setPreviousSolutionCompressedIndex(-1);
    cellDescription.setPreviousSolutionCompressed(nullptr);
    cellDescription.setSolutionCompressedIndex(-1);
    cellDescription.setSolutionCompressed(nullptr);

    lock.free();
  }

  // deallocate update and boundary arrays
  if (
      !holdsFaceData(cellDescription) &&
      DataHeap::getInstance().isValidIndex(cellDescription.getUpdateIndex())
  ) {
    // update
    assertion(DataHeap::getInstance().isValidIndex(cellDescription.getUpdateIndex()));
    if ( cellDescription.getUpdateIndex()>=0 ) {
      assertion(cellDescription.getUpdateCompressedIndex()==-1);

      DataHeap::getInstance().deleteData(cellDescription.getUpdateIndex());
      cellDescription.setUpdateIndex(-1);
      cellDescription.setUpdate(nullptr);
    }
    else {
      assertion(CompressionAccuracy>0.0);
      assertion(cellDescription.getUpdateIndex()==-1);

      CompressedDataHeap::getInstance().deleteData(cellDescription.getUpdateCompressedIndex());
      cellDescription.setUpdateCompressedIndex(-1);
      cellDescription.setUpdateCompressed(nullptr);
    }
    DataHeap::getInstance().deleteData(cellDescription.getUpdateAveragesIndex());
    cellDescription.setUpdateAveragesIndex(-1);
    cellDescription.setUpdateAverages(nullptr);

    // extrapolated predictor
    tarch::multicore::Lock lock(exahype::HeapSemaphore);
    if ( cellDescription.getExtrapolatedPredictorIndex()>=0 ) {
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex()));
      assertion(cellDescription.getExtrapolatedPredictorCompressedIndex()==-1);

      DataHeap::getInstance().deleteData(cellDescription.getExtrapolatedPredictorIndex());
      cellDescription.setExtrapolatedPredictorIndex(-1);
      cellDescription.setExtrapolatedPredictor(nullptr);
    }
    else {
      assertion(CompressionAccuracy>0.0);
      assertion(cellDescription.getExtrapolatedPredictorIndex()==-1);

      CompressedDataHeap::getInstance().deleteData(cellDescription.getExtrapolatedPredictorCompressedIndex());
      cellDescription.setExtrapolatedPredictorCompressedIndex(-1);
      cellDescription.setExtrapolatedPredictorCompressed(nullptr);
    }
    DataHeap::getInstance().deleteData(cellDescription.getExtrapolatedPredictorAveragesIndex());
    cellDescription.setExtrapolatedPredictorAveragesIndex(-1);
    cellDescription.setExtrapolatedPredictorAverages(nullptr);

    // gradient of extrapolated predictor
    if (isUseViscousFlux() && cellDescription.getExtrapolatedPredictorGradientIndex() >= 0) {
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorGradientIndex()));

      DataHeap::getInstance().deleteData(cellDescription.getExtrapolatedPredictorGradientIndex());
      cellDescription.setExtrapolatedPredictorGradientIndex(-1);
      cellDescription.setExtrapolatedPredictorGradient(nullptr);
    }

    // fluctuations
    if ( cellDescription.getFluctuationIndex()>=0 ) {
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationIndex()));
      assertion(cellDescription.getFluctuationCompressedIndex()==-1);

      DataHeap::getInstance().deleteData(cellDescription.getFluctuationIndex());
      cellDescription.setFluctuationIndex(-1);
      cellDescription.setFluctuation(nullptr);
    }
    else {
      assertion(CompressionAccuracy>0.0);
      assertion(cellDescription.getFluctuationIndex()==-1);

      CompressedDataHeap::getInstance().deleteData(cellDescription.getFluctuationCompressedIndex());
      cellDescription.setFluctuationCompressedIndex(-1);
      cellDescription.setFluctuationCompressed(nullptr);
    }
    DataHeap::getInstance().deleteData(cellDescription.getFluctuationAveragesIndex());
    cellDescription.setFluctuationAveragesIndex(-1);
    cellDescription.setFluctuationAverages(nullptr);

    if ( getDMPObservables()>0 ) {
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getSolutionMinIndex()));
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getSolutionMaxIndex()));
      DataHeap::getInstance().deleteData(cellDescription.getSolutionMinIndex());
      DataHeap::getInstance().deleteData(cellDescription.getSolutionMaxIndex());

      cellDescription.setSolutionMinIndex(-1);
      cellDescription.setSolutionMin(nullptr);
      cellDescription.setSolutionMaxIndex(-1);
      cellDescription.setSolutionMax(nullptr);
    }

    lock.free();
  }
}

void exahype::solvers::ADERDGSolver::checkDataHeapIndex(const CellDescription& cellDescription, const int arrayIndex,const std::string arrayName) {
  assertion1(DataHeap::getInstance().isValidIndex(arrayIndex),cellDescription.toString());
  if ( arrayIndex < 0 ) {
    logError("checkDataHeapIndex(...)","The data heap array 'cellDescription."<<arrayName<<"' could not be allocated! Likely reason: Not enough memory available." <<
             " CellDescription="<<cellDescription.toString());
    std::abort();
  }
}

void exahype::solvers::ADERDGSolver::ensureNecessaryMemoryIsAllocated(
    CellDescription& cellDescription) const {
  // allocate solution
  if (
      cellDescription.getType()==CellDescription::Type::Cell &&
      !DataHeap::getInstance().isValidIndex(cellDescription.getSolutionIndex())
  ) {
    assertion(!DataHeap::getInstance().isValidIndex(cellDescription.getPreviousSolutionIndex()));

    tarch::multicore::Lock lock(exahype::HeapSemaphore);
    // Allocate volume DoF
    const int dataPerCell = getDataPerCell(); // Only the solution and previousSolution store material parameters
    cellDescription.setPreviousSolutionIndex( DataHeap::getInstance().createData( dataPerCell, dataPerCell ) );
    cellDescription.setSolutionIndex        ( DataHeap::getInstance().createData( dataPerCell, dataPerCell ) );
    checkDataHeapIndex(cellDescription,cellDescription.getPreviousSolutionIndex(),"getPreviousSolutionIndex()");
    checkDataHeapIndex(cellDescription,cellDescription.getSolutionIndex(),"getSolutionIndex()");
    cellDescription.setPreviousSolution( getDataHeapEntries(cellDescription.getPreviousSolutionIndex()).data() ) ;
    cellDescription.setSolution        ( getDataHeapEntries(cellDescription.getSolutionIndex()).data() );
    std::fill_n(static_cast<double*>(cellDescription.getPreviousSolution()),getDataPerCell(),std::numeric_limits<double>::quiet_NaN());
    std::fill_n(static_cast<double*>(cellDescription.getSolution()),getDataPerCell(),std::numeric_limits<double>::quiet_NaN());
    
    cellDescription.setSolutionCompressedIndex(-1);
    cellDescription.setSolutionCompressed(nullptr);
    cellDescription.setPreviousSolutionCompressedIndex(-1);
    cellDescription.setPreviousSolutionCompressed(nullptr);

    const int dataPerNode = getNumberOfVariables()+getNumberOfParameters();
    cellDescription.setPreviousSolutionAveragesIndex( DataHeap::getInstance().createData( dataPerNode, dataPerNode ) );
    cellDescription.setSolutionAveragesIndex(         DataHeap::getInstance().createData( dataPerNode, dataPerNode ) );
    checkDataHeapIndex(cellDescription,cellDescription.getPreviousSolutionAveragesIndex(),"getPreviousSolutionAveragesIndex()");
    checkDataHeapIndex(cellDescription,cellDescription.getSolutionAveragesIndex(),"getSolutionAveragesIndex()");
    cellDescription.setPreviousSolutionAverages( getDataHeapEntries(cellDescription.getPreviousSolutionAveragesIndex()).data() ) ;
    cellDescription.setSolutionAverages        ( getDataHeapEntries(cellDescription.getSolutionAveragesIndex()).data() ) ;
    std::fill_n(static_cast<double*>(cellDescription.getPreviousSolutionAverages()),dataPerNode,std::numeric_limits<double>::quiet_NaN());
    std::fill_n(static_cast<double*>(cellDescription.getSolutionAverages()),dataPerNode,std::numeric_limits<double>::quiet_NaN());

    cellDescription.setCompressionState(CellDescription::Uncompressed);

    lock.free();
  }

  // allocate update and boundary arrays
  if (
      holdsFaceData(cellDescription) &&
      !DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex())
  ) {
    assertion(!DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationIndex()));

    tarch::multicore::Lock lock(exahype::HeapSemaphore);

    // allocate update dof
    cellDescription.setUpdateIndex        ( DataHeap::getInstance().createData( getUpdateSize(), getUpdateSize() ) );
    cellDescription.setUpdateAveragesIndex( DataHeap::getInstance().createData( getNumberOfVariables(), getNumberOfVariables() ) );
    cellDescription.setUpdateCompressedIndex(-1);
    cellDescription.setUpdateCompressed(nullptr);
    checkDataHeapIndex(cellDescription,cellDescription.getUpdateIndex(),"getUpdate()");
    checkDataHeapIndex(cellDescription,cellDescription.getUpdateAveragesIndex(),"getUpdateAverages()");
    cellDescription.setUpdate        ( getDataHeapEntries(cellDescription.getUpdateIndex()).data() ) ;
    cellDescription.setUpdateAverages( getDataHeapEntries(cellDescription.getUpdateAveragesIndex()).data() ) ;
    // touch the memory
    std::fill_n(static_cast<double*>(cellDescription.getUpdate()),getUpdateSize(),std::numeric_limits<double>::quiet_NaN());
    std::fill_n(static_cast<double*>(cellDescription.getUpdateAverages()),getNumberOfVariables(),std::numeric_limits<double>::quiet_NaN());

    // extrapolated predictor
    const int dataPerBnd = getBndTotalSize();
    cellDescription.setExtrapolatedPredictorIndex( DataHeap::getInstance().createData(dataPerBnd, dataPerBnd) );
    cellDescription.setExtrapolatedPredictorCompressedIndex(-1);
    cellDescription.setExtrapolatedPredictorCompressed(nullptr);
    const int boundaryData = (getNumberOfParameters()+getNumberOfVariables()) * DIMENSIONS_TIMES_TWO; //TODO JMG / Dominic adapt for padding with optimized kernels //TODO Tobias: Does it make sense to pad these arrays.
    cellDescription.setExtrapolatedPredictorAveragesIndex( DataHeap::getInstance().createData( boundaryData,  boundaryData  ) );
    checkDataHeapIndex(cellDescription,cellDescription.getExtrapolatedPredictorIndex(),"getExtrapolatedPredictor()");
    checkDataHeapIndex(cellDescription,cellDescription.getExtrapolatedPredictorAveragesIndex(),"getExtrapolatedPredictorAverages()");
    cellDescription.setExtrapolatedPredictor        ( getDataHeapEntries(cellDescription.getExtrapolatedPredictorIndex()).data() ) ;
    cellDescription.setExtrapolatedPredictorAverages( getDataHeapEntries(cellDescription.getExtrapolatedPredictorAveragesIndex()).data() ) ;
    // touch the memory
    std::fill_n(static_cast<double*>(cellDescription.getExtrapolatedPredictor()),dataPerBnd,std::numeric_limits<double>::quiet_NaN());
    std::fill_n(static_cast<double*>(cellDescription.getExtrapolatedPredictorAverages()),boundaryData,std::numeric_limits<double>::quiet_NaN());

    if (isUseViscousFlux()) {
      // gradients of extrapolated predictor
      const int gradientSizePerBnd = _numberOfVariables * power(_nodesPerCoordinateAxis, DIMENSIONS - 1) * DIMENSIONS_TIMES_TWO * DIMENSIONS;
      cellDescription.setExtrapolatedPredictorGradientIndex( DataHeap::getInstance().createData(gradientSizePerBnd, gradientSizePerBnd) );
      cellDescription.setExtrapolatedPredictorGradient( getDataHeapEntries(cellDescription.getExtrapolatedPredictorGradientIndex()).data() ) ;
      // touch the memory
      std::fill_n(static_cast<double*>(cellDescription.getExtrapolatedPredictorGradient()),gradientSizePerBnd,std::numeric_limits<double>::quiet_NaN());
    } else {
      cellDescription.setExtrapolatedPredictorGradientIndex(-1);
    }

    //
    // fluctuations
    //
    const int dofPerBnd  = getBndFluxTotalSize();
    cellDescription.setFluctuationIndex( DataHeap::getInstance().createData(dofPerBnd,  dofPerBnd) );
    cellDescription.setFluctuationCompressedIndex(-1);
    cellDescription.setFluctuationCompressed(nullptr);
    const int boundaryUnknowns = getNumberOfVariables() * DIMENSIONS_TIMES_TWO;     //TODO JMG / Dominic adapt for padding with optimized kernels //TODO Tobias: Does it make sense to pad these arrays.
    cellDescription.setFluctuationAveragesIndex( DataHeap::getInstance().createData( boundaryUnknowns, boundaryUnknowns ) );
    checkDataHeapIndex(cellDescription,cellDescription.getFluctuationIndex(),"getFluctuation()");
    checkDataHeapIndex(cellDescription,cellDescription.getFluctuationAveragesIndex(),"getFluctuationAverages()");
    cellDescription.setFluctuation        ( getDataHeapEntries(cellDescription.getFluctuationIndex()).data() ) ;
    cellDescription.setFluctuationAverages( getDataHeapEntries(cellDescription.getFluctuationAveragesIndex()).data() ) ;
    // touch the memory
    std::fill_n(static_cast<double*>(cellDescription.getFluctuation()),dofPerBnd,std::numeric_limits<double>::quiet_NaN());
    std::fill_n(static_cast<double*>(cellDescription.getFluctuationAverages()),boundaryUnknowns,std::numeric_limits<double>::quiet_NaN());

    // Allocate volume DoF for limiter (we need for every of the 2*DIMENSIONS faces an array of min values
    // and array of max values of the neighbour at this face).
    const int numberOfObservables = getDMPObservables();
    if ( numberOfObservables>0 ) {
      cellDescription.setSolutionMinIndex(DataHeap::getInstance().createData(
          numberOfObservables * DIMENSIONS_TIMES_TWO, numberOfObservables * DIMENSIONS_TIMES_TWO ));
      cellDescription.setSolutionMaxIndex(DataHeap::getInstance().createData(
          numberOfObservables * DIMENSIONS_TIMES_TWO, numberOfObservables * DIMENSIONS_TIMES_TWO ));
      checkDataHeapIndex(cellDescription,cellDescription.getSolutionMinIndex(),"getSolutionMinIndex()");
      checkDataHeapIndex(cellDescription,cellDescription.getSolutionMaxIndex(),"getSolutionMaxIndex()");
      cellDescription.setSolutionMin( getDataHeapEntries(cellDescription.getSolutionMinIndex()).data() ) ;
      cellDescription.setSolutionMax( getDataHeapEntries(cellDescription.getSolutionMaxIndex()).data() ) ;
      // touch the memory
      std::fill_n(static_cast<double*>(cellDescription.getSolutionMin()),numberOfObservables * DIMENSIONS_TIMES_TWO,std::numeric_limits<double>::quiet_NaN());
      std::fill_n(static_cast<double*>(cellDescription.getSolutionMax()),numberOfObservables * DIMENSIONS_TIMES_TWO,std::numeric_limits<double>::quiet_NaN());
    }

    lock.free();
  }
}

void exahype::solvers::ADERDGSolver::eraseCellDescriptions(
    const int cellDescriptionsIndex) {
  assertion(Heap::getInstance().isValidIndex(cellDescriptionsIndex));
  for (auto& p : Heap::getInstance().getData(cellDescriptionsIndex)) {
    auto *solver = exahype::solvers::RegisteredSolvers[p.getSolverNumber()];

    ADERDGSolver* aderdgSolver = nullptr;
    if (solver->getType()==Solver::Type::ADERDG) {
      aderdgSolver = static_cast<ADERDGSolver*>(solver);
    }
    else if (solver->getType()==Solver::Type::LimitingADERDG) {
      aderdgSolver =
          static_cast<LimitingADERDGSolver*>(solver)->getSolver().get();
    }
    assertion(aderdgSolver!=nullptr);

    p.setType(CellDescription::Type::Erased);
    aderdgSolver->ensureNoUnnecessaryMemoryIsAllocated(p);
  }

  Heap::getInstance().getData(cellDescriptionsIndex).clear();
}

exahype::solvers::ADERDGSolver::ADERDGSolver(
    const std::string& identifier,
    const int numberOfVariables,
    const int numberOfParameters,
    const int numberOfGlobalObservables,
    const int basisSize,
    const double maximumMeshSize,
    const int maximumAdaptiveMeshDepth,
    const int haloCells,
    const int regularisedFineGridLevels,
    const exahype::solvers::Solver::TimeStepping timeStepping,
    const int DMPObservables,
    std::unique_ptr<profilers::Profiler> profiler)
    : Solver(identifier, Solver::Type::ADERDG, numberOfVariables,
             numberOfParameters, numberOfGlobalObservables, basisSize,
             maximumMeshSize, maximumAdaptiveMeshDepth,
             timeStepping, std::move(profiler)),
     _previousMinTimeStamp( std::numeric_limits<double>::infinity() ),
     _previousMinTimeStepSize( std::numeric_limits<double>::infinity() ),
     _minTimeStamp( std::numeric_limits<double>::infinity() ),
     _minTimeStepSize( std::numeric_limits<double>::infinity() ),
     _estimatedTimeStepSize( std::numeric_limits<double>::infinity() ),
     _stabilityConditionWasViolated( false ),
     _refineOrKeepOnFineGrid(1+haloCells),
     _DMPObservables(DMPObservables),
     _minRefinementStatusForTroubledCell(_refineOrKeepOnFineGrid+3),
     _checkForNaNs(true),
      _meshUpdateEvent(MeshUpdateEvent::None) {
  // register tags with profiler
  for (const char* tag : tags) {
    _profiler->registerTag(tag);
  }

  #ifdef Parallel
  _invalidExtrapolatedPredictor.resize(getBndFaceSize());
  _invalidFluctuations.resize(getBndFluxSize());
  std::fill_n(_invalidExtrapolatedPredictor.data(),_invalidExtrapolatedPredictor.size(),-1);
  std::fill_n(_invalidFluctuations.data(),_invalidFluctuations.size(),-1);

  _receivedExtrapolatedPredictor.resize(getBndFaceSize());
  _receivedFluctuations.resize(getBndFluxSize());

  _receivedUpdate.reserve(getUpdateSize());
  #endif
}

int exahype::solvers::ADERDGSolver::getUnknownsPerFace() const {
  return _numberOfVariables * power(_nodesPerCoordinateAxis, DIMENSIONS - 1);
}

int exahype::solvers::ADERDGSolver::getUnknownsPerCellBoundary() const {
  return DIMENSIONS_TIMES_TWO * getUnknownsPerFace();
}

int exahype::solvers::ADERDGSolver::getUnknownsPerCell() const {
  return _numberOfVariables * power(_nodesPerCoordinateAxis, DIMENSIONS + 0);
}

int exahype::solvers::ADERDGSolver::getFluxUnknownsPerCell() const {
  return (DIMENSIONS + 1) * getUnknownsPerCell(); // +1 for sources
}

int exahype::solvers::ADERDGSolver::getSpaceTimeUnknownsPerCell() const {
  return _numberOfVariables * power(_nodesPerCoordinateAxis, DIMENSIONS + 1);
}

int exahype::solvers::ADERDGSolver::getSpaceTimeFluxUnknownsPerCell() const {
  return (DIMENSIONS + 1) * getSpaceTimeUnknownsPerCell();  // +1 for sources
}

int exahype::solvers::ADERDGSolver::getDataPerFace() const {
  return (_numberOfVariables+_numberOfParameters) * power(_nodesPerCoordinateAxis, DIMENSIONS - 1);
}

int exahype::solvers::ADERDGSolver::getDataPerCellBoundary() const {
  return (_numberOfVariables+_numberOfParameters) * power(_nodesPerCoordinateAxis, DIMENSIONS - 1) * DIMENSIONS_TIMES_TWO;
}

int exahype::solvers::ADERDGSolver::getDataPerCell() const {
  return (_numberOfVariables+_numberOfParameters) * power(_nodesPerCoordinateAxis, DIMENSIONS + 0);
}

int exahype::solvers::ADERDGSolver::getSpaceTimeDataPerCell() const {
  return (_numberOfVariables+_numberOfParameters) * power(_nodesPerCoordinateAxis, DIMENSIONS + 1);
}

int exahype::solvers::ADERDGSolver::getDMPObservables() const {
  return _DMPObservables;
}

int exahype::solvers::ADERDGSolver::getMinRefinementStatusForTroubledCell() const {
  return _minRefinementStatusForTroubledCell;
}

void exahype::solvers::ADERDGSolver::resetMeshUpdateEvent() {
  _meshUpdateEvent = MeshUpdateEvent::None;
}

void exahype::solvers::ADERDGSolver::updateMeshUpdateEvent(MeshUpdateEvent meshUpdateEvent) {
  tarch::multicore::Lock lock(ReductionSemaphore);
  _meshUpdateEvent = mergeMeshUpdateEvents(_meshUpdateEvent,meshUpdateEvent);
  lock.free();
}

exahype::solvers::ADERDGSolver::MeshUpdateEvent
exahype::solvers::ADERDGSolver::getMeshUpdateEvent() const {
  return _meshUpdateEvent;
}

std::tuple<double,double> exahype::solvers::ADERDGSolver::getRiemannSolverTimeStepData(
    const CellDescription& cellDescription1,
    const CellDescription& cellDescription2) const {
  auto result  = std::make_tuple<double,double>(0.0,0.0);
  switch (_timeStepping) {
    case TimeStepping::Global:
    case TimeStepping::GlobalFixed:
      std::get<0>(result) = _minTimeStamp;
      std::get<1>(result) = _minTimeStepSize;
      break;
    default:
//      std::get<0>(result) = std::max(cellDescription1.getTimeStamp(),cellDescription2.getTimeStamp());
//      std::get<1>(result) = std::min(cellDescription1.getTimeStepSize(),cellDescription2.getTimeStepSize());
      logError("getRiemannSolverTimeStepData(...)","Unknown time stepping scheme.")
      std::abort();
  }
  return result;
}

std::tuple<double,double> exahype::solvers::ADERDGSolver::getPredictionTimeStepData(
    const CellDescription& cellDescription,const bool duringFusedTimeStep) const {
  auto result  = std::make_tuple<double,double>(0.0,0.0);
  switch (_timeStepping) {
    case TimeStepping::Global:
      if ( duringFusedTimeStep ) {
        std::get<0>(result) = _minTimeStamp+_minTimeStepSize;
        std::get<1>(result) = _estimatedTimeStepSize;
      } else {
        std::get<0>(result) = _minTimeStamp;
        std::get<1>(result) = _minTimeStepSize;
      }
      break;
    case TimeStepping::GlobalFixed:
      if ( duringFusedTimeStep ) {
        std::get<0>(result) = _minTimeStamp+_minTimeStepSize;
        std::get<1>(result) = _minTimeStepSize;
      } else {
        std::get<0>(result) = _minTimeStamp;
        std::get<1>(result) = _minTimeStepSize;
      }
      break;
      break;
    default:
      logError("getRiemannSolverTimeStepData(...)","Unknown time stepping scheme.")
      std::abort();
  }
  return result;
}

void exahype::solvers::ADERDGSolver::synchroniseTimeStepping(
    CellDescription& p) const {
  switch (_timeStepping) {
    case TimeStepping::Global:
    case TimeStepping::GlobalFixed:
      p.setPreviousTimeStamp(_previousMinTimeStamp);
      p.setPreviousTimeStepSize(_previousMinTimeStepSize);
  
      p.setTimeStamp(_minTimeStamp);
      p.setTimeStepSize(_minTimeStepSize);
  }
}

void exahype::solvers::ADERDGSolver::kickOffTimeStep(const bool isFirstTimeStepOfBatchOrNoBatch) {
  if ( isFirstTimeStepOfBatchOrNoBatch ) {
    _meshUpdateEvent               = MeshUpdateEvent::None;
    _admissibleTimeStepSize        = std::numeric_limits<double>::infinity();
    _stabilityConditionWasViolated = false;
  }

  // call user code
  _globalObservables = resetGlobalObservables();
  beginTimeStep(_minTimeStamp,isFirstTimeStepOfBatchOrNoBatch);
}

void exahype::solvers::ADERDGSolver::wrapUpTimeStep(const bool isFirstTimeStepOfBatchOrNoBatch,const bool isLastTimeStepOfBatchOrNoBatch) {
  if ( isFirstTimeStepOfBatchOrNoBatch ) {
    _previousMinTimeStepSize  = _minTimeStepSize;
    _previousMinTimeStamp     = _minTimeStamp;
  }
  _minTimeStamp += _minTimeStepSize;

  _stabilityConditionWasViolated = false;
  if (
      tarch::parallel::Node::getInstance().isGlobalMaster() &&
      getTimeStepping() != TimeStepping::GlobalFixed // fix the time step size in intermediate batch iterations
  ) {
    if ( FuseAllADERDGPhases && !isLinear() ) {
      if ( isLastTimeStepOfBatchOrNoBatch ) {
        if ( _estimatedTimeStepSize > _admissibleTimeStepSize ) { // rerun
          _minTimeStepSize       = FusedTimeSteppingRerunFactor * _admissibleTimeStepSize;
          _estimatedTimeStepSize = _minTimeStepSize;
          _stabilityConditionWasViolated = true;
        } else {
          _minTimeStepSize       = _estimatedTimeStepSize; // as we have computed the predictor with an estimate, we have to use the estimated time step size to perform the face integral
          _estimatedTimeStepSize = 0.5 * ( FusedTimeSteppingDiffusionFactor * _admissibleTimeStepSize + _estimatedTimeStepSize );
        }
      } else { // use fixed time step size in intermediate batch iterations
        _minTimeStepSize  = _estimatedTimeStepSize;
      }
    } else if ( !isLinear() ) { // non-fused, non-linear
      _minTimeStepSize = _admissibleTimeStepSize;
    } // else if linear do not change the time step size at all
  }

  // call user code
  endTimeStep(_minTimeStamp,isLastTimeStepOfBatchOrNoBatch);
}

void exahype::solvers::ADERDGSolver::updateTimeStepSize() {
  if ( FuseAllADERDGPhases ) {
    _minTimeStepSize        = FusedTimeSteppingRerunFactor * _admissibleTimeStepSize;
    _estimatedTimeStepSize  = _minTimeStepSize;
  } else {
    _minTimeStepSize        = _admissibleTimeStepSize;
  }
  _admissibleTimeStepSize = std::numeric_limits<double>::infinity();
  _stabilityConditionWasViolated = false;
}

void exahype::solvers::ADERDGSolver::rollbackToPreviousTimeStep() {
  _minTimeStamp    = _previousMinTimeStamp;
  _minTimeStepSize = _previousMinTimeStepSize;

  _previousMinTimeStamp    = std::numeric_limits<double>::infinity();
  _previousMinTimeStepSize = std::numeric_limits<double>::infinity();

  // TODO(Lukas) Maybe also rollback global observables?
}

double exahype::solvers::ADERDGSolver::getMinTimeStamp() const {
  return _minTimeStamp;
}

double exahype::solvers::ADERDGSolver::getMinTimeStepSize() const {
  return _minTimeStepSize;
}

double exahype::solvers::ADERDGSolver::getEstimatedTimeStepSize() const {
  return _estimatedTimeStepSize;
}

double exahype::solvers::ADERDGSolver::getPreviousMinTimeStepSize() const {
  return _previousMinTimeStepSize;
}

double exahype::solvers::ADERDGSolver::getPreviousMinTimeStamp() const {
  return _previousMinTimeStamp;
}

double exahype::solvers::ADERDGSolver::getAdmissibleTimeStepSize() const {
  return _admissibleTimeStepSize;
}

void exahype::solvers::ADERDGSolver::updateAdmissibleTimeStepSize( double value ) {
  tarch::multicore::Lock lock(ReductionSemaphore);
  _admissibleTimeStepSize = std::min(_admissibleTimeStepSize,value);
  lock.free();
}

void exahype::solvers::ADERDGSolver::resetAdmissibleTimeStepSize() {
  _admissibleTimeStepSize = std::numeric_limits<double>::infinity();
}

void exahype::solvers::ADERDGSolver::initSolver(
    const double                                timeStamp,
    const tarch::la::Vector<DIMENSIONS,double>& domainOffset,
    const tarch::la::Vector<DIMENSIONS,double>& domainSize,
    const double                                boundingBoxSize,
    const double                                boundingBoxMeshSize,
    const std::vector<std::string>&             cmdlineargs,
    const exahype::parser::ParserView&          parserView
) {
  _domainOffset=domainOffset;
  _domainSize=domainSize;
  std::pair<double,int> coarsestMeshInfo =
      exahype::solvers::Solver::computeCoarsestMeshSizeAndLevel(
          std::min(boundingBoxMeshSize,_maximumMeshSize),boundingBoxSize);
  _coarsestMeshSize  = coarsestMeshInfo.first;
  _coarsestMeshLevel = coarsestMeshInfo.second;

  _previousMinTimeStamp = timeStamp;
  _minTimeStamp         = timeStamp;

  _previousMinTimeStepSize = 0.0;
  _minTimeStepSize         = 0.0;

  _estimatedTimeStepSize   = std::numeric_limits<double>::infinity();
  _admissibleTimeStepSize  = std::numeric_limits<double>::infinity();

  _meshUpdateEvent = MeshUpdateEvent::InitialRefinementRequested;

  _globalObservables = resetGlobalObservables();

  init(cmdlineargs,parserView); // call user define initalisiation
}

bool exahype::solvers::ADERDGSolver::isPerformingPrediction(
    const exahype::State::AlgorithmSection& section) const {
  bool isPerformingPrediction = false;

  switch (section) {
    case exahype::State::AlgorithmSection::TimeStepping:
      isPerformingPrediction = true;
      break;
    case exahype::State::AlgorithmSection::PredictionRerunAllSend:
      isPerformingPrediction = !hasRequestedAnyMeshRefinement() &&
                               getStabilityConditionWasViolated();
      break;
    case exahype::State::AlgorithmSection::PredictionOrLocalRecomputationAllSend:
      isPerformingPrediction = hasRequestedAnyMeshRefinement();
      break;
    default:
      break;
  }

  return isPerformingPrediction;
}

bool exahype::solvers::ADERDGSolver::isMergingMetadata(
    const exahype::State::AlgorithmSection& section) const {
  return ( exahype::State::AlgorithmSection::MeshRefinement==section ) &&
         hasRequestedAnyMeshRefinement();
}

bool exahype::solvers::ADERDGSolver::getStabilityConditionWasViolated() const {
  return  _stabilityConditionWasViolated;
}

void exahype::solvers::ADERDGSolver::setStabilityConditionWasViolated(const bool state) {
  _stabilityConditionWasViolated = state;
}

bool exahype::solvers::ADERDGSolver::isValidCellDescriptionIndex(
    const int cellDescriptionsIndex) {
  bool result = cellDescriptionsIndex>=0;
  assertion1(!result || Heap::getInstance().isValidIndex(cellDescriptionsIndex),cellDescriptionsIndex);
  return result;
}

int exahype::solvers::ADERDGSolver::tryGetElement(
    const int cellDescriptionsIndex,
    const int solverNumber) const {
  if ( isValidCellDescriptionIndex(cellDescriptionsIndex) ) {
    int element=0;
    for (auto& p : Heap::getInstance().getData(cellDescriptionsIndex)) {
      if (p.getSolverNumber()==solverNumber) {
        return element;
      }
      ++element;
    }
  }
  return NotFound;
}

///////////////////////////////////
// CELL-LOCAL MESH REFINEMENT
///////////////////////////////////
bool exahype::solvers::ADERDGSolver::progressMeshRefinementInEnterCell(
    exahype::Cell& fineGridCell,
    exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    const int  solverNumber,
    const bool stillInRefiningMode) {
  bool newComputeCell = false;

  // Fine grid cell based uniform mesh refinement.
  const int fineGridElement   = tryGetElement(fineGridCell.getCellDescriptionsIndex(),solverNumber);
  const int coarseGridElement = tryGetElement(coarseGridCell.getCellDescriptionsIndex(),solverNumber);
  if (
      fineGridElement==exahype::solvers::Solver::NotFound &&
      fineGridVerticesEnumerator.getLevel()==_coarsestMeshLevel
  ) {
    logDebug("progressMeshRefinementInEnterCell(...)","Add new uniform grid cell at centre="<<fineGridVerticesEnumerator.getCellCenter() <<", level="<<fineGridVerticesEnumerator.getLevel()
        << " for solver=" << solverNumber);

    addNewCell(
        fineGridCell,fineGridVerticesEnumerator,
        multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex,
        solverNumber);
    newComputeCell = true;
  }
  else if ( fineGridElement!=exahype::solvers::Solver::NotFound ) {
    CellDescription& fineGridCellDescription = getCellDescription(fineGridCell.getCellDescriptionsIndex(),fineGridElement);

    // wait for background jobs to complete
    waitUntilCompletedLastStep(fineGridCellDescription,false,false);

    #ifdef Asserts
    const tarch::la::Vector<DIMENSIONS,double> center = fineGridCellDescription.getOffset()+0.5*fineGridCellDescription.getSize();
    #endif
    assertion5(Vertex::equalUpToRelativeTolerance(fineGridVerticesEnumerator.getCellCenter(),center),
               fineGridVerticesEnumerator.getCellCenter(),center,fineGridVerticesEnumerator.getLevel(),fineGridCellDescription.getLevel(),tarch::parallel::Node::getInstance().getRank());
    assertionEquals3(fineGridVerticesEnumerator.getLevel(),fineGridCellDescription.getLevel(),fineGridVerticesEnumerator.getCellCenter(),fineGridCellDescription.getOffset()+0.5*fineGridCellDescription.getSize(),tarch::parallel::Node::getInstance().getRank());

    // Update the status flagging
    updateCommunicationStatus(fineGridCellDescription);
    ensureNecessaryMemoryIsAllocated(fineGridCellDescription);
    ensureNoUnnecessaryMemoryIsAllocated(fineGridCellDescription);

    updateAugmentationStatus(fineGridCellDescription);

    updateRefinementStatus(fineGridCellDescription,fineGridCellDescription.getNeighbourMergePerformed());
    if ( coarseGridElement != exahype::solvers::Solver::NotFound ) {
      CellDescription& coarseGridCellDescription = getCellDescription(
          coarseGridCell.getCellDescriptionsIndex(),coarseGridElement);
      updateCoarseGridAncestorRefinementStatus(fineGridCellDescription,coarseGridCellDescription);
    }

    progressCollectiveRefinementOperationsInEnterCell(fineGridCellDescription);

    decideOnRefinement(fineGridCellDescription,stillInRefiningMode);
    decideOnVirtualRefinement(fineGridCellDescription);

    ensureFineGridCoarseGridConsistency(fineGridCellDescription,coarseGridCell.getCellDescriptionsIndex()); // must come after refinement status update
  }

  // Coarse grid cell based adaptive mesh refinement operations.
  // Add new cells to the grid and veto erasing or erasing virtual children
  // requests if there are cells on the fine level.
  if (coarseGridElement!=exahype::solvers::Solver::NotFound) {
    CellDescription& coarseGridCellDescription = getCellDescription(
        coarseGridCell.getCellDescriptionsIndex(),coarseGridElement);

    alterErasingRequestsIfNecessary(
        coarseGridCellDescription,
        fineGridCell.getCellDescriptionsIndex());

    addNewDescendantIfVirtualRefiningRequested(
            fineGridCell,fineGridVertices,fineGridVerticesEnumerator,
            coarseGridCellDescription,coarseGridCell.getCellDescriptionsIndex());
    newComputeCell |=
        addNewCellIfRefinementRequested(
            fineGridCell,fineGridVertices,fineGridVerticesEnumerator,
            coarseGridCellDescription,coarseGridCell.getCellDescriptionsIndex());
  }

  return newComputeCell;
}

int exahype::solvers::ADERDGSolver::evaluateRefinementCriterion(
    const CellDescription& cellDescription, const double* const solution, const double& timeStamp) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell,cellDescription.toString());
  assertion1(
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingChildrenRequested ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested,
      cellDescription.toString());

  Solver::RefinementControl refinementControl =
      refinementCriterion(
          solution,cellDescription.getOffset()+0.5*cellDescription.getSize(),
          cellDescription.getSize(),
          timeStamp, // is done after the update
          cellDescription.getLevel());

  switch ( refinementControl ) {
  case exahype::solvers::Solver::RefinementControl::Refine:
      return _refineOrKeepOnFineGrid;
  case exahype::solvers::Solver::RefinementControl::Keep:
    return ( cellDescription.getLevel()==getMaximumAdaptiveMeshLevel() ) ? _refineOrKeepOnFineGrid : Keep;
  case exahype::solvers::Solver::RefinementControl::Erase:
    return Erase;
  default:
    logError("adjustSolutionDuringMeshRefinementBody(...)",
        "unknown refinement control value=" << static_cast<int>(refinementControl) <<
        ". Please check the return values of your refinement criterion.");
    std::abort();
    return Pending-1;
  }
}

void exahype::solvers::ADERDGSolver::markForRefinement(CellDescription& cellDescription) {
  const double* const solution = static_cast<double*>(cellDescription.getSolution());
  const int refinementStatus = evaluateRefinementCriterion(
      cellDescription,solution,cellDescription.getTimeStamp());
  if ( refinementStatus==_refineOrKeepOnFineGrid ) {
    cellDescription.setRefinementFlag(true);
  }
  cellDescription.setRefinementStatus( std::max(cellDescription.getRefinementStatus(),refinementStatus) );
}

void exahype::solvers::ADERDGSolver::decideOnRefinement(
    CellDescription& fineGridCellDescription,const bool stillInRefiningMode) {
  // top-down refining
  if (
     stillInRefiningMode &&
     fineGridCellDescription.getType()==CellDescription::Type::Cell &&
     (fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None ||
     fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::VirtualRefiningRequested)
     && fineGridCellDescription.getLevel()<getMaximumAdaptiveMeshLevel()
     && fineGridCellDescription.getRefinementStatus() > 0
  ) {
    fineGridCellDescription.setRefinementEvent(CellDescription::RefiningRequested);
  }
  // bottom-up refining (halo refinement)
  else if (
      stillInRefiningMode &&
      fineGridCellDescription.getType()==CellDescription::Type::Descendant &&
      fineGridCellDescription.getLevel()==getMaximumAdaptiveMeshLevel() &&
      (fineGridCellDescription.getRefinementStatus()>0 ||
      fineGridCellDescription.getPreviousRefinementStatus() > 0)
  ) {
    Solver::SubcellPosition subcellPosition = amr::computeSubcellPositionOfDescendant<CellDescription,Heap>(fineGridCellDescription);
    CellDescription& topMostParent =
      getCellDescription(subcellPosition.parentCellDescriptionsIndex,subcellPosition.parentElement);
    tarch::multicore::Lock lock(ADERDGSolver::CoarseGridSemaphore);
    if ( topMostParent.getType()==CellDescription::Type::Cell ) {
      topMostParent.setRefinementStatus(_refineOrKeepOnFineGrid);
    }
    lock.free();
  }
}

void exahype::solvers::ADERDGSolver::decideOnVirtualRefinement(
    CellDescription& fineGridCellDescription) {
  // TODO(Dominic): We will balance in the decideOnRefinement (vetoErasingRequests)
  // routines based on the augmentation status flag

  // 1. Check if we can request augmenting or erasing virtual children request.
  bool idleCellOrDescendant =
      (fineGridCellDescription.getType()==CellDescription::Type::Cell ||
      fineGridCellDescription.getType()==CellDescription::Type::Descendant) &&
      fineGridCellDescription.getRefinementEvent()==CellDescription::None;

  if (
      idleCellOrDescendant &&
      fineGridCellDescription.getHasVirtualChildren() &&
      fineGridCellDescription.getAugmentationStatus()<MinimumAugmentationStatusForVirtualRefining
  ) {
    fineGridCellDescription.setRefinementEvent( CellDescription::RefinementEvent::ErasingVirtualChildrenRequested );
  }
  else if (
      idleCellOrDescendant &&
      !fineGridCellDescription.getHasVirtualChildren() &&
      fineGridCellDescription.getAugmentationStatus()>=MinimumAugmentationStatusForVirtualRefining
  ) {
    fineGridCellDescription.setRefinementEvent(CellDescription::VirtualRefiningRequested);
  }

  // 2. Check if we must veto the erasing virtual children request of the parent.
  if (
      fineGridCellDescription.getHasVirtualChildren() ||
      fineGridCellDescription.getAugmentationStatus()>0 // TODO(Dominic): Still necessary?
  ) {
    const int coarseGridCellElement = tryGetElement(fineGridCellDescription.getParentIndex(),
                                              fineGridCellDescription.getSolverNumber());
    if (coarseGridCellElement!=exahype::solvers::Solver::NotFound) {
      auto& coarseGridCellDescription = getCellDescription(fineGridCellDescription.getParentIndex(),
                                                           coarseGridCellElement);
      tarch::multicore::Lock lock(CoarseGridSemaphore);
      if ( coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingVirtualChildrenRequested ) {
        coarseGridCellDescription.setRefinementEvent(CellDescription::None);

        assertion1(fineGridCellDescription.getType()==CellDescription::Type::Descendant,
                   fineGridCellDescription.toString());
        assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Cell ||
                   coarseGridCellDescription.getType()==CellDescription::Type::Descendant,
                   coarseGridCellDescription.toString());
      }
      lock.free();
    }
  }
}

void exahype::solvers::ADERDGSolver::alterErasingRequestsIfNecessary(
    CellDescription& coarseGridCellDescription,
    const int fineGridCellDescriptionsIndex) const {
  const int fineGridElement = tryGetElement(
      fineGridCellDescriptionsIndex,coarseGridCellDescription.getSolverNumber());
  if ( fineGridElement!=exahype::solvers::Solver::NotFound ) {
    CellDescription& fineGridCellDescription =
       getCellDescription(fineGridCellDescriptionsIndex,fineGridElement);
    if (
        fineGridCellDescription.getHasVirtualChildren()
        || fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::VirtualRefining
        || fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::VirtualRefiningRequested
    ) {
      tarch::multicore::Lock lock(CoarseGridSemaphore);
      switch (coarseGridCellDescription.getRefinementEvent()) {
        case CellDescription::RefinementEvent::ErasingVirtualChildrenRequested: {
          assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Cell ||
                     coarseGridCellDescription.getType()==CellDescription::Type::Descendant,
                     coarseGridCellDescription.toString());

          coarseGridCellDescription.setRefinementEvent(CellDescription::None);
        }  break;
        case CellDescription::RefinementEvent::ErasingChildrenRequested: {
          assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Cell,
              coarseGridCellDescription.toString());

          coarseGridCellDescription.setRefinementEvent(
              CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested);
        } break;
        default:
          break;
      }
      lock.free();
    }
  }
}

void exahype::solvers::ADERDGSolver::addNewCell(
    exahype::Cell& fineGridCell,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    const int coarseGridCellDescriptionsIndex,
    const int solverNumber) {
  logDebug("addNewCell(...)","Add new grid cell with center "<<fineGridVerticesEnumerator.getCellCenter() <<
              " at level "<<fineGridVerticesEnumerator.getLevel());

  CellInfo cellInfo = fineGridCell.addNewCellDescription(
      solverNumber,
      CellDescription::Type::Cell,
      CellDescription::None,
      fineGridVerticesEnumerator.getLevel(),
      coarseGridCellDescriptionsIndex,
      fineGridVerticesEnumerator.getCellSize(),
      fineGridVerticesEnumerator.getVertexPosition());

  const int fineGridElement = cellInfo.indexOfADERDGCellDescription(solverNumber);
  CellDescription& fineGridCellDescription = cellInfo._ADERDGCellDescriptions[fineGridElement]; //TODO(Dominic): Multi-solvers: Might need to lock this?
  if ( !exahype::solvers::Solver::SpawnAMRBackgroundJobs ) {
    ensureNecessaryMemoryIsAllocated(fineGridCellDescription);
  }

  fineGridCellDescription.setPreviousRefinementStatus(Erase); // reasonable state after rollback 
  fineGridCellDescription.setRefinementStatus(Pending); 

  #ifdef Asserts
  fineGridCellDescription.setCreation(CellDescription::Creation::UniformRefinement);
  #endif
}

void exahype::solvers::ADERDGSolver::addNewDescendantIfVirtualRefiningRequested(
     exahype::Cell& fineGridCell,
     exahype::Vertex* const fineGridVertices,
     const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
     CellDescription& coarseGridCellDescription,
     const int coarseGridCellDescriptionsIndex) {
  const int fineGridElement = tryGetElement(
      fineGridCell.getCellDescriptionsIndex(),coarseGridCellDescription.getSolverNumber());

  // read and modify coarse grid
  tarch::multicore::Lock lock(CoarseGridSemaphore);
  const bool virtualRefiningRequested =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::VirtualRefiningRequested;
  const bool virtualRefining =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::VirtualRefining;

  if ( virtualRefining || virtualRefiningRequested ) {
    assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Cell ||
               coarseGridCellDescription.getType()==CellDescription::Type::Descendant,
               coarseGridCellDescription.toString());
    coarseGridCellDescription.setRefinementEvent(CellDescription::None);
    if ( fineGridElement==exahype::solvers::Solver::NotFound ) {
      coarseGridCellDescription.setRefinementEvent(CellDescription::VirtualRefining);
    } else if ( virtualRefiningRequested ) {
      coarseGridCellDescription.setRefinementEvent(CellDescription::None);

      #ifdef Asserts
      CellDescription& fineGridCellDescription =
          getCellDescription(fineGridCell.getCellDescriptionsIndex(),fineGridElement);
      #endif
      assertion1(fineGridCellDescription.getType()==CellDescription::Type::Descendant,
                 fineGridCellDescription.toString());
    }
  }
  lock.free();

  // work on fine grid
  if (
      (virtualRefiningRequested || virtualRefining) &&
      fineGridElement==exahype::solvers::Solver::NotFound
  ) {
    fineGridCell.addNewCellDescription( // (EmptyDescendant),None
        coarseGridCellDescription.getSolverNumber(),
        CellDescription::Type::Descendant,
        CellDescription::None, // This should be removed from the signature and defaulted to none
        fineGridVerticesEnumerator.getLevel(),
        coarseGridCellDescriptionsIndex,
        fineGridVerticesEnumerator.getCellSize(),
        fineGridVerticesEnumerator.getVertexPosition());

    #ifdef Asserts
    const int fineGridElement = tryGetElement(
        fineGridCell.getCellDescriptionsIndex(),coarseGridCellDescription.getSolverNumber());
    #endif
    assertion(fineGridElement!=exahype::solvers::Solver::NotFound);
  }
}

// TODO(Dominic): Cannot be called multiple times. Need extra state?
bool exahype::solvers::ADERDGSolver::addNewCellIfRefinementRequested(
    exahype::Cell& fineGridCell,
    exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    CellDescription& coarseGridCellDescription,
    const int coarseGridCellDescriptionsIndex) {
  // read and modify coarse grid
  tarch::multicore::Lock lock(CoarseGridSemaphore);
  bool refiningOrRefiningRequested =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::RefiningRequested ||
      coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Refining;
  if ( refiningOrRefiningRequested ) {
    assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Cell,
               coarseGridCellDescription.toString());
    coarseGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::Refining);
  }
  lock.free();

  // work on fine grid
  if ( refiningOrRefiningRequested ) {
    const int fineGridElement = tryGetElement(
        fineGridCell.getCellDescriptionsIndex(),coarseGridCellDescription.getSolverNumber());

    if ( fineGridElement==exahype::solvers::Solver::NotFound ) {
      addNewCell(fineGridCell,fineGridVerticesEnumerator,
                 coarseGridCellDescriptionsIndex,
                 coarseGridCellDescription.getSolverNumber());
      CellDescription& fineGridCellDescription =
          getCellDescriptions(fineGridCell.getCellDescriptionsIndex()).back();
      fineGridCellDescription.setRefinementEvent(CellDescription::Prolongating);
      #ifdef Asserts
      fineGridCellDescription.setCreation(CellDescription::Creation::AdaptiveRefinement);
      #endif
    } else {
      CellDescription& fineGridCellDescription = getCellDescription(fineGridCell.getCellDescriptionsIndex(),fineGridElement);
      #ifdef Parallel
      assertion4(fineGridCellDescription.getType()==CellDescription::Type::Descendant ||
                 fineGridCellDescription.getType()==CellDescription::Type::Cell,
                 fineGridCellDescription.toString(),coarseGridCellDescription.toString(),
                 coarseGridCellDescriptionsIndex,
                 tarch::parallel::Node::getInstance().getRank());
      #else  
      assertion2(fineGridCellDescription.getType()==CellDescription::Type::Descendant,
                 fineGridCellDescription.toString(),coarseGridCellDescription.toString());
      #endif 
      assertion2(fineGridCellDescription.getParentIndex()==coarseGridCellDescriptionsIndex,
                 fineGridCellDescription.toString(),coarseGridCellDescriptionsIndex);

      fineGridCellDescription.setType(CellDescription::Type::Cell);
      fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::Prolongating);
      fineGridCellDescription.setCommunicationStatus(CellCommunicationStatus);
      fineGridCellDescription.setFacewiseCommunicationStatus(0); // implicit conversion
      ensureNecessaryMemoryIsAllocated(fineGridCellDescription);
      #ifdef Asserts
      fineGridCellDescription.setCreation(CellDescription::Creation::AdaptiveRefinement);
      #endif
      fineGridCellDescription.setPreviousRefinementStatus(Erase); // reasonable state after rollback 
      fineGridCellDescription.setRefinementStatus(Pending);
    }
    return true;
  }
  return false;
}

void exahype::solvers::ADERDGSolver::prolongateVolumeData(
    CellDescription&       fineGridCellDescription,
    const bool initialGrid) {
  const int coarseGridElement =
      tryGetElement(fineGridCellDescription.getParentIndex(),fineGridCellDescription.getSolverNumber());
  assertion1(coarseGridElement!=exahype::solvers::Solver::NotFound,fineGridCellDescription.toString());
  CellDescription& coarseGridCellDescription =
      getCellDescription(fineGridCellDescription.getParentIndex(),coarseGridElement);

  tarch::la::Vector<DIMENSIONS,int> subcellIndex =
      exahype::amr::computeSubcellIndex(
          fineGridCellDescription.getOffset(),
          fineGridCellDescription.getSize(),coarseGridCellDescription.getOffset());

  const int levelFine = fineGridCellDescription.getLevel();
  const int levelCoarse = coarseGridCellDescription.getLevel();
  assertion(levelCoarse < levelFine);

  // current solution
  double* solutionFine   = static_cast<double*>(fineGridCellDescription.getSolution());
  double* solutionCoarse = static_cast<double*>(coarseGridCellDescription.getSolution());
  volumeUnknownsProlongation(
      solutionFine,solutionCoarse,
      levelCoarse,levelFine,
      subcellIndex);

  // previous solution
  assertion(DataHeap::getInstance().isValidIndex(fineGridCellDescription.getPreviousSolutionIndex()));
  double* previousSolutionFine   = static_cast<double*>(fineGridCellDescription.getPreviousSolution());
  double* previousSolutionCoarse = static_cast<double*>(coarseGridCellDescription.getPreviousSolution());
  volumeUnknownsProlongation(
      previousSolutionFine,previousSolutionCoarse,
      levelCoarse,levelFine,
      subcellIndex);

  fineGridCellDescription.setTimeStamp(coarseGridCellDescription.getTimeStamp());
  fineGridCellDescription.setTimeStepSize(coarseGridCellDescription.getTimeStepSize());

  // TODO Dominic: This is a little inconsistent since I orignially tried to hide
  // the limiting from the pure ADER-DG scheme
  fineGridCellDescription.setPreviousRefinementStatus(Erase); // TODO(Dominic): NEW CELLS
  fineGridCellDescription.setRefinementStatus(Pending);

  // TODO Dominic:
  // During the inital mesh build where we only refine
  // according to the PAD, we don't want to have a too broad refined area.
  // We thus do not flag children with troubled
  if (
      !initialGrid &&
      coarseGridCellDescription.getRefinementStatus()>=_minRefinementStatusForTroubledCell
  ) {
    fineGridCellDescription.setRefinementStatus(_minRefinementStatusForTroubledCell);
    fineGridCellDescription.setIterationsToCureTroubledCell(coarseGridCellDescription.getIterationsToCureTroubledCell());
  }
  fineGridCellDescription.setFacewiseRefinementStatus(Pending);
}

bool exahype::solvers::ADERDGSolver::attainedStableState(
        exahype::Cell&                       fineGridCell,
        exahype::Vertex* const               fineGridVertices,
        const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
        const int                            solverNumber,
        const bool                           stillInRefiningMode) const {
  const int element = tryGetElement(fineGridCell.getCellDescriptionsIndex(),solverNumber);
  if ( element!=exahype::solvers::Solver::NotFound ) {
    CellDescription& cellDescription = getCellDescription(fineGridCell.getCellDescriptionsIndex(),element);
    // compute flagging gradients in inside cells
    bool flaggingHasConverged = true;
    if ( !peano::grid::aspects::VertexStateAnalysis::isOneVertexBoundary(fineGridVertices,fineGridVerticesEnumerator) ) { // no check on boundary
      if ( cellDescription.getType()==CellDescription::Type::Cell || cellDescription.getType()==CellDescription::Type::Ancestor ) {
        for (int d=0; d<DIMENSIONS; d++) {
          flaggingHasConverged &=
              std::abs(cellDescription.getFacewiseAugmentationStatus(2*d+1)  - cellDescription.getFacewiseAugmentationStatus(2*d+0)) <= 2;
          flaggingHasConverged &=
              std::abs(cellDescription.getFacewiseCommunicationStatus(2*d+1) - cellDescription.getFacewiseCommunicationStatus(2*d+0)) <= 2;
        }
      }
      // refinement status is only spread on finest level
      if (
          cellDescription.getType()  == CellDescription::Type::Cell &&
          cellDescription.getLevel() == getMaximumAdaptiveMeshLevel()
      ) {
        for (int d=0; d<DIMENSIONS; d++) {
          flaggingHasConverged &=
              std::abs(cellDescription.getFacewiseRefinementStatus(2*d+1) - cellDescription.getFacewiseRefinementStatus(2*d+0)) <= 2;
        }
      }
    }
 
    bool stable =
      flaggingHasConverged
      &&
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None
      &&
      (cellDescription.getType()!=CellDescription::Cell || // cell must not have pending refinement status and must not require refinement on coarser grids
      (cellDescription.getRefinementStatus()!=Pending &&
      (cellDescription.getLevel() == getMaximumAdaptiveMeshLevel() ||
      cellDescription.getRefinementStatus()<=0)))
      &&
      (cellDescription.getType()!=CellDescription::Descendant || // descendant must not have refinement status > 0 on finest level
      cellDescription.getLevel() != getMaximumAdaptiveMeshLevel() ||
      cellDescription.getRefinementStatus()<=0);

      if ( !stable ) {
        #ifdef MonitorMeshRefinement
        logInfo("attainedStableState(...)","cell has not attained stable state (yet):");
        logInfo("attainedStableState(...)","type="<<cellDescription.toString(cellDescription.getType()));
        logInfo("attainedStableState(...)","x="<<cellDescription.getOffset());
        logInfo("attainedStableState(...)","level="<<cellDescription.getLevel());
        logInfo("attainedStableState(...)","flaggingHasConverged="<<flaggingHasConverged);
        logInfo("attainedStableState(...)","refinementEvent="<<cellDescription.toString(cellDescription.getRefinementEvent()));
        logInfo("attainedStableState(...)","refinementStatus="<<cellDescription.getRefinementStatus());
        logInfo("attainedStableState(...)","getFacewiseAugmentationStatus="<<cellDescription.getFacewiseAugmentationStatus());
        logInfo("attainedStableState(...)","getFacewiseCommunicationStatus="<<cellDescription.getFacewiseCommunicationStatus());
        logInfo("attainedStableState(...)","getFacewiseRefinementStatus="<<cellDescription.getFacewiseRefinementStatus());
        #ifdef Asserts
        logInfo("attainedStableState(...)","cellDescription.getCreation()="<<cellDescription.toString(cellDescription.getCreation()));
        #endif
        logInfo("attainedStableState(...)","fineGridCell.getCellDescriptionsIndex()="<<fineGridCell.getCellDescriptionsIndex());
        logInfo("attainedStableState(...)","solver.getCoarsestMeshLevel="<<getCoarsestMeshLevel());
        logInfo("attainedStableState(...)","solver.getMaximumAdaptiveMeshLevel="<<getMaximumAdaptiveMeshLevel());
        logInfo("attainedStableState(...)","solver.getMaximumAdaptiveMeshDepth="<<getMaximumAdaptiveMeshDepth());
        #endif
        if (
            !stillInRefiningMode &&
            cellDescription.getType()  == CellDescription::Descendant &&
            cellDescription.getLevel() == getMaximumAdaptiveMeshLevel() &&
            cellDescription.getRefinementStatus() > 0
        ) {
          logError("attainedStableState(...)","Virtual subcell requests refining of coarse grid parent but mesh refinement is not in refining mode anymore. Inform Dominic.");
          logError("attainedStableState(...)","cell="<<cellDescription.toString());
          std::terminate();
        }
      }

    return stable;
  } else {
    return true;
  }
}

bool exahype::solvers::ADERDGSolver::progressMeshRefinementInLeaveCell(
    exahype::Cell& fineGridCell,
    exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
    const int solverNumber,
    const bool stillInRefiningMode) {
  bool newComputeCell = false;

  const int fineGridElement =
      tryGetElement(fineGridCell.getCellDescriptionsIndex(),solverNumber);
  if ( fineGridElement!=exahype::solvers::Solver::NotFound ) {
    CellDescription& fineGridCellDescription = getCellDescription(
        fineGridCell.getCellDescriptionsIndex(),fineGridElement);

    // start or finish collective operations
    progressCollectiveRefinementOperationsInLeaveCell(fineGridCellDescription,stillInRefiningMode);

    // skip remainder if the refinement criterion has not been evaluated yet for a Cell
    // Reading the refinement request might result into data race but this is accepted at this point
    // as we only read and not write
    const int coarseGridElement =
        tryGetElement(coarseGridCell.getCellDescriptionsIndex(),solverNumber);
    if ( coarseGridElement!=exahype::solvers::Solver::NotFound ) {
      assertion3(fineGridCellDescription.getParentIndex()==coarseGridCell.getCellDescriptionsIndex(),
                 fineGridCellDescription.toString(),fineGridCell.toString(),
                 coarseGridCell.toString()); // see mergeCellDescriptionsWithRemoteData.

      CellDescription& coarseGridCellDescription = getCellDescription(
          fineGridCellDescription.getParentIndex(),coarseGridElement);
      assertion1(fineGridCellDescription.getSolverNumber()==
          coarseGridCellDescription.getSolverNumber(),
                     fineGridCellDescription.toString());

      restrictVolumeDataIfErasingRequested(
          fineGridCellDescription,coarseGridCellDescription);

      eraseCellDescriptionIfNecessary(
              fineGridCell.getCellDescriptionsIndex(),
              fineGridElement,
              coarseGridCellDescription);
    }
  }
  return newComputeCell;
}

exahype::solvers::Solver::RefinementControl
exahype::solvers::ADERDGSolver::eraseOrRefineAdjacentVertices(
    const int cellDescriptionsIndex,
    const int solverNumber,
    const tarch::la::Vector<DIMENSIONS, double>& cellOffset,
    const tarch::la::Vector<DIMENSIONS, double>& cellSize,
    const int level,
    const bool checkThoroughly) const {
  if ( level < _coarsestMeshLevel ) {
     return RefinementControl::Refine;
  } else {
    const int isValidIndex =
        cellDescriptionsIndex > 0 &&
        (!checkThoroughly ||
        Heap::getInstance().getInstance().isValidIndex(cellDescriptionsIndex));

    if ( isValidIndex ) {
      const int element = tryGetElement(cellDescriptionsIndex,solverNumber);
      if (element!=NotFound) {
        CellDescription& cellDescription = getCellDescription(
            cellDescriptionsIndex,element);

        if (
            !checkThoroughly ||
            Vertex::equalUpToRelativeTolerance(cellDescription.getOffset(), cellOffset)
        ) {
          bool refineAdjacentVertices =
              cellDescription.getType()==CellDescription::Type::Ancestor ||
              cellDescription.getHasVirtualChildren() ||
              cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested ||
              cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeChildrenToVirtualChildren ||
              cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::VirtualRefiningRequested ||
              cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::RefiningRequested ||
              cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Refining ||
              cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::VirtualRefining;

          #ifdef Asserts
          assertion1(
              cellDescription.getRefinementEvent()!=CellDescription::RefinementEvent::RefiningRequested ||
              cellDescription.getType()==CellDescription::Type::Cell,
              cellDescription.toString());
          assertion1(
              cellDescription.getRefinementEvent()!=CellDescription::RefinementEvent::VirtualRefiningRequested ||
              cellDescription.getType()==CellDescription::Type::Cell ||
              cellDescription.getType()==CellDescription::Type::Descendant,
              cellDescription.toString());
          #endif

          bool eraseAdjacentVertices =
              (cellDescription.getType()==CellDescription::Type::Cell ||
                  cellDescription.getType()==CellDescription::Type::Descendant)
                  &&
                  cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None
                  &&
                  !cellDescription.getHasVirtualChildren()
                  &&
                  cellDescription.getAugmentationStatus()==0 // TODO(Dominic): Probably can tune here. This is chosen to large
                  &&
                  cellDescription.getRefinementStatus()==0;

          if (refineAdjacentVertices) {
            return RefinementControl::Refine;
          } else if (eraseAdjacentVertices) {
            return RefinementControl::Erase;
          } else {
            return RefinementControl::Keep;
          }
        } else {
          return RefinementControl::Keep; // ?
        }
      } else {
        return RefinementControl::Erase;
      }
    } else {
      return RefinementControl::Erase;
    }
  }
}

void exahype::solvers::ADERDGSolver::prepareVolumeDataRestriction(
    CellDescription& cellDescription) const {
  double* solution =
      static_cast<double*>(cellDescription.getSolution());
  std::fill_n(solution,getDataPerCell(),0.0);
  double* previousSolution =
      static_cast<double*>(cellDescription.getPreviousSolution());
  std::fill_n(previousSolution,getDataPerCell(),0.0);
}

void exahype::solvers::ADERDGSolver::changeCellToAncestor(CellDescription& cellDescription) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell,
             cellDescription.toString());
  cellDescription.setType(CellDescription::Type::Ancestor);
  cellDescription.setAugmentationStatus(MaximumAugmentationStatus);
  cellDescription.setHasVirtualChildren(false); // since we might replace descendants with cells
  cellDescription.setRefinementStatus(Keep);
  cellDescription.setCommunicationStatus(0);
  cellDescription.setFacewiseAugmentationStatus(0); // implicit conversion
  cellDescription.setFacewiseRefinementStatus(Pending);
  cellDescription.setFacewiseCommunicationStatus(0); // implicit conversion
  ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
  cellDescription.setRefinementEvent(CellDescription::None);
}

void exahype::solvers::ADERDGSolver::progressCollectiveRefinementOperationsInEnterCell(
     CellDescription& fineGridCellDescription) {
  fineGridCellDescription.setVetoErasingChildren(false);

  switch (fineGridCellDescription.getRefinementEvent()) {
    case CellDescription::Refining:
      changeCellToAncestor(fineGridCellDescription);
      break;
    case CellDescription::VirtualRefining:
      fineGridCellDescription.setHasVirtualChildren(true);
      fineGridCellDescription.setRefinementEvent(CellDescription::None);
      break;
    default:
      break;
  }
}

bool exahype::solvers::ADERDGSolver::markPreviousAncestorForRefinement(CellDescription& cellDescription) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell &&
             (cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingChildrenRequested ||
             cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested),
             cellDescription.toString());
    double* solution = static_cast<double*>(cellDescription.getSolution());
    adjustSolution(solution,
          cellDescription.getOffset()+0.5*cellDescription.getSize(),
          cellDescription.getSize(),
          cellDescription.getTimeStamp(),
          cellDescription.getTimeStepSize());

    double* previousSolution = static_cast<double*>(cellDescription.getPreviousSolution());
    adjustSolution(previousSolution,
          cellDescription.getOffset()+0.5*cellDescription.getSize(),
          cellDescription.getSize(),
          cellDescription.getPreviousTimeStamp(),
          cellDescription.getPreviousTimeStepSize());

    cellDescription.setRefinementStatus( evaluateRefinementCriterion(
            cellDescription,solution,cellDescription.getTimeStamp())
    );
    cellDescription.setPreviousRefinementStatus( evaluateRefinementCriterion(
            cellDescription,previousSolution,cellDescription.getPreviousTimeStamp())
    );

    return cellDescription.getRefinementStatus()        !=_refineOrKeepOnFineGrid &&
           cellDescription.getPreviousRefinementStatus()!=_refineOrKeepOnFineGrid;
}

void exahype::solvers::ADERDGSolver::progressCollectiveRefinementOperationsInLeaveCell(
     CellDescription& fineGridCellDescription,
     const bool stillInRefiningMode) {
  switch ( fineGridCellDescription.getRefinementEvent() ) {
    case CellDescription::RefinementEvent::ErasingChildrenRequested:
      // evaluate refinement criterion now that fine grid cells have restricted their data
      if ( markPreviousAncestorForRefinement(fineGridCellDescription) ) {
        fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::ErasingChildren);
      } else { // veto erasing request
        changeCellToAncestor(fineGridCellDescription);
      }
      break;
    case CellDescription::RefinementEvent::ErasingChildren:
      //logInfo("progressCollectiveRefinementOperationsInLeaveCell(...)","ErasingChildren done");
      fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::None);
      break;
    case CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested:
      // evaluate refinement criterion now that fine grid cells have restricted their data
      if ( markPreviousAncestorForRefinement(fineGridCellDescription) ) {
        fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::ChangeChildrenToVirtualChildren);
      } else { // veto erasing request
        changeCellToAncestor(fineGridCellDescription);
      }
      break;
    case CellDescription::ChangeChildrenToVirtualChildren:
      fineGridCellDescription.setHasVirtualChildren(true);
      fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::None);
      break;
    case CellDescription::RefinementEvent::ErasingVirtualChildrenRequested:
      fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::ErasingVirtualChildren);
      break;
    case CellDescription::RefinementEvent::ErasingVirtualChildren:
      fineGridCellDescription.setHasVirtualChildren(false);
      fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::None);
      break;
    default:
      break;
  }

  if (  // The children of this cell description have all flagged themselves and their parent (this cell) with Erase
      !stillInRefiningMode &&
      fineGridCellDescription.getType()==CellDescription::Type::Ancestor &&
      fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None &&
      fineGridCellDescription.getVetoErasingChildren()==false &&
      fineGridCellDescription.getRefinementStatus()==Erase &&
      fineGridCellDescription.getPreviousRefinementStatus()==Erase
  ) {
    //logInfo("progressCollectiveRefinementOperationsInLeaveCell(...)","ErasingChildren requested: "<<fineGridCellDescription.getRefinementStatus()<< ", "<<fineGridCellDescription.getPreviousRefinementStatus());
    fineGridCellDescription.setType(CellDescription::Type::Cell);
    fineGridCellDescription.setAugmentationStatus(0);
    fineGridCellDescription.setFacewiseAugmentationStatus(0); // implicit conversion
    fineGridCellDescription.setCommunicationStatus(CellCommunicationStatus);
    fineGridCellDescription.setFacewiseCommunicationStatus(CellCommunicationStatus); // implicit conversion
    ensureNecessaryMemoryIsAllocated(fineGridCellDescription);
    prepareVolumeDataRestriction(fineGridCellDescription);
    fineGridCellDescription.setRefinementEvent(CellDescription::RefinementEvent::ErasingChildrenRequested);
  }
}

void exahype::solvers::ADERDGSolver::eraseCellDescriptionIfNecessary(
    const int cellDescriptionsIndex,
    const int fineGridElement,
    CellDescription& coarseGridCellDescription) {
  tarch::multicore::Lock lock(CoarseGridSemaphore);
  const bool erasingChildren =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingChildren;
  const bool changeChildrenToDescendants =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::ChangeChildrenToVirtualChildren;
  const bool deaugmentingChildren =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingVirtualChildren;
  lock.free();

  if ( changeChildrenToDescendants ) {
    CellDescription& fineGridCellDescription = getCellDescription(
        cellDescriptionsIndex,fineGridElement);

    // erase cell description // or change to descendant
    fineGridCellDescription.setType(CellDescription::Type::Descendant);
    fineGridCellDescription.setCommunicationStatus(0);
    fineGridCellDescription.setFacewiseCommunicationStatus(0); // implicit conversion
    ensureNoUnnecessaryMemoryIsAllocated(fineGridCellDescription);
  }
  else if ( erasingChildren || deaugmentingChildren ) {
    CellDescription& fineGridCellDescription = getCellDescription(
          cellDescriptionsIndex,fineGridElement);

    fineGridCellDescription.setType(CellDescription::Erased);
    fineGridCellDescription.setCommunicationStatus(0);
    fineGridCellDescription.setFacewiseCommunicationStatus(0); // implicit conversion
    ensureNoUnnecessaryMemoryIsAllocated(fineGridCellDescription);

    getCellDescriptions(cellDescriptionsIndex).erase(
        getCellDescriptions(cellDescriptionsIndex).begin()+fineGridElement);
  }
}

void exahype::solvers::ADERDGSolver::restrictVolumeDataIfErasingRequested(
    const CellDescription& fineGridCellDescription,
    const CellDescription& coarseGridCellDescription) {
//  assertion1(coarseGridCellDescription.getLimiterStatus()==CellDescription::LimiterStatus::Ok,
//      coarseGridCellDescription.toString()); // TODO(Dominic): Does not always apply see veto
  tarch::multicore::Lock lock(CoarseGridSemaphore);
  const bool restrictVolumeData =
      coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingChildrenRequested ||
      coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested;
  lock.free();

  if ( restrictVolumeData ) {
    //logInfo("restrictVolumeData(..)","restricting solution");

    tarch::la::Vector<DIMENSIONS,int> subcellIndex =
        exahype::amr::computeSubcellIndex(
            fineGridCellDescription.getOffset(),
            fineGridCellDescription.getSize(),coarseGridCellDescription.getOffset());

    // restrict values.
    tarch::multicore::Lock lock(RestrictionSemaphore);
    assertion1(fineGridCellDescription.getRefinementStatus()==-1,fineGridCellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(fineGridCellDescription.getSolutionIndex()),fineGridCellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(coarseGridCellDescription.getSolutionIndex()),coarseGridCellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(fineGridCellDescription.getPreviousSolutionIndex()),fineGridCellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(coarseGridCellDescription.getPreviousSolutionIndex()),coarseGridCellDescription.toString());

    const int levelFine   = fineGridCellDescription.getLevel();
    const int levelCoarse = coarseGridCellDescription.getLevel();
    assertion(levelCoarse < levelFine);

    if ( !DataHeap::getInstance().isValidIndex(fineGridCellDescription.getSolutionIndex()) ) {
      logError("restrictVolumeData(..)","solution not valid for cell="<<fineGridCellDescription.toString());
      std::abort();
    }

    // restrict current solution
    double* solutionFine   = static_cast<double*>(fineGridCellDescription.getSolution());
    double* solutionCoarse = static_cast<double*>(coarseGridCellDescription.getSolution());
    volumeUnknownsRestriction(
        solutionCoarse,solutionFine,
        levelCoarse,levelFine,
        subcellIndex);

    // restrict next solution
    double* previousSolutionFine   = static_cast<double*>(fineGridCellDescription.getPreviousSolution());
    double* previousSolutionCoarse = static_cast<double*>(coarseGridCellDescription.getPreviousSolution());
    volumeUnknownsRestriction(
        previousSolutionCoarse,previousSolutionFine,
        levelCoarse,levelFine,
        subcellIndex);

    // TODO(Dominic): Do later, move out

    // Reset the min and max
    const int numberOfObservables = getDMPObservables();
    if ( numberOfObservables>0 ) {
      double* solutionMin = static_cast<double*>(coarseGridCellDescription.getSolutionMin());
      std::fill_n(solutionMin,DIMENSIONS_TIMES_TWO*numberOfObservables,
          std::numeric_limits<double>::infinity());
      double* solutionMax = static_cast<double*>(coarseGridCellDescription.getSolutionMax());
      std::fill_n(solutionMax,DIMENSIONS_TIMES_TWO*numberOfObservables,
          -std::numeric_limits<double>::infinity()); // Be aware of "-"
    }

    // TODO(Dominic): What to do with the time step data for anarchic time stepping?
    // Tobias proposed some waiting procedure. Until they all have reached
    // a certain time level.
    //  coarseGridCellDescription.setTimeStamp(fineGridCellDescription.getTimeStamp());
    //  coarseGridCellDescription.setPredictorTimeStamp(fineGridCellDescription.getPredictorTimeStamp());
    //  coarseGridCellDescription.setTimeStepSize(fineGridCellDescription.getTimeStepSize());
    //  coarseGridCellDescription.setPredictorTimeStepSize(fineGridCellDescription.getPredictorTimeStepSize());
    // TODO(Dominic): Reconsider for anarchic time stepping.
    // coarseGridCellDescription.setTimeStamp(fineGridCellDescription.getTimeStamp());
    // coarseGridCellDescription.setPredictorTimeStamp(fineGridCellDescription.getPredictorTimeStamp());
    lock.free();
  }
}

void exahype::solvers::ADERDGSolver::ensureFineGridCoarseGridConsistency(
    CellDescription& fineGridCellDescription,
    const int coarseGridCellDescriptionsIndex) {

  const int coarseGridElement = tryGetElement(coarseGridCellDescriptionsIndex,fineGridCellDescription.getSolverNumber());
  if ( coarseGridElement!=exahype::solvers::Solver::NotFound ) {
    assertion2(
        fineGridCellDescription.getParentIndex()==coarseGridCellDescriptionsIndex ||
        fineGridCellDescription.getParentIndex()==multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex,
        coarseGridCellDescriptionsIndex,
        fineGridCellDescription.toString());
    fineGridCellDescription.setParentIndex(coarseGridCellDescriptionsIndex);

    #if defined(Asserts) || defined(Debug)
    assertion2(coarseGridElement==exahype::solvers::Solver::NotFound || fineGridCellDescription.getParentIndex()==coarseGridCellDescriptionsIndex,
        fineGridCellDescription.toString(), getCellDescription(coarseGridCellDescriptionsIndex,coarseGridElement).toString());
    #endif

  } else {
    fineGridCellDescription.setParentIndex(multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
  }
}

void exahype::solvers::ADERDGSolver::finaliseStateUpdates(
      const int solverNumber,
      CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element!=exahype::solvers::Solver::NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    if (cellDescription.getType()==CellDescription::Type::Cell) {
      validateCellDescriptionData(cellDescription,cellDescription.getTimeStamp()>0,false,true,"finaliseStateUpdates");
    }
    cellDescription.setRefinementFlag(false);

    if ( getMeshUpdateEvent()==MeshUpdateEvent::InitialRefinementRequested ) {
      cellDescription.setPreviousRefinementStatus(cellDescription.getRefinementStatus());
    }
  }
}

////////////////////////////////////////
// CELL-LOCAL
////////////////////////////////////////
void exahype::solvers::ADERDGSolver::validateCellDescriptionData(
  const CellDescription& cellDescription,
  const bool validateTimeStepData,
  const bool afterCompression,
  const bool beforePrediction,
  const std::string& methodTraceOfCaller) const {
  #ifdef Asserts
  if ( _checkForNaNs && validateTimeStepData ) {
    assertion2(std::isfinite(cellDescription.getTimeStamp()),
        cellDescription.toString(),toString());
    assertion2(cellDescription.getTimeStamp()>=0,
        cellDescription.toString(),toString());
  }
  assertion1(cellDescription.getRefinementEvent()==CellDescription::None,cellDescription.toString());
  assertion1(getType()==exahype::solvers::Solver::Type::ADERDG,cellDescription.toString());

  if ( _checkForNaNs && afterCompression) {
    // TODO(Dominic)
  } else if ( _checkForNaNs ) {
    assertion1(DataHeap::getInstance().isValidIndex(cellDescription.getSolutionIndex()),cellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(cellDescription.getUpdateIndex()),cellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex()),cellDescription.toString());
    assertion1(DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationIndex()),cellDescription.toString());

    double* luh  = static_cast<double*>(cellDescription.getSolution());
    double* lduh = static_cast<double*>(cellDescription.getUpdate());

    double* lQhbnd = static_cast<double*>(cellDescription.getExtrapolatedPredictor());
    double* lFhbnd = static_cast<double*>(cellDescription.getFluctuation());

    int dataPerCell             = getDataPerCell();
    int updateSize              = getUpdateSize();

    int dataPerCellBoundary     = getBndTotalSize();
    int unknownsPerCellBoundary = getBndFluxTotalSize();

    for (int i=0; i<dataPerCell; i++) {
      assertion4(tarch::la::equals(cellDescription.getTimeStepSize(),0.0) || std::isfinite(luh[i]),
          cellDescription.toString(),toString(),methodTraceOfCaller,i);
    }
    if ( !beforePrediction ) {
      for (int i=0; i<updateSize; i++) {
       assertion4(tarch::la::equals(cellDescription.getTimeStepSize(),0.0) || std::isfinite(lduh[i]),
           cellDescription.toString(),toString(),methodTraceOfCaller,i);
      }

      for (int i=0; i<dataPerCellBoundary; i++) {
        assertion4(tarch::la::equals(cellDescription.getTimeStepSize(),0.0) || std::isfinite(lQhbnd[i]),
            cellDescription.toString(),toString(),methodTraceOfCaller,i);
      }

      for (int i=0; i<unknownsPerCellBoundary; i++) {
        assertion4(tarch::la::equals(cellDescription.getTimeStepSize(),0.0) || std::isfinite(lFhbnd[i]),
            cellDescription.toString(),toString(),methodTraceOfCaller,i);
      }
    }
  }
  #endif
}

exahype::solvers::Solver::MeshUpdateEvent
exahype::solvers::ADERDGSolver::evaluateRefinementCriteriaAfterSolutionUpdate(
    CellDescription&                                           cellDescription,
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed
) {
  if ( OnlyInitialMeshRefinement ) {
    return MeshUpdateEvent::None;
  }

  cellDescription.setRefinementFlag(false);
  if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    const double* solution = static_cast<double*>(cellDescription.getSolution());
    RefinementControl refinementControl = refinementCriterion(
                      solution,cellDescription.getOffset()+0.5*cellDescription.getSize(),
                      cellDescription.getSize(),
                      cellDescription.getTimeStamp(), // must be called after advancing in time
                      cellDescription.getLevel());
    if (
        (refinementControl==RefinementControl::Refine ||
        (cellDescription.getLevel()==getMaximumAdaptiveMeshLevel()    &&
        refinementControl==RefinementControl::Keep))
    ) {
      cellDescription.setRefinementStatus(_refineOrKeepOnFineGrid);
      cellDescription.setRefinementFlag(true);
    } else if (
        cellDescription.getRefinementStatus()<_refineOrKeepOnFineGrid &&
        cellDescription.getLevel()<getMaximumAdaptiveMeshLevel()     &&
        refinementControl==RefinementControl::Keep
    ) {
      cellDescription.setRefinementStatus(Keep);
    }
    else if (
        cellDescription.getRefinementStatus()<=Keep &&
        refinementControl==RefinementControl::Erase
    ) {
      cellDescription.setRefinementStatus(Erase);
    }

    // update refinement status after prescribing refinement values
    updateRefinementStatus(cellDescription,neighbourMergePerformed);

    return
        (cellDescription.getLevel() < getMaximumAdaptiveMeshLevel() &&
         refinementControl==RefinementControl::Refine ) ?
            MeshUpdateEvent::RefinementRequested : MeshUpdateEvent::None;
  } else if ( cellDescription.getType()==CellDescription::Type::Descendant ) {
    // bottom up refinement criterion TODO(Dominic): Add to docu
    // We allow the halo region to diffuse into the virtual subcells
    // up to some point.
    updateRefinementStatus(cellDescription,neighbourMergePerformed);
    if (
        cellDescription.getRefinementStatus() > _refineOrKeepOnFineGrid-1 &&
        cellDescription.getLevel()==getMaximumAdaptiveMeshLevel()
    ) {
      cellDescription.setRefinementFlag(true);
      return MeshUpdateEvent::RefinementRequested;
    } else {
      return MeshUpdateEvent::None;
    }
  } else {
    cellDescription.setRefinementStatus(Pending); // Cannot override the refinement / limiter status in other cells
    return MeshUpdateEvent::None;
  }
}

exahype::solvers::Solver::UpdateResult exahype::solvers::ADERDGSolver::fusedTimeStepBody(
    CellDescription&                                           cellDescription,
    CellInfo&                                                  cellInfo,
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed,
    const double                                               predictionTimeStamp,
    const double                                               predictionTimeStepSize,
    const bool                                                 isFirstTimeStepOfBatch,
    const bool                                                 isLastTimeStepOfBatch,
    const bool                                                 isSkeletonCell,
    const bool                                                 mustBeDoneImmediately) {
  #ifdef USE_ITAC
  if ( isSkeletonCell ) {
    VT_begin(fusedTimeStepBodyHandleSkeleton);
  } else {
    VT_begin(fusedTimeStepBodyHandle);
  }
  #endif

  correction(cellDescription,neighbourMergePerformed,isFirstTimeStepOfBatch,isFirstTimeStepOfBatch/*addSurfaceIntegralContributionToUpdate*/);

  UpdateResult result;
  result._timeStepSize    = startNewTimeStep(cellDescription,isFirstTimeStepOfBatch);
  cellDescription.setPreviousRefinementStatus(cellDescription.getRefinementStatus());
  result._meshUpdateEvent = evaluateRefinementCriteriaAfterSolutionUpdate(cellDescription,neighbourMergePerformed);

  if (
      SpawnPredictionAsBackgroundJob &&
      !mustBeDoneImmediately &&
      isLastTimeStepOfBatch // only spawned in last iteration if a FusedTimeStepJob was spawned before
  ) {
    const int element = cellInfo.indexOfADERDGCellDescription(cellDescription.getSolverNumber());
    peano::datatraversal::TaskSet( new PredictionJob(
        *this, cellDescription, cellInfo._cellDescriptionsIndex,element,
        predictionTimeStamp, predictionTimeStepSize,
        false/*is uncompressed*/, isSkeletonCell, isLastTimeStepOfBatch/*addVolumeIntegralResultToUpdate*/ ) );
  } else {
    predictionAndVolumeIntegralBody(
        cellDescription,
        predictionTimeStamp, predictionTimeStepSize,
        false, isSkeletonCell, isLastTimeStepOfBatch/*addVolumeIntegralResultToUpdate*/);
  }

  #ifdef USE_ITAC
  if ( isSkeletonCell ) {
    VT_end(fusedTimeStepBodyHandleSkeleton);
  } else {
    VT_end(fusedTimeStepBodyHandle);
  }
  #endif
  return result;
}

exahype::solvers::Solver::UpdateResult exahype::solvers::ADERDGSolver::fusedTimeStepOrRestrict(
    const int  solverNumber,
    CellInfo&  cellInfo,
    const bool isFirstTimeStepOfBatch,
    const bool isLastTimeStepOfBatch,
    const bool isAtRemoteBoundary) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    synchroniseTimeStepping(cellDescription);
    cellDescription.setHasCompletedLastStep(false);

    if ( cellDescription.getType()==CellDescription::Type::Cell ) {
      const bool isAMRSkeletonCell     = cellDescription.getHasVirtualChildren();
      const bool isSkeletonCell        = isAMRSkeletonCell || isAtRemoteBoundary;
      const bool mustBeDoneImmediately = isSkeletonCell && PredictionSweeps==1;

      if (
          (SpawnUpdateAsBackgroundJob || (SpawnPredictionAsBackgroundJob && !isLastTimeStepOfBatch)) &&
          !mustBeDoneImmediately
      ) {
        const auto predictionTimeStepData = getPredictionTimeStepData(cellDescription,true);
        peano::datatraversal::TaskSet( new FusedTimeStepJob(
            *this, cellDescription, cellInfo,
            std::get<0>(predictionTimeStepData),std::get<1>(predictionTimeStepData),
            isFirstTimeStepOfBatch, isLastTimeStepOfBatch, isSkeletonCell) );
        return UpdateResult();
      } else {
        const auto predictionTimeStepData = getPredictionTimeStepData(cellDescription,true);
        return
            fusedTimeStepBody(
                cellDescription,cellInfo,cellDescription.getNeighbourMergePerformed(),
                std::get<0>(predictionTimeStepData),std::get<1>(predictionTimeStepData),
                isFirstTimeStepOfBatch,isLastTimeStepOfBatch,
                isSkeletonCell,mustBeDoneImmediately );
      }
    } else if (
        cellDescription.getType()==CellDescription::Type::Descendant &&
        cellDescription.getCommunicationStatus()>=MinimumCommunicationStatusForNeighbourCommunication
    ) {
      restrictToTopMostParent(cellDescription,isFirstTimeStepOfBatch/*addToCoarseGridUpdate*/);
      cellDescription.setHasCompletedLastStep(true);
      // TODO(Dominic): Evaluate ref crit here too // halos
      return UpdateResult();
    } else {
      cellDescription.setHasCompletedLastStep(true);
      return UpdateResult();
    }
  } else {
    return UpdateResult();
  }
}

exahype::solvers::Solver::UpdateResult exahype::solvers::ADERDGSolver::updateBody(
    CellDescription&                                           cellDescription,
    CellInfo&                                                  cellInfo,
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed,
    const bool                                                 isAtRemoteBoundary) {
  #ifdef USE_ITAC
  VT_begin(updateBodyHandle);
  #endif

  assertion1(cellDescription.getType()==CellDescription::Type::Cell,cellDescription.toString());
  uncompress(cellDescription);

  correction(cellDescription,neighbourMergePerformed,true,false/*effect: add face integral result directly to solution*/);

  UpdateResult result;
  result._timeStepSize    = startNewTimeStep(cellDescription,true);
  cellDescription.setPreviousRefinementStatus(cellDescription.getRefinementStatus());
  result._meshUpdateEvent = evaluateRefinementCriteriaAfterSolutionUpdate(cellDescription,neighbourMergePerformed);

  compress(cellDescription,isAtRemoteBoundary);

  cellDescription.setHasCompletedLastStep(true); // required as prediction checks the flag too. Field should be renamed "setHasCompletedLastOperation(...)".

  #ifdef USE_ITAC
  VT_end(updateBodyHandle);
  #endif
  return result;
}

exahype::solvers::Solver::UpdateResult exahype::solvers::ADERDGSolver::updateOrRestrict(
      const int  solverNumber,
      CellInfo&  cellInfo,
      const bool isAtRemoteBoundary){
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    synchroniseTimeStepping(cellDescription);
    cellDescription.setHasCompletedLastStep(false);

    if ( cellDescription.getType()==CellDescription::Type::Cell && SpawnUpdateAsBackgroundJob ) {
      peano::datatraversal::TaskSet ( new UpdateJob( *this, cellDescription, cellInfo, isAtRemoteBoundary ) );
      return UpdateResult();
    }
    else if ( cellDescription.getType()==CellDescription::Type::Cell ) {
      return updateBody(cellDescription,cellInfo,cellDescription.getNeighbourMergePerformed(),isAtRemoteBoundary);
    }
    else if ( // TODO(Dominic): Evaluate ref crit here too // halos
        cellDescription.getType()==CellDescription::Type::Descendant &&
        cellDescription.getCommunicationStatus()>=MinimumCommunicationStatusForNeighbourCommunication
    ) {
      restrictToTopMostParent(cellDescription,false/*effect: add face integral result directly to solution*/);
      cellDescription.setHasCompletedLastStep(true);
      return UpdateResult();
    }
    else {
      cellDescription.setHasCompletedLastStep(true);
      return UpdateResult();
    }
  }
  else {
    return UpdateResult();
  }
}

void exahype::solvers::ADERDGSolver::compress(
      const int solverNumber,
      CellInfo& cellInfo,
      const bool isAtRemoteBoundary) const {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element!=NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    if (cellDescription.getType()==CellDescription::Type::Cell) {
      const bool isSkeletonCell = cellDescription.getHasVirtualChildren();
      compress(cellDescription,isSkeletonCell);
    }
  }
}

int exahype::solvers::ADERDGSolver::predictionAndVolumeIntegralBody(
    CellDescription& cellDescription,
    const double predictorTimeStamp,
    const double predictorTimeStepSize,
    const bool   uncompressBefore,
    const bool   isSkeletonCell,
    const bool   addVolumeIntegralResultToUpdate) {
  #ifdef USE_ITAC
  if ( isSkeletonCell ) {
    VT_begin(predictorBodyHandleSkeleton);
  } else {
    VT_begin(predictorBodyHandle);
  }
  #endif
  if (uncompressBefore) { uncompress(cellDescription); }

  validateCellDescriptionData(cellDescription,true,false,true,"exahype::solvers::ADERDGSolver::performPredictionAndVolumeIntegralBody [pre]");

  double* luh    = static_cast<double*>(cellDescription.getSolution());
  double* lduh   = static_cast<double*>(cellDescription.getUpdate());
  double* lQhbnd = static_cast<double*>(cellDescription.getExtrapolatedPredictor());
  double* lGradQhbnd = static_cast<double*>(cellDescription.getExtrapolatedPredictorGradient());
  double* lFhbnd = static_cast<double*>(cellDescription.getFluctuation());


  #ifdef Asserts
  for (int i=0; i<getDataPerCell(); i++) {
    assertion3(tarch::la::equals(cellDescription.getTimeStepSize(),0.0) || std::isfinite(luh[i]),cellDescription.toString(),"performPredictionAndVolumeIntegral(...)",i);
  }
  #endif

  #if !defined(SharedMemoryParallelisation) && !defined(Parallel) && defined(Asserts)
  static int counter = 0;
  static double timeStamp = 0;
  if ( !tarch::la::equals(timeStamp,_minTimeStamp,1e-9) ) {
    logInfo("performPredictionAndVolumeIntegralBody(...)","#predictions="<<counter);
    timeStamp = _minTimeStamp;
    counter=0;
  }
  counter++;
  #endif

  // TODO(Lukas) Is this really the corrector timestamp?
  const auto correctorTimeStamp    = cellDescription.getTimeStamp();
  const auto correctorTimeStepSize = cellDescription.getTimeStepSize(); // TODO(Dominic): where is this used?

  const int numberOfPicardIterations = fusedSpaceTimePredictorVolumeIntegral(
      lduh,lQhbnd,lGradQhbnd,lFhbnd,
      luh,
      cellDescription.getOffset()+0.5*cellDescription.getSize(),
      cellDescription.getSize(),
      predictorTimeStamp,
      predictorTimeStepSize,
      addVolumeIntegralResultToUpdate); // TODO(Dominic): fix 'false' case

  compress(cellDescription,isSkeletonCell);

  validateCellDescriptionData(cellDescription,true,true,false,"exahype::solvers::ADERDGSolver::performPredictionAndVolumeIntegralBody [post]");

  cellDescription.setHasCompletedLastStep(true);

  
  #ifdef USE_ITAC
  if ( isSkeletonCell ) {
    VT_end(predictorBodyHandleSkeleton);
  } else {
    VT_end(predictorBodyHandle);
  }
  #endif

  return numberOfPicardIterations;
}

void exahype::solvers::ADERDGSolver::predictionAndVolumeIntegral(
    const int    solverNumber,
    CellInfo&    cellInfo,
    const double predictorTimeStamp,
    const double predictorTimeStepSize,
    const bool   uncompressBefore,
    const bool   isAtRemoteBoundary,
    const bool   addVolumeIntegralResultToUpdate) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

  if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    cellDescription.setHasCompletedLastStep(false);

    const bool isAMRSkeletonCell     = cellDescription.getHasVirtualChildren();
    const bool isSkeletonCell        = isAMRSkeletonCell || isAtRemoteBoundary;
    const bool mustBeDoneImmediately = isSkeletonCell && PredictionSweeps==1;

    if ( SpawnPredictionAsBackgroundJob && !mustBeDoneImmediately ) {
      peano::datatraversal::TaskSet( new PredictionJob(
              *this, cellDescription, cellInfo._cellDescriptionsIndex, element,
              predictorTimeStamp,predictorTimeStepSize,
              uncompressBefore,isSkeletonCell,addVolumeIntegralResultToUpdate) );
    }
    else {
      predictionAndVolumeIntegralBody(
          cellDescription,
          predictorTimeStamp,predictorTimeStepSize,
          uncompressBefore,isSkeletonCell,addVolumeIntegralResultToUpdate);
    }
  }
}

void exahype::solvers::ADERDGSolver::predictionAndVolumeIntegral(
    const int solverNumber,
    CellInfo& cellInfo,
    const bool isAtRemoteBoundary) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != Solver::NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    synchroniseTimeStepping(cellDescription);

    const bool isAMRSkeletonCell = cellDescription.getHasVirtualChildren();
    const bool isSkeletonCell    = isAMRSkeletonCell || isAtRemoteBoundary;
    waitUntilCompletedLastStep(cellDescription,isSkeletonCell,false);
    if ( cellDescription.getType()==CellDescription::Type::Cell ) {
      const auto predictionTimeStepData = getPredictionTimeStepData(cellDescription,false); // this is either the fused scheme or a predictor recomputation

      const bool rollbacksPossible = !OnlyInitialMeshRefinement;
      if ( !FuseAllADERDGPhases && rollbacksPossible ) { // backup previous solution here as prediction already adds a contribution to solution if not all alg. phases are fused.
        std::copy_n(
            static_cast<double*>(cellDescription.getSolution()),getDataPerCell(),
            static_cast<double*>(cellDescription.getPreviousSolution()));
      }

      predictionAndVolumeIntegral(solverNumber,cellInfo,
          std::get<0>(predictionTimeStepData),
          std::get<1>(predictionTimeStepData),
          true,isAtRemoteBoundary,
          FuseAllADERDGPhases/*addVolumeIntegralResultToUpdate*/);
    }
  }
}

double exahype::solvers::ADERDGSolver::computeTimeStepSize(CellDescription& cellDescription) {
  if( cellDescription.getType()==CellDescription::Type::Cell ) {
    assertion1( cellDescription.getRefinementEvent()==CellDescription::None,cellDescription.toString());
    const double* luh = static_cast<double*>(cellDescription.getSolution());

    validateCellDescriptionData(cellDescription,false,false,true,"computeTimeStepSizes(...)");
    double admissibleTimeStepSize = stableTimeStepSize(luh,cellDescription.getSize());
    assertion2(!_checkForNaNs || admissibleTimeStepSize>0,admissibleTimeStepSize,cellDescription.toString());

    assertion3(!_checkForNaNs || admissibleTimeStepSize<std::numeric_limits<double>::infinity(),std::numeric_limits<double>::infinity(),admissibleTimeStepSize,cellDescription.toString());
    assertion2(!_checkForNaNs || std::isfinite(admissibleTimeStepSize),admissibleTimeStepSize,cellDescription.toString());

    return admissibleTimeStepSize;
  } else {
    return std::numeric_limits<double>::infinity();
  }
}


double exahype::solvers::ADERDGSolver::startNewTimeStep(
    CellDescription& cellDescription,
    const bool isFirstTimeStepOfBatch) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell,cellDescription.toString());
  // n-1
  if ( isFirstTimeStepOfBatch ) {
    cellDescription.setPreviousTimeStamp(cellDescription.getTimeStamp());
    cellDescription.setPreviousTimeStepSize(cellDescription.getTimeStepSize());
  }
  // n
  cellDescription.setTimeStamp(cellDescription.getTimeStamp()+cellDescription.getTimeStepSize());
  if ( getTimeStepping() != TimeStepping::GlobalFixed ) {
    double admissibleTimeStepSize = computeTimeStepSize(cellDescription);
    cellDescription.setTimeStepSize(admissibleTimeStepSize);
    return admissibleTimeStepSize;
  } else {
    return cellDescription.getTimeStepSize();
  }
}

double exahype::solvers::ADERDGSolver::updateTimeStepSize(const int solverNumber,CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    const double admissibleTimeStepSize = computeTimeStepSize(cellDescription);
    cellDescription.setTimeStepSize( admissibleTimeStepSize );
    cellDescription.setHasCompletedLastStep(true);
    return admissibleTimeStepSize;
  } else {
    return std::numeric_limits<double>::infinity();
  }
}

void exahype::solvers::ADERDGSolver::rollbackToPreviousTimeStep(CellDescription& cellDescription) const {
  cellDescription.setTimeStamp   (cellDescription.getPreviousTimeStamp());
  cellDescription.setTimeStepSize(std::numeric_limits<double>::infinity());

  cellDescription.setPreviousTimeStamp(std::numeric_limits<double>::infinity());
  cellDescription.setPreviousTimeStepSize(std::numeric_limits<double>::infinity());
}

void exahype::solvers::ADERDGSolver::adjustSolutionDuringMeshRefinement(
    const int solverNumber,
    CellInfo& cellInfo) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    synchroniseTimeStepping(cellDescription);

    const bool isInitialMeshRefinement = getMeshUpdateEvent()==MeshUpdateEvent::InitialRefinementRequested;
    if ( exahype::solvers::Solver::SpawnAMRBackgroundJobs ) {
      cellDescription.setHasCompletedLastStep(false);
      peano::datatraversal::TaskSet( new AdjustSolutionDuringMeshRefinementJob(*this,cellDescription,isInitialMeshRefinement));
    } else {
      adjustSolutionDuringMeshRefinementBody(cellDescription,isInitialMeshRefinement);
    }
  }
}

void exahype::solvers::ADERDGSolver::adjustSolutionDuringMeshRefinementBody(
    CellDescription& cellDescription,
    const bool isInitialMeshRefinement) {
  #ifdef USE_ITAC
  VT_begin(adjustSolutionHandle);
  #endif

  if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    assertion1(
        cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None ||
        cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Prolongating
        ,cellDescription.toString());

    if (cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Prolongating) {
      prolongateVolumeData(cellDescription,isInitialMeshRefinement);
      cellDescription.setRefinementEvent(CellDescription::RefinementEvent::None);
    }
    
    adjustSolution(cellDescription);
    markForRefinement(cellDescription);
  }

  cellDescription.setHasCompletedLastStep(true);

  #ifdef USE_ITAC
  VT_end(adjustSolutionHandle);
  #endif
}

void exahype::solvers::ADERDGSolver::adjustSolution(CellDescription& cellDescription) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell,cellDescription.toString());    
  assertion1(
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::None ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingChildrenRequested
      ,cellDescription.toString());

  double* solution = static_cast<double*>(cellDescription.getSolution());
  adjustSolution(
      solution,
      cellDescription.getOffset()+0.5*cellDescription.getSize(),
      cellDescription.getSize(),
      cellDescription.getTimeStamp(),
      cellDescription.getTimeStepSize());

  double* previousSolution = static_cast<double*>(cellDescription.getPreviousSolution());
  adjustSolution(
      previousSolution,
      cellDescription.getOffset()+0.5*cellDescription.getSize(),
      cellDescription.getSize(),
      cellDescription.getPreviousTimeStamp(),
      cellDescription.getPreviousTimeStepSize());

  #ifdef Asserts
  if ( _checkForNaNs ) {
    for (int i=0; i<getDataPerCell(); i++) {
      assertion3(std::isfinite(solution[i]),cellDescription.toString(),"adjustSolution(...)",i);
    }
  }
  #endif
}


void exahype::solvers::ADERDGSolver::printADERDGSolution2D(const CellDescription& cellDescription)  const {
  #if DIMENSIONS==2
  double* solution = static_cast<double*>(cellDescription.getSolution());
  assertion1(cellDescription.getType()==CellDescription::Type::Cell,cellDescription.toString());
  assertion1(solution!=nullptr,cellDescription.toString());

  std::cout <<  "solution:" << std::endl;
  const int numberOfData = _numberOfVariables + _numberOfParameters;
  for (int unknown=0; unknown < numberOfData; unknown++) {
    std::cout <<  "unknown=" << unknown << std::endl;
    dfor(i,_nodesPerCoordinateAxis) {
      int iScalar = peano::utils::dLinearisedWithoutLookup(i,_nodesPerCoordinateAxis)*numberOfData+unknown;
      std::cout << std::setprecision(3) << solution[iScalar] << ",";
      if (i(0)==_nodesPerCoordinateAxis-1) {
        std::cout << std::endl;
      }
    }
  }
  std::cout <<  "}" << std::endl;
  #endif
}

void exahype::solvers::ADERDGSolver::printADERDGExtrapolatedPredictor2D(const CellDescription& cellDescription) const {
  #if DIMENSIONS==2
  double* extrapolatedPredictor = static_cast<double*>(cellDescription.getExtrapolatedPredictor());
  assertion1(extrapolatedPredictor!=nullptr,cellDescription.toString());

  std::cout <<  "extrapolated predictor:" << std::endl;
  const int numberOfData = _numberOfVariables + _numberOfParameters;
  for (int f=0; f<DIMENSIONS_TIMES_TWO; f++) {
    std::cout <<  "face=" << f << std::endl;
    for (int unknown=0; unknown < numberOfData; unknown++) {
      std::cout <<  "unknown=" << unknown << std::endl;
      for (int i=0; i<_nodesPerCoordinateAxis; i++) {
        int iScalar = f*numberOfData*_nodesPerCoordinateAxis + numberOfData*i + unknown;
        std::cout << std::setprecision(3) << extrapolatedPredictor[iScalar] << ",";
        if (i==_nodesPerCoordinateAxis-1) {
          std::cout << std::endl;
        }
      }
    }
  }
  std::cout <<  "}" << std::endl;
  #endif
}

void exahype::solvers::ADERDGSolver::printADERDGFluctuations2D(const CellDescription& cellDescription) const {
  #if DIMENSIONS==2
  double* fluctuation = static_cast<double*>(cellDescription.getFluctuation());
  assertion1(fluctuation!=nullptr,cellDescription.toString());

  std::cout << "fluctuations:" << std::endl;
  const int numberOfData = _numberOfVariables + _numberOfParameters;
  for (int f=0; f<DIMENSIONS_TIMES_TWO; f++) {
    std::cout <<  "face=" << f << std::endl;
    for (int unknown=0; unknown < numberOfData; unknown++) {
      std::cout <<  "unknown=" << unknown << std::endl;
      for (int i=0; i<_nodesPerCoordinateAxis; i++) {
        int iScalar = f*numberOfData*_nodesPerCoordinateAxis + numberOfData*i + unknown;
        std::cout << std::setprecision(3) << fluctuation[iScalar] << ",";
        if (i==_nodesPerCoordinateAxis-1) {
          std::cout << std::endl;
        }
      }
    }
  }
  std::cout <<  "}" << std::endl;
  #endif
}

void exahype::solvers::ADERDGSolver::surfaceIntegral(
    const CellDescription&                                     cellDescription,
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed,
    const bool                                                 addToUpdate) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell && cellDescription.getRefinementEvent()==CellDescription::None,cellDescription.toString());
  if ( !tarch::la::equals(neighbourMergePerformed,static_cast<signed char>(true)) && !ProfileUpdate ) {
    logError("surfaceIntegral(...)","Riemann solve was not performed on all faces of cell= "<<cellDescription.toString());
    std::terminate();
  }

  double* output= static_cast<double*>(cellDescription.getSolution());
  if ( addToUpdate ) {
    output = static_cast<double*>(cellDescription.getUpdate());
  }

  #ifdef Asserts
  assertion1( tarch::la::equals(neighbourMergePerformed,static_cast<signed char>(true)) || ProfileUpdate,cellDescription.toString());
  if ( _checkForNaNs ) {
    for (int i=0; i<getUnknownsPerCell(); i++) { // update does not store parameters
      assertion3(tarch::la::equals(cellDescription.getTimeStepSize(),0.0)  || std::isfinite(output[i]),cellDescription.toString(),"surfaceIntegral",i);
    }
  }
  #endif

  // the actual operation
  const int dofsPerFace = getBndFluxSize();
  for (int direction=0; direction<DIMENSIONS; direction++) {
    for (int orientation=0; orientation<2; orientation++) {
      const int faceIndex=2*direction+orientation;
      if ( cellDescription.getFacewiseAugmentationStatus(faceIndex)<MaximumAugmentationStatus ) { // ignore Ancestors
        const double* const lFhbnd = static_cast<double*>(cellDescription.getFluctuation()) + dofsPerFace * faceIndex;
        faceIntegral(output,lFhbnd,direction,orientation,0/*implicit conversion*/,0,cellDescription.getSize(),
                     cellDescription.getTimeStepSize(),addToUpdate);
      }
    }
  }

  #ifdef Asserts
  if ( _checkForNaNs ) {
    for (int i=0; i<getUnknownsPerCell(); i++) { // update does not store parameters
      assertion3(std::isfinite(output[i]),cellDescription.toString(),"updateSolution(...)",i);
    }
  }
  #endif
}

void exahype::solvers::ADERDGSolver::addUpdateToSolution(CellDescription& cellDescription,const bool backupPreviousSolution) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell && cellDescription.getRefinementEvent()==CellDescription::None,cellDescription.toString());
  double* update = static_cast<double*>(cellDescription.getUpdate());

  if ( backupPreviousSolution ) {
    // perform the update
    swapSolutionAndPreviousSolution(cellDescription); // solution is overwritten with the current solution plus the update,
    // while current solution is remembered as the previous solution.
    double* solution         = static_cast<double*>(cellDescription.getSolution());
    double* previousSolution = static_cast<double*>(cellDescription.getPreviousSolution());
    addUpdateToSolution(solution,previousSolution,update,cellDescription.getTimeStepSize());
  } else {
    double* solution = static_cast<double*>(cellDescription.getSolution());
    addUpdateToSolution(solution,solution,update,cellDescription.getTimeStepSize());
  }
}

void exahype::solvers::ADERDGSolver::adjustSolutionAfterUpdate(CellDescription& cellDescription) {
  double* solution = static_cast<double*>(cellDescription.getSolution());
  adjustSolution(
      solution,
      cellDescription.getOffset()+0.5*cellDescription.getSize(),
      cellDescription.getSize(),
      cellDescription.getTimeStamp()+cellDescription.getTimeStepSize(),
      cellDescription.getTimeStepSize());

  // only for profiling
  if ( Solver::ProfileUpdate ) { swapSolutionAndPreviousSolution(cellDescription); }

  #ifdef Asserts
  if ( _checkForNaNs ) {
    for (int i=0; i<getUnknownsPerCell(); i++) { // update does not store parameters
      assertion3(std::isfinite(solution[i]),cellDescription.toString(),"updateSolution(...)",i);
    }
  }
  #endif
}

void exahype::solvers::ADERDGSolver::correction(
    CellDescription&                                           cellDescription,
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed,
    const bool                                                 backupPreviousSolution,
    const bool                                                 addSurfaceIntegralResultToUpdate) {
  assertion1(cellDescription.getType()==CellDescription::Type::Cell && cellDescription.getRefinementEvent()==CellDescription::None,cellDescription.toString())
  assertion1(std::isfinite(cellDescription.getTimeStamp()   ),cellDescription.toString());
  assertion1(std::isfinite(cellDescription.getTimeStepSize()),cellDescription.toString());
  #if !defined(SharedMemoryParallelisation) && !defined(Parallel) && defined(Asserts)
  static int counter = 0;
  static double timeStamp = 0;
  if ( !tarch::la::equals(timeStamp,_minTimeStamp,1e-9) ) {
    logInfo("mergeNeighboursData(...)","#updateSolution="<<counter);
    timeStamp = _minTimeStamp;
    counter=0;
  }
  counter++;
  #endif

  surfaceIntegral(cellDescription,neighbourMergePerformed,addSurfaceIntegralResultToUpdate);
  if ( addSurfaceIntegralResultToUpdate ) {
    addUpdateToSolution(cellDescription,backupPreviousSolution);
  }
  adjustSolutionAfterUpdate(cellDescription);

  // update grid flags
  updateCommunicationStatus(cellDescription);
  updateAugmentationStatus(cellDescription);
}

void exahype::solvers::ADERDGSolver::swapSolutionAndPreviousSolution(CellDescription& cellDescription) const {
  assertion(cellDescription.getType()==CellDescription::Type::Cell);
  assertion(cellDescription.getRefinementEvent()==CellDescription::None);

  // Simply swap the heap indices
  const int previousSolutionIndex = cellDescription.getPreviousSolutionIndex();
  void* previousSolution          = cellDescription.getPreviousSolution(); // pointer
  cellDescription.setPreviousSolutionIndex(cellDescription.getSolutionIndex());
  cellDescription.setPreviousSolution(cellDescription.getSolution());
  cellDescription.setSolutionIndex(previousSolutionIndex);
  cellDescription.setSolution(previousSolution);
}

void exahype::solvers::ADERDGSolver::prolongateObservablesMinAndMax(
    const CellDescription& cellDescription,
    const CellDescription& parentCellDescription) const {
  const int numberOfObservables = getDMPObservables();
  for (int faceIndex = 0; faceIndex < DIMENSIONS_TIMES_TWO; ++faceIndex) {
    if ( cellDescription.getFacewiseCommunicationStatus(faceIndex)==CellCommunicationStatus ) { // TODO(Dominic): If the grid changes dynamically during the time steps,
      // fine
      double* minFine = static_cast<double*>(cellDescription.getSolutionMin()) + numberOfObservables * faceIndex;
      double* maxFine = static_cast<double*>(cellDescription.getSolutionMax()) + numberOfObservables * faceIndex;
      // coarse
      const double* minCoarse = static_cast<double*>(parentCellDescription.getSolutionMin()) +  numberOfObservables * faceIndex;
      const double* maxCoarse = static_cast<double*>(parentCellDescription.getSolutionMax()) +  numberOfObservables * faceIndex;

      std::copy_n( minCoarse,numberOfObservables, minFine );
      std::copy_n( maxCoarse,numberOfObservables, maxFine );
    }
  }
}

void exahype::solvers::ADERDGSolver::prolongateFaceDataToDescendant(
    CellDescription& cellDescription,
    const CellDescription& parentCellDescription,
    const tarch::la::Vector<DIMENSIONS,int>& subcellIndex) {
  #ifdef USE_ITAC
  VT_begin(prolongateFaceDataToDescendantHandle);
  #endif

  assertion(parentCellDescription.getSolverNumber() == cellDescription.getSolverNumber());
  assertion(parentCellDescription.getType() == CellDescription::Type::Cell ||
            parentCellDescription.getType() == CellDescription::Type::Descendant);

  const int levelFine   = cellDescription.getLevel();
  const int levelCoarse = parentCellDescription.getLevel();
  assertion(levelCoarse < levelFine);

  double* update = static_cast<double*>(cellDescription.getUpdate());
  std::fill_n(update,getUpdateSize(),0.0);

  for (int faceIndex = 0; faceIndex < DIMENSIONS_TIMES_TWO; ++faceIndex) {
    const int direction = faceIndex/2;
    if ( cellDescription.getFacewiseCommunicationStatus(faceIndex)==CellCommunicationStatus ) { // TODO(Dominic): If the grid changes dynamically during the time steps,
      #ifdef Parallel
      assertion4( exahype::amr::faceIsOnBoundaryOfParent(faceIndex,subcellIndex,levelFine-levelCoarse),
            cellDescription.toString(),parentCellDescription.toString(),tarch::parallel::Node::getInstance().getRank(),
            tarch::parallel::NodePool::getInstance().getMasterRank() ); // necessary but not sufficient
      #else
      assertion2( exahype::amr::faceIsOnBoundaryOfParent(faceIndex,subcellIndex,levelFine-levelCoarse),
                  cellDescription.toString(),parentCellDescription.toString() ); // necessary but not sufficient
      #endif

      logDebug("prolongateFaceDataToDescendant(...)","cell=" << cellDescription.getOffset() <<
               ",level=" << cellDescription.getLevel() <<
               ",face=" << faceIndex <<
               ",subcellIndex" << subcellIndex.toString() <<
               " from " <<
               " parentCell="<<parentCellDescription.getOffset()<<
               " level="<<parentCellDescription.getLevel());

      // extrapolated predictor and flux interpolation
      // extrapolated predictor
      assertion1(DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex()),cellDescription.toString());
      assertion1(DataHeap::getInstance().isValidIndex(parentCellDescription.getExtrapolatedPredictorIndex()),parentCellDescription.toString());

      const int dataPerFace = getBndFaceSize();
      const int dofsPerFace = getBndFluxSize();

      // fine
      double* lQhbndFine = static_cast<double*>(cellDescription.getExtrapolatedPredictor()) + dataPerFace * faceIndex; // TODO(Dominic ): Pointer should be obtained only once
      double* lFhbndFine = static_cast<double*>(cellDescription.getFluctuation())           + dofsPerFace  * faceIndex ;
      // coarse
      const double* lQhbndCoarse = static_cast<double*>(parentCellDescription.getExtrapolatedPredictor()) + dataPerFace * faceIndex;
      const double* lFhbndCoarse = static_cast<double*>(parentCellDescription.getFluctuation()          ) + dofsPerFace  * faceIndex;

      faceUnknownsProlongation(lQhbndFine,lFhbndFine,lQhbndCoarse,lFhbndCoarse, levelCoarse, levelFine,
                               exahype::amr::getSubfaceIndex(subcellIndex,direction));
    }
  }

  if ( getDMPObservables()>0 ) {
    prolongateObservablesMinAndMax(cellDescription,parentCellDescription);
  }

  cellDescription.setHasCompletedLastStep(true);

  #ifdef USE_ITAC
  VT_end(prolongateFaceDataToDescendantHandle);
  #endif
}

void exahype::solvers::ADERDGSolver::prolongateFaceData(
    const int solverNumber,
    CellInfo& cellInfo,
    const bool isAtRemoteBoundary) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);

  if ( element != Solver::NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

    if (
        cellDescription.getType()==CellDescription::Type::Descendant &&
        cellDescription.getCommunicationStatus()>=MinimumCommunicationStatusForNeighbourCommunication &&
        isValidCellDescriptionIndex(cellDescription.getParentIndex()) // might be at master-worker boundary
    ) {
        Solver::SubcellPosition subcellPosition = amr::computeSubcellPositionOfDescendant<CellDescription,Heap>(cellDescription);
        CellDescription& parentCellDescription = getCellDescription(
            subcellPosition.parentCellDescriptionsIndex,subcellPosition.parentElement);
        assertion1(parentCellDescription.getType()==CellDescription::Type::Cell,parentCellDescription.toString());

        waitUntilCompletedLastStep<CellDescription>(parentCellDescription,true,false); // TODO(Dominic): We wait for skeleton jobs here. It might make sense to receiveDanglingMessages here too
        if (
            !SpawnProlongationAsBackgroundJob ||
            isAtRemoteBoundary
        ) {
          prolongateFaceDataToDescendant(cellDescription,parentCellDescription,subcellPosition.subcellIndex);
        } else {
          cellDescription.setHasCompletedLastStep(false); // done here in order to skip lookup of cell description in job constructor
          peano::datatraversal::TaskSet spawn( new ProlongationJob( *this, 
              cellDescription, parentCellDescription, subcellPosition.subcellIndex) );
        }
      }
      assertion2(
          cellDescription.getType()!=CellDescription::Type::Descendant ||
          isValidCellDescriptionIndex(cellDescription.getParentIndex()),
          cellDescription.toString(),
          tarch::parallel::Node::getInstance().getRank());
  }
}

void exahype::solvers::ADERDGSolver::restrictObservablesMinAndMax(
    const CellDescription& cellDescription,
    const CellDescription& parentCellDescription) const {
  const int numberOfObservables = getDMPObservables();
  for (int faceIndex=0; faceIndex<DIMENSIONS_TIMES_TWO; faceIndex++) {
    if ( cellDescription.getFacewiseCommunicationStatus(faceIndex)==CellCommunicationStatus ) {
      // fine
      const double* minFine = static_cast<double*>(cellDescription.getSolutionMin()) + numberOfObservables* faceIndex;
      const double* maxFine = static_cast<double*>(cellDescription.getSolutionMax()) + numberOfObservables* faceIndex;
      // coarse
      double* minCoarse = static_cast<double*>(parentCellDescription.getSolutionMin()) + numberOfObservables * faceIndex;
      double* maxCoarse = static_cast<double*>(parentCellDescription.getSolutionMax()) + numberOfObservables * faceIndex;

      tarch::multicore::Lock lock(RestrictionSemaphore);
      for (int i=0; i<numberOfObservables; i++) {
        *(minCoarse+i) = std::min( *(minFine+i), *(minCoarse+i) );
        *(maxCoarse+i) = std::max( *(maxFine+i), *(maxCoarse+i) );
      }
      lock.free();
    }
  }
}

void exahype::solvers::ADERDGSolver::restrictToTopMostParent(const CellDescription& cellDescription,const bool addToCoarseGridUpdate) {
  #ifdef USE_ITAC
  VT_begin(restrictToTopMostParentHandle);
  #endif

  // validate and obtain parent
  assertion1( cellDescription.getType()==CellDescription::Type::Descendant &&
              cellDescription.getCommunicationStatus()>=MinimumCommunicationStatusForNeighbourCommunication, cellDescription.toString() );
  assertion1( tryGetElement(cellDescription.getParentIndex(),cellDescription.getSolverNumber()) != NotFound, cellDescription.toString());
  exahype::solvers::Solver::SubcellPosition subcellPosition =
      exahype::amr::computeSubcellPositionOfDescendant<CellDescription,Heap>(cellDescription);
  assertion1(subcellPosition.parentElement!=exahype::solvers::Solver::NotFound,cellDescription.toString());

  CellDescription& parentCellDescription =
      getCellDescription(subcellPosition.parentCellDescriptionsIndex,subcellPosition.parentElement);

  assertion(parentCellDescription.getSolverNumber()==cellDescription.getSolverNumber());
  assertion1(cellDescription.getType()==CellDescription::Type::Descendant,cellDescription.toString());
  assertion1(parentCellDescription.getType()==CellDescription::Type::Cell,parentCellDescription.toString());


  // Perform the face integrals on fine grid cell
  double* updateFine = static_cast<double*>(cellDescription.getUpdate()); // TODO(Dominic): Can be temporary
  std::fill_n(updateFine,getUpdateSize(),0.0);

  const int levelDelta = cellDescription.getLevel() - parentCellDescription.getLevel();
  const tarch::la::Vector<DIMENSIONS,int> subcellIndex =
      exahype::amr::computeSubcellIndex(
          cellDescription.getOffset(),cellDescription.getSize(),
          parentCellDescription.getOffset()); // TODO(Dominic): Maybe, I get can get rid of some variables again

  // gather contributions
  double dt = parentCellDescription.getTimeStepSize();
  switch (getTimeStepping()) {
    case TimeStepping::Global:
    case TimeStepping::GlobalFixed:
      // do nothing
      break;
    default:
      logError("restrictToTopMostParent(...)","Time stepping scheme not supported.");
      break;
  }

  const int dofsPerFace = getBndFluxSize();
  for (int faceIndex=0; faceIndex<DIMENSIONS_TIMES_TWO; faceIndex++) {
    const int direction   = faceIndex / 2;
    const int orientation = faceIndex % 2;
    const tarch::la::Vector<DIMENSIONS-1,int> subfaceIndex =
      exahype::amr::getSubfaceIndex(subcellIndex,direction);
    if ( cellDescription.getFacewiseCommunicationStatus(faceIndex)==CellCommunicationStatus ) {
      assertion1(exahype::amr::faceIsOnBoundaryOfParent(faceIndex,subcellIndex,levelDelta),cellDescription.toString());
      assertion1(SpawnProlongationAsBackgroundJob || cellDescription.getNeighbourMergePerformed(faceIndex),cellDescription.toString());// necessary but not sufficient

      const double* const lFhbnd = static_cast<double*>(cellDescription.getFluctuation()) + dofsPerFace * faceIndex;
      faceIntegral(updateFine,lFhbnd,direction,orientation,subfaceIndex,levelDelta,cellDescription.getSize(),dt,addToCoarseGridUpdate);

      logDebug("restrictToTopMostParent(...)","cell=" << cellDescription.getOffset() <<
             ",level=" << cellDescription.getLevel() <<
             ",face=" << faceIndex <<
             ",subcellIndex" << subcellIndex.toString() <<
             " to " <<
             " parentCell="<<parentCellDescription.getOffset()<<
             " level="<<parentCellDescription.getLevel());
    }
  }

  // Add child contributions to parent
  if ( addToCoarseGridUpdate ) {
    double* updateCoarse = static_cast<double*>(parentCellDescription.getUpdate());
    tarch::multicore::Lock lock(RestrictionSemaphore);
    for (int i = 0; i < getUpdateSize(); ++i) { // TODO(Dominic): There must be a correction somewhere here or to the face integral if LTS is performed
      updateCoarse[i] += updateFine[i];
    }
    lock.free();
  } else {
    double* solutionCoarse = static_cast<double*>(parentCellDescription.getSolution());
    tarch::multicore::Lock lock(RestrictionSemaphore);
    addUpdateToSolution(solutionCoarse,solutionCoarse,updateFine,dt);
    lock.free();
  }

  if ( getDMPObservables()>0 ) {
    restrictObservablesMinAndMax(cellDescription,parentCellDescription);
  }

  #ifdef USE_ITAC
  VT_end(restrictToTopMostParentHandle);
  #endif
}

void exahype::solvers::ADERDGSolver::disableCheckForNaNs() {
  _checkForNaNs = false;
}

///////////////////////////////////
// NEIGHBOUR
///////////////////////////////////
void exahype::solvers::ADERDGSolver::mergeWithRefinementStatus(
    CellDescription& cellDescription,
    const int faceIndex,
    const int otherRefinementStatus) const {
  cellDescription.setFacewiseRefinementStatus( faceIndex, otherRefinementStatus );
}

void
exahype::solvers::ADERDGSolver::updateCommunicationStatus(
    exahype::solvers::ADERDGSolver::CellDescription& cellDescription) const {
  cellDescription.setCommunicationStatus(determineCommunicationStatus(cellDescription));
  assertion1(
      cellDescription.getType()!=CellDescription::Type::Cell ||
      cellDescription.getCommunicationStatus()==CellCommunicationStatus,
      cellDescription.toString());
}

int
exahype::solvers::ADERDGSolver::determineCommunicationStatus(
    exahype::solvers::ADERDGSolver::CellDescription& cellDescription) const {
  if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    return CellCommunicationStatus;
  } else {
    int max = 0;
    for (unsigned int i=0; i<DIMENSIONS_TIMES_TWO; i++) {
      if ( cellDescription.getNeighbourMergePerformed(i) ) {
        max = std::max( max, cellDescription.getFacewiseCommunicationStatus(i)-1 );
      }
    }
    return max;
  }
}

void exahype::solvers::ADERDGSolver::mergeWithCommunicationStatus(
    CellDescription& cellDescription,
    const int faceIndex,
    const int otherCommunicationStatus) const {
  assertion3(cellDescription.getCommunicationStatus()<=CellCommunicationStatus,
             cellDescription.getCommunicationStatus(),otherCommunicationStatus,
             cellDescription.getCommunicationStatus());
  cellDescription.setFacewiseCommunicationStatus( faceIndex, otherCommunicationStatus );
}

void
exahype::solvers::ADERDGSolver::updateAugmentationStatus(
    exahype::solvers::ADERDGSolver::CellDescription& cellDescription) const {
  cellDescription.setAugmentationStatus(determineAugmentationStatus(cellDescription));
  assertion1(
      cellDescription.getType()!=CellDescription::Type::Ancestor ||
      cellDescription.getAugmentationStatus()==MaximumAugmentationStatus,
      cellDescription.toString());
}

int
exahype::solvers::ADERDGSolver::determineAugmentationStatus(
    exahype::solvers::ADERDGSolver::CellDescription& cellDescription) const {
  if (cellDescription.getType()==CellDescription::Type::Ancestor) {
    return MaximumAugmentationStatus;
  } else {
    int max = 0;
    for (unsigned int i=0; i<DIMENSIONS_TIMES_TWO; i++) {
      if ( cellDescription.getNeighbourMergePerformed(i) ) {
        max = std::max( max, cellDescription.getFacewiseAugmentationStatus(i)-1 );
      }
    }
    return max;
  }
}

void exahype::solvers::ADERDGSolver::mergeWithAugmentationStatus(
    CellDescription& cellDescription,
    const int faceIndex,
    const int otherAugmentationStatus) const {
  assertion3(
      cellDescription.getAugmentationStatus()<=MaximumAugmentationStatus,
      cellDescription.getAugmentationStatus(),otherAugmentationStatus,
      cellDescription.getAugmentationStatus());
  cellDescription.setFacewiseAugmentationStatus( faceIndex, otherAugmentationStatus );
}

void exahype::solvers::ADERDGSolver::updateRefinementStatus(
    CellDescription&                                           cellDescription,
    const tarch::la::Vector<DIMENSIONS_TIMES_TWO,signed char>& neighbourMergePerformed) const {
  if ( cellDescription.getLevel()==getMaximumAdaptiveMeshLevel() ) {
    int max = Erase;
    if ( cellDescription.getRefinementFlag() )
      max = _refineOrKeepOnFineGrid;
    if ( cellDescription.getRefinementStatus() == getMinRefinementStatusForTroubledCell() )
      max = cellDescription.getRefinementStatus();

    for (unsigned int i=0; i<DIMENSIONS_TIMES_TWO; i++) {
      if ( neighbourMergePerformed[i] ) {
        max = std::max( max, cellDescription.getFacewiseRefinementStatus(i)-1 );
      }
    }
    cellDescription.setRefinementStatus(max);
  }
}

void exahype::solvers::ADERDGSolver::updateCoarseGridAncestorRefinementStatus(
    const CellDescription& fineGridCellDescription,
    CellDescription& coarseGridCellDescription) {
  // fine to coarse grid
  if ( coarseGridCellDescription.getType()==CellDescription::Type::Ancestor ) {
    tarch::multicore::Lock lock(CoarseGridSemaphore);
    if ( fineGridCellDescription.getType()==CellDescription::Type::Cell ) {
      coarseGridCellDescription.setRefinementStatus(
          std::max( coarseGridCellDescription.getRefinementStatus(), fineGridCellDescription.getRefinementStatus()) );
      // TODO(Dominic): Does that make sense?
      coarseGridCellDescription.setPreviousRefinementStatus(
          std::max( coarseGridCellDescription.getPreviousRefinementStatus(), fineGridCellDescription.getPreviousRefinementStatus()) );
    } else if ( fineGridCellDescription.getType()==CellDescription::Type::Ancestor ) {
      coarseGridCellDescription.setVetoErasingChildren(true);
    }
    lock.free();
  }
}

// TODO(Dominic): Check that we have rolled back in time as well
void exahype::solvers::ADERDGSolver::rollbackSolutionGlobally(const int solverNumber,CellInfo& cellInfo) const {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

    // 1. Rollback time step data
    rollbackToPreviousTimeStep(cellDescription);
    // 2. Rollback solution to previous one
    if (cellDescription.getType()==CellDescription::Type::Cell) {
      swapSolutionAndPreviousSolution(cellDescription);
    }

    // 3. Reset the previous refinement status on the finest mesh level
    if ( cellDescription.getLevel()==getMaximumAdaptiveMeshLevel() ) {
      cellDescription.setRefinementStatus(cellDescription.getPreviousRefinementStatus());
    } else {
      cellDescription.setRefinementStatus(Pending);
      cellDescription.setPreviousRefinementStatus(Pending);
    }
  }
}

void exahype::solvers::ADERDGSolver::mergeNeighboursMetadata(
    const int                                    solverNumber,
    Solver::CellInfo&                            cellInfo1,
    Solver::CellInfo&                            cellInfo2,
    const tarch::la::Vector<DIMENSIONS, int>&    pos1,
    const tarch::la::Vector<DIMENSIONS, int>&    pos2,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const tarch::la::Vector<DIMENSIONS, double>& h,
    const bool                                   checkThoroughly) const {
  const int element1 = cellInfo1.indexOfADERDGCellDescription(solverNumber);
  const int element2 = cellInfo2.indexOfADERDGCellDescription(solverNumber);
  if ( element1 != Solver::NotFound && element2 != Solver::NotFound ) {
    Solver::InterfaceInfo face(pos1,pos2);
    CellDescription& cellDescription1 = cellInfo1._ADERDGCellDescriptions[element1];
    CellDescription& cellDescription2 = cellInfo2._ADERDGCellDescriptions[element2];

    bool mergeMetadata = true;
    if ( checkThoroughly ) {
      const tarch::la::Vector<DIMENSIONS,double> baryCentreFrom1 =
          exahype::Cell::computeFaceBarycentre(cellDescription1.getOffset(),cellDescription1.getSize(),face._direction,face._orientation1);
      const tarch::la::Vector<DIMENSIONS,double> baryCentreFrom2 =
          exahype::Cell::computeFaceBarycentre(cellDescription2.getOffset(),cellDescription2.getSize(),face._direction,face._orientation2);
      const tarch::la::Vector<DIMENSIONS,double> baryCentreFromVertex =
          exahype::Vertex::computeFaceBarycentre(x,h,face._direction,pos2); // or pos 1
      mergeMetadata &= Vertex::equalUpToRelativeTolerance(baryCentreFrom1,baryCentreFromVertex) &&
                       Vertex::equalUpToRelativeTolerance(baryCentreFrom2,baryCentreFromVertex);
    }
    if ( mergeMetadata ) {
      mergeWithCommunicationStatus(cellDescription1,face._faceIndex1,cellDescription2.getCommunicationStatus());
      mergeWithAugmentationStatus(cellDescription1,face._faceIndex1,cellDescription2.getAugmentationStatus());
      mergeWithRefinementStatus(cellDescription1,face._faceIndex1,cellDescription2.getRefinementStatus());

      mergeWithCommunicationStatus(cellDescription2,face._faceIndex2,cellDescription1.getCommunicationStatus());
      mergeWithAugmentationStatus(cellDescription2,face._faceIndex2,cellDescription1.getAugmentationStatus());
      mergeWithRefinementStatus(cellDescription2,face._faceIndex2,cellDescription1.getRefinementStatus());

      cellDescription1.setNeighbourMergePerformed(face._faceIndex1,true); // here we only set, doesn't matter if operation is done twice.
      cellDescription2.setNeighbourMergePerformed(face._faceIndex2,true);
    }
  }
}

// merge compute data
void exahype::solvers::ADERDGSolver::mergeNeighboursData(
    const int                                 solverNumber,
    Solver::CellInfo&                         cellInfo1,
    Solver::CellInfo&                         cellInfo2,
    const tarch::la::Vector<DIMENSIONS, int>& pos1,
    const tarch::la::Vector<DIMENSIONS, int>& pos2) {
  #ifdef USE_ITAC
  VT_begin(mergeNeighboursHandle);
  #endif

  const int element1 = cellInfo1.indexOfADERDGCellDescription(solverNumber);
  const int element2 = cellInfo2.indexOfADERDGCellDescription(solverNumber);
  if ( element1 != Solver::NotFound && element2 != Solver::NotFound ) {
    Solver::InterfaceInfo face(pos1,pos2);
    CellDescription& cellDescription1 = cellInfo1._ADERDGCellDescriptions[element1];
    CellDescription& cellDescription2 = cellInfo2._ADERDGCellDescriptions[element2];

     if ( ADERDGSolver::communicateWithNeighbour(cellDescription1,face._faceIndex1) ) {
      assertion1( ADERDGSolver::communicateWithNeighbour(cellDescription2,face._faceIndex2),cellDescription2.toString() );

      #if !defined(SharedMemoryParallelisation) && !defined(Parallel) && defined(Asserts)
      static int counter = 0;
      static double timeStamp = 0;
      if ( !tarch::la::equals(timeStamp,_minTimeStamp,1e-9) ) {
        logInfo("mergeNeighboursData(...)","#riemanns="<<counter);
        timeStamp = _minTimeStamp;
        counter=0;
      }
      counter++;
      #endif

      waitUntilCompletedLastStep<CellDescription>(cellDescription1,false,false);  // must be done before any other operation on the patches
      waitUntilCompletedLastStep<CellDescription>(cellDescription2,false,false);

      if ( CompressionAccuracy > 0.0 ) {
        peano::datatraversal::TaskSet uncompression(
            [&] () -> bool {
          uncompress(cellDescription1);
          return false;
        },
        [&] () -> bool {
          uncompress(cellDescription1);
          return false;
        },
        peano::datatraversal::TaskSet::TaskType::Background,
        peano::datatraversal::TaskSet::TaskType::Background,
        true
        );
      }

      //
      // 1. Solve Riemann problem (merge data)
      //
      solveRiemannProblemAtInterface(cellDescription1,cellDescription2,face);
    }
  }

  #ifdef USE_ITAC
  VT_end(mergeNeighboursHandle);
  #endif
}

std::string exahype::solvers::ADERDGSolver::riemannDataToString(
    const double* const Q,const double* const F,std::string suffix) const {
  const int numberOfData = _numberOfVariables + _numberOfParameters;
  const int nodesPerFace = getBndFaceSize()/numberOfData;

  std::ostringstream stream;
  stream << std::endl;
  stream << "riemann states ("<<suffix<<"):" << std::endl;
  for (int i = 0; i<numberOfData; i++) {
    double minQ = std::numeric_limits<double>::infinity();
    double maxQ = -std::numeric_limits<double>::infinity();
    for(int n=0; n<nodesPerFace; ++n) {
      minQ = std::min(Q[n*numberOfData+i],minQ);
      maxQ = std::max(Q[n*numberOfData+i],maxQ);
    }
    stream << "Q"<<suffix<<"["<<i<<"] in ["<<std::setprecision(2)<<minQ<<","<<std::setprecision(2)<<maxQ<<"], ";
    stream << std::endl;
  }
  stream << "riemann fluxes ("<<suffix<<"):" << std::endl;
  for (int i = 0; i<_numberOfVariables; i++) {
    double minF = std::numeric_limits<double>::infinity();
    double maxF = -std::numeric_limits<double>::infinity();
    for(int n=0; n<nodesPerFace; ++n) {
      minF = std::min(F[n*_numberOfVariables+i],minF);
      maxF = std::max(F[n*_numberOfVariables+i],maxF);
    }
    stream << "F"<<suffix<<"["<<i<<"] in  ["<<std::setprecision(2)<<minF<<","<<std::setprecision(2)<<maxF<<"], ";
    stream << std::endl;
  }
  return stream.str();
}

void exahype::solvers::ADERDGSolver::solveRiemannProblemAtInterface(
    CellDescription& cellDescription1,
    CellDescription& cellDescription2,
    Solver::InterfaceInfo& face) {
  CellDescription& pLeft  =
      (face._orientation1==1) ? cellDescription1 : cellDescription2;
  CellDescription& pRight =
      (face._orientation1==1) ? cellDescription2 : cellDescription1;

  assertion2(DataHeap::getInstance().isValidIndex(pLeft.getExtrapolatedPredictorIndex()),pLeft.toString(),pRight.toString());
  assertion2(DataHeap::getInstance().isValidIndex(pLeft.getFluctuationIndex()),pLeft.toString(),pRight.toString());
  assertion2(DataHeap::getInstance().isValidIndex(pRight.getExtrapolatedPredictorIndex()),pLeft.toString(),pRight.toString());
  assertion2(DataHeap::getInstance().isValidIndex(pRight.getFluctuationIndex()),pLeft.toString(),pRight.toString());
  assertion1(pLeft.getRefinementEvent()==CellDescription::None,pLeft.toString());
  assertion1(pRight.getRefinementEvent()==CellDescription::None,pRight.toString());

  const int dataPerFace = getBndFaceSize();
  const int dofsPerFace  = getBndFluxSize();

  double* QL = static_cast<double*>(pLeft.getExtrapolatedPredictor()) +  dataPerFace * face._faceIndexLeft;
  double* FL = static_cast<double*>(pLeft.getFluctuation()          ) +  dofsPerFace  * face._faceIndexLeft;

  double* QR = static_cast<double*>(pRight.getExtrapolatedPredictor()) + dataPerFace * face._faceIndexRight;
  double* FR = static_cast<double*>(pRight.getFluctuation()          ) + dofsPerFace  * face._faceIndexRight;

  // todo Time step must be interpolated in local time stepping case
  // both time step sizes are the same, so the min has no effect here.
  assertion3(std::isfinite(pLeft.getTimeStepSize()),pLeft.toString(),face._faceIndexLeft,face._direction);
  assertion3(std::isfinite(pRight.getTimeStepSize()),pRight.toString(),face._faceIndexRight,face._direction);
  assertion3(pLeft.getTimeStepSize()>=0.0,pLeft.toString(),face._faceIndexLeft,face._direction);
  assertion3(pRight.getTimeStepSize()>=0.0,pRight.toString(),face._faceIndexRight,face._direction);

  #ifdef Asserts
  std::string inputDataL = riemannDataToString(QL,FL,"L");
  std::string inputDataR = riemannDataToString(QR,FR,"R");
  #endif

  std::tuple<double,double> timeStepData = getRiemannSolverTimeStepData(pLeft,pRight);
  riemannSolver(
      FL,FR,QL,QR,
      std::get<0>(timeStepData),
      std::get<1>(timeStepData),
      pLeft.getSize(),
      face._direction, false, -1); // TODO(Dominic): Merge Riemann solver directly with the face integral and push the result on update
                                   // does not make sense to overwrite the flux when performing local time stepping; coarse grid flux must be constant, or not?

  #ifdef Asserts
  if ( _checkForNaNs ) { // assumes the solver is used as part of the hybrid solver
    std::string outputInformationL = riemannDataToString(QL,FL,"L");
    std::string outputInformationR = riemannDataToString(QR,FR,"R");

    const int nodesPerFace = dofsPerFace/_numberOfVariables;
    for (int i = 0; i<_numberOfVariables; i++) {
      for(int n=0; n<nodesPerFace; ++n) {
        assertion10(tarch::la::equals(pLeft.getTimeStepSize(),0.0) || (std::isfinite(FL[i]) && std::isfinite(FR[i])),
                   pLeft.toString(),pRight.toString(),face._direction,i,FL[i],FR[i],
                   inputDataL,inputDataR,outputInformationL,outputInformationR);
      }
    }
  }
  #endif
}

void exahype::solvers::ADERDGSolver::mergeWithBoundaryData(
    const int                                 solverNumber,
    Solver::CellInfo&                         cellInfo,
    const tarch::la::Vector<DIMENSIONS, int>& posCell,
    const tarch::la::Vector<DIMENSIONS, int>& posBoundary) {
  #ifdef USE_ITAC
  VT_begin(mergeNeighboursHandle);
  #endif

  assertion2(tarch::la::countEqualEntries(posCell,posBoundary)==(DIMENSIONS-1),posCell.toString(),posBoundary.toString());
  Solver::BoundaryFaceInfo face(posCell,posBoundary);

  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != Solver::NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

    if ( cellDescription.getType()==CellDescription::Type::Cell ) {
      waitUntilCompletedLastStep<CellDescription>(cellDescription,false,false); // must be done before any other operation on the patch

//      synchroniseTimeStepping(cellDescription); // TODO(Dominic): Not used anymore because of data races

      uncompress(cellDescription);

      applyBoundaryConditions(cellDescription,face);

      mergeWithAugmentationStatus(cellDescription,face._faceIndex,BoundaryStatus);
      mergeWithCommunicationStatus(cellDescription,face._faceIndex,BoundaryStatus);
      mergeWithRefinementStatus(cellDescription,face._faceIndex,BoundaryStatus);
    }
  }

  #ifdef USE_ITAC
  VT_end(mergeNeighboursHandle);
  #endif
}

void exahype::solvers::ADERDGSolver::applyBoundaryConditions(CellDescription& p,Solver::BoundaryFaceInfo& face) {
  assertion1(p.getType()==CellDescription::Type::Cell,p.toString());
  assertion1(p.getRefinementEvent()==CellDescription::None,p.toString());
  assertion1(DataHeap::getInstance().isValidIndex(p.getExtrapolatedPredictorIndex()),p.toString());
  assertion1(DataHeap::getInstance().isValidIndex(p.getFluctuationIndex()),p.toString());
  #if !defined(SharedMemoryParallelisation) && !defined(Parallel) && defined(Asserts)
  static int counter = 0;
  static double timeStamp = 0;
  if ( !tarch::la::equals(timeStamp,_minTimeStamp,1e-9) ) {
    logInfo("applyBoundaryConditions(...)","#boundaryConditions="<<counter);
    timeStamp = _minTimeStamp;
    counter=0;
  }
  counter++;
  #endif

  const int dataPerFace = getBndFaceSize();
  const int dofsPerFace = getBndFluxSize();
  const int gradientDataPerFace = _numberOfVariables * power(_nodesPerCoordinateAxis, DIMENSIONS - 1) * DIMENSIONS;
  double* QIn = static_cast<double*>(p.getExtrapolatedPredictor()) +  dataPerFace * face._faceIndex;
  double* gradQIn = static_cast<double*>(p.getExtrapolatedPredictorGradient()) +  gradientDataPerFace * face._faceIndex;
  double* FIn = static_cast<double*>(p.getFluctuation())           +  dofsPerFace * face._faceIndex;
  const double* luh = static_cast<double*>(p.getSolution());

  #ifdef Asserts
  std::string inputData = riemannDataToString(QIn,FIn,"In");
  #endif

  #ifdef Asserts
  assertion4(std::isfinite(p.getTimeStamp()),p.toString(),face._faceIndex,face._direction,p.getTimeStamp());
  assertion4(std::isfinite(p.getTimeStepSize()),p.toString(),face._faceIndex,face._direction,p.getTimeStepSize());
  assertion4(p.getTimeStepSize()>=0.0, p.toString(),face._faceIndex,face._direction,p.getTimeStepSize());
  if ( _checkForNaNs ) {
    for(int i=0; i<dofsPerFace; ++i) {
      assertion6(tarch::la::equals(p.getTimeStepSize(),0.0) || std::isfinite(FIn[i]),p.toString(),face._faceIndex,face._direction,i,FIn[i],inputData);
    }
  }
  #endif

  // TODO(Dominic): Hand in space-time volume data. Time integrate it afterwards

  std::tuple<double,double> timeStepData = getRiemannSolverTimeStepData(p,p);
  boundaryConditions(
      FIn,QIn, gradQIn,
      luh,
      p.getOffset() + 0.5*p.getSize(),
      p.getSize(),
      std::get<0>(timeStepData),
      std::get<1>(timeStepData),
      face._direction,face._orientation);

  #ifdef Asserts
  assertion4(std::isfinite(p.getTimeStamp()),p.toString(),face._faceIndex,face._direction,p.getTimeStamp());
  assertion4(std::isfinite(p.getTimeStepSize()),p.toString(),face._faceIndex,face._direction,p.getTimeStepSize());
  assertion4(p.getTimeStepSize()>=0.0, p.toString(),face._faceIndex,face._direction,p.getTimeStepSize());
  if ( _checkForNaNs ) {
    for(int i=0; i<dofsPerFace; ++i) {
      assertion6(tarch::la::equals(p.getTimeStepSize(),0.0) || std::isfinite(FIn[i]),p.toString(),face._faceIndex,face._direction,i,FIn[i],inputData);
    }
  }
  #endif
}

#ifdef Parallel
const int exahype::solvers::ADERDGSolver::DataMessagesPerNeighbourCommunication    = 2;
const int exahype::solvers::ADERDGSolver::DataMessagesPerForkOrJoinCommunication   = 2;
const int exahype::solvers::ADERDGSolver::DataMessagesPerMasterWorkerCommunication = 2;

/**
 * After the forking the master's cell descriptions
 * are not accessed by enterCell(...) on the master
 * anymore. However we still use them as buffer for saving data.
 */
bool exahype::solvers::ADERDGSolver::sendCellDescriptions(
    const int                                     toRank,
    const int                                     cellDescriptionsIndex,
    const bool                                    fromWorkerSide,
    const peano::heap::MessageType&               messageType,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) {
  if ( isValidCellDescriptionIndex(cellDescriptionsIndex) ) {
    logDebug("sendCellDescriptions(...)","send "<< Heap::getInstance().getData(cellDescriptionsIndex).size()<<
        " cell descriptions to rank "<<toRank<<" (x="<< x.toString() << ",level="<< level << ")");
    bool oneSolverRequiresVerticalCommunication = false;
    for (auto& cellDescription : Heap::getInstance().getData(cellDescriptionsIndex)) {
      // wait for background jobs to complete
      if ( !cellDescription.getHasCompletedLastStep() ) {
        peano::datatraversal::TaskSet::startToProcessBackgroundJobs();
      }
      int numberOfBackgroundJobsToProcess = 1;
      while ( !cellDescription.getHasCompletedLastStep() ) {
        tarch::multicore::jobs::processBackgroundJobs(numberOfBackgroundJobsToProcess);
        numberOfBackgroundJobsToProcess++;
      }
      oneSolverRequiresVerticalCommunication &=
          cellDescription.getType()==CellDescription::Type::Descendant && cellDescription.getHasVirtualChildren();
    }
    Heap::getInstance().sendData(cellDescriptionsIndex,toRank,x,level,messageType);
    return oneSolverRequiresVerticalCommunication;
  }
  else {
    logDebug("sendCellDescriptions(...)","send "
        " empty cell descriptions to rank "<<toRank<<" (x="<< x.toString() << ",level="<< level << ")");
    
    sendEmptyCellDescriptions(toRank,messageType,x,level);
    return false;
  }
}

void exahype::solvers::ADERDGSolver::sendEmptyCellDescriptions(
    const int                                     toRank,
    const peano::heap::MessageType&               messageType,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) {
  logDebug("sendEmptyCellDescriptions(...)","send empty message to " <<
          "rank "<<toRank <<
          " at (center="<< x.toString() <<
          ",level="<< level << ")");

  Heap::HeapEntries emptyMessage(0);
  Heap::getInstance().sendData(emptyMessage,
      toRank,x,level,messageType);
}

void exahype::solvers::ADERDGSolver::receiveCellDescriptions(
    const int                                    fromRank,
    exahype::Cell&                               localCell,
    const peano::heap::MessageType&              messageType,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) {
  Heap::getInstance().receiveData(
      localCell.getCellDescriptionsIndex(),fromRank,x,level,messageType);

  logDebug("mergeCellDescriptionsWithRemoteData(...)","received " <<
          Heap::getInstance().getData(localCell.getCellDescriptionsIndex()).size() <<
          " cell descriptions for cell (centre="<< x.toString() << ", level="<< level << ")");

  for (auto& cellDescription : Heap::getInstance().getData(localCell.getCellDescriptionsIndex())) {
    resetIndicesAndFlagsOfReceivedCellDescription(
        cellDescription,multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex);
  }
}

void exahype::solvers::ADERDGSolver::resetIndicesAndFlagsOfReceivedCellDescription(
    CellDescription& cellDescription,const int parentIndex) {
  cellDescription.setParentIndex(parentIndex);

  // Default field data indices
  cellDescription.setSolutionIndex(-1);
  cellDescription.setSolution(nullptr);
  cellDescription.setPreviousSolutionIndex(-1);
  cellDescription.setPreviousSolution(nullptr);
  cellDescription.setUpdateIndex(-1);
  cellDescription.setUpdate(nullptr);
  cellDescription.setExtrapolatedPredictorIndex(-1);
  cellDescription.setExtrapolatedPredictor(nullptr);
  cellDescription.setFluctuationIndex(-1);
  cellDescription.setFluctuation(nullptr);

  // Limiter meta data (oscillations identificator)
  cellDescription.setSolutionMinIndex(-1);
  cellDescription.setSolutionMin(nullptr);
  cellDescription.setSolutionMaxIndex(-1);
  cellDescription.setSolutionMax(nullptr);

  // compression
  cellDescription.setCompressionState(CellDescription::CompressionState::Uncompressed);

  cellDescription.setExtrapolatedPredictorCompressedIndex(-1);
  cellDescription.setExtrapolatedPredictorCompressed(nullptr);
  cellDescription.setFluctuationCompressedIndex(-1);
  cellDescription.setFluctuationCompressed(nullptr);
  cellDescription.setSolutionCompressedIndex(-1);
  cellDescription.setSolutionCompressed(nullptr);
  cellDescription.setPreviousSolutionCompressedIndex(-1);
  cellDescription.setPreviousSolutionCompressed(nullptr);
  cellDescription.setUpdateCompressedIndex(-1);
  cellDescription.setUpdateCompressed(nullptr);

  cellDescription.setSolutionAveragesIndex(-1);
  cellDescription.setSolutionAverages(nullptr);
  cellDescription.setSolutionAveragesIndex(-1);
  cellDescription.setSolutionAverages(nullptr);
  cellDescription.setUpdateAveragesIndex(-1);
  cellDescription.setUpdateAverages(nullptr);
  cellDescription.setExtrapolatedPredictorAveragesIndex(-1);
  cellDescription.setExtrapolatedPredictorAverages(nullptr);
  cellDescription.setFluctuationAveragesIndex(-1);
  cellDescription.setFluctuationAverages(nullptr);
  cellDescription.setBytesPerDoFInPreviousSolution(-1);
  cellDescription.setBytesPerDoFInSolution(-1);
  cellDescription.setBytesPerDoFInUpdate(-1);
  cellDescription.setBytesPerDoFInExtrapolatedPredictor(-1);
  cellDescription.setBytesPerDoFInFluctuation(-1);

  // reset the facewise flags
  cellDescription.setFacewiseAugmentationStatus(0);
  cellDescription.setFacewiseCommunicationStatus(0);

  cellDescription.setFacewiseRefinementStatus(Pending);

  // limiter flagging
  cellDescription.setIterationsToCureTroubledCell(-1);

  #ifdef Asserts
  cellDescription.setCreation(CellDescription::Creation::ReceivedDueToForkOrJoin);
  #endif

  // background jobs
  cellDescription.setHasCompletedLastStep(true);
}

void exahype::solvers::ADERDGSolver::ensureOnlyNecessaryMemoryIsAllocated(CellDescription& cellDescription) {
  auto* solver = RegisteredSolvers[cellDescription.getSolverNumber()];
  switch (solver->getType()) {
  case exahype::solvers::Solver::Type::ADERDG:
    static_cast<ADERDGSolver*>(solver)->ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
    static_cast<ADERDGSolver*>(solver)->ensureNecessaryMemoryIsAllocated(cellDescription);
    break;
  case exahype::solvers::Solver::Type::LimitingADERDG:
    static_cast<LimitingADERDGSolver*>(solver)->
    getSolver()->ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
    static_cast<LimitingADERDGSolver*>(solver)->
        getSolver()->ensureNecessaryMemoryIsAllocated(cellDescription);
    break;
  case exahype::solvers::Solver::Type::FiniteVolumes:
    assertionMsg(false,"Solver type not supported!");
    break;
  }
}

/**
 * Drop cell descriptions received from \p fromRank.
 */
void exahype::solvers::ADERDGSolver::dropCellDescriptions(
    const int                                     fromRank,
    const peano::heap::MessageType&               messageType,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) {
  Heap::getInstance().receiveData(fromRank,x,level,messageType);
}

////////////////////////////////////
// MASTER <=> WORKER
////////////////////////////////////
void
exahype::solvers::ADERDGSolver::appendMasterWorkerCommunicationMetadata(
    MetadataHeap::HeapEntries& metadata,
    const int cellDescriptionsIndex,
    const int solverNumber) const {
  const int element = tryGetElement(cellDescriptionsIndex,solverNumber);

  if (element!=exahype::solvers::Solver::NotFound)  {
    CellDescription& cellDescription =
        getCellDescription(cellDescriptionsIndex,element);
    metadata.push_back(static_cast<int>(cellDescription.getType()));
    metadata.push_back(cellDescription.getAugmentationStatus()); // TODO(Dominic): Add to docu: Might be merged multiple times!
    metadata.push_back(cellDescription.getCommunicationStatus());
    metadata.push_back(cellDescription.getRefinementStatus());
    metadata.push_back( (cellDescription.getHasVirtualChildren()) ? 1 : 0 );
  } else {
    for (int i = 0; i < exahype::MasterWorkerCommunicationMetadataPerSolver; ++i) {
      metadata.push_back(exahype::InvalidMetadataEntry); // implicit conversion
    }
  }
}

void exahype::solvers::ADERDGSolver::deduceChildCellErasingEvents(CellDescription& cellDescription) const {
  const int coarseGridElement = tryGetElement(cellDescription.getParentIndex(),cellDescription.getSolverNumber());
  if ( coarseGridElement!=exahype::solvers::Solver::NotFound ) {
    CellDescription& coarseGridCellDescription =
        getCellDescription(cellDescription.getParentIndex(),coarseGridElement);

    tarch::multicore::Lock lock(CoarseGridSemaphore);

    switch (coarseGridCellDescription.getRefinementEvent()) {
    case CellDescription::RefinementEvent::ErasingChildrenRequested: {  // TODO(Dominic): Fix this part too
      assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Ancestor,
          coarseGridCellDescription.toString());
      cellDescription.setRefinementEvent(CellDescription::RefinementEvent::ErasingRequested);
    } break;
    case CellDescription::RefinementEvent::ErasingChildren: {  // TODO(Dominic): Fix this part too
      assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Ancestor,
          coarseGridCellDescription.toString());
      cellDescription.setRefinementEvent(CellDescription::RefinementEvent::Erasing);
    } break;
    //
    case CellDescription::RefinementEvent::ChangeChildrenToVirtualChildrenRequested: {
      assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Ancestor,coarseGridCellDescription.toString());
      cellDescription.setRefinementEvent(CellDescription::RefinementEvent::ChangeToVirtualCellRequested);
    } break;
    case CellDescription::RefinementEvent::ChangeChildrenToVirtualChildren: {
      assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Ancestor,coarseGridCellDescription.toString());
      cellDescription.setRefinementEvent(CellDescription::RefinementEvent::ChangeToVirtualCell);
    } break;
    //
    case CellDescription::RefinementEvent::ErasingVirtualChildren: {
      assertion1(coarseGridCellDescription.getType()==CellDescription::Type::Cell ||
          coarseGridCellDescription.getType()==CellDescription::Type::Descendant,coarseGridCellDescription.toString());
      cellDescription.setRefinementEvent(CellDescription::RefinementEvent::ErasingVirtualCell);
    }  break;
    default:
      break;
    }
    lock.free();
  }
}

void exahype::solvers::ADERDGSolver::progressMeshRefinementInPrepareSendToWorker(
    const int workerRank,
    exahype::Cell& fineGridCell,
    exahype::Vertex* const fineGridVertices,
    const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
    exahype::Cell& coarseGridCell,
    const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
    const int solverNumber) {
  // coarse grid based operations
  const int coarseGridCellDescriptionsIndex = coarseGridCell.getCellDescriptionsIndex();
  const int coarseGridCellElement = tryGetElement(coarseGridCellDescriptionsIndex,solverNumber);
  if (coarseGridCellElement!=exahype::solvers::Solver::NotFound) {
    CellDescription& coarseGridCellDescription = getCellDescription(
        coarseGridCell.getCellDescriptionsIndex(),coarseGridCellElement);

    addNewDescendantIfVirtualRefiningRequested(
        fineGridCell,fineGridVertices,fineGridVerticesEnumerator,
        coarseGridCellDescription,coarseGridCell.getCellDescriptionsIndex());

    bool addedNewCell =
        addNewCellIfRefinementRequested(
            fineGridCell,fineGridVertices,fineGridVerticesEnumerator,
            coarseGridCellDescription,coarseGridCell.getCellDescriptionsIndex());
    
    const int cellDescriptionsIndex = fineGridCell.getCellDescriptionsIndex();
    const int element = tryGetElement(cellDescriptionsIndex,solverNumber);
    
    if ( element!=exahype::solvers::Solver::NotFound ) {
      CellDescription& fineGridCellDescription = getCellDescription(cellDescriptionsIndex,element);
      if ( 
        fineGridCellDescription.getType()==CellDescription::Type::Descendant &&
        fineGridCellDescription.getHasVirtualChildren()
      ) {
        Solver::SubcellPosition subcellPosition =  amr::computeSubcellPositionOfDescendant<CellDescription,Heap>(fineGridCellDescription);
        CellDescription& topMostParentCellDescription = 
            getCellDescription(subcellPosition.parentCellDescriptionsIndex,subcellPosition.parentElement);
        if ( topMostParentCellDescription.getType()==CellDescription::Type::Cell ) {
           logDebug( "progressMeshRefinementInPrepareSendToWorker(...)"," try to refine parent " << topMostParentCellDescription.toString());
           topMostParentCellDescription.setRefinementStatus(_refineOrKeepOnFineGrid);
        }
      }
    }

    if ( addedNewCell ) {
      CellDescription& fineGridCellDescription = getCellDescription(cellDescriptionsIndex,element);
      prolongateVolumeData(fineGridCellDescription,getMeshUpdateEvent()==MeshUpdateEvent::InitialRefinementRequested);
      assertion1( fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Prolongating,
        fineGridCellDescription.toString());
    } 
  }
}

void exahype::solvers::ADERDGSolver::sendDataToWorkerIfProlongating(
    const int                                     workerRank,
    const int                                     cellDescriptionsIndex,
    const int                                     element,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) const {
  CellDescription& fineGridCellDescription = getCellDescription(cellDescriptionsIndex,element);

  // send out the data
  if ( fineGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Prolongating ) {
    logDebug( "sendDataToWorkerIfProlongating(...)","send prolongated solution to rank "<<workerRank<< " at x="<<x.toString()<< ",level="<<level << " cell="<<fineGridCellDescription.toString());

    sendDataToWorkerOrMasterDueToForkOrJoin(workerRank,cellDescriptionsIndex,element,
        peano::heap::MessageType::MasterWorkerCommunication,x,level);
  }
}

void exahype::solvers::ADERDGSolver::receiveDataFromMasterIfProlongating(
  const int masterRank,
	const int receivedCellDescriptionsIndex,
  const int receivedElement,
  const tarch::la::Vector<DIMENSIONS,double>& x,
  const int level) const {
  CellDescription& receivedCellDescription = getCellDescription(receivedCellDescriptionsIndex,receivedElement);

  if ( receivedCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Prolongating ) {
    logDebug( "receiveDataFromMasterIfProlongating(...)","receiving prolongated solution from rank "<<masterRank<< " at x="<<x.toString()<< ",level="<<level);

    mergeWithWorkerOrMasterDataDueToForkOrJoin(
      masterRank,receivedCellDescriptionsIndex,receivedElement,
      peano::heap::MessageType::MasterWorkerCommunication,x,level);
  }
}

bool exahype::solvers::ADERDGSolver::progressMeshRefinementInMergeWithWorker(
    const int localCellDescriptionsIndex,
    const int receivedCellDescriptionsIndex, const int receivedElement) {
  auto& receivedCellDescriptions = getCellDescriptions(receivedCellDescriptionsIndex);
  assertion1( isValidCellDescriptionIndex(localCellDescriptionsIndex), localCellDescriptionsIndex );
  auto& localCellDescriptions = getCellDescriptions(localCellDescriptionsIndex);

  int localElement = NotFound;
  for (unsigned int element = 0; element < localCellDescriptions.size(); ++element) {
    if ( localCellDescriptions[element].getSolverNumber()==receivedCellDescriptions[receivedElement].getSolverNumber() ) {
      localElement = element;
    }
    element++;
  }
  if ( localElement==NotFound ) { // We have already received data and allocated all memory in this case
    CellDescription& receivedCellDescription = receivedCellDescriptions[receivedElement];
    localCellDescriptions.push_back(receivedCellDescription);
    assertion1(receivedCellDescription.getType()==CellDescription::Type::Descendant,receivedCellDescription.toString());
    receivedCellDescriptions.erase(receivedCellDescriptions.begin()+receivedElement);
    return false;
  } else {
    CellDescription& receivedCellDescription = receivedCellDescriptions[receivedElement];
    if ( receivedCellDescriptions[receivedElement].getRefinementEvent()==CellDescription::RefinementEvent::Prolongating ) {
      CellDescription& localCellDescription = localCellDescriptions[localElement];

      logDebug( "progressMeshRefinementInMergeWithWorker(...)","merging prolongated solution");

      assertion( localCellDescription.getType()==CellDescription::Type::Cell ||
          localCellDescription.getType()==CellDescription::Type::Descendant);
      assertion(receivedCellDescription.getType()==CellDescription::Type::Cell);
      assertion(DataHeap::getInstance().isValidIndex(receivedCellDescription.getSolutionIndex()));
      assertion(DataHeap::getInstance().isValidIndex(receivedCellDescription.getPreviousSolutionIndex()));

      // we know we have received data in this case
      localCellDescription.setType(CellDescription::Type::Cell);
      localCellDescription.setRefinementEvent(CellDescription::RefinementEvent::Prolongating);
      localCellDescription.setPreviousRefinementStatus(Pending);
      localCellDescription.setCommunicationStatus(CellCommunicationStatus);
      localCellDescription.setFacewiseCommunicationStatus(0); // implicit conversion

      ensureNecessaryMemoryIsAllocated(localCellDescription); // copy indices
      localCellDescription.setSolution(receivedCellDescription.getSolution());
      localCellDescription.setPreviousSolution(receivedCellDescription.getPreviousSolution());
      receivedCellDescription.setSolutionIndex(-1);
      receivedCellDescription.setPreviousSolutionIndex(-1);
      receivedCellDescription.setSolution(nullptr);
      receivedCellDescription.setPreviousSolution(nullptr);

      // adjust solution
      localCellDescription.setRefinementEvent(CellDescription::RefinementEvent::None);
      return true;
    } else {
      return false;
    }
  }
}

void exahype::solvers::ADERDGSolver::progressMeshRefinementInPrepareSendToMaster(
    const int masterRank,
    const int cellDescriptionsIndex, const int element,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int level) const {
  CellDescription& cellDescription = getCellDescription(cellDescriptionsIndex,element);

  // send out data
  if (
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingRequested ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeToVirtualCellRequested
  ) {
    sendDataToWorkerOrMasterDueToForkOrJoin(masterRank,cellDescriptionsIndex,element,
        peano::heap::MessageType::MasterWorkerCommunication,x,level); // assumes blocking/copy
  }

  // erase or change type of cell descriptions
  if (
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Erasing ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingVirtualCell
  ) {
    cellDescription.setType(CellDescription::Type::Erased);
    ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
    getCellDescriptions(cellDescriptionsIndex).erase(
        getCellDescriptions(cellDescriptionsIndex).begin()+element);
  } else if ( cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeToVirtualCell ) {
    assertion(cellDescription.getType()==CellDescription::Type::Cell)
            cellDescription.setType(CellDescription::Type::Descendant);
    ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
    ensureNecessaryMemoryIsAllocated(cellDescription);
  }
}

bool exahype::solvers::ADERDGSolver::progressMeshRefinementInMergeWithMaster(
    const int worker,
    const int localCellDescriptionsIndex,
    const int localElement,
    const int coarseGridCellDescriptionsIndex,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level,
    const bool                                   stillInRefiningMode) {
  CellDescription& cellDescription = getCellDescription(localCellDescriptionsIndex,localElement);
  ensureFineGridCoarseGridConsistency(cellDescription,coarseGridCellDescriptionsIndex);
  #ifdef Asserts
  cellDescription.setCreation(CellDescription::Creation::ReceivedFromWorker);
  #endif

  const int coarseGridElement = tryGetElement(
      cellDescription.getParentIndex(),cellDescription.getSolverNumber());
  if ( // receive restricted data
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingRequested ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeToVirtualCellRequested
  ) {
    assertion1(coarseGridElement!=exahype::solvers::Solver::NotFound,cellDescription.toString());
    mergeWithWorkerOrMasterDataDueToForkOrJoin(worker,localCellDescriptionsIndex,localElement,
        peano::heap::MessageType::MasterWorkerCommunication,x,level); // assumes blocking/copy

    // use the received data
    CellDescription& coarseGridCellDescription =
        getCellDescription(cellDescription.getParentIndex(),coarseGridElement); // TODO(Dominic): Have helper function for that

    restrictVolumeDataIfErasingRequested(cellDescription,coarseGridCellDescription);
  }

  // work with the data
  if (
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::Erasing ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeToVirtualCell ||
      cellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingVirtualCell
  ) {
    CellDescription& coarseGridCellDescription =
        getCellDescription(cellDescription.getParentIndex(),coarseGridElement); // TODO(Dominic): Have helper function for that
    assertion2( coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingChildren ||
        coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ChangeChildrenToVirtualChildren ||
        coarseGridCellDescription.getRefinementEvent()==CellDescription::RefinementEvent::ErasingVirtualChildren,
        cellDescription.toString(),coarseGridCellDescription.toString());

    eraseCellDescriptionIfNecessary(localCellDescriptionsIndex,localElement,coarseGridCellDescription);
  }

  // potentially veto
  if ( coarseGridElement != exahype::solvers::Solver::NotFound ) {
    CellDescription& coarseGridCellDescription = getCellDescription(
        cellDescription.getParentIndex(),coarseGridElement);
    updateCoarseGridAncestorRefinementStatus(cellDescription,coarseGridCellDescription);

    if ( 
      coarseGridCellDescription.getType()==CellDescription::Type::Ancestor && 
      cellDescription.getHasVirtualChildren() 
    ) {            // no lock required; serial context
       coarseGridCellDescription.setVetoErasingChildren(true);  // we should not overwrite a veto with false. Hence the if-clause
    }
  }

  progressCollectiveRefinementOperationsInLeaveCell(cellDescription,stillInRefiningMode);
  // ignore return value as responsibiliy is still on fine grid.

  // check if any cell description requires vertical communication
  bool solverRequiresVerticalCommunication = false;

  // block erasing request of coarse grid cell description if deployed cell
  // does not want to be erased
  decideOnRefinement(cellDescription,stillInRefiningMode);

  return solverRequiresVerticalCommunication;
}

///////////////////////////////////
// FORK OR JOIN
///////////////////////////////////

void exahype::solvers::ADERDGSolver::sendDataToWorkerOrMasterDueToForkOrJoin(
    const int                                     toRank,
    const int                                     cellDescriptionsIndex,
    const int                                     element,
    const peano::heap::MessageType&               messageType,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) const {
  assertion1(Heap::getInstance().isValidIndex(cellDescriptionsIndex),cellDescriptionsIndex);
  assertion1(element>=0,element);
  assertion2(static_cast<unsigned int>(element)<Heap::getInstance().getData(cellDescriptionsIndex).size(),
             element,Heap::getInstance().getData(cellDescriptionsIndex).size());
  CellDescription& cellDescription = getCellDescription(cellDescriptionsIndex,element);
  
  #ifdef Asserts
  const tarch::la::Vector<DIMENSIONS,double> center = cellDescription.getOffset()+0.5*cellDescription.getSize();
  #endif
  assertion5(Vertex::equalUpToRelativeTolerance(x,center),x,center,level,cellDescription.getLevel(),tarch::parallel::Node::getInstance().getRank());
  assertion2(cellDescription.getLevel()==level,cellDescription.getLevel(),level);

  if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    logDebug("sendDataToWorkerOrMasterDueToForkOrJoin(...)",""
            "solution of solver " << cellDescription.getSolverNumber() << " sent to rank "<<toRank<<
                 ", cell: "<< x << ", level: " << level);

    assertion2(DataHeap::getInstance().isValidIndex(cellDescription.getSolutionIndex()),
        cellDescriptionsIndex,cellDescription.toString());
    assertion2(DataHeap::getInstance().isValidIndex(cellDescription.getPreviousSolutionIndex()),
            cellDescriptionsIndex,cellDescription.toString());
    DataHeap::getInstance().sendData(
        static_cast<double*>(cellDescription.getSolution()),
        getDataPerCell(), toRank, x, level,messageType);
    DataHeap::getInstance().sendData(
        static_cast<double*>(cellDescription.getPreviousSolution()),
        getDataPerCell(), toRank, x, level,messageType);
  }
}

void exahype::solvers::ADERDGSolver::mergeWithWorkerOrMasterDataDueToForkOrJoin(
    const int                                     fromRank,
    const int                                     cellDescriptionsIndex,
    const int                                     element,
    const peano::heap::MessageType&               messageType,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) const {
  CellDescription& cellDescription = getCellDescription(cellDescriptionsIndex,element);
  #ifdef Asserts
  const tarch::la::Vector<DIMENSIONS,double> center = cellDescription.getOffset()+0.5*cellDescription.getSize();
  #endif
  assertion5(Vertex::equalUpToRelativeTolerance(x,center),x,center,level,cellDescription.getLevel(),tarch::parallel::Node::getInstance().getRank());
  assertion2(cellDescription.getLevel()==level,cellDescription.getLevel(),level);

  // allocate memory
  if ( messageType==peano::heap::MessageType::ForkOrJoinCommunication ) {
    ensureNecessaryMemoryIsAllocated(cellDescription);
    ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
  } else if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    ensureNecessaryMemoryIsAllocated(cellDescription);
    ensureNoUnnecessaryMemoryIsAllocated(cellDescription);
  }

  // receive data
  if ( cellDescription.getType()==CellDescription::Type::Cell ) {
    logDebug("mergeWithRemoteDataDueToForkOrJoin(...)","[solution] receive from rank "<<fromRank<<
             ", cell: "<< x << ", level: " << level);

    getDataHeapEntries(cellDescription.getSolutionIndex()).clear();
    getDataHeapEntries(cellDescription.getPreviousSolutionIndex()).clear();
    DataHeap::getInstance().receiveData(cellDescription.getSolutionIndex(),
        fromRank,x,level,messageType);
    DataHeap::getInstance().receiveData(cellDescription.getPreviousSolutionIndex(),
        fromRank,x,level,messageType);
  }
}

///////////////////////////////////
// NEIGHBOUR
///////////////////////////////////
void
exahype::solvers::ADERDGSolver::appendNeighbourCommunicationMetadata(
    exahype::MetadataHeap::HeapEntries& metadata,
    const tarch::la::Vector<DIMENSIONS,int>& src,
    const tarch::la::Vector<DIMENSIONS,int>& dest,
    const int cellDescriptionsIndex,
    const int solverNumber) const {
  const int element = tryGetElement(cellDescriptionsIndex,solverNumber);

  if (element!=exahype::solvers::Solver::NotFound)  {
    CellDescription& cellDescription = getCellDescription(cellDescriptionsIndex,element);

    metadata.push_back(static_cast<int>(cellDescription.getType()));
    metadata.push_back(cellDescription.getAugmentationStatus()); // TODO(Dominic): Add to docu: Might be merged multiple times!
    metadata.push_back(cellDescription.getCommunicationStatus());
    metadata.push_back(cellDescription.getRefinementStatus());
  } else {
    for (int i = 0; i < exahype::NeighbourCommunicationMetadataPerSolver; ++i) {
      metadata.push_back(exahype::InvalidMetadataEntry); // implicit conversion
    }
  }
}

void exahype::solvers::ADERDGSolver::mergeWithNeighbourMetadata(
    const int                                 solverNumber,
    Solver::CellInfo&                         cellInfo,
    const MetadataHeap::HeapEntries&          neighbourMetadata,
    const tarch::la::Vector<DIMENSIONS, int>& src,
    const tarch::la::Vector<DIMENSIONS, int>& dest) const {
  assertion(tarch::la::countEqualEntries(src,dest)==DIMENSIONS-1); // only consider faces
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element!=Solver::NotFound ) {
    Solver::BoundaryFaceInfo face(dest,src);
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

    const int neighbourAugmentationStatus  = neighbourMetadata[exahype::NeighbourCommunicationMetadataAugmentationStatus  ];
    const int neighbourCommunicationStatus = neighbourMetadata[exahype::NeighbourCommunicationMetadataCommunicationStatus ];
    const int neighbourRefinementStatus    = neighbourMetadata[exahype::NeighbourCommunicationMetadataLimiterStatus       ];

    mergeWithAugmentationStatus (cellDescription,face._faceIndex,neighbourAugmentationStatus);
    mergeWithCommunicationStatus(cellDescription,face._faceIndex,neighbourCommunicationStatus);
    mergeWithRefinementStatus   (cellDescription,face._faceIndex,neighbourRefinementStatus);

    cellDescription.setNeighbourMergePerformed(face._faceIndex,true);
  }
}

void exahype::solvers::ADERDGSolver::sendDataToNeighbour(
    const int                                     toRank,
    const int                                     solverNumber,
    Solver::CellInfo&                             cellInfo,
    const tarch::la::Vector<DIMENSIONS, int>&     src,
    const tarch::la::Vector<DIMENSIONS, int>&     dest,
    const tarch::la::Vector<DIMENSIONS, double>&  x,
    const int                                     level) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != Solver::NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    Solver::BoundaryFaceInfo face(src,dest);

    if ( communicateWithNeighbour(cellDescription,face._faceIndex) ) {
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex()));
      assertion(DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationIndex()));

      const int dofsPerFace = getBndFluxSize();
      const int dataPerFace = getBndFaceSize();

      const double* lQhbnd = static_cast<double*>(cellDescription.getExtrapolatedPredictor()) + dataPerFace * face._faceIndex;
      const double* lFhbnd = static_cast<double*>(cellDescription.getFluctuation())           + dofsPerFace * face._faceIndex;

      waitUntilCompletedLastStep<CellDescription>(cellDescription,true,true);

      // Send order: lQhbnd,lFhbnd,observablesMin,observablesMax
      // Receive order: observablesMax,observablesMin,lFhbnd,lQhbnd
      DataHeap::getInstance().sendData(
          lQhbnd, dataPerFace, toRank, x, level,
          peano::heap::MessageType::NeighbourCommunication);
      DataHeap::getInstance().sendData(
          lFhbnd, dofsPerFace, toRank, x, level,
          peano::heap::MessageType::NeighbourCommunication);
      // TODO(Dominic): If anarchic time stepping send the time step over too.
    }
  }
}

// TODO(Dominic): Add to docu: We only perform a Riemann solve if a Cell is involved.
void exahype::solvers::ADERDGSolver::mergeWithNeighbourData(
    const int                                    fromRank,
    const int                                    solverNumber,
    Solver::CellInfo&                            cellInfo,
    const tarch::la::Vector<DIMENSIONS, int>&    src,
    const tarch::la::Vector<DIMENSIONS, int>&    dest,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    Solver::BoundaryFaceInfo face(dest,src);
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

    if( communicateWithNeighbour(cellDescription,face._faceIndex) ) {
      // Send order: lQhbnd,lFhbnd
      // Receive order: lFhbnd,lQhbnd
      // TODO(Dominic): If anarchic time stepping, receive the time step too.
       const int dofsPerFace = getBndFluxSize();
       const int dataPerFace = getBndFaceSize();
       DataHeap::getInstance().receiveData(
           const_cast<double*>(_receivedFluctuations.data()),dofsPerFace, // TODO const-correct peano
           fromRank, x, level,peano::heap::MessageType::NeighbourCommunication);
       DataHeap::getInstance().receiveData(                              // TODO const-correct peano
           const_cast<double*>(_receivedExtrapolatedPredictor.data()),dataPerFace,
           fromRank, x, level, peano::heap::MessageType::NeighbourCommunication);

       solveRiemannProblemAtInterface(
           cellDescription, face,
           _receivedExtrapolatedPredictor.data(),
           _receivedFluctuations.data(),
           fromRank);
    }
  }
}

void exahype::solvers::ADERDGSolver::solveRiemannProblemAtInterface(
    CellDescription& cellDescription,
    Solver::BoundaryFaceInfo& face,
    const double* const lQhbnd,
    const double* const lFhbnd,
    const int fromRank) {
  logDebug("solveRiemannProblemAtInterface(...)",
      "cell-description=" << cellDescription.toString());

  assertion(DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex()));
  assertion(DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationIndex()));

  const int dataPerFace = getBndFaceSize();
  const int dofsPerFace = getBndFluxSize();
  if ( face._orientation==0 ) {
    const double* const QL = lQhbnd;
    double* FL             = const_cast<double*>(lFhbnd); // TODO const-correct kernels
    const double* const QR = static_cast<double*>( cellDescription.getExtrapolatedPredictor()) + dataPerFace * face._faceIndex;
    double* FR             = static_cast<double*>( cellDescription.getFluctuation())           + dofsPerFace * face._faceIndex;
    // TODO const-correct kernels

    #ifdef Asserts
    std::string inputDataL = riemannDataToString(QL,FL,"L");
    std::string inputDataR = riemannDataToString(QR,FR,"R");
    #endif

    riemannSolver(
        FL, FR, QL, QR,
	cellDescription.getTimeStamp(), cellDescription.getTimeStepSize(),
	cellDescription.getSize(), face._direction,false,face._faceIndex);
    #ifdef Asserts
    if ( _checkForNaNs ) {
      for (int ii = 0; ii<dofsPerFace; ii++) {
        assertion8(std::isfinite(FL[ii]), cellDescription.toString(),
            face._faceIndex, ii, QR[ii], QL[ii], FR[ii], FL[ii],fromRank);
        assertion8(std::isfinite(FR[ii]), cellDescription.toString(),
            face._faceIndex, ii, QR[ii], QL[ii], FR[ii], FL[ii],fromRank);
      }
    }
    #endif
  } else {
    const double* const QR = lQhbnd;
    const double* const QL = static_cast<double*>(cellDescription.getExtrapolatedPredictor()) + dataPerFace * face._faceIndex;
    double* FR = const_cast<double*>(lFhbnd); // TODO const-correct kernels
    double* FL = static_cast<double*>(cellDescription.getFluctuation()) + dofsPerFace * face._faceIndex; // TODO const-correct kernels

    #ifdef Asserts
    std::string inputDataL = riemannDataToString(QL,FL,"L");
    std::string inputDataR = riemannDataToString(QR,FR,"R");
    #endif

    std::tuple<double,double> timeStepData = getRiemannSolverTimeStepData(cellDescription,cellDescription);
    riemannSolver(
        FL, FR, QL, QR,
        std::get<0>(timeStepData),
        std::get<1>(timeStepData),
	cellDescription.getSize(),
        face._direction,false,face._faceIndex);
    
    #ifdef Asserts
    if ( _checkForNaNs ) {
      for (int ii = 0; ii<dofsPerFace; ii++) {
        assertion10(std::isfinite(FL[ii]), cellDescription.toString(),
            face._faceIndex, ii, QR[ii], QL[ii], FR[ii], FL[ii],fromRank,inputDataL,inputDataR);
        assertion10(std::isfinite(FR[ii]), cellDescription.toString(),
            face._faceIndex, ii, QR[ii], QL[ii], FR[ii], FL[ii],fromRank,inputDataL,inputDataR);
      }
    }
    #endif
  }
}

void exahype::solvers::ADERDGSolver::dropNeighbourData(
    const int                                    fromRank,
    const int                                    solverNumber,
    Solver::CellInfo&                            cellInfo,
    const tarch::la::Vector<DIMENSIONS, int>&    src,
    const tarch::la::Vector<DIMENSIONS, int>&    dest,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) const {
  const int element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if ( element != NotFound ) {
    Solver::BoundaryFaceInfo face(dest,src);
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];

    if( communicateWithNeighbour(cellDescription,face._faceIndex) ) {
      for(int receives=0; receives<DataMessagesPerNeighbourCommunication; ++receives)
        DataHeap::getInstance().receiveData(
            fromRank, x, level,
            peano::heap::MessageType::NeighbourCommunication);
    }
  }
}

///////////////////////////////////
// WORKER->MASTER
///////////////////////////////////
void exahype::solvers::ADERDGSolver::sendDataToMaster(
    const int                                    masterRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) const {
  DataHeap::HeapEntries messageForMaster = compileMessageForMaster();

  if ( !tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    logDebug("sendDataToMaster(...)","Sending data to master: " <<
        "data[0]=" << messageForMaster[0] << "," <<
        "data[1]=" << messageForMaster[1] << "," <<
        "to rank " << masterRank <<
        ", message size="<<messageForMaster.size()
    );
  }

  // MPI_Send(
  //   messageForMaster.data(), messageForMaster.size(),
  //   MPI_DOUBLE,
  //   masterRank,
  //   MasterWorkerCommunicationTag,
  //   tarch::parallel::Node::getInstance().getCommunicator());

  DataHeap::getInstance().sendData(
      messageForMaster.data(), messageForMaster.size(),
      masterRank,x,level,peano::heap::MessageType::MasterWorkerCommunication);
}

exahype::DataHeap::HeapEntries
exahype::solvers::ADERDGSolver::compileMessageForMaster(const int capacity) const {
  const auto messageSize = 2 + _numberOfGlobalObservables;
  DataHeap::HeapEntries message;
  message.reserve(std::max(messageSize,capacity));

  message.push_back(_admissibleTimeStepSize);
  message.push_back(convertToDouble(_meshUpdateEvent));

  for (const auto observable : _globalObservables) {
    message.push_back(observable);
  }

  assertion1(message.size()==messageSize,message.size());
  assertion1(std::isfinite(message[0]),message[0]);
  return message;
}

/**
 * At the time of the merging,
 * the workers and the master have already performed
 * at local update of the next predictor time step size
 * and of the predictor time stamp.
 * We thus need to minimise over both quantities.
 */
void exahype::solvers::ADERDGSolver::mergeWithWorkerData(
    const int                                    workerRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) {
  const auto messageSize = 2 + _numberOfGlobalObservables;
  DataHeap::HeapEntries messageFromWorker(messageSize);

  //  MPI_Recv(
  //    messageFromWorker.data(), messageFromWorker.size(),
  //    MPI_DOUBLE,
  //    workerRank,
  //    MasterWorkerCommunicationTag,
  //    tarch::parallel::Node::getInstance().getCommunicator(),
  //    MPI_STATUS_IGNORE);

  DataHeap::getInstance().receiveData(
      messageFromWorker.data(), messageFromWorker.size(),
      workerRank,x,level,peano::heap::MessageType::MasterWorkerCommunication);

  if ( tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    logDebug("mergeWithWorkerData(...)","Receive data from worker rank: " <<
             "data[0]=" << messageFromWorker[0] << "," <<
             "data[1]=" << messageFromWorker[1] << "," <<
             "from worker " << workerRank << "," <<
             "message size="<<messageFromWorker.size());
   }

  assertion1(messageFromWorker.size()==2,messageFromWorker.size());
  mergeWithWorkerData(messageFromWorker);

  if ( tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    logDebug("mergeWithWorkerData(...)","Updated fields: " <<
             "_admissibleTimeStepSize=" << _admissibleTimeStepSize << "," <<
             "_meshUpdateEvent="        << Solver::toString(_meshUpdateEvent) );
  }
}

void exahype::solvers::ADERDGSolver::mergeWithWorkerData(const DataHeap::HeapEntries& message) {
  int index=0; // post update
  _admissibleTimeStepSize = std::min( _admissibleTimeStepSize, message[index++] );
  _meshUpdateEvent       = mergeMeshUpdateEvents(_meshUpdateEvent,convertToMeshUpdateEvent(message[index++]));
  auto observablesFromWorker = std::vector<double>(_numberOfGlobalObservables);
  for (int i = 0; i < _numberOfGlobalObservables; ++i) {
    observablesFromWorker[i] = message[index++];
  }
  reduceGlobalObservables(_globalObservables, observablesFromWorker);

}

///////////////////////////////////
// MASTER->WORKER
///////////////////////////////////
exahype::DataHeap::HeapEntries
exahype::solvers::ADERDGSolver::compileMessageForWorker(const int capacity) const {
  const auto messageSize = 5 + _numberOfGlobalObservables;
  DataHeap::HeapEntries message;
  message.reserve(std::max(5,capacity));

  message.push_back(_minTimeStamp);
  message.push_back(_minTimeStepSize);
  message.push_back(_estimatedTimeStepSize);
  message.push_back(convertToDouble(_meshUpdateEvent));
  message.push_back(_stabilityConditionWasViolated ? 1.0 : -1.0);

  for (const auto observable : _globalObservables) {
    message.push_back(observable);
  }

  assertion1(message.size()==messageSize,message.size());
  return message;
}

void exahype::solvers::ADERDGSolver::sendDataToWorker(
    const int                                    workerRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) const {
  DataHeap::HeapEntries messageForWorker = compileMessageForWorker();

  if ( tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    logDebug(
        "sendDataToWorker(...)","Broadcasting time step data: " <<
        "data[0]=" << messageForWorker[0] << "," <<
        "data[1]=" << messageForWorker[1] << "," <<
        "data[2]=" << messageForWorker[2] << "," <<
        "data[3]=" << messageForWorker[3] << "," <<
        "data[4]=" << messageForWorker[4]);
  }
  
  // MPI_Send(
  //   messageForWorker.data(), messageForWorker.size(),
  //   MPI_DOUBLE,
  //   workerRank,
  //   MasterWorkerCommunicationTag,
  //   tarch::parallel::Node::getInstance().getCommunicator());

  DataHeap::getInstance().sendData(
      messageForWorker.data(), messageForWorker.size(),
      workerRank,x,level,peano::heap::MessageType::MasterWorkerCommunication);
}

void exahype::solvers::ADERDGSolver::mergeWithMasterData(const DataHeap::HeapEntries& message) {
  int index=0;
  _minTimeStamp                  = message[index++];
  _minTimeStepSize               = message[index++];
  _estimatedTimeStepSize         = message[index++];
  _meshUpdateEvent               = convertToMeshUpdateEvent(message[index++]);
  _stabilityConditionWasViolated = (message[index++] > 0.0) ? true : false;

  for (int i = 0; i < _numberOfGlobalObservables; ++i) {
    _globalObservables[i] = message[index++];
  }
}

void exahype::solvers::ADERDGSolver::mergeWithMasterData(
    const int                                    masterRank,
    const tarch::la::Vector<DIMENSIONS, double>& x,
    const int                                    level) {
  //const auto messageSize = 7 + _numberOfGlobalObservables;
  // TODO(Lukas) Used to be 7+...
  const auto messageSize = 5 + _numberOfGlobalObservables;
  DataHeap::HeapEntries messageFromMaster(messageSize);
  // MPI_Recv(
  //     messageFromMaster.data(), messageFromMaster.size(),
  //     MPI_DOUBLE,
  //     masterRank,
  //     MasterWorkerCommunicationTag,
  //     tarch::parallel::Node::getInstance().getCommunicator(),MPI_STATUS_IGNORE);

  DataHeap::getInstance().receiveData(
      messageFromMaster.data(), messageFromMaster.size(),
      masterRank,x,level,peano::heap::MessageType::MasterWorkerCommunication);

  assertion1(messageFromMaster.size()==messageSize,messageFromMaster.size());
  mergeWithMasterData(messageFromMaster);

  if ( !tarch::parallel::Node::getInstance().isGlobalMaster() ) {
    logDebug(
        "mergeWithMasterData(...)","Received message from master: " <<
        "data[0]=" << messageFromMaster[0] << "," <<
        "data[1]=" << messageFromMaster[1] << "," <<
        "data[2]=" << messageFromMaster[2] << "," <<
        "data[3]=" << messageFromMaster[3] << "," <<
        "data[4]=" << messageFromMaster[4]);
  }
}
#endif

std::string exahype::solvers::ADERDGSolver::toString() const {
  std::ostringstream stringstr;
  toString(stringstr);
  return stringstr.str();
}

void exahype::solvers::ADERDGSolver::toString (std::ostream& out) const {
  out << "(";
  out << "_identifier:"                   << _identifier;
  out << "_type:"                         << Solver::toString(_type) << ",";
  out << "_numberOfVariables:"            << _numberOfVariables << ",";
  out << "_numberOfParameters:"           << _numberOfParameters << ",";
  out << "_nodesPerCoordinateAxis:"       << _nodesPerCoordinateAxis << ",";
  out << "_maximumMeshSize:"              << _maximumMeshSize << ",";
  out << "_timeStepping:"                 << Solver::toString(_timeStepping) << ",";
  out << "_unknownsPerFace:"              << getUnknownsPerFace() << ",";
  out << "_unknownsPerCellBoundary:"      << getUnknownsPerCellBoundary() << ",";
  out << "_unknownsPerCell:"              << getUnknownsPerCell() << ",";
  out << "_fluxUnknownsPerCell:"          << getFluxUnknownsPerCell() << ",";
  out << "_spaceTimeUnknownsPerCell:"     << getSpaceTimeUnknownsPerCell() << ",";
  out << "_spaceTimeFluxUnknownsPerCell:" << getSpaceTimeFluxUnknownsPerCell() << ",";
  out << "_previousMinTimeStamp:"         << _previousMinTimeStamp << ",";
  out << "_previousMinTimeStepSize:"      << _previousMinTimeStepSize << ",";
  out << "_minTimeStamp:"                 << _minTimeStamp << ",";
  out << "_minTimeStepSize:"              << _minTimeStepSize << ",";
  out << "_estimatedTimeStepSize:"        << _estimatedTimeStepSize << ",";
  out <<  ")";
}

exahype::solvers::ADERDGSolver::CompressionJob::CompressionJob(
  const ADERDGSolver& solver,
  CellDescription&    cellDescription,
  const bool          isSkeletonJob):
  tarch::multicore::jobs::Job(tarch::multicore::jobs::JobType::BackgroundTask,0,getTaskPriority(isSkeletonJob)),
  _solver(solver),
  _cellDescription(cellDescription),
  _isSkeletonJob(isSkeletonJob) {
  if (_isSkeletonJob) {
    NumberOfSkeletonJobs.fetch_add(1);
  } else {
    NumberOfEnclaveJobs.fetch_add(1);
  }
}


bool exahype::solvers::ADERDGSolver::CompressionJob::run() {
  _solver.determineUnknownAverages(_cellDescription);
  _solver.computeHierarchicalTransform(_cellDescription,-1.0);
  _solver.putUnknownsIntoByteStream(_cellDescription);

  if (_isSkeletonJob) {
    NumberOfSkeletonJobs.fetch_sub(1);
    assertion( NumberOfSkeletonJobs.load()>=0 );
  } else {
    NumberOfEnclaveJobs.fetch_sub(1);
    assertion( NumberOfEnclaveJobs.load()>=0 );
  }
  return false;
}


void exahype::solvers::ADERDGSolver::compress( CellDescription& cellDescription, const bool isSkeletonCell ) const {
  assertion1( cellDescription.getCompressionState() ==  CellDescription::Uncompressed, cellDescription.toString() );
  if (CompressionAccuracy>0.0) {
    if ( SpawnCompressionAsBackgroundJob ) {
      cellDescription.setCompressionState(CellDescription::CurrentlyProcessed);
      CompressionJob compressionJob( *this, cellDescription, isSkeletonCell );
      assertionMsg( false, "this call is invalid" );
/*
      if ( isSkeletonCell ) {
        peano::datatraversal::TaskSet spawnedSet( compressionJob, peano::datatraversal::TaskSet::TaskType::IsTaskAndRunAsSoonAsPossible  );
      } else {
        peano::datatraversal::TaskSet spawnedSet( compressionJob, peano::datatraversal::TaskSet::TaskType::Background  );
      }
*/
    }
    else {
      determineUnknownAverages(cellDescription);
      computeHierarchicalTransform(cellDescription,-1.0);
      putUnknownsIntoByteStream(cellDescription);
      cellDescription.setCompressionState(CellDescription::Compressed);
    }
  }
}


void exahype::solvers::ADERDGSolver::uncompress(CellDescription& cellDescription) const {
  #ifdef SharedMemoryParallelisation
  bool madeDecision = CompressionAccuracy<=0.0;
  bool uncompress   = false;

  while (!madeDecision) {
    peano::datatraversal::TaskSet::finishToProcessBackgroundJobs();

    tarch::multicore::Lock lock(exahype::ReductionSemaphore);
    madeDecision = cellDescription.getCompressionState() != CellDescription::CurrentlyProcessed;
    uncompress   = cellDescription.getCompressionState() == CellDescription::Compressed;
    if (uncompress) {
      cellDescription.setCompressionState( CellDescription::CurrentlyProcessed );
    }
    lock.free();
  }
  #else
  bool uncompress = CompressionAccuracy>0.0
      && cellDescription.getCompressionState() == CellDescription::Compressed;
  #endif

  if (uncompress) {
    pullUnknownsFromByteStream(cellDescription);
    computeHierarchicalTransform(cellDescription,1.0);

    tarch::multicore::Lock lock(exahype::ReductionSemaphore);
      cellDescription.setCompressionState(CellDescription::Uncompressed);
    lock.free();
  }
}


void exahype::solvers::ADERDGSolver::determineUnknownAverages(
  CellDescription& cellDescription) const {
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getSolutionIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getPreviousSolutionIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getUpdateIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationIndex()), cellDescription.toString() );

  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getSolutionAveragesIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getPreviousSolutionAveragesIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getUpdateAveragesIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getExtrapolatedPredictorAveragesIndex()), cellDescription.toString() );
  assertion1( DataHeap::getInstance().isValidIndex(cellDescription.getFluctuationAveragesIndex()), cellDescription.toString() );

  const int dataPerNode  = getNumberOfParameters()+getNumberOfVariables();
  const int nodesPerCell = getDataPerCell()/ dataPerNode;
  const int nodesPerFace = getDataPerFace() / dataPerNode;

  double* solutionAverages              = static_cast<double*>(cellDescription.getSolutionAverages());
  double* previousSolutionAverage       = static_cast<double*>(cellDescription.getPreviousSolutionAverages());
  double* updateAverages                = static_cast<double*>(cellDescription.getUpdateAverages());
  double* extrapolatedPredictorAverages = static_cast<double*>(cellDescription.getExtrapolatedPredictorAverages());
  double* fluctuationAverages           = static_cast<double*>(cellDescription.getFluctuationAverages());

  double* solution              = static_cast<double*>(cellDescription.getSolution());
  double* previousSolution      = static_cast<double*>(cellDescription.getPreviousSolution());
  double* update                = static_cast<double*>(cellDescription.getUpdate());
  double* extrapolatedPredictor = static_cast<double*>(cellDescription.getExtrapolatedPredictor());
  double* fluctuation           = static_cast<double*>(cellDescription.getFluctuation());

  // patch data
  kernels::idx2 idx_cellData    (nodesPerCell,dataPerNode);
  kernels::idx2 idx_cellUnknowns(nodesPerCell,getNumberOfVariables());
  for (int i=0; i<nodesPerCell; i++) {
    for (int variableNumber=0; variableNumber<dataPerNode; variableNumber++) { // variables+parameters
      solutionAverages[variableNumber]        += solution        [idx_cellData(i,variableNumber)];
      previousSolutionAverage[variableNumber] += previousSolution[idx_cellData(i,variableNumber)];
    }
    for (int variableNumber=0; variableNumber<getNumberOfVariables(); variableNumber++) { // variables
      updateAverages[variableNumber]          += update[idx_cellUnknowns(i,variableNumber)];
    }
  }
  for (int variableNumber=0; variableNumber<dataPerNode; variableNumber++) { // variables+parameters
    solutionAverages[variableNumber]        = solutionAverages[variableNumber]        / (double) nodesPerCell;
    previousSolutionAverage[variableNumber] = previousSolutionAverage[variableNumber] / (double) nodesPerCell;
  }
  for (int variableNumber=0; variableNumber<getNumberOfVariables(); variableNumber++) { // variables
    updateAverages[variableNumber]          = updateAverages[variableNumber]          / (double) nodesPerCell;
  }

  // face data
  kernels::idx2 idx_faceDataAvg    (DIMENSIONS_TIMES_TWO,dataPerNode);
  kernels::idx2 idx_faceUnknownsAvg(DIMENSIONS_TIMES_TWO,getNumberOfVariables());
  kernels::idx3 idx_faceData       (DIMENSIONS_TIMES_TWO,nodesPerFace,dataPerNode);
  kernels::idx3 idx_faceUnknowns   (DIMENSIONS_TIMES_TWO,nodesPerFace,getNumberOfVariables());
  for (int face=0; face<2*DIMENSIONS; face++) {
    for (int i=0; i<nodesPerFace; i++) {
      for (int variableNumber=0; variableNumber<dataPerNode; variableNumber++) { // variables+parameters
        extrapolatedPredictorAverages[idx_faceDataAvg(face,variableNumber)] +=
            extrapolatedPredictor[idx_faceData(face,i,variableNumber)];
      }
      for (int variableNumber=0; variableNumber<getNumberOfVariables(); variableNumber++) { // variables
        fluctuationAverages[idx_faceUnknownsAvg(face,variableNumber)] +=
            fluctuation[idx_faceUnknowns(face,i,variableNumber)];
      }
    }
    for (int variableNumber=0; variableNumber<dataPerNode; variableNumber++) { // variables+parameters
      extrapolatedPredictorAverages[idx_faceDataAvg(face,variableNumber)] =
          extrapolatedPredictorAverages[idx_faceDataAvg(face,variableNumber)] / (double) nodesPerFace;
    }
    for (int variableNumber=0; variableNumber<getNumberOfVariables(); variableNumber++) { // variables
      fluctuationAverages[idx_faceUnknownsAvg(face,variableNumber)] =
          fluctuationAverages[idx_faceUnknownsAvg(face,variableNumber)]       / (double) nodesPerFace;
    }
  }
}


void exahype::solvers::ADERDGSolver::computeHierarchicalTransform(
    CellDescription& cellDescription, double sign) const {
  const int dataPerNode  = getNumberOfParameters()+getNumberOfVariables();
  const int nodesPerCell = getDataPerCell()/ dataPerNode;
  const int nodesPerFace = getDataPerFace() / dataPerNode;

  double* solutionAverages              = static_cast<double*>(cellDescription.getSolutionAverages());
  double* previousSolutionAverage       = static_cast<double*>(cellDescription.getPreviousSolutionAverages());
  double* updateAverages                = static_cast<double*>(cellDescription.getUpdateAverages());
  double* extrapolatedPredictorAverages = static_cast<double*>(cellDescription.getExtrapolatedPredictorAverages());
  double* fluctuationAverages           = static_cast<double*>(cellDescription.getFluctuationAverages());

  double* solution              = static_cast<double*>(cellDescription.getSolution());
  double* previousSolution      = static_cast<double*>(cellDescription.getPreviousSolution());
  double* update                = static_cast<double*>(cellDescription.getUpdate());
  double* extrapolatedPredictor = static_cast<double*>(cellDescription.getExtrapolatedPredictor());
  double* fluctuation           = static_cast<double*>(cellDescription.getFluctuation());

  // patch data
  kernels::idx2 idx_cellData    (nodesPerCell,dataPerNode);
  kernels::idx2 idx_cellUnknowns(nodesPerCell,getNumberOfVariables());
  for (int i=0; i<nodesPerCell; i++) {
    for (int variableNumber=0; variableNumber<dataPerNode; variableNumber++) { // variables+parameters
      solution        [idx_cellData(i,variableNumber)] += sign * solutionAverages[variableNumber];
      previousSolution[idx_cellData(i,variableNumber)] += sign * previousSolutionAverage[variableNumber];
    }
    for (int variableNumber=0; variableNumber<getNumberOfVariables(); variableNumber++) { // variables
      update[idx_cellUnknowns(i,variableNumber)] += sign * updateAverages[variableNumber];
    }
  }

  // face data
  kernels::idx2 idx_faceDataAvg    (DIMENSIONS_TIMES_TWO,dataPerNode);
  kernels::idx2 idx_faceUnknownsAvg(DIMENSIONS_TIMES_TWO,getNumberOfVariables());
  kernels::idx3 idx_faceData       (DIMENSIONS_TIMES_TWO,nodesPerFace,dataPerNode);
  kernels::idx3 idx_faceUnknowns   (DIMENSIONS_TIMES_TWO,nodesPerFace,getNumberOfVariables());
  for (int face=0; face<DIMENSIONS_TIMES_TWO; face++) {
    for (int i=0; i<nodesPerFace; i++) {
      for (int variableNumber=0; variableNumber<dataPerNode; variableNumber++) {  // variables+parameters
        extrapolatedPredictor[idx_faceData(face,i,variableNumber)] +=
            sign * extrapolatedPredictorAverages[idx_faceDataAvg(face,variableNumber)];
      }
      for (int variableNumber=0; variableNumber<getNumberOfVariables(); variableNumber++) {  // variables
        fluctuation[idx_faceUnknowns(face,i,variableNumber)] +=
            sign * fluctuationAverages[idx_faceUnknownsAvg(face,variableNumber)];
      }
    }
  }
}

void exahype::solvers::ADERDGSolver::putUnknownsIntoByteStream(
    CellDescription& cellDescription) const {
  assertion(CompressionAccuracy>0.0);

  assertion( cellDescription.getPreviousSolutionCompressedIndex()==-1 );
  assertion( cellDescription.getSolutionCompressedIndex()==-1 );
  assertion( cellDescription.getUpdateCompressedIndex()==-1 );
  assertion( cellDescription.getExtrapolatedPredictorCompressedIndex()==-1 );
  assertion( cellDescription.getFluctuationCompressedIndex()==-1 );

  int compressionOfPreviousSolution;
  int compressionOfSolution;
  int compressionOfUpdate;
  int compressionOfExtrapolatedPredictor;
  int compressionOfFluctuation;

  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getSolutionIndex() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getPreviousSolutionIndex() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getUpdateIndex() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getExtrapolatedPredictorIndex() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getFluctuationIndex() ));

  peano::datatraversal::TaskSet compressionFactorIdentification(
    [&]() -> bool { compressionOfPreviousSolution = peano::heap::findMostAgressiveCompression(
      static_cast<double*>(cellDescription.getPreviousSolution()),
      getDataPerCell(),
      CompressionAccuracy,true
      );
      return false;
      },
    [&] () -> bool  { compressionOfSolution = peano::heap::findMostAgressiveCompression(
      static_cast<double*>(cellDescription.getSolution()),
      getDataPerCell(),
      CompressionAccuracy,true
      );
      return false;
      },
    [&]() -> bool  { compressionOfUpdate = peano::heap::findMostAgressiveCompression(
      static_cast<double*>(cellDescription.getUpdate()),
      getUnknownsPerCell(),
      CompressionAccuracy,true
      );
      return false;
      },
    [&]() -> bool  { compressionOfExtrapolatedPredictor = peano::heap::findMostAgressiveCompression(
      static_cast<double*>(cellDescription.getExtrapolatedPredictor()),
      getDataPerCellBoundary(),
      CompressionAccuracy,true
      );
      return false;
      },
    [&]() -> bool  { compressionOfFluctuation = peano::heap::findMostAgressiveCompression(
      static_cast<double*>(cellDescription.getFluctuation()),
      getUnknownsPerCellBoundary(),
      CompressionAccuracy,true
      );
      return false;
      },
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
    true
  );

  assertion(1<=compressionOfPreviousSolution);
  assertion(1<=compressionOfSolution);
  assertion(1<=compressionOfUpdate);
  assertion(1<=compressionOfExtrapolatedPredictor);
  assertion(1<=compressionOfFluctuation);

  assertion(compressionOfPreviousSolution<=7);
  assertion(compressionOfSolution<=7);
  assertion(compressionOfUpdate<=7);
  assertion(compressionOfExtrapolatedPredictor<=7);
  assertion(compressionOfFluctuation<=7);

  peano::datatraversal::TaskSet runParallelTasks(
    [&]() -> bool {
      cellDescription.setBytesPerDoFInPreviousSolution(compressionOfPreviousSolution);
      if (compressionOfPreviousSolution<7) {
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          cellDescription.setPreviousSolutionCompressedIndex( CompressedDataHeap::getInstance().createData(0,0) );
          assertion( cellDescription.getPreviousSolutionCompressedIndex()>=0 );
          cellDescription.setPreviousSolutionCompressed( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getPreviousSolutionCompressedIndex()).data() ) );
        lock.free();

        const int numberOfEntries = getDataPerCell();
        tearApart(numberOfEntries, cellDescription.getPreviousSolutionIndex(), cellDescription.getPreviousSolutionCompressedIndex(), compressionOfPreviousSolution);

        #if defined(TrackGridStatistics)
        lock.lock();
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getPreviousSolutionIndex()).size() * 8.0;
          PipedCompressedBytes   += CompressedDataHeap::getInstance().getData( cellDescription.getPreviousSolutionCompressedIndex() ).size();
        lock.free();
        #endif

        #if !defined(ValidateCompressedVsUncompressedData)
        lock.lock();
          DataHeap::getInstance().deleteData( cellDescription.getPreviousSolutionIndex(), true );
          cellDescription.setPreviousSolutionIndex(-1);
          cellDescription.setPreviousSolution(nullptr);
        lock.free();
        #endif
      }
      else {
        #if defined(TrackGridStatistics)
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getPreviousSolutionIndex()).size() * 8.0;
          PipedCompressedBytes   += getDataHeapEntries(cellDescription.getPreviousSolutionIndex()).size() * 8.0;
        lock.free();
        #endif
      }
      return false;
    },
    [&]() -> bool {
      cellDescription.setBytesPerDoFInSolution(compressionOfSolution);
      if (compressionOfSolution<7) {
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          cellDescription.setSolutionCompressedIndex(CompressedDataHeap::getInstance().createData(0,0));
          assertion1( cellDescription.getSolutionCompressedIndex()>=0, cellDescription.getSolutionCompressedIndex() );
          cellDescription.setSolutionCompressed( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getSolutionCompressedIndex()).data() ) );
        lock.free();

        const int numberOfEntries = getDataPerCell();

        tearApart(numberOfEntries, cellDescription.getSolutionIndex(), cellDescription.getSolutionCompressedIndex(), compressionOfSolution);

        #if defined(TrackGridStatistics)
        lock.lock();
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getSolutionIndex()).size() * 8.0;
          PipedCompressedBytes   += CompressedDataHeap::getInstance().getData( cellDescription.getSolutionCompressedIndex() ).size();
        lock.free();
        #endif

        #if !defined(ValidateCompressedVsUncompressedData)
        lock.lock();
          DataHeap::getInstance().deleteData( cellDescription.getSolutionIndex(), true );
          cellDescription.setSolutionIndex(-1);
          cellDescription.setSolution(nullptr);
        lock.free();
        #endif
      }
      else {
        #if defined(TrackGridStatistics)
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getSolutionIndex()).size() * 8.0;
          PipedCompressedBytes   += getDataHeapEntries(cellDescription.getSolutionIndex()).size() * 8.0;
        lock.free();
        #endif
      }
      return false;
    },
    [&]() -> bool {
      cellDescription.setBytesPerDoFInUpdate(compressionOfUpdate);
      if (compressionOfUpdate<7) {
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          cellDescription.setUpdateCompressedIndex( CompressedDataHeap::getInstance().createData(0,0) );
          assertion( cellDescription.getUpdateCompressedIndex()>=0 );
          cellDescription.setUpdateCompressed( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getUpdateCompressedIndex()).data() ) );
        lock.free();

        const int numberOfEntries = getUnknownsPerCell();
        tearApart(numberOfEntries, cellDescription.getUpdateIndex(), cellDescription.getUpdateCompressedIndex(), compressionOfUpdate);

        #if defined(TrackGridStatistics)
        lock.lock();
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getUpdateIndex()).size() * 8.0;
          PipedCompressedBytes   += CompressedDataHeap::getInstance().getData( cellDescription.getUpdateCompressedIndex() ).size();
        lock.free();
        #endif

        #if !defined(ValidateCompressedVsUncompressedData)
        lock.lock();
          DataHeap::getInstance().deleteData( cellDescription.getUpdateIndex(), true );
          cellDescription.setUpdateIndex(-1);
          cellDescription.setUpdate(nullptr);
        lock.free();
        #endif
      }
      else {
        #if defined(TrackGridStatistics)
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getUpdateIndex()).size() * 8.0;
          PipedCompressedBytes   += getDataHeapEntries(cellDescription.getUpdateIndex()).size() * 8.0;
        lock.free();
        #endif
      }
      return false;
    },
    [&]() -> bool {
      cellDescription.setBytesPerDoFInExtrapolatedPredictor(compressionOfExtrapolatedPredictor);
      if (compressionOfExtrapolatedPredictor<7) {
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          cellDescription.setExtrapolatedPredictorCompressedIndex( CompressedDataHeap::getInstance().createData(0,0) );
          assertion( cellDescription.getExtrapolatedPredictorCompressedIndex()>=0 );
          cellDescription.setExtrapolatedPredictorCompressed( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getExtrapolatedPredictorCompressedIndex()).data() ) );
        lock.free();

        const int numberOfEntries = getDataPerCellBoundary();
        tearApart(numberOfEntries, cellDescription.getExtrapolatedPredictorIndex(), cellDescription.getExtrapolatedPredictorCompressedIndex(), compressionOfExtrapolatedPredictor);

        #if defined(TrackGridStatistics)
        lock.lock();
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getExtrapolatedPredictorIndex()).size() * 8.0;
          PipedCompressedBytes   += CompressedDataHeap::getInstance().getData( cellDescription.getExtrapolatedPredictorCompressedIndex() ).size();
        lock.free();
        #endif

        #if !defined(ValidateCompressedVsUncompressedData)
        lock.lock();
          DataHeap::getInstance().deleteData( cellDescription.getExtrapolatedPredictorIndex(), true );
          cellDescription.setExtrapolatedPredictorIndex(-1);
          cellDescription.setExtrapolatedPredictor(nullptr);
        lock.free();
        #endif
      }
      else {
        #if defined(TrackGridStatistics)
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getExtrapolatedPredictorIndex()).size() * 8.0;
          PipedCompressedBytes   += getDataHeapEntries(cellDescription.getExtrapolatedPredictorIndex()).size() * 8.0;
        lock.free();
        #endif
      }
      return false;
    },
    [&]() -> bool {
      cellDescription.setBytesPerDoFInFluctuation(compressionOfFluctuation);
      if (compressionOfFluctuation<7) {
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          cellDescription.setFluctuationCompressedIndex( CompressedDataHeap::getInstance().createData(0,0) );
          assertion( cellDescription.getFluctuationCompressedIndex()>=0 );
          cellDescription.setFluctuationCompressed( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getFluctuationCompressedIndex()).data() ) );
        lock.free();

        const int numberOfEntries = getUnknownsPerCellBoundary();
        tearApart(numberOfEntries, cellDescription.getFluctuationIndex(), cellDescription.getFluctuationCompressedIndex(), compressionOfFluctuation);

        #if defined(TrackGridStatistics)
        lock.lock();
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getFluctuationIndex()).size() * 8.0;
          PipedCompressedBytes   += CompressedDataHeap::getInstance().getData( cellDescription.getFluctuationCompressedIndex() ).size();
        lock.free();
        #endif

        #if !defined(ValidateCompressedVsUncompressedData)
        lock.lock();
          DataHeap::getInstance().deleteData( cellDescription.getFluctuationIndex(), true );
          cellDescription.setFluctuationIndex(-1);
          cellDescription.setFluctuation(nullptr);
        lock.free();
        #endif
      }
      else {
        #if defined(TrackGridStatistics)
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          PipedUncompressedBytes += getDataHeapEntries(cellDescription.getFluctuationIndex()).size() * 8.0;
          PipedCompressedBytes   += getDataHeapEntries(cellDescription.getFluctuationIndex()).size() * 8.0;
        lock.free();
        #endif
      }
      return false;
    },
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
	peano::datatraversal::TaskSet::TaskType::Background,
    true
  );
}


void exahype::solvers::ADERDGSolver::pullUnknownsFromByteStream(
    CellDescription& cellDescription) const {
  assertion(CompressionAccuracy>0.0);

  #if !defined(ValidateCompressedVsUncompressedData)
  const int dataPointsPerCell       = getDataPerCell();
  const int unknownsPerCellBoundary = getUnknownsPerCellBoundary();

  {
    tarch::multicore::Lock lock(exahype::ReductionSemaphore);
      cellDescription.setPreviousSolutionIndex( DataHeap::getInstance().createData( dataPointsPerCell, dataPointsPerCell ) );
      cellDescription.setSolutionIndex( DataHeap::getInstance().createData(         dataPointsPerCell, dataPointsPerCell ) );
      cellDescription.setUpdateIndex( DataHeap::getInstance().createData(           getUpdateSize(),   getUpdateSize() ) );

      cellDescription.setExtrapolatedPredictorIndex( DataHeap::getInstance().createData( unknownsPerCellBoundary, unknownsPerCellBoundary ) );
      cellDescription.setFluctuationIndex( DataHeap::getInstance().createData(           unknownsPerCellBoundary, unknownsPerCellBoundary ) );
    lock.free();

    if (cellDescription.getPreviousSolutionIndex()==-1) {
      ensureAllJobsHaveTerminated(JobType::SkeletonJob);
      ensureAllJobsHaveTerminated(JobType::EnclaveJob);
      lock.lock();
        cellDescription.setPreviousSolutionIndex( DataHeap::getInstance().createData( dataPointsPerCell, dataPointsPerCell ) );
      lock.free();
    }
    if (cellDescription.getSolutionIndex()==-1) {
      ensureAllJobsHaveTerminated(JobType::SkeletonJob);
      ensureAllJobsHaveTerminated(JobType::EnclaveJob);
      lock.lock();
        cellDescription.setSolutionIndex( DataHeap::getInstance().createData( dataPointsPerCell, dataPointsPerCell ) );
      lock.free();
    }
    if (cellDescription.getUpdateIndex()==-1) {
      ensureAllJobsHaveTerminated(JobType::SkeletonJob);
      ensureAllJobsHaveTerminated(JobType::EnclaveJob);
      lock.lock();
        cellDescription.setUpdateIndex( DataHeap::getInstance().createData( getUpdateSize(), getUpdateSize() ) );
      lock.free();
    }
    if (cellDescription.getExtrapolatedPredictorIndex()==-1) {
      ensureAllJobsHaveTerminated(JobType::SkeletonJob);
      ensureAllJobsHaveTerminated(JobType::EnclaveJob);
      lock.lock();
        cellDescription.setExtrapolatedPredictorIndex( DataHeap::getInstance().createData(unknownsPerCellBoundary ) );
      lock.free();
    }
    if (cellDescription.getFluctuationIndex()==-1) {
      ensureAllJobsHaveTerminated(JobType::SkeletonJob);
      ensureAllJobsHaveTerminated(JobType::EnclaveJob);
      lock.lock();
        cellDescription.setFluctuationIndex( DataHeap::getInstance().createData( unknownsPerCellBoundary, unknownsPerCellBoundary ) );
      lock.free();
    }

    cellDescription.setPreviousSolution     ( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getPreviousSolutionIndex()).data() ) );
    cellDescription.setSolution             ( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getSolutionIndex()).data() ) );
    cellDescription.setExtrapolatedPredictor( static_cast<void*>(CompressedDataHeap::getInstance().getData(cellDescription.getExtrapolatedPredictorIndex()).data() ) );

  }
  #else
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getPreviousSolution() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getSolution() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getUpdate() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getExtrapolatedPredictor() ));
  assertion( DataHeap::getInstance().isValidIndex( cellDescription.getFluctuation() ));
  #endif

  assertion1(
      CompressedDataHeap::getInstance().isValidIndex( cellDescription.getPreviousSolutionCompressedIndex() ),
      cellDescription.getPreviousSolutionCompressedIndex()
    );
  assertion1(
    CompressedDataHeap::getInstance().isValidIndex( cellDescription.getSolutionCompressedIndex() ),
    cellDescription.getSolutionCompressedIndex()
  );
  assertion1(
    CompressedDataHeap::getInstance().isValidIndex( cellDescription.getUpdateCompressedIndex() ),
    cellDescription.getUpdateCompressedIndex()
  );
  assertion1(
    CompressedDataHeap::getInstance().isValidIndex( cellDescription.getExtrapolatedPredictorCompressedIndex() ),
    cellDescription.getExtrapolatedPredictorCompressedIndex()
  );
  assertion1(
    CompressedDataHeap::getInstance().isValidIndex( cellDescription.getFluctuationCompressedIndex() ),
    cellDescription.getFluctuationCompressedIndex()
  );

  peano::datatraversal::TaskSet glueTasks(
    [&]() -> bool {
      if (cellDescription.getBytesPerDoFInPreviousSolution()<7) {
        assertion1( DataHeap::getInstance().isValidIndex( cellDescription.getPreviousSolutionIndex() ), cellDescription.getPreviousSolutionIndex());
        assertion( CompressedDataHeap::getInstance().isValidIndex( cellDescription.getPreviousSolutionCompressedIndex() ));
        const int numberOfEntries = getDataPerCell();
        glueTogether(numberOfEntries, cellDescription.getPreviousSolutionIndex(), cellDescription.getPreviousSolutionCompressedIndex(), cellDescription.getBytesPerDoFInPreviousSolution());
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          CompressedDataHeap::getInstance().deleteData( cellDescription.getPreviousSolutionCompressedIndex(), true );
          cellDescription.setPreviousSolutionCompressedIndex(-1);
          cellDescription.setPreviousSolutionCompressed(nullptr);
        lock.free();
      }
      return false;
    },
    [&]() -> bool {
      if (cellDescription.getBytesPerDoFInSolution()<7) {
        assertion1( DataHeap::getInstance().isValidIndex( cellDescription.getSolutionIndex() ), cellDescription.getSolutionIndex() );
        assertion( CompressedDataHeap::getInstance().isValidIndex( cellDescription.getSolutionCompressedIndex() ));
        const int numberOfEntries = getDataPerCell();
        glueTogether(numberOfEntries, cellDescription.getSolutionIndex(), cellDescription.getSolutionCompressedIndex(), cellDescription.getBytesPerDoFInSolution());
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          CompressedDataHeap::getInstance().deleteData( cellDescription.getSolutionCompressedIndex(), true );
          cellDescription.setSolutionCompressedIndex(-1);
          cellDescription.setSolutionCompressed(nullptr);
        lock.free();
      }
      return false;
    },
    [&]() -> bool {
      if (cellDescription.getBytesPerDoFInUpdate()<7) {
        assertion1( DataHeap::getInstance().isValidIndex( cellDescription.getUpdateIndex() ), cellDescription.getUpdateIndex());
        assertion( CompressedDataHeap::getInstance().isValidIndex( cellDescription.getUpdateCompressedIndex() ));
        const int numberOfEntries = getUnknownsPerCell();
        glueTogether(numberOfEntries, cellDescription.getUpdateIndex(), cellDescription.getUpdateCompressedIndex(), cellDescription.getBytesPerDoFInUpdate());
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          CompressedDataHeap::getInstance().deleteData( cellDescription.getUpdateCompressedIndex(), true );
          cellDescription.setUpdateCompressedIndex(-1);
          cellDescription.setUpdateCompressed(nullptr);
        lock.free();
      }
      return false;
    },
    [&]() -> bool {
      if (cellDescription.getBytesPerDoFInExtrapolatedPredictor()<7) {
        assertion1( DataHeap::getInstance().isValidIndex( cellDescription.getExtrapolatedPredictorIndex() ), cellDescription.getExtrapolatedPredictorIndex());
        assertion( CompressedDataHeap::getInstance().isValidIndex( cellDescription.getExtrapolatedPredictorCompressedIndex() ));
        const int numberOfEntries = getDataPerCellBoundary();
        glueTogether(numberOfEntries, cellDescription.getExtrapolatedPredictorIndex(), cellDescription.getExtrapolatedPredictorCompressedIndex(), cellDescription.getBytesPerDoFInExtrapolatedPredictor());
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          CompressedDataHeap::getInstance().deleteData( cellDescription.getExtrapolatedPredictorCompressedIndex(), true );
          cellDescription.setExtrapolatedPredictorCompressedIndex(-1);
          cellDescription.setExtrapolatedPredictorCompressed(nullptr);
        lock.free();
      }
      return false;
    },
    [&]() -> bool {
      if (cellDescription.getBytesPerDoFInFluctuation()<7) {
        assertion1( DataHeap::getInstance().isValidIndex( cellDescription.getFluctuationIndex() ), cellDescription.getFluctuationIndex());
        assertion( CompressedDataHeap::getInstance().isValidIndex( cellDescription.getFluctuationCompressedIndex() ));
        const int numberOfEntries = getUnknownsPerCellBoundary();
        glueTogether(numberOfEntries, cellDescription.getFluctuationIndex(), cellDescription.getFluctuationCompressedIndex(), cellDescription.getBytesPerDoFInFluctuation());
        tarch::multicore::Lock lock(exahype::ReductionSemaphore);
          CompressedDataHeap::getInstance().deleteData( cellDescription.getFluctuationCompressedIndex(), true );
          cellDescription.setFluctuationCompressedIndex(-1);
          cellDescription.setFluctuationCompressed(nullptr);
        lock.free();
      }
      return false;
    },
    peano::datatraversal::TaskSet::TaskType::Background,
    peano::datatraversal::TaskSet::TaskType::Background,
    peano::datatraversal::TaskSet::TaskType::Background,
    peano::datatraversal::TaskSet::TaskType::Background,
    peano::datatraversal::TaskSet::TaskType::Background,
    true
  );
}

void exahype::solvers::ADERDGSolver::reduceGlobalObservables(
        std::vector<double>& globalObservables,
        CellInfo cellInfo, int solverNumber) const {
  tarch::multicore::Lock lock(ReductionSemaphore);
  if (globalObservables.empty()) {
    return;
  }
  const auto element = cellInfo.indexOfADERDGCellDescription(solverNumber);
  if (element != NotFound ) {
    CellDescription& cellDescription = cellInfo._ADERDGCellDescriptions[element];
    if (cellDescription.getType() != CellDescription::Type::Cell) {
      return;
    }
    assert(cellDescription.getType()==CellDescription::Type::Cell);
    double* luh  = static_cast<double*>(cellDescription.getSolution());
    const auto& dx = cellDescription.getSize();
    const auto curGlobalObservables = mapGlobalObservables(luh, dx);
    reduceGlobalObservables(globalObservables, curGlobalObservables);
  }
  lock.free();
}

///////////////////////
// PROFILING
///////////////////////

exahype::solvers::Solver::CellProcessingTimes exahype::solvers::ADERDGSolver::measureCellProcessingTimes(const int numberOfRuns) {
  // Setup
  const int cellDescriptionsIndex = ADERDGSolver::Heap::getInstance().createData(0,1);

  Solver::CellInfo cellInfo(cellDescriptionsIndex);
  addNewCellDescription(
      0,cellInfo,CellDescription::Type::Cell,CellDescription::RefinementEvent::None,
      getMaximumAdaptiveMeshLevel(), /* needs to be on the fine grid for the limiter cells */-1,
      getCoarsestMeshSize(),
      _domainOffset);

  CellDescription& cellDescription   = cellInfo._ADERDGCellDescriptions[0];

  ensureNecessaryMemoryIsAllocated(cellDescription);

  adjustSolutionDuringMeshRefinementBody(cellDescription,true);
  cellDescription.setRefinementEvent(CellDescription::RefinementEvent::None);
  updateTimeStepSize(0,cellInfo);

  // ADER-DG specific setup ( all Riemanns have been performed, cell is surrounded by other Cell type cells )
  cellDescription.setNeighbourMergePerformed(true);
  cellDescription.setAugmentationStatus(0);
  cellDescription.setFacewiseAugmentationStatus(0);
  cellDescription.setCommunicationStatus(ADERDGSolver::CellCommunicationStatus);
  cellDescription.setFacewiseCommunicationStatus(ADERDGSolver::CellCommunicationStatus);

  // MEASUREMENTS
  CellProcessingTimes result;

  // measure ADERDG STP
  {
    const std::chrono::high_resolution_clock::time_point timeStart = std::chrono::high_resolution_clock::now();
    int numberOfPicardIterations = std::numeric_limits<int>::max();
    for (int it=0; it<numberOfRuns; it++) {
      numberOfPicardIterations = predictionAndVolumeIntegralBody(cellDescription,cellDescription.getTimeStamp(),cellDescription.getTimeStepSize(),false,true,true);
    }
    const double time_sec = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now()-timeStart).count() * 1e-9;
    result._minTimePredictor = time_sec / numberOfRuns / numberOfPicardIterations;
    result._maxTimePredictor = result._minTimePredictor * getNodesPerCoordinateAxis(); // * (order+1)
  }

  // measure ADER-DG cells
  cellDescription.setRefinementStatus(_refineOrKeepOnFineGrid);
  cellDescription.setFacewiseRefinementStatus(_refineOrKeepOnFineGrid);
  {
    const std::chrono::high_resolution_clock::time_point timeStart = std::chrono::high_resolution_clock::now();
    for (int it=0; it<numberOfRuns; it++) {
      updateBody(cellDescription,cellInfo,cellDescription.getNeighbourMergePerformed(),true);

      swapSolutionAndPreviousSolution(cellDescription); // assumed to be very cheap
      rollbackToPreviousTimeStep(cellDescription);
    }
    const double time_sec = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now()-timeStart).count() * 1e-9;
    result._timeADERDGUpdate = time_sec / numberOfRuns;
  }

  // Clean up
  cellDescription.setType(CellDescription::Type::Erased);
  ensureNoUnnecessaryMemoryIsAllocated(cellDescription);

  DataHeap::getInstance().deleteAllData();
  ADERDGSolver::Heap::getInstance().deleteAllData();
  FiniteVolumesSolver::Heap::getInstance().deleteAllData();

  return result;
}
