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
 * \author Dominic E. Charrier, Tobias Weinzierl
 **/

#ifndef _EXAHYPE_SOLVERS_SOLVER_H_
#define _EXAHYPE_SOLVERS_SOLVER_H_

#include <utility>

#include <memory>
#include <string>
#include <iostream>
#include <iomanip>
#include <vector>

#include <atomic>

#include "tarch/compiler/CompilerSpecificSettings.h"
#include "peano/utils/PeanoOptimisations.h"
#include "tarch/multicore/MulticoreDefinitions.h"
#include "tarch/la/Vector.h"
#include "tarch/la/VectorVectorOperations.h"
#include "tarch/multicore/BooleanSemaphore.h"

#include "peano/utils/Globals.h"
#include "peano/grid/VertexEnumerator.h"
#include "peano/heap/Heap.h"
#include "peano/heap/DoubleHeap.h"
#include "peano/heap/CharHeap.h"
#include "peano/heap/HeapAllocator.h"

#include "multiscalelinkedcell/HangingVertexBookkeeper.h"

#include "exahype/State.h"

#include "exahype/profilers/Profiler.h"
#include "exahype/profilers/simple/NoOpProfiler.h"

// cell descriptions
#include "exahype/records/ADERDGCellDescription.h"
#include "exahype/records/FiniteVolumesCellDescription.h"

#include <functional>

#if defined(CompilerICC) && defined(SharedTBB)
// See: https://www.threadingbuildingblocks.org/tutorial-intel-tbb-scalable-memory-allocator
#include <tbb/cache_aligned_allocator.h> // prevents false sharing
#endif

#if defined(USE_ITAC_ALL) and !defined(USE_ITAC)
#define USE_ITAC 1
#endif
#ifdef USE_ITAC
#include "VT.h"
#endif

// Some helpers
constexpr int power(int basis, int exp) {
  return (exp == 0) ? 1 : basis * power(basis, exp - 1);
}

#ifdef ALIGNMENT
constexpr int addPadding(const int originalSize) {
  return ALIGNMENT/8 * static_cast<int>((originalSize+(ALIGNMENT/8-1))/(ALIGNMENT/8));
}
#else
constexpr int addPadding(const int originalSize) {
  return originalSize;
}
#endif

namespace exahype {
// Forward declarations
class Cell;
class Vertex;

namespace parser {
class ParserView;
}  // namespace parser
/**
 * We store the degrees of freedom associated with the ADERDGCellDescription and FiniteVolumesCellDescription
 * instances on this heap.
 * We further use this heap to send and receive face data from one MPI rank to the other.
 *
 * !!! CreateCopiesOfSentData
 *
 * All solvers must store the face data they send to neighbours at persistent addresses.
 */
#ifdef ALIGNMENT
#if defined(CompilerICC) && defined(SharedTBB)
typedef tbb::cache_aligned_allocator<double> AlignedAllocator;
typedef tbb::cache_aligned_allocator<char> AlignedCharAllocator;
#else
typedef peano::heap::HeapAllocator<double, ALIGNMENT > AlignedAllocator;
typedef peano::heap::HeapAllocator<char, ALIGNMENT > AlignedCharAllocator;
#endif
typedef peano::heap::AlignedDoubleSendReceiveTask<ALIGNMENT> AlignedDoubleSendReceiveTask;
typedef peano::heap::AlignedCharSendReceiveTask<ALIGNMENT>   AlignedCharSendReceiveTask;
#endif

#if defined(UsePeanosSymmetricBoundaryExchanger) and defined(UsePeanosRLEBoundaryExchanger)
#error UsePeanosSymmetricBoundaryExchanger and UsePeanosRLEBoundaryExchanger must not be defined at the same time!
#endif

// aligned data -> AlignedDoubleSendReceiveTask
#if defined(ALIGNMENT) and defined(UsePeanosSymmetricBoundaryExchanger)
typedef peano::heap::DoubleHeap<
    peano::heap::SynchronousDataExchanger< double, true, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    peano::heap::SynchronousDataExchanger< double, true, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    peano::heap::SymmetricBoundaryDataExchanger< double, false, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    std::vector< double, AlignedAllocator >
>     DataHeap;
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    peano::heap::SynchronousDataExchanger< char, true, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    peano::heap::SymmetricBoundaryDataExchanger< char, false, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    std::vector< char, AlignedCharAllocator >
>     CompressedDataHeap;
#elif defined(ALIGNMENT) and defined(UsePeanosRLEBoundaryExchanger)
typedef peano::heap::DoubleHeap<
    peano::heap::SynchronousDataExchanger< double, true, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    peano::heap::SynchronousDataExchanger< double, true, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    peano::heap::AggregationBoundaryDataExchanger< double, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    std::vector< double, AlignedAllocator >
>     DataHeap;
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    peano::heap::SynchronousDataExchanger< char, true, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    peano::heap::AggregationBoundaryDataExchanger< char, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    std::vector< char, AlignedCharAllocator >
>     CompressedDataHeap;
#elif defined(ALIGNMENT) // Default: AggregationBoundaryDataExchanger
typedef peano::heap::DoubleHeap<
    peano::heap::SynchronousDataExchanger< double, true, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    peano::heap::SynchronousDataExchanger< double, true, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    peano::heap::AggregationBoundaryDataExchanger< double, AlignedDoubleSendReceiveTask, std::vector< double, AlignedAllocator > >,
    std::vector< double, AlignedAllocator >
>     DataHeap;
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    peano::heap::SynchronousDataExchanger< char, true, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    peano::heap::AggregationBoundaryDataExchanger< char, AlignedCharSendReceiveTask, std::vector< char, AlignedCharAllocator > >,
    std::vector< char, AlignedCharAllocator >
>     CompressedDataHeap;
#endif
// non-aligned data -> SendReceiveTask
#if !defined(ALIGNMENT) and defined(UsePeanosSymmetricBoundaryExchanger)
typedef peano::heap::DoubleHeap<
    peano::heap::SynchronousDataExchanger< double, true,  peano::heap::SendReceiveTask<double> >,
    peano::heap::SynchronousDataExchanger< double, true,  peano::heap::SendReceiveTask<double> >,
    peano::heap::SymmetricBoundaryDataExchanger< double, false, peano::heap::SendReceiveTask<double> >
>     DataHeap;
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SymmetricBoundaryDataExchanger< char, false, peano::heap::SendReceiveTask<char> >
>     CompressedDataHeap;
#elif !defined(ALIGNMENT) and defined(UsePeanosRLEBoundaryExchanger)
typedef peano::heap::DoubleHeap<
    peano::heap::SynchronousDataExchanger< double, true,  peano::heap::SendReceiveTask<double> >,
    peano::heap::SynchronousDataExchanger< double, true,  peano::heap::SendReceiveTask<double> >,
    peano::heap::RLEBoundaryDataExchanger< double, false, peano::heap::SendReceiveTask<double> >
>     DataHeap;
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::RLEBoundaryDataExchanger< char, false, peano::heap::SendReceiveTask<char> >
>     CompressedDataHeap;
#elif !defined(ALIGNMENT) // Default: AggregationBoundaryDataExchanger
typedef peano::heap::DoubleHeap<
    peano::heap::SynchronousDataExchanger< double, true,  peano::heap::SendReceiveTask<double> >,
    peano::heap::SynchronousDataExchanger< double, true,  peano::heap::SendReceiveTask<double> >,
    peano::heap::AggregationBoundaryDataExchanger< double, peano::heap::SendReceiveTask<double>, std::vector<double> >
>     DataHeap;
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::AggregationBoundaryDataExchanger< char, peano::heap::SendReceiveTask<char>, std::vector<char> >
>     CompressedDataHeap;
#endif

/**
 * @return a data heap array as vector.
 *
 * @param index heap index of the array.
 */
DataHeap::HeapEntries& getDataHeapEntries(const int index);

const DataHeap::HeapEntries& getDataHeapEntriesForReadOnlyAccess(const int index);

/**
 * Moves a DataHeap array, i.e. copies the found
 * data at "fromIndex" to the array at "toIndex" and
 * deletes the "fromIndex" array afterwards.
 */
void moveDataHeapEntries(const int fromIndex,const int toIndex,bool recycleFromArray);

/**
 * @see waitUntilAllBackgroundTasksHaveTerminated()
 */
extern tarch::multicore::BooleanSemaphore ReductionSemaphore;

/**
 * A semaphore for serialising heap access.
 */
extern tarch::multicore::BooleanSemaphore HeapSemaphore;

#ifdef Parallel
/**
 * An empty DataHeap message.
 *
 * !!! CreateCopiesOfSentData
 *
 * If we have set CreateCopiesOfSentData to
 * false for the DataHeap, all messages need to
 * have a fixed address as long as the send
 * process takes.
 *
 * Has to be declared extern in C++ standard as
 * it is instantiated in the corresponding cpp file.
 */
extern DataHeap::HeapEntries EmptyDataHeapMessage;

/**
 * We abuse this heap to send and receive metadata from one MPI rank to the other.
 * We never actually store data on this heap.
 *
 * !!! CreateCopiesOfSentData
 *
 * It is assumed by the metadata send routines of the solvers that
 * all data exchangers of the MetadataHeap create copies of the data to send.
 *
 * <h2> Implementation </h2>
 *
 * These meta data are not symmetric, i..e we can use the Aggregation heap but we
 * may not use any symmetric heap.
 */
#if defined(UsePeanosSymmetricBoundaryExchangerForMetaData) and defined(UsePeanosRLEBoundaryExchangerForMetaData)
#error UsePeanosSymmetricBoundaryExchangerForMetaData and UsePeanosRLEBoundaryExchangerForMetaData must not be defined at the same time!
#endif

#if defined(UsePeanosSymmetricBoundaryExchangerForMetaData)
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SymmetricBoundaryDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >
>     MetadataHeap;
#elif defined(UsePeanosRLEBoundaryExchangerForMetaData)
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::RLEBoundaryDataExchanger< char, true, peano::heap::SendReceiveTask<char> >
>     MetadataHeap;
#else
typedef peano::heap::CharHeap<
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::SynchronousDataExchanger< char, true,  peano::heap::SendReceiveTask<char> >,
    peano::heap::AggregationBoundaryDataExchanger< char, peano::heap::SendReceiveTask<char>, std::vector<char> >
>     MetadataHeap;
#endif

/**
 * Defines an invalid metadata entry.
 */
static constexpr int InvalidMetadataEntry = -10;

/**
 * Defines the length of the metadata
 * we send out per solver.
 *
 * First entry is the cell (description) type.
 * Second entry is the augmentation status,
 * third the helper status and the fourth the
 * limiter status.
 */
static constexpr int NeighbourCommunicationMetadataPerSolver           = 4;

static constexpr int NeighbourCommunicationMetadataCellType            = 0;
static constexpr int NeighbourCommunicationMetadataAugmentationStatus  = 1;
static constexpr int NeighbourCommunicationMetadataCommunicationStatus = 2;
static constexpr int NeighbourCommunicationMetadataLimiterStatus       = 3;

static constexpr int MasterWorkerCommunicationMetadataPerSolver        = 5;

static constexpr int MasterWorkerCommunicationMetadataCellType            = 0;
static constexpr int MasterWorkerCommunicationMetadataAugmentationStatus  = 1;
static constexpr int MasterWorkerCommunicationMetadataCommunicationStatus = 2;
static constexpr int MasterWorkerCommunicationMetadataLimiterStatus       = 3;
static constexpr int MasterWorkerCommunicationMetadataSendReceiveData     = 4;
/**
 * TODO(Dominic): Docu is outdated
 *
 * Encodes the metadata as integer sequence.
 *
 * The first element refers to the number of
 * ADERDGCellDescriptions associated with this cell (nADERG).
 * The next 2*nADERG elements store a pair of
 * solver number, and cell description type (encoded as int)
 * for each ADERDGCellDescription associated with this cell (description).
 *
 * The element 1+2*nADERDG refers to the number of
 * FiniteVolumesCellDescriptions associated with this cell (nFV).
 * The remaining 2*nFV elements store a pair of
 * solver number, and cell description type (encoded as int)
 * for each FiniteVolumesCellDescription associated with this cell
 * (description).
 */
MetadataHeap::HeapEntries gatherNeighbourCommunicationMetadata(
    const int cellDescriptionsIndex,
    const tarch::la::Vector<DIMENSIONS,int>& src,
    const tarch::la::Vector<DIMENSIONS,int>& dest);

/**
 * Send metadata to rank @p toRank.
 */
void sendNeighbourCommunicationMetadata(
    const int                                   toRank,
    const int                                   cellDescriptionsIndex,
    const tarch::la::Vector<DIMENSIONS,int>&    src,
    const tarch::la::Vector<DIMENSIONS,int>&    dest,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level);

/**
 * Receive metadata to rank @p toRank.
 *
 * @note Clears and enlarges the buffer
 * if necessary.
 *
 * @param[in] doNotReceiveAndFillBufferWithInvalidEntries Within batches, we sometimes do not want to receive metadata.
 *
 * \return The index of the received metadata message
 * on the exahype::MetadataHeap.
 */
void receiveNeighbourCommunicationMetadata(
    MetadataHeap::HeapEntries&                  buffer,
    const int                                   fromRank,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level);

/**
 * Send a metadata sequence filled with InvalidMetadataEntry
 * to rank @p toRank.
 */
void sendNeighbourCommunicationMetadataSequenceWithInvalidEntries(
    const int                                   toRank,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level);

/**
 * Drop metadata sent by rank @p fromRank.
 */
void dropMetadata(
    const int                                   fromRank,
    const peano::heap::MessageType&             messageType,
    const tarch::la::Vector<DIMENSIONS,double>& x,
    const int                                   level);
#endif

namespace solvers {
class Solver;

typedef std::vector<Solver*> RegisteredSolversEntries;
/**
 * All the registered solvers. Has to be declared extern in C++ standard as
 * it is instantiated in the corresponding cpp file.
 */
extern std::vector<Solver*> RegisteredSolvers;
}
}

/**
 * Describes one solver.
 */
class exahype::solvers::Solver {
private:
  /**
   * Log device.
   */
  static tarch::logging::Log _log;

protected:
  void tearApart(int numberOfEntries, int normalHeapIndex, int compressedHeapIndex, int bytesForMantissa) const;
  void glueTogether(int numberOfEntries, int normalHeapIndex, int compressedHeapIndex, int bytesForMantissa) const;

public:
#ifdef USE_ITAC
  /**
   * These handles are used to trace solver events with Intel Trace Analyzer and Collector.
   */
  static int waitUntilCompletedLastStepHandle;
  static int ensureAllJobsHaveTerminatedHandle;
#endif


#ifdef Parallel
  /**
   * Tag used for master worker communication.
   */
  static int MasterWorkerCommunicationTag;
#endif

  /**
   * Default return value of function getElement(...)
   * If we do not find the element in a vector
   * stored at a heap address.
   */
  static constexpr int NotFound = -1;

  /**
   * An extensible structure linking to the data of a cell.
   * It is passed to all solver routines.
   */
  typedef struct CellInfo {
    const int _cellDescriptionsIndex= -1;
    peano::heap::RLEHeap<exahype::records::ADERDGCellDescription>::HeapEntries&        _ADERDGCellDescriptions;
    peano::heap::RLEHeap<exahype::records::FiniteVolumesCellDescription>::HeapEntries& _FiniteVolumesCellDescriptions;

    CellInfo(const CellInfo& cellInfo) :
      _cellDescriptionsIndex        (cellInfo._cellDescriptionsIndex        ),
      _ADERDGCellDescriptions       (cellInfo._ADERDGCellDescriptions       ),
      _FiniteVolumesCellDescriptions(cellInfo._FiniteVolumesCellDescriptions)
    {}

    CellInfo(const int cellDescriptionsIndex,void* ADERDGCellDescriptions,void* FiniteVolumesCellDescriptions) :
      _cellDescriptionsIndex(cellDescriptionsIndex),
      _ADERDGCellDescriptions       (*static_cast<peano::heap::RLEHeap<exahype::records::ADERDGCellDescription>::HeapEntries*>(ADERDGCellDescriptions)),
      _FiniteVolumesCellDescriptions(*static_cast<peano::heap::RLEHeap<exahype::records::FiniteVolumesCellDescription>::HeapEntries*>(FiniteVolumesCellDescriptions))
    {}

    CellInfo(const int cellDescriptionsIndex) :
      _cellDescriptionsIndex(cellDescriptionsIndex),
      _ADERDGCellDescriptions       (peano::heap::RLEHeap<exahype::records::ADERDGCellDescription>::getInstance().getData(_cellDescriptionsIndex)),
      _FiniteVolumesCellDescriptions(peano::heap::RLEHeap<exahype::records::FiniteVolumesCellDescription>::getInstance().getData(_cellDescriptionsIndex))
    {}

    /**
     * @return if no data was found for the cell.
     */
    bool empty() const {
      return _ADERDGCellDescriptions.empty() && _FiniteVolumesCellDescriptions.empty();
    }

    /**
     * @return the first cell description with the given @p solverNumber.
     *
     * @param cellDescriptions an ordered collection of cell descriptions
     * @param solverNumber     identification number of a solver
     */
    template <typename CellDescriptionHeapEntries>
    static int indexOfCellDescription(CellDescriptionHeapEntries& cellDescriptions,const int solverNumber) {
      int index = exahype::solvers::Solver::NotFound;
      for (unsigned int element = 0; element < cellDescriptions.size(); ++element) {
        if (cellDescriptions[element].getSolverNumber()==solverNumber) {
          index = element;
          break;
        }
      }
      return index;
    }

    /**
     * @return Index of an ADER-DG cell description or Solver::NotFound (-1).
     * @param  solverNumber identification number of a solver
     */
    int indexOfADERDGCellDescription(const int solverNumber) {
      return indexOfCellDescription(_ADERDGCellDescriptions,solverNumber);
    }
    /**
     * @return Index of an Finite Volumes cell description or Solver::NotFound (-1).
     * @param  solverNumber identification number of a solver
     */
    int indexOfFiniteVolumesCellDescription(const int solverNumber) {
      return indexOfCellDescription(_FiniteVolumesCellDescriptions,solverNumber);
    }

    /**
     * @return If there is any cell description which belogns to
     * the solver with @p solverNumber.
     *
     * @param solverNumber identification number of a solver
     */
    bool foundCellDescriptionForSolver(const int solverNumber) const {
      bool found = false;
      for (auto& p : _ADERDGCellDescriptions) {
        found |= p.getSolverNumber()==solverNumber;
      }
      for (auto& p : _FiniteVolumesCellDescriptions) {
        found |= p.getSolverNumber()==solverNumber;
      }
      return found;
    }

  } CellInfo;

  /**
   * This struct computes and stores some
   * commonly used indices when merging neighbouring
   * cells via a common interfaces.
   *
   * TODO(Dominic): Move to more appropriate place?
   */
  typedef struct InterfaceInfo {
    int _direction;     /*! coordinate direction the normal vector is aligned with (0->x,1->y,2->z) */
    int _orientation1;  /*! orientation of the normal vector from pos1's perspective (0/1 -> pointing in/out of element) */
    int _orientation2;  /*! orientation of the normal vector from pos2's perspective (0/1 -> pointing in/out of element) */
    int _faceIndex1;    /*! interface index from perspective of pos1 */
    int _faceIndex2;    /*! interface index from perspective of pos2 */
    int _faceIndexLeft; /*! interface index from perspective of the "left" cell, i.e. the cell with orientation=1 (normal vector points out) */
    int _faceIndexRight;/*! interface index from perspective of the "right" cell, i.e. the cell with orientation=0 (normal vector points in) */

    InterfaceInfo(const tarch::la::Vector<DIMENSIONS,int>& pos1,const tarch::la::Vector<DIMENSIONS,int>& pos2)
    :
      _direction     (tarch::la::equalsReturnIndex(pos1, pos2)),
      _orientation1  ((1 + pos2(_direction) - pos1(_direction))/2),
      _orientation2  (1-_orientation1),
      _faceIndex1    (2*_direction+_orientation1),
      _faceIndex2    (2*_direction+_orientation2),
      _faceIndexLeft (2*_direction+1),
      _faceIndexRight(2*_direction+0) {
      assertionEquals(tarch::la::countEqualEntries(pos1,pos2),DIMENSIONS-1);
    }

    std::string toString() {
      std::ostringstream stringstream;
      stringstream << "(";
      stringstream << "_direction="      << _direction;
      stringstream << ",_orientation1="   << _orientation1;
      stringstream << ",_orientation2="   << _orientation2;
      stringstream << ",_faceIndex1="     << _faceIndex1;
      stringstream << ",_faceIndex2="     << _faceIndex2;
      stringstream << ",_faceIndexLeft="  << _faceIndexLeft;
      stringstream << ",_faceIndexRight=" << _faceIndexRight;
      stringstream << ")";
      return stringstream.str();
    }
  } InterfaceInfo;

  /**
   * This struct computes and stores some
   * commonly used indices when merging a cell
   * with boundary data at a boundary face.
   *
   * TODO(Dominic): Move to more appropriate place?
   */
  typedef struct BoundaryFaceInfo {
    int _direction;    /*! coordinate direction the normal vector is aligned with (0->x,1->y,2->z) */
    int _orientation;  /*! orientation of the normal vector from posCell's perspective (0/1 -> pointing in/out of element) */
    int _faceIndex;    /*! interface index from perspective of posCell */

    BoundaryFaceInfo(const tarch::la::Vector<DIMENSIONS,int>& posCell,const tarch::la::Vector<DIMENSIONS,int>& posBoundary)
    :
      _direction  (tarch::la::equalsReturnIndex(posCell, posBoundary)),
      _orientation((1 + posBoundary(_direction) - posCell(_direction))/2),
      _faceIndex  (2*_direction+_orientation) {
      assertionEquals(tarch::la::countEqualEntries(posCell,posBoundary),DIMENSIONS-1);
    }

    std::string toString() {
      std::ostringstream stringstream;
      stringstream << "(";
      stringstream << "_direction="   << _direction;
      stringstream << ",_orientation=" << _orientation;
      stringstream << ",_faceIndex1="  << _faceIndex;
      stringstream << ")";
      return stringstream.str();
    }
  } BoundaryFaceInfo;

  /**
   * TrackGridStatistics is a flag from Peano that I "misuse" here as these
   * data also are grid statistics.
   */
#ifdef TrackGridStatistics
  static double PipedUncompressedBytes;
  static double PipedCompressedBytes;
#endif

  /** @name Global profiling options
   *
   * Global profiling options which are set via the parser.
   */
  ///@{
  /**
   * The solvers need to do adjust some operations slightly
   * when those are run multiple times after each other in isolation.
   */
  static bool ProfileUpdate;
  ///@}

  /** @name Global solver optimisations
   *
   * Global solver optimisations which are set via the parser.
   */
  ///@{

  /**
   * A flag indicating that only initial mesh refinement is used, i.e.
   * the mesh remains static.
   *
   * Multiple operations can then be omitted during the time stepping
   * iterations, e.g. backing up a previous solution for
   * the pure ADER-DG scheme, evaluating the refinement criterion,
   * computing a new time step size if the scheme is linear, ...
   */
  static bool OnlyInitialMeshRefinement;

  /**
   * Indicates that only static limiting is used.
   * In this case, the limiter criteria do not need
   * to be evaluated and no FV->DG projection needs to
   * be performed in troubled cells. This
   * makes the troubled cells pure FV cells.
   */
  static bool OnlyStaticLimiting;

  /**
   * A flag indicating we fuse the algorithmic
   * phases of all ADERDGSolver and
   * LimitingADERDGSolver instances.
   *
   */
  static bool FuseAllADERDGPhases;
  /**
   * This factor (alpha) is used to scale
   * the admissible time step size if the fused
   * time stepping (for nonlinear PDEs) scheme is reset.
   *
   * The fused times stepping time step size estimate is then reset to
   *
   * dt_est = alpha * dt_adm.
   */
  static double FusedTimeSteppingRerunFactor;
  /**
   * This factor (beta) is used in the fused time stepping (for nonlinear PDEs)
   * time step size estimate as follows:
   *
   * dt_est = 0.5 ( beta * dt_adm + dt_est ),
   *
   * i.e dt_est approaches beta * dt_adm over time.
   *
   * This adds additional numerical diffusion as
   * a smaller time step size is chosen as necessary.
   */
  static double FusedTimeSteppingDiffusionFactor;

  /**
   * The number of Prediction,PredictionRerun,PredictionOrLocalRecomputation<
   * and FusedTimeStep iterations we need to run per time step.
   */
  static int PredictionSweeps;

  /**
   * Set to 0 if no floating point compression is used. Is usually done in the
   * runner once at startup and from hereon is a read-only variable. The
   * subsequent field SpawnCompressionAsBackgroundThread has no semantics if
   * the present value is set to 0.
   */
  static double CompressionAccuracy;

  static bool SpawnCompressionAsBackgroundJob;

  /**
   * Maximum number of background job consumer tasks
   * which are allowed to run during the mesh traversal.
   *
   * Default is zero.
   */
  static int MaxNumberOfRunningBackgroundJobConsumerTasksDuringTraversal;
  /**
   * Set to true if the prediction, and the first and intermediate fused time steps in
   * a batch should be launched as background job.
   */
  static bool SpawnPredictionAsBackgroundJob;
  /**
   * Set to true if the update and last fused time step in a batch
   * should be launched as background job.
   */
  static bool SpawnUpdateAsBackgroundJob;
  /**
   * Set to true if the prolongation
   * should be launched as background job whenever possible.
   *
   * Requires that the prediction is launched as background job too.
   */
  static bool SpawnProlongationAsBackgroundJob;
  /**
   * Set to true if the mesh refinement iterations
   * should run background jobs whenever possible.
   */
  static bool SpawnAMRBackgroundJobs;

  /**
   * If this is set, we can skip sending metadata around during
   * batching iterations.
   */
  static bool DisableMetaDataExchangeInBatchedTimeSteps;
  /**
   * If this is set, we can skip Peano vertex neighbour exchange during batching iterations.
   */
  static bool DisablePeanoNeighbourExchangeInTimeSteps;
  ///@}

  enum class JobType { AMRJob, ReductionJob, EnclaveJob, SkeletonJob };

  enum class JobSystemWaitBehaviourType { ProcessAnyJobs, ProcessJobsWithSamePriority, OnlyPollMPI };

  /**
   * What to do whenever the job system needs to wait until a
   * job is completed.
   */
  static JobSystemWaitBehaviourType JobSystemWaitBehaviour;
  /**
   * \see ensureAllBackgroundJobsHaveTerminated
   */
  static std::atomic<int> NumberOfAMRBackgroundJobs;

  /**
   * Number of jobs spawned which perform a reduction.
   *
   * Reduction Jobs are spawned as high priority.
   * They might be enclave or skeleton jobs.
   */
  static std::atomic<int> NumberOfReductionJobs;

  /**
   * Number of background jobs spawned
   * from enclave cells.
   *
   * \see ensureAllBackgroundJobsHaveTerminated
   */
  static std::atomic<int> NumberOfEnclaveJobs;
  /**
   * Number of background jobs spawned
   * from skeleton cells, i.e. cells at parallel
   * or adaptivity boundaries.
   *
   * \see ensureAllBackgroundJobsHaveTerminated
   */
  static std::atomic<int> NumberOfSkeletonJobs;

  /**
   * The type of a solver.
   */
  enum class Type { ADERDG, FiniteVolumes, LimitingADERDG };

  /**
   * The time stepping mode.
   */
  enum class TimeStepping {
    /**
     * In the global time stepping mode, every cells works with the same time step.
     */
    Global,
    /**
     * In the fixed time stepping mode, we assume that each cell advanced in
     * time with the prescribed time step size. No CFL condition is checked.
     */
    GlobalFixed
    // Local, Anarchic
  };

  /**
   * The refinement control states
   * returned by the user functions.
   */
  enum class RefinementControl { Keep = 0, Refine = 1, Erase = 2 };

  /**
   * The limiter domain change that was detected after a solution
   * update or during the limiter status spreading.
   */
  enum class MeshUpdateEvent {
    /**
     * A regular change of the limiter domain
     * has occurred. This might be either no change at
     * all or a situation where a cell directly next to a
     * troubled cell has been newly marked as troubled.
     */
    None = 0,

        /**
         * The limiter domain of this solver changed in an irregular
         * fashion, i.e. a troubled cell appeared suddenly.
         * Its appearance was not anticipated
         *
         * During the consequent refinement
         * status spreading, if we observe that
         * we also need to update the mesh,
         * this event is changed to RefinementRequested.
         */
        IrregularLimiterDomainChange = 1,

        /**
         * Scenario 1:
         * A cell which is not directly next to a troubled cell
         * has newly been marked as troubled.
         * The cell is not on the finest mesh level.
         *
         * Scenario 2:
         * A cell of type Descendant/EmptyDescendant
         * was marked with LimiterStatus other than Ok.
         * The cell is on the finest mesh level.
         *
         * <h2>Consequences</h2>
         * The runner must then refine the mesh accordingly, and perform a
         * rollback in all cells to the previous solution. It computes
         * a new time step size in all cells. Next, it recomputes the predictor in all
         * cells, troubled or not. Finally, it reruns the whole ADERDG time step in
         * all cells, troubled or not.
         *
         * This can potentially be relaxed for anarchic time stepping where
         * each cell has its own time step size and stamp.
         */
        RefinementRequested = 2,

        /**
         * The initial mesh will be created.
         */
        InitialRefinementRequested = 3
  };


  /**
   * \return String representation of the meshUpdateEvent.
   */
  static std::string toString(const MeshUpdateEvent& meshUpdateEvent);

  /**
   * Converts LimiterDomainChange to its double equivalent.
   */
  static double convertToDouble(const MeshUpdateEvent& meshUpdateEvent);

  /**
   * Converts a double to a LimiterDomainChange.
   */
  static MeshUpdateEvent convertToMeshUpdateEvent(const double value);

  /**
   * \return the larger (cast to int) value of both events.
   */
  static MeshUpdateEvent mergeMeshUpdateEvents(
      const MeshUpdateEvent meshUpdateEvent1,const MeshUpdateEvent meshUpdateEvent2);

  /**
   * This struct is returned after the update or fusedTimeStep
   * methods are run.
   */
  typedef struct UpdateResult {
    double _timeStepSize                     = std::numeric_limits<double>::infinity();
    MeshUpdateEvent _meshUpdateEvent         = MeshUpdateEvent::None;

    UpdateResult() {}
  } UpdateResult;

  /**
   * This struct is used in the AMR context
   * to lookup a parent cell description and
   * for computing the subcell position of the child
   * with respect to this parent.
   *
   * TODO(Dominic): Move to more appropriate place?
   */
  typedef struct SubcellPosition {
    int parentCellDescriptionsIndex;
    int parentElement;
    tarch::la::Vector<DIMENSIONS,int> subcellIndex;
    int levelDifference;

    SubcellPosition() :
      parentCellDescriptionsIndex(),
      parentElement(NotFound),
      subcellIndex(-1),
      levelDifference(-1) {}

    void invalidate() {
      parentCellDescriptionsIndex = multiscalelinkedcell::HangingVertexBookkeeper::InvalidAdjacencyIndex;
      parentElement = NotFound;
      for (int i=0; i<DIMENSIONS; i++) {
        subcellIndex[i] = -1;
      }
      levelDifference = -1;
    }

    ~SubcellPosition() {}
  } SubcellPosition;

  /**
   * The augmentation control states.
   */
  enum class AugmentationControl {
    /**
     * Indicates that a spacetree cell is next to another spacetree cell
     * of type exahype::records::ADERDGCellDescription::Cell.
     */
    NextToCell = 0,
        /**
         * Indicates that a spacetree cell is next to another spacetree cell
         * of type exahype::records::ADERDGCellDescription::Ancestor or
         * exahype::records::ADERDGCellDescription::EmptyAncestor.
         */
        NextToAncestor = 1,
        /**
         * Indicates that a spacetree cell is both, next to another spacetree cell
         * of type exahype::records::ADERDGCellDescription::Ancestor or
         * exahype::records::ADERDGCellDescription::EmptyAncestor, and
         * a spacetree cell of type
         * exahype::records::ADERDGCellDescription::Cell.
         */
        NextToCellAndAncestor = 2,
        /**
         * Indicates that a spacetree cell is neither, next to another spacetree cell
         * of type exahype::records::ADERDGCellDescription::Ancestor or
         * exahype::records::ADERDGCellDescription::EmptyAncestor, nor
         * next to a spacetree cell of type exahype::records::ADERDGCellDescription::Cell.
         *
         * A cell of type exahype::records::ADERDGCellDescription::Descendant can then request erasing.
         * A cell of type exahype::records::ADERDGCellDescription::Cell does then not need
         * to request augmenting.
         */
        Default = 3
  };

  /**
   * @return if dynamic mesh refinement or limiting is used.
   */
  static bool areRollbacksPossible() {
    return !OnlyInitialMeshRefinement || !OnlyStaticLimiting;
  }

  /**
   * Checks if one of the solvers is of a certain
   * type.
   */
  static bool oneSolverIsOfType(const Type& type);

  /**
   * Run over all solvers and identify the minimal time stamp.
   */
  static double getMinTimeStampOfAllSolvers();

  /**
   * Run over all solvers and identify the minimal sum of minimal time stamp
   * plus the minimal time step size.
   *
   * The result is a lower bound of the minimum time stamp
   * that will be obtained in the following time step.
   */
  static double estimateMinNextSolverTimeStampOfAllSolvers();

  /**
   * Run over all solvers and identify the minimal time step size.
   */
  static double getMinTimeStepSizeOfAllSolvers();

  /**
   * Run over all solvers and identify the minimal time step size.
   */
  static double getMaxSolverTimeStepSizeOfAllSolvers();

  /**
   * Run over all solvers and identify the maximal time stamp.
   *
   * On the individual patches, we do use the min time stamp so
   * far, so the routine returns the maximum over all min solver
   * time stamps.
   */
  static double getMaxTimeStampOfAllSolvers();

  static bool allSolversUseTimeSteppingScheme(solvers::Solver::TimeStepping scheme);

  static double getCoarsestMaximumMeshSizeOfAllSolvers();
  static double getFinestMaximumMeshSizeOfAllSolvers();

  /**
   * Returns the coarsest level which holds patches of
   * a solver.
   *
   * @note It is very important that initSolvers
   * has been called on all solvers before this
   * method is used.
   *
   * @note That we start counting the mesh level
   * at 1. In a uniform mesh with level l, there
   * are thus 3^{DIMENSIONS*(l-1)} cells.
   */
  static int getCoarsestMeshLevelOfAllSolvers();

  /**
   * Returns the finest mesh level where a solver
   * has his uniform mesh.
   *
   * @note It is very important that initSolvers
   * has been called on all solvers before this
   * method is used.
   */
  static int getFinestUniformMeshLevelOfAllSolvers();

  /**
   * @return the maximum mesh level which might be occupied
   * by a solver's cells.
   */
  static int getMaximumAdaptiveMeshLevelOfAllSolvers();

  /**
   * Returns the coarsest mesh size a solver is actually
   * using.
   *
   * @note It is very important that initSolvers
   * has been called on all solvers before this
   * method is used.
   */
  static double getCoarsestMeshSizeOfAllSolvers();

  static const tarch::la::Vector<DIMENSIONS,double>& getDomainSize();
  static const tarch::la::Vector<DIMENSIONS,double>& getDomainOffset();

  /**
   * Loop over the solver registry and check if no solver
   * performs adaptive mesh refinement.
   */
  static bool allSolversPerformOnlyUniformRefinement();


  /**
   * Loop over the solver registry and check if one
   * of the solvers has requested a mesh update.
   */
  static bool oneSolverRequestedMeshRefinement();

  /**
   * Returns true if one of the solvers used a time step size
   * that violated the CFL condition.
   *
   * TODO(Dominic): Rename. Name can be confused with
   * oneSolverHasNotAttainedStableState.
   */
  static bool oneSolverViolatedStabilityCondition();

  /*
   * Check if a solver requested limiter status spreading.
   * Such a request might stem from a limiting ADERDGSolver which
   * has requested mesh refinement or a local
   * or global recomputation.
   */
  static bool oneSolverRequestedRefinementStatusSpreading();

  /*
   * Check if a solver requested local recomputation
   * recomputation.
   */
  static bool oneSolverRequestedLocalRecomputation();

  /*
   * Check if a solver requested either global
   * recomputation.
   */
  static bool oneSolverRequestedGlobalRecomputation();

  /**
   * Loops over all registered LimitingADERDGSolver instances
   * and determines the maximum value of their
   * minimum limiter status for a troubled cell.
   *
   * This value determines how long we have to perform
   * limiter status spreading.
   *
   * The minimum possible return value is three.
   */
  static int getMaxRefinementStatus();

  /**
   * Specify if solvers spawn background jobs and
   * configure the number of sweeps run by the adapters FusedTimeStep, Prediction, PredictionRerun,
   * and PredictorOrLocalRecomputation.
   */
  static void configurePredictionPhase(const bool usePredictionBackgroundJobs, bool useProlongationBackgroundJobs);


  static std::string toString(const JobType& jobType);

  static int getNumberOfQueuedJobs(const JobType& jobType);


  /**
   * Ensure that all background jobs (such as prediction or compression jobs) have terminated before progressing
   * further. We have to wait until all tasks have terminated if we want to modify the heap,
   * i.e. insert new data or remove data.
   * Therefore, the wait (as well as the underlying semaphore) belong
   * into this abstract superclass.
   *
   * @param[in] backgroundJobCounter A reference to a background job counter.
   */
  static void ensureAllJobsHaveTerminated(JobType jobType);

  /**
   * Waits until the @p cellDescription has completed its time step.
   *
   * Thread-safety
   * -------------
   *
   * We only read (sample) the hasCompletedLastStep flag and thus do not need any locks.
   * If this flag were to assume an undefined state, this would happen after the job working processing the
   * cell description was completed. This routine will then do an extra iteration or finish.
   * Either is fine.
   *
   * The flag is modified before a job is spawned and after it was processed.
   * As the job cannot be processed before it is spawned, setting the flag
   * is thread-safe.
   *
   * MPI
   * ---
   *
   * Tries to receive dangling MPI messages while waiting if this
   * is specified by the user.
   *
   * Work Stealing
   * -------------
   *
   * Assume this rank has stolen jobs from another rank.
   * If this routine actually waits, this indicates it has to wait for a local job
   * and not for a stolen one.
   * Therefore, we exclude stolen jobs from being processed by this routine.
   *
   * @note Only use receiveDanglingMessages=true if the routine
   * is called from a serial context.
   *
   * @param cellDescription a cell description
   * @param waitForHighPriorityJob a cell description's task was spawned as high priority job
   * @param receiveDanglingMessages receive dangling messages while waiting
   */
  template <typename CellDescription>
  void waitUntilCompletedLastStep(
      const CellDescription& cellDescription,const bool waitForHighPriorityJob,const bool receiveDanglingMessages) {
    #ifdef USE_ITAC
    VT_begin(waitUntilCompletedLastStepHandle);
    #endif

    if ( !cellDescription.getHasCompletedLastStep() ) {
      peano::datatraversal::TaskSet::startToProcessBackgroundJobs();
    }
    while ( !cellDescription.getHasCompletedLastStep() ) {
      #ifdef Parallel
      {
        tarch::multicore::RecursiveLock lock( tarch::services::Service::receiveDanglingMessagesSemaphore );
        tarch::parallel::Node::getInstance().receiveDanglingMessages();
        lock.free();
      }
      #endif

      switch ( JobSystemWaitBehaviour ) {
      case JobSystemWaitBehaviourType::ProcessJobsWithSamePriority:
        tarch::multicore::jobs::processBackgroundJobs( 1, getTaskPriority(waitForHighPriorityJob) );
        break;
      case JobSystemWaitBehaviourType::ProcessAnyJobs:
        tarch::multicore::jobs::processBackgroundJobs( 1 );
        break;
      default:
        break;
      }
    }

    #ifdef USE_ITAC
    VT_end(waitUntilCompletedLastStepHandle);
    #endif
  }

  /**
   * @return the default priority.
   */
  static int getDefaultTaskPriority() {
    return tarch::multicore::DefaultPriority;
  }
  /**
   * @return a high priority.
   */
  static int getHighPriorityTaskPriority() {
    return tarch::multicore::DefaultPriority*2;
  }
  /**
   * @return a high priority if the argument is set to true. Otherwise,
   * the default priority.
   */
  static int getTaskPriority( const bool isHighPriorityJob ) {
    return isHighPriorityJob ? getHighPriorityTaskPriority() : getDefaultTaskPriority();
  }
  /**
   * @return a very high priority.
   */
  static int  getCompressionTaskPriority() {
    return tarch::multicore::DefaultPriority*8;
  }

  /**
   * Return a string representation for the type @p param.
   */
  static std::string toString(const exahype::solvers::Solver::Type& param);

  /**
   * Return a string representation for the time stepping mode @p param.
   */
  static std::string toString(const exahype::solvers::Solver::TimeStepping& param);

  /**
   * @return mesh resolution and mesh level (incremented by 1) such that
   * @p boundingBoxSize / 3^level <= @p meshSize.
   *
   * @note The domain root cell is actually at Peano mesh level 1
   * as the domain itself is embedded in a 3^d mesh in Peano.
   *
   * @note Load balancing makes only sense for a Peano mesh with
   * at least 3 (Peano) levels. This is not ensured or checked in this routine.
   *
   * @param meshSize        the coarsest allowed mesh size.
   * @param boundingBoxSize size of the bounding box.
   */
  static std::pair<double,int> computeCoarsestMeshSizeAndLevel(double meshSize, double boundingBoxSize);

protected:

  /**
   * Each solver has an identifier/name. It is used for debug purposes only.
   */
  const std::string _identifier;

  const Type _type;

  /**
   * The number of state variables of the conservation or balance law.
   */
  const int _numberOfVariables;

  /**
   * The number of parameters, e.g, material parameters.
   */
  const int _numberOfParameters;

  /**
   * The number of global observables, e.g. indicators used by AMR.
   */
  const int _numberOfGlobalObservables ;

  /**
   * The number of nodal basis functions that are employed in each
   * coordinate direction.
   */
  const int _nodesPerCoordinateAxis;

  /**
   * The offset of the computational domain.
   *
   * Is initialised by the initSolver method.
   */
  tarch::la::Vector<DIMENSIONS,double> _domainOffset;

  /**
   * The size of the computational domain.
   * * Is initialised by the initSolver method.
   */
  tarch::la::Vector<DIMENSIONS,double> _domainSize;

  /**
   * The maximum extent a cell is allowed to have in each coordinate direction.
   *
   * @note This is an upper bound specified in the specification file.
   * This is not the actual maximum extent of a cell.
   */
  const double _maximumMeshSize;

  /**
   * The maximum depth the adaptive mesh is allowed
   * to occupy (set by the user).
   * Summing this value with _coarsestMeshdLevel results in
   * the finest mesh level the solver might occupy during the
   * simulation.
   */
  const int _maximumAdaptiveMeshDepth;

  /**
   * The time stepping mode of this solver.
   */
  const TimeStepping _timeStepping;


  /**
   * The coarsest level of the adaptive mesh that is
   * occupied by this solver.
   *
   * @note Is set by initSolver(...) function
   */
  int _coarsestMeshLevel;

  /**
   * The reduced global observables over the entire domain.
   */
  std::vector<double> _globalObservables;

  /*
   * The coarsest mesh size this solver is using, i.e.
   * the mesh size chosen for the uniform base grid.
   *
   * @note Is set by initSolver(...) function
   */
  double _coarsestMeshSize;

  /**
   * A profiler for this solver.
   */
  std::unique_ptr<profilers::Profiler> _profiler;

public:
  Solver(const std::string& identifier, exahype::solvers::Solver::Type type,
      int numberOfVariables, int numberOfParameters,
      int numberOfGlobalObservables,
      int nodesPerCoordinateAxis,
      double maximumMeshSize,
      int maximumAdaptiveMeshDepth,
      exahype::solvers::Solver::TimeStepping timeStepping,
      std::unique_ptr<profilers::Profiler> profiler =
          std::unique_ptr<profilers::Profiler>(
              new profilers::simple::NoOpProfiler("")));

  virtual ~Solver() { _profiler->writeToConfiguredOutput(); }

  // Disallow copy and assignment
  Solver(const Solver& other) = delete;
  Solver& operator=(const Solver& other) = delete;

  virtual std::string toString() const;

  virtual void toString(std::ostream& out) const;

  /**
   * Returns the maximum extent a mesh cell is allowed to have
   * in all coordinate directions.
   * This maximum mesh size is used both as a
   * constraint on the AMR as well as to set up the initial
   * grid. If you return the extent of the computational domain in
   * each coordinate direction or larger values,
   * you indicate that this solver is not active in the domain.
   *
   * @note This is just an upper bound on the coarsest mesh
   * size. For the actual coarsest mesh size, see method
   * getCoarsestMeshSize().
   *
   * TODO(Dominic): Rename to getUserMeshSize()?
   */
  double getMaximumMeshSize() const;

  /**
   * The coarsest level of the adaptive mesh that is
   * occupied by this solver.
   *
   * @note Only safe to use after initSolver(...) was called.
   */
  int getCoarsestMeshLevel() const;

  /**
   * The coarsest mesh size this solver is using,
   * i.e. the size of the cells on the uniform base grid.
   *
   * @note Only safe to use after initSolver(...) was called.
   *
   * @note This is not not the maximumMeshSize specified by the user.
   */
  double getCoarsestMeshSize() const;

  /**
   * The maximum depth the adaptive mesh is allowed to
   * occupy (set by the user).
   * Summing this value with _coarsestMeshdLevel results in
   * the finest mesh level the solver might occupy during the
   * simulation.
   */
  int getMaximumAdaptiveMeshDepth() const;

  /**
   * The finest level of the adaptive mesh that might be
   * occupied by this solver.
   */
  int getMaximumAdaptiveMeshLevel() const;

  /**
   * Returns the identifier of this solver.
   */
  std::string getIdentifier() const;

  /**
   * Returns the type of this solver.
   */
  Type getType() const;

  /**
   * Returns the time stepping algorithm this solver is using.
   */
  TimeStepping getTimeStepping() const;

  /**
   * Returns the number of state variables.
   */
  int getNumberOfVariables() const;

  /**
   * Returns the number of parameters, e.g.,material constants etc.
   */
  int getNumberOfParameters() const;

  /**
   * Returns the number of global observables, e.g. indicators for AMR.
   */
  int getNumberOfGlobalObservables() const;

  /**
   * If you use a higher order method, then this operation returns the
   * polynomial degree plus one. If you use a Finite Volume method, it
   * returns the number of cells within a patch per coordinate axis.
   */
  int getNodesPerCoordinateAxis() const;

  /**
   * \see mergeMeshUpdateEvents
   *
   * @note Implementation must ensure thread-safety!
   */
  virtual void updateMeshUpdateEvent(MeshUpdateEvent meshUpdateEvent) = 0;

  /**
   * Sets the _nextMeshUpdateEvent as this solver's
   * current event. Furthermore resets the
   * _nextMeshUpdateEvent variable.
   */
  virtual void resetMeshUpdateEvent() = 0;
  /**
   * \return the currently set mesh update event.
   */
  virtual MeshUpdateEvent getMeshUpdateEvent() const = 0;

  /**
   * \return true if the current mesh update event
   * is either RefinementRequested or InitialRefinementRequested.
   */
  bool hasRequestedAnyMeshRefinement() const;
  /**
   * \return minimum time stamp of all cell descriptions.
   */
  virtual double getMinTimeStamp() const = 0;

  /**
   * \return minimum time step size of all cell descriptions computed during the last time step or
   * after the last mesh refinement.
   */
  virtual double getMinTimeStepSize() const = 0;

  /**
   * Update the admissible time step size which is computed
   * as the minium of the admissible time step size of all cells.
   *
   * @param value a value the current minium is compared against.
   *
   * @note Implementation must ensure thread-safety!
   */
  virtual void updateAdmissibleTimeStepSize(double value) = 0;

  /**
   * @return the admissible time step size which is only
   * available at the end of a time step.
   *
   * @note Access is not thread-safe.
   */
  virtual double getAdmissibleTimeStepSize() const=0;

  /**
   * Reset the admissible time step size to infinity in order
   * to search for a minimum over all cells.
   */
  virtual void resetAdmissibleTimeStepSize() = 0;

  // TODO(Lukas) Is this still needed?
  /*
  virtual void updateNextGlobalObservables(const std::vector<double>& globalObservables);
   */

  virtual std::vector<double>& getGlobalObservables();
  // TODO(Lukas) Is this still needed?
  /*
  virtual std::vector<double>& getNextGlobalObservables();
   */


  /**
   * Initialise the solver's time stamps and time step sizes.
   * Further use the bounding box and the already known
   * maximum mesh size to compute the coarsest grid level
   * this solver is placed on.
   *
   * @note It is very important that the domainSize
   * is chosen as an multiple of the coarsest mesh size
   * of all solvers within the grid.
   *
   * The maximum adaptive refinement level is defined
   * with respect to this level.
   *
   * @param timeStamp            the initial time stamp.
   * @param domainOffset         offset of the domain.
   * @param domainSize           size of the domain.
   * @param boundingBoxSize      size of the bounding box.
   * @param cellsOutsideOfDomainPerDimension cells which are placed outside of the domain due to bounding box scaling.
   * @param cmdlineargs          command line arguments.
   * @param parserView           view on the specification file for the solver.
   */
  virtual void initSolver(
      const double timeStamp,
      const tarch::la::Vector<DIMENSIONS,double>& domainOffset,
      const tarch::la::Vector<DIMENSIONS,double>& domainSize,
      const double                                boundingBoxSize,
      const double                                boundingBoxMeshSize,
      const std::vector<std::string>&             cmdlineargs,
      const exahype::parser::ParserView&          parserView) = 0;

  /**
   * Notify the solver that a time step just started.
   *
   * @param isFirstTimeStepOfBatchOrNoBatch  if this is the first time step of a batch or no batch is run.
   */
  virtual void kickOffTimeStep(const bool isFirstTimeStepOfBatchOrNoBatch) = 0;

  /**
   * Notify the solver that a time step just finished.
   *
   * @param isFirstTimeStepOfBatchOrNoBatch  if this is the first time step of a batch or no batch is run.
   * @param isLastTimeStepOfBatchOrNoBatch   if this is the last time step of a batch or if no batch is run.
   */
  virtual void wrapUpTimeStep(const bool isFirstTimeStepOfBatchOrNoBatch,const bool isLastTimeStepOfBatchOrNoBatch) = 0;

  /**
   * \return true if the solver is computing in the current algorithmic section.
   * This depends usually on internal flags of the solver such as ones indicating
   * a mesh update request or a limiter domain change during a previous time stepping
   * iteration.
   *
   * All mappings introduced for a specific job, e.g. limiting, mesh refinement etc.,
   * do not rely on this method. It is used only for mappings which are shared by different
   * algorithm sections. These are
   * BroadcastAndMergeTimeStepData, Merging, Prediction, and TimeStepSizeComputation,
   * MeshRefinement (+FinaliseMeshRefinement)
   *
   * E.g. a time step size computation via mapping TimeStepSizeComputation is required in
   * algorithm section "LocalRecomputationAllSend" for all solvers which
   * have finished a global recomputation or a mesh refinement.
   * It is not required for limiting ADER-DG solvers which are currently performing
   * a local recomputation.
   *
   */
  virtual bool isPerformingPrediction(const exahype::State::AlgorithmSection& section) const = 0;

  /**
   * \return true if this solver needs to merge metadata only(!) in the current algorithm section.
   */
  virtual bool isMergingMetadata(const exahype::State::AlgorithmSection& section) const = 0;

  /**
   * In contrast to startNewTimeStep(), this
   * method does not shift the time stamp.
   *
   * It simply updates the time step size.
   *
   * This method is used after a mesh refinement.
   */
  virtual void updateTimeStepSize() = 0;

  /**
   * Roll back the minimum time stamp to the one of
   * the previous time step.
   *
   * @note This is a global rollback which is performed happens after mesh refinement.
   * The time step size then needs to be recomputed. The old one is thus not restored.
   */
  virtual void rollbackToPreviousTimeStep() = 0;
  /**
   * If an entry for this solver exists,
   * return the element index of the cell description
   * in the array at address @p cellDescriptionsIndex.
   * Otherwise and if @p cellDescriptionsIndex is an
   * invalid index, return Solver::NotFound.
   */
  virtual int tryGetElement(
      const int cellDescriptionsIndex,
      const int solverNumber) const = 0;

  /**
   * Modify a cell description in enter cell event.
   * This event should be used for single cell operations
   * like marking for refinement, erasing, augmenting,
   * or deaugmenting.
   *
   * \return a struct of type bool.
   *
   * @note We use this at the moment only
   * for refinement events. We can consider later
   * on to merge the time stepping functionality
   * (solution update, predictor comp.) into
   * this hook.
   */
  virtual bool progressMeshRefinementInEnterCell(
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const int  solverNumber,
      const bool stillInRefiningMode) = 0;

  /**
   * Refinement routine that should be used for
   * collective children-parent operations.
   *
   * \return If a new compute cell was introduced
   * as part of a refinement operation.
   */
  virtual bool progressMeshRefinementInLeaveCell(
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const tarch::la::Vector<DIMENSIONS, int>& fineGridPositionOfCell,
      const int solverNumber,
      const bool stillInRefiningMode) = 0;

  /**
   * \return if the vertices around a cell should be erased, kept,
   * or refined.
   *
   * @param checkThoroughly If set to true, check that the indices found in the
   *                        adjacency map are actual heap indices and that the
   *                        geometry information of the cell descriptions found at
   *                        the heap index indicates that these cells are adjacent
   *                        to the vertex.
   */
  virtual exahype::solvers::Solver::RefinementControl eraseOrRefineAdjacentVertices(
      const int cellDescriptionsIndex,
      const int solverNumber,
      const tarch::la::Vector<DIMENSIONS, double>& cellOffset,
      const tarch::la::Vector<DIMENSIONS, double>& cellSize,
      const int level,
      const bool checkThoroughly) const = 0;

  /**
   * Returns true if the solver has attained
   * a stable state on the cell description
   *
   * @param fineGridCell               a fine grid cell
   * @param fineGridVertices           vertices surrounding the fine grid cell
   * @param fineGridVerticesEnumerator a enumerator for the fine grid vertices
   * @param solverNumber               a solver number
   * @param stillInRefiningMode        indicates if the mesh refinement
   *                                   is still in refining mode (true) or switched to coarsening mode (false).
   */
  virtual bool attainedStableState(
      exahype::Cell&                       fineGridCell,
      exahype::Vertex* const               fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      const int                            solverNumber,
      const bool                           stillInRefiningMode) const = 0;

  /**
   * This method is called after the
   * mesh refinement iterations where this
   * solver performs states updates in
   * the enterCell() and leaveCell().
   *
   * This method is used to finalise some state
   * updates or prepare some states for
   * the time stepping or the next
   * mesh update iterations.
   */
  virtual void finaliseStateUpdates(
      const int solverNumber,
      CellInfo& cellInfo) = 0;

  /////////////////////////////////////
  // CELL-LOCAL
  /////////////////////////////////////

  /**
   * Computes a new time step size and overwrites
   * a cell description's time stamps and time step sizes
   * with it.
   *
   * In contrast to startNewTimeStep(int,int), this
   * method does not shift time stamps.
   *
   * This method is usually called after mesh refinement
   * was performed.
   *
   * @note Has no const modifier since kernels are not const functions yet.
   */
  virtual double updateTimeStepSize(const int solverNumber,CellInfo& cellInfo) = 0;

  /**
   * Impose initial conditions and mark for refinement.
   *
   * @note Make sure to reset neighbour merge
   * helper variables in this method call.
   *
   * @note Has no const modifier since kernels are not const functions yet.
   */
  virtual void adjustSolutionDuringMeshRefinement(const int solverNumber,CellInfo& cellInfo) = 0;

  /**
   * Fuse algorithmic phases of the solvers.
   *
   * FiniteVolumesSolver:
   *
   * This call degenerates to an updateSolution
   * call for the FiniteVolumesSolver.
   *
   * ADERDGSolver:
   *
   * Runs the triad of updateSolution,performPredictionAndVolumeIntegral
   * plus startNewTimeStep.
   *
   * LimitingADERDGSolver:
   *
   * Either runs the ADERDGSolver triad or
   * performs an FV update. Performs some additional
   * tasks.
   *
   * Compression
   * -----------
   *
   * Uncompresses the data before performing PDE operations
   * Compresses the data after performing PDE operations.
   *
   * Background Jobs
   * ---------------
   *
   * The FiniteVolumesSolver, ADERDGSolver and LimitingADERDGSolver implementations
   * show the following behaviour:
   *
   *   - If exahype::solvers::Solver::SpawnPredictionAsBackgroundJob is set to false,
   *     this function will not spawn any background jobs.
   *
   *   - If exahype::solvers::Solver::SpawnPredictionAsBackgroundJob is set to true:
   *
   *     - This function will spawn a FusedTimeStepJob in intermediate batch iterations.
   *       Here, it distinguishes between skeleton and enclave cells.
   *       ADERDGSolver and LimitingADERDGSolver variants do not spawn a PredictionJob in this case.
   *
   *     - This function will not a spawn a FusedTimeStepJob in the first and last iteration of
   *       a batch. ADERDGSolver and LimitingADERDGSolver may still spawn a PredictionJob in this case.
   *
   * @param[in] isFirstIterationOfBatch Indicates that we currently run no batch or
   *                                    we are in the first iteration of a batch.
   * @param[in] isLastIterationOfBatch  Indicates that we currently run no batch or
   *                                    we are in the last iteration of a batch.
   *                                    (If no batch is run, both flags
   *                                    @p isFirstIterationOfBatch and
   *                                    @p isLastIterationOfBatch are true).
   * @param[in] isAtRemoteBoundary Flag indicating that the cell hosting the
   *                                    cell description is adjacent to a remote rank.
   */
  virtual UpdateResult fusedTimeStepOrRestrict(
      const int  solverNumber,
      CellInfo&  cellInfo,
      const bool isFirstIterationOfBatch,
      const bool isLastIterationOfBatch,
      const bool isAtRemoteBoundary) = 0;

  /**
   * The nonfused update routine.
   *
   * FiniteVolumesSolver:
   *
   * This call degenerates to an updateSolution
   * call and a startNewTimeStep call for the FiniteVolumesSolver.
   *
   * ADERDGSolver:
   *
   * Update the solution and evaluate the refinement criterion.
   *
   * LimitingADERDGSolver:
   *
   * Update the ADER-DG and or FV solution and
   * evaluate the limiter and
   * the refinement criteria.
   *
   * @note Make sure to reset neighbour merge
   * helper variables in this method call.
   *
   * @note Has no const modifier since kernels are not const functions yet.
   *
   * @param cellInfo           links to the data associated with the mesh cell
   * @param solverNumber       id of a solver
   * @param isAtRemoteBoundary indicates if this cell is adjacent to the domain of another rank
   * @return see UpdateResult
   */
  virtual UpdateResult updateOrRestrict(
      const int solverNumber,
      CellInfo& cellInfo,
      const bool isAtRemoteBoundary) = 0;

  /**
   * Go back to previous time step with
   * time step data and solution.
   *
   * Keep the new refinement status.
   *
   * Allocate necessary new limiter patches.
   */
  virtual void rollbackSolutionGlobally(const int solverNumber,CellInfo& cellInfo) const = 0;

  /**
   * Explicitly ask the solver to compress
   * a cell description.
   *
   * @param[in] isAtRemoteBoundary Flag indicating that the cell hosting the
   *                               cell description is adjacent to a remote rank.
   */
  virtual void compress(
      const int solverNumber,
      CellInfo& cellInfo,
      const bool isAtRemoteBoundary) const = 0;

  ///////////////////////////////////
  // NEIGHBOUR
  ///////////////////////////////////

#ifdef Parallel
  /**
   * On coarser grids, the solver can hint on the eventual load and memory distribution
   * with this function.
   *
   * @note Only called on coarser grids by LoadBalancing mapping.
   * @note Only invokes user callback during initial mesh refinement.
   *
   * @param  cellCentre the cell centre.
   * @param  cellSize   the cell size.
   * @return Estimate of the load based on the geometry.
   */
  int computeGeometricLoadBalancingWeight(
      const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>& cellSize);

  /**
   * If a cell description was allocated at heap address @p cellDescriptionsIndex
   * for solver @p solverNumber, encode metadata of the cell description
   * and push it to the back of the metadata vector @p metadata.
   *
   * Otherwise, push exahype::NeighbourCommunicationMetadataPerSolver
   * times exahype::InvalidMetadataEntry to the back of the vector.
   */
  virtual void appendNeighbourCommunicationMetadata(
      MetadataHeap::HeapEntries& metadata,
      const tarch::la::Vector<DIMENSIONS,int>& src,
      const tarch::la::Vector<DIMENSIONS,int>& dest,
      const int cellDescriptionsIndex,
      const int solverNumber) const = 0;

  ///////////////////////////////////
  // WORKER<=>MASTER
  ///////////////////////////////////
  /**
   * Finishes outstanding refinement operations
   * and sends solution data down to the worker
   * if required.
   */
  virtual void progressMeshRefinementInPrepareSendToWorker(
      const int workerRank,
      exahype::Cell& fineGridCell,
      exahype::Vertex* const fineGridVertices,
      const peano::grid::VertexEnumerator& fineGridVerticesEnumerator,
      exahype::Cell& coarseGridCell,
      const peano::grid::VertexEnumerator& coarseGridVerticesEnumerator,
      const int solverNumber) = 0;

  /**
   * Just receive data or not from the master
   * depending on the refinement event.
   */
  virtual void sendDataToWorkerIfProlongating(
      const int                                     toRank,
      const int                                     cellDescriptionsIndex,
      const int                                     element,
      const tarch::la::Vector<DIMENSIONS, double>&  x,
      const int                                     level) const = 0;

  /**
   * Just receive data or not from the master
   * depending on the refinement event.
   */
  virtual void receiveDataFromMasterIfProlongating(
      const int masterRank,
      const int receivedCellDescriptionsIndex,
      const int receivedElement,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int level) const = 0;

  /**
   * Finish prolongation operations started on the master.
   */
  virtual bool progressMeshRefinementInMergeWithWorker(
      const int localCellDescriptionsIndex,
      const int receivedCellDescriptionsIndex, const int receivedElement) = 0;

  /**
   * Finish erasing operations on the worker side and
   * send data up to the master if necessary.
   * This data is then picked up to finish restriction
   * operations.
   */
  virtual void progressMeshRefinementInPrepareSendToMaster(
      const int masterRank,
      const int cellDescriptionsIndex, const int element,
      const tarch::la::Vector<DIMENSIONS,double>& x,
      const int level) const = 0;

  /**
   * Finish erasing operations started on the master which
   * require data from the worker.
   *
   * Veto erasing requests from the coarse grid cell as well.
   */
  virtual bool progressMeshRefinementInMergeWithMaster(
      const int worker,
      const int localCellDescriptionsIndex,
      const int localElement,
      const int coarseGridCellDescriptionsIndex,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level,
      const bool                                   stillInRefiningMode) = 0;

  /**
   * If a cell description was allocated at heap address @p cellDescriptionsIndex
   * for solver @p solverNumber, encode metadata of the cell description
   * and push it to the back of the metadata vector @p metadata.
   *
   * Otherwise, push exahype::MasterWorkerCommunicationMetadataPerSolver
   * times exahype::InvalidMetadataEntry to the back of the vector.
   *
   */
  virtual void appendMasterWorkerCommunicationMetadata(
      MetadataHeap::HeapEntries& metadata,
      const int cellDescriptionsIndex,
      const int solverNumber) const = 0;

  /**
   * Send solver data to master or worker rank. Read the data from
   * the cell description @p element in
   * the cell descriptions vector stored at @p
   * cellDescriptionsIndex.
   *
   * @param[in] element Index of the cell description
   *                    holding the data to send out in
   *                    the array with address @p cellDescriptionsIndex.
   *                    This is not the solver number.
   */
  virtual void sendDataToWorkerOrMasterDueToForkOrJoin(
      const int                                    toRank,
      const int                                    cellDescriptionsIndex,
      const int                                    element,
      const peano::heap::MessageType&              messageType,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const = 0;

  /**
   * Merge with solver data from master or worker rank
   * that was sent out due to a fork or join. Wrote the data to
   * the cell description @p element in
   * the cell descriptions vector stored at @p
   * cellDescriptionsIndex.
   *
   * @param[in] element Index of the cell description
   *                    holding the data to send out in
   *                    the array with address @p cellDescriptionsIndex.
   *                    This is not the solver number.
   */
  virtual void mergeWithWorkerOrMasterDataDueToForkOrJoin(
      const int                                    fromRank,
      const int                                    cellDescriptionsIndex,
      const int                                    element,
      const peano::heap::MessageType&              messageType,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const = 0;

  ///////////////////////////////////
  // WORKER->MASTER
  ///////////////////////////////////

  /**
   * Send data to the master that is not
   * depending on a particular cell description.
   *
   * This operation might be used for the reduction of a global
   * minimum time step size over all MPI ranks.
   *
   * @note We always assume that
   * startNewTimeStep() has been already called on the
   * local solver instance. You thus
   * have to return the updated local time step size.
   *
   * \see startNewTimeStep(), mergeWithWorkerData()
   */
  virtual void sendDataToMaster(
      const int                                    masterRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const = 0;

  /**
   * Merge with solver data from worker rank.
   */
  virtual void mergeWithWorkerData(
      const int                                    workerRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) = 0;

  /*
   * Send the rank-local mesh update event to
   * the master.
   *
   * At the time of sending data to the master,
   * we have already set the next
   * mesh update event locally.
   * We thus need to communicate the
   * current mesh update event to the master.
   */
  void sendMeshUpdateEventToMaster(
      const int                                    masterRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const;

  /**
   * Merge with the worker's mesh update event.
   *
   * The master has not yet swapped
   * the current event with the next event yet.
   * This will happen after the merge.
   */
  void mergeWithWorkerMeshUpdateEvent(
      const int                                    workerRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level);

  ///////////////////////////////////
  // MASTER->WORKER
  ///////////////////////////////////
  /**
   * Send data to the worker that is not
   * depending on a particular cell description.
   *
   * This operation might be used for the synchronisation
   * of a global minimum time step size over all MPI ranks.
   *
   * \see startNewTimeStep(), mergeWithMasterData()
   */
  virtual void sendDataToWorker(
      const int                                    workerRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) const = 0;

  /**
   * Merge with solver data from master rank.
   */
  virtual void mergeWithMasterData(
      const int                                    masterRank,
      const tarch::la::Vector<DIMENSIONS, double>& x,
      const int                                    level) = 0;
#endif


  /**
   * Maps the solution values Q to
   * the global observables.
   *
   * As we can observe all state variables,
   * we interpret an 'observable' here as
   * 'worthy to be observed'.
   *
   *\param[inout] globalObservables The mapped observables.
   *\param[in]    Q           The state variables.
   */
  virtual std::vector<double> mapGlobalObservables(const double* const Q,
      const tarch::la::Vector<DIMENSIONS,double>& dx) const = 0;

  /**
   * Resets the vector of global observables to some suitable initial value, e.g.
   * the smallest possible double if one wants to compute the maximum.
   *
   *\param[out] globalObservables The mapped observables.
   */
  virtual std::vector<double> resetGlobalObservables() const = 0;

  /**
   * Function that reduces the global observables.
   * For example, if one wants to compute the maximum of global variables
   * one should set
   * reducedGlobalObservables[0] = std::max(reducucedGlobalObservables[i],
   * curGlobalObservables[0])
   *
   * and so on.
   *
   *\param[inout] reducedGlobalObservables The reduced observables.
   *\param[in]    curGlobalObservables The current vector of global observables.
   */
  virtual void reduceGlobalObservables(
      std::vector<double>& reducedGlobalObservables,
      const std::vector<double>& curGlobalObservables) const = 0;

  virtual void reduceGlobalObservables(std::vector<double>& globalObservables,
      CellInfo cellInfo,
      int solverNumber) const = 0;
  ///////////////////////
  // PROFILING
  ///////////////////////


  /**
   * A struct holding averaged runtime measurements for different cell types.
   */
  typedef struct CellProcessingTimes {
    double _minTimePredictor    = std::numeric_limits<double>::quiet_NaN(); ///> The time (sec) required to run a single ADERDG space-time predictor Picard iteration.
    double _maxTimePredictor    = std::numeric_limits<double>::quiet_NaN(); ///> The time (sec) required to run order+1 ADERDG space-time predictor Picard iterations.
    double _timeADERDGUpdate    = std::numeric_limits<double>::quiet_NaN(); ///> The time (sec) to process a (pure) ADER-DG cell minus the predictor computation (plus evaluating the limiting criterion in the LimitingADERDGSolver case).
    double _timeADERDG2FVUpdate = std::numeric_limits<double>::quiet_NaN(); ///> The time (sec) to process an ADER-DG cell minus the predictor computation which additionally projects the DG solution into FV space (plus evaluating the limiting criterion), i.e. only one Picard iteration is used.
    double _timeFV2ADERDGUpdate = std::numeric_limits<double>::quiet_NaN(); ///> The time (sec) to process an FV cell which additionally projects the FV solution into DG space (plus evaluating the limiting criterion), i.e. only one Picard iteration is used.
    double _timeFVUpdate        = std::numeric_limits<double>::quiet_NaN(); ///> The time (sec) to process an FV cell (plus evaluating the limiting criterion in the LimitingADERDGsolver case).

    void toString(std::ostream& out,const double conversion=1.0,const int precision=8,std::string unit="sec",std::string prefix="") const {
      out.precision(precision);
      out << prefix << "minTimePredictor    = "<<std::setw(12)<<std::fixed<<_minTimePredictor   *conversion<<" "<<unit<<std::endl;
      out << prefix << "maxTimePredictor    = "<<std::setw(12)<<std::fixed<<_maxTimePredictor   *conversion<<" "<<unit<<std::endl;
      out << prefix << "timeADERDGUpdate    = "<<std::setw(12)<<std::fixed<<_timeADERDGUpdate   *conversion<<" "<<unit<<std::endl;
      out << prefix << "timeADERDG2FVUpdate = "<<std::setw(12)<<std::fixed<<_timeADERDG2FVUpdate*conversion<<" "<<unit<<std::endl;
      out << prefix << "timeFV2ADERDGUpdate = "<<std::setw(12)<<std::fixed<<_timeFV2ADERDGUpdate*conversion<<" "<<unit<<std::endl;
      out << prefix << "timeFVUpdate        = "<<std::setw(12)<<std::fixed<<_timeFVUpdate       *conversion<<" "<<unit<<std::endl;
    }
  } CellProcessingTimes;

  /**
   * We perform @p numberOfRuns runs per cell type.
   *
   * @note Must be called after exahype::solvers::Solver::initSolver(...)
   * was called for this solver. As we need to process the
   *
   * @note Precondition: SwitchOffNeighbourMergePerformedCheck must be set to true before
   * measuring the cell processing times.
   *
   * @note No const modifier as kernels are not const.
   *
   * @param numberOfRuns the number of measurements to perform for each cell type
   *
   * @return @see exahype::solvers::Solver::CellProcessingTimes
   */
  virtual CellProcessingTimes measureCellProcessingTimes(const int numberOfRuns=100) { return CellProcessingTimes(); }


  /** @defgroup userHooks User Hooks
   *  Hooks for user solvers
   *  @{
   */

protected:
  /**
   * On coarser grids, the solver can hint on the eventual load or memory distribution
   * with this function.
   *
   * @note Only called on coarser grids by LoadBalancing mapping.
   * @note Only invokes user callback during initial mesh refinement (time stamp = 0).
   * @note LimitingADERDGSolver will invoke the main solvers routine.
   *
   * @param  cellCentre the cell centre.
   * @param  cellSize   the cell size.
   * @return Estimate of the load based on the geometry.
   */
  virtual int getGeometricLoadBalancingWeight(
      const tarch::la::Vector<DIMENSIONS,double>& cellCentre,
      const tarch::la::Vector<DIMENSIONS,double>& cellSize) { return 1; }

public:
  /**
   * Signals a user solver that ExaHyPE just started a new time step.
   *
   * @param[in] minTimeStamp the minimum time stamp (over all cells)
   *
   * @note This function is invoked before the first predictor computation
   * when the non fused time stepping is run. Otherwise, it is invoked after
   * the first predictor computation. It will always be called before
   * "adjustSolution" is invoked.
   *
   * @note [MPI] Do not use collective MPI operations in here. Not all ranks call this function.
   *
   * @note [MPI] If you perform broadcasts via MPI primitives in here, ensure
   * that isFirstTimeStepOfBatchOrNoBatch is set to true in order to not synchronise
   * the MPI ranks during batched time steps.
   */
  virtual void beginTimeStep(const double minTimeStamp,const bool isFirstTimeStepOfBatchOrNoBatch) {}

  /**
   * Signals a user solver that ExaHyPE just finished a time step.
   *
   * @param[in] minTimeStamp the minimum time stamp (over all cells)
   *
   * @note This function is invoked after the solution was updated in all
   * cells.
   *
   * @note [MPI] Do not use collective MPI operations in here. Not all ranks call this function.
   *
   * @note [MPI] If you perform reductions via MPI primitives in here, ensure
   * that isLastTimeStepOfBatchOrNoBatch is set to true in order to not synchronise
   * the MPI ranks during batched time steps.
   *
   * @param isFirstTimeStepOfBatchOrNoBatch  if this is the first time step of a batch or no batch is run.
   * @param isLastTimeStepOfBatchOrNoBatch   if this is the last time step of a batch or if no batch is run.
   */
  virtual void endTimeStep(const double minTimeStamp,const bool isLastTimeStepOfBatchOrNoBatch)   {}
  /** @} */ // end of userHooks
};

#endif
