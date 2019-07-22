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
 * @authors: Sven Koeppel
 **/

#ifndef _EXAHYPE_PLOTTERS_CARPET_ASCII_WRITER_
#define _EXAHYPE_PLOTTERS_CARPET_ASCII_WRITER_

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/Carpet/CarpetWriter.h"
#include "exahype/plotters/slicing/CartesianSlicer.h"
#include "kernels/KernelUtils.h" // idx::kernels

namespace exahype {
  namespace plotters {
    class CarpetASCIIWriter;

    namespace CarpetASCIIDatasets {
        struct CompatFull;
	struct Coord1D;
	struct Coord2D;
	struct Coord3D;
    }

    namespace ascii { class CSVStackWriter; } // external forward decl, #include exahype/plotters/ascii/CSVWriter.h
  }
}

/**
 * <h2>Writing ASCII/CSV files which are compatible to Cactus/Carpet/EinsteinToolkit</h2>
 * 
 * This writer allows you to write Carpet ASCII files, you don't need HDF5 support to use it.
 * Using this class, you can generate Carpet-compatible files (which can be read with various
 * tools or directly gnuplotted). In higher dimensions (2D,3D) these files can get quite large.
 * In 1D however, this format is quite useful.
 * 
 * @author Sven Köppel
 *
 **/
class exahype::plotters::CarpetASCIIWriter : public exahype::plotters::CarpetWriter {
  tarch::logging::Log _log;

public:
  typedef tarch::la::Vector<DIMENSIONS, double> dvec;
  typedef tarch::la::Vector<DIMENSIONS, bool> boolvec;
  typedef tarch::la::Vector<DIMENSIONS, int> ivec;

	
  bool fullCarpetCompatibility; ///< Generate all columns of carpet

  /// See superclass for documentation of arguments
  CarpetASCIIWriter(const std::string& _filename, int _basisSize, int _solverUnknowns, int _writtenUnknowns, exahype::parser::ParserView _plotterParameters, char** writtenQuantitiesNames);
  
  virtual ~CarpetASCIIWriter();

  void openFile() override;
  void flushFile() override;
  void closeFile() override;

  typedef exahype::plotters::ascii::CSVStackWriter  CSVWriterType;
  std::vector<CSVWriterType> files; ///< List of pointers to H5Files. Has length 1 if allUnknownsInOneFile.

  void startPlotting(double time) override;
  void finishPlotting() override;

  // Default values for limiterStatus for plotPatch* functions, used for instance from
  // a pure FV solver which has no limiter status flag.
  constexpr static int nonLimitingLimiterStatus = -1;

  void plotPatch(
      const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
      const tarch::la::Vector<DIMENSIONS, double>& dx,
      double* mappedCell, double timeStamp, int limiterStatus=nonLimitingLimiterStatus) override;
  
  void plotPatchForSingleUnknown(
      const dvec& offsetOfPatch, const dvec& sizeOfPatch, const dvec& dx,
      double* mappedCell, double timeStamp, int limiterStatus_data,
      int writtenUnknown, CSVWriterType& target);
}; // class

#endif /* _EXAHYPE_PLOTTERS_CARPET_ASCII_WRITER_ */
