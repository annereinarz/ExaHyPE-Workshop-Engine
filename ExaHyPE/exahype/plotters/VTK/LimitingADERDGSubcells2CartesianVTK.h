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

#ifndef _EXAHYPE_PLOTTERS_LIMITING_ADERDG_SUBCELLS_2_CARTESIAN_VTK_H_
#define _EXAHYPE_PLOTTERS_LIMITING_ADERDG_SUBCELLS_2_CARTESIAN_VTK_H_

#include "tarch/plotter/griddata/blockstructured/PatchWriterUnstructured.h"
#include "tarch/plotter/griddata/VTUTimeSeriesWriter.h"

#include "exahype/plotters/Plotter.h"
#include "exahype/plotters/slicing/Slicer.h"

namespace exahype {
	namespace plotters {
		class LimitingADERDGSubcells2CartesianVTK;

		class LimitingADERDGSubcells2CartesianCellsVTKAscii;
		class LimitingADERDGSubcells2CartesianCellsVTKBinary;
		class LimitingADERDGSubcells2CartesianCellsVTUAscii;
		class LimitingADERDGSubcells2CartesianCellsVTUBinary;
	}
}

/**
 * Common VTK class. Usually not used directly but through one of the subclasses.
 */
class exahype::plotters::LimitingADERDGSubcells2CartesianVTK : public exahype::plotters::Plotter::Device {
protected:
	enum class PlotterType {
		BinaryVTK,
		ASCIIVTK,
		BinaryVTU,
		ASCIIVTU
	};
private:
	int           _fileCounter;
	//const bool    _isBinary;
	//const bool    _isVTK;
	const PlotterType _plotterType;
	std::string   _filename;
	int           _order;
	int           _solverUnknowns;
	int           _writtenUnknowns;
	/**
	 * The ghost layer width the finite volumes patch is using.
	 */
	const int     _ghostLayerWidth;
	exahype::parser::ParserView   _plotterParameters;

	static tarch::logging::Log _log;


	double _time = 0;
	exahype::plotters::Slicer *_slicer = nullptr;

	tarch::plotter::griddata::VTUTimeSeriesWriter _timeSeriesWriter;

	tarch::plotter::griddata::blockstructured::PatchWriter::SinglePatchWriter* _gridWriter;
	tarch::plotter::griddata::blockstructured::PatchWriterUnstructured*        _patchWriter;
	tarch::plotter::griddata::Writer::VertexDataWriter*                        _vertexDataWriter;
	tarch::plotter::griddata::Writer::CellDataWriter*                          _cellDataWriter;
	tarch::plotter::griddata::Writer::CellDataWriter*                          _timeStampCellDataWriter;
	tarch::plotter::griddata::Writer::CellDataWriter*                          _cellRefinementStatusWriter;
	tarch::plotter::griddata::Writer::CellDataWriter*                          _cellPreviousRefinementStatusWriter;

public:
	LimitingADERDGSubcells2CartesianVTK(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
		const int ghostLayerWidth, PlotterType plotterType); // , const bool isBinary, const bool isVTK);
	virtual ~LimitingADERDGSubcells2CartesianVTK();

	virtual void init(const std::string& filename, int orderPlusOne, int solverUnknowns, int writtenUnknowns, exahype::parser::ParserView plotterParameters);

	void plotPatch(const int solverNumber, solvers::Solver::CellInfo& cellInfo) override;

	/**
	 * Plot a finite volumes solution.
	 */
	void plotFiniteVolumesPatch(
		const tarch::la::Vector<DIMENSIONS, double>& offsetOfPatch,
		const tarch::la::Vector<DIMENSIONS, double>& sizeOfPatch,
		const double* const u,
		const double timeStamp,
		const int RefinementStatus, const int previousRefinementStatus);

	void startPlotting(double time) override;
	void finishPlotting() override;
};

// VTK subclasses
class exahype::plotters::LimitingADERDGSubcells2CartesianCellsVTKAscii : public exahype::plotters::LimitingADERDGSubcells2CartesianVTK {
public:
	static std::string getIdentifier();
	LimitingADERDGSubcells2CartesianCellsVTKAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
		const int ghostLayerWidth);
};



class exahype::plotters::LimitingADERDGSubcells2CartesianCellsVTKBinary : public exahype::plotters::LimitingADERDGSubcells2CartesianVTK {
public:
	static std::string getIdentifier();
	LimitingADERDGSubcells2CartesianCellsVTKBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing,
		const int ghostLayerWidth);
};
// VTU subclasses

class exahype::plotters::LimitingADERDGSubcells2CartesianCellsVTUAscii : public exahype::plotters::LimitingADERDGSubcells2CartesianVTK {
public:
	static std::string getIdentifier();
	LimitingADERDGSubcells2CartesianCellsVTUAscii(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth);
};


class exahype::plotters::LimitingADERDGSubcells2CartesianCellsVTUBinary : public exahype::plotters::LimitingADERDGSubcells2CartesianVTK {
public:
	static std::string getIdentifier();
	LimitingADERDGSubcells2CartesianCellsVTUBinary(exahype::plotters::Plotter::UserOnTheFlyPostProcessing* postProcessing, const int ghostLayerWidth);
};
#endif // _EXAHYPE_PLOTTERS_LIMITING_ADERDG_SUBCELLS_2_CARTESIAN_VTK_H_
