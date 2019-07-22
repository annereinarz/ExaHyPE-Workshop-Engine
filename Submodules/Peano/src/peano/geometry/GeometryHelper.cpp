#include "peano/geometry/GeometryHelper.h"



tarch::logging::Log peano::geometry::GeometryHelper::_log( "peano::geometry::GeometryHelper" );


peano::geometry::GeometryHelper::GeometryHelper() {
}


peano::geometry::GeometryHelper::~GeometryHelper() {
}


tarch::la::Vector<DIMENSIONS,double> peano::geometry::GeometryHelper::getCellCenter(
  const tarch::la::Vector<DIMENSIONS,double>& bottomVertex,
  const tarch::la::Vector<DIMENSIONS,double>& h
) {
  logTraceInWith2Arguments( "getCellCenter(...)", bottomVertex, h );

  tarch::la::Vector<DIMENSIONS,double> result( bottomVertex + 0.5 * h );

  logTraceOutWith1Argument( "getCellCenter(...)", result );

  return result;
}


peano::geometry::GeometryHelper::VertexAction peano::geometry::GeometryHelper::getVertexCommand(
  bool  pointWithHEnvironmentIsInside,
  bool  pointIsOutsideOfDomainClosure,
  bool  pointWithHEnvironmentIsOutside,
  bool  allCellConnectedPointsAreOutside,
  bool  mayEraseAlongArtificiallyRefinedBoundary,
  const CurrentVertexState& currentVertexState
) {
  logTraceIn( "getVertexCommand(...)" );

  peano::geometry::GeometryHelper::VertexAction result = LeaveVertexUnaltered;

  if (pointWithHEnvironmentIsInside && (currentVertexState != Inside)) {
    result = CreateInnerVertex;
  }
  else if (pointIsOutsideOfDomainClosure && (currentVertexState != Outside)) {
    result = DestroyVertexAndSwitchToOutside;
  }
  // embedding property (notably important on coarsest rank)
  else if (pointIsOutsideOfDomainClosure && allCellConnectedPointsAreOutside && !pointWithHEnvironmentIsOutside) {
    result = LeaveVertexUnalteredButRefine;
  }
  else if (pointIsOutsideOfDomainClosure && mayEraseAlongArtificiallyRefinedBoundary) {
    result = EraseOutsideVertex;
  }
  else if ( !pointWithHEnvironmentIsInside && !pointIsOutsideOfDomainClosure && currentVertexState != Boundary ) {
    result = CreateBoundaryVertex;
  }

  logTraceOutWith1Argument( "getVertexCommand(...)", (int)(result) );
  return result;
}


peano::geometry::GeometryHelper::CellAction peano::geometry::GeometryHelper::getCellCommand(
  bool centerOfCellWithH2EnvironmentIsInside,
  bool centerOfCellWithH2EnvironmentIsOutside,
  bool currentCellIsInside
) {
  logTraceInWith3Arguments( "getCellCommand(...)", centerOfCellWithH2EnvironmentIsInside, centerOfCellWithH2EnvironmentIsOutside, currentCellIsInside );

  if ( centerOfCellWithH2EnvironmentIsInside && !currentCellIsInside) {
    logTraceOutWith1Argument( "getCellCommand(...)", "CreateInnerCell" );
    return CreateInnerCell;
  }
  else if ( centerOfCellWithH2EnvironmentIsOutside && currentCellIsInside ) {
    logTraceOutWith1Argument( "getCellCommand(...)", "CreateOuterCellAndDoNotAnalyseItFurtherIfItsRefined" );
    return CreateOuterCellAndDoNotAnalyseItFurtherIfItsRefined;
  }
  else if ( !centerOfCellWithH2EnvironmentIsOutside && !centerOfCellWithH2EnvironmentIsInside && currentCellIsInside ) {
    logTraceOutWith1Argument( "getCellCommand(...)", "CreateOuterCell" );
    return CreateOuterCell;
  }

  logTraceOutWith1Argument( "getCellCommand(...)", "LeaveCellUnaltered" );
  return LeaveCellUnaltered;
}
