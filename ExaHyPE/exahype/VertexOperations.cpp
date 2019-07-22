// THIS FILE WAS MODIFIED BY HAND. BE CAREFUL WHEN RUNNING PEANO'S PDT
#include "exahype/VertexOperations.h"
#include "peano/utils/Loop.h"
#include "peano/grid/Checkpoint.h"
exahype::VertexOperations::VertexOperations() { 
}

tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>  exahype::VertexOperations::readCellDescriptionsIndex(const peano::grid::VertexEnumerator& enumerator, const Vertex* const vertices)  { tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int> result; dfor2(x) tarch::la::slice(result,vertices[ enumerator(x) ]._vertexData.getCellDescriptionsIndex(),xScalar*TWO_POWER_D); enddforx return result; }
tarch::la::Vector<TWO_POWER_D,int>  exahype::VertexOperations::readCellDescriptionsIndex(const Vertex& vertex)  { return vertex._vertexData.getCellDescriptionsIndex(); }
int  exahype::VertexOperations::readCellDescriptionsIndex(const Vertex& vertex, int index)  { return vertex._vertexData.getCellDescriptionsIndex(index); }
void exahype::VertexOperations::writeCellDescriptionsIndex(const peano::grid::VertexEnumerator& enumerator, Vertex* const vertices, const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>& values) { dfor2(x) tarch::la::Vector<TWO_POWER_D,int> temp = tarch::la::slice<TWO_POWER_D>(values,xScalar*TWO_POWER_D); vertices[ enumerator(x) ]._vertexData.setCellDescriptionsIndex( temp ); enddforx }
void exahype::VertexOperations::writeCellDescriptionsIndex(Vertex& vertex, const tarch::la::Vector<TWO_POWER_D,int>& values) { vertex._vertexData.setCellDescriptionsIndex(values ); }
void exahype::VertexOperations::writeCellDescriptionsIndex(Vertex& vertex, int index, int value) { vertex._vertexData.setCellDescriptionsIndex(index, value ); }

tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*>  exahype::VertexOperations::readADERDGCellDescriptions(const peano::grid::VertexEnumerator& enumerator, const Vertex* const vertices)  { tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*> result; dfor2(x) tarch::la::slice(result,vertices[ enumerator(x) ]._vertexData.getADERDGCellDescriptions(),xScalar*TWO_POWER_D); enddforx return result; }
tarch::la::Vector<TWO_POWER_D,void*>  exahype::VertexOperations::readADERDGCellDescriptions(const Vertex& vertex)  { return vertex._vertexData.getADERDGCellDescriptions(); }
void*  exahype::VertexOperations::readADERDGCellDescriptions(const Vertex& vertex, int index)  { return vertex._vertexData.getADERDGCellDescriptions(index); }
void exahype::VertexOperations::writeADERDGCellDescriptions(const peano::grid::VertexEnumerator& enumerator, Vertex* const vertices, const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*>& values) { dfor2(x) tarch::la::Vector<TWO_POWER_D,void*> temp = tarch::la::slice<TWO_POWER_D>(values,xScalar*TWO_POWER_D); vertices[ enumerator(x) ]._vertexData.setADERDGCellDescriptions( temp ); enddforx }
void exahype::VertexOperations::writeADERDGCellDescriptions(Vertex& vertex, const tarch::la::Vector<TWO_POWER_D,void*>& values) { vertex._vertexData.setADERDGCellDescriptions(values ); }
void exahype::VertexOperations::writeADERDGCellDescriptions(Vertex& vertex, int index, void* value) { vertex._vertexData.setADERDGCellDescriptions(index, value ); }

tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*>  exahype::VertexOperations::readFiniteVolumesCellDescriptions(const peano::grid::VertexEnumerator& enumerator, const Vertex* const vertices)  { tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*> result; dfor2(x) tarch::la::slice(result,vertices[ enumerator(x) ]._vertexData.getFiniteVolumesCellDescriptions(),xScalar*TWO_POWER_D); enddforx return result; }
tarch::la::Vector<TWO_POWER_D,void*>  exahype::VertexOperations::readFiniteVolumesCellDescriptions(const Vertex& vertex)  { return vertex._vertexData.getFiniteVolumesCellDescriptions(); }
void*  exahype::VertexOperations::readFiniteVolumesCellDescriptions(const Vertex& vertex, int index)  { return vertex._vertexData.getFiniteVolumesCellDescriptions(index); }
void exahype::VertexOperations::writeFiniteVolumesCellDescriptions(const peano::grid::VertexEnumerator& enumerator, Vertex* const vertices, const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*>& values) { dfor2(x) tarch::la::Vector<TWO_POWER_D,void*> temp = tarch::la::slice<TWO_POWER_D>(values,xScalar*TWO_POWER_D); vertices[ enumerator(x) ]._vertexData.setFiniteVolumesCellDescriptions( temp ); enddforx }
void exahype::VertexOperations::writeFiniteVolumesCellDescriptions(Vertex& vertex, const tarch::la::Vector<TWO_POWER_D,void*>& values) { vertex._vertexData.setFiniteVolumesCellDescriptions(values ); }
void exahype::VertexOperations::writeFiniteVolumesCellDescriptions(Vertex& vertex, int index, void* value) { vertex._vertexData.setFiniteVolumesCellDescriptions(index, value ); }
