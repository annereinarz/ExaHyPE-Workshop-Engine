// THIS FILE WAS MODIFIED BY HAND. BE CAREFUL WHEN RUNNING PEANO'S PDT
#ifndef _EXAHYPE_VERTEX_OPERATIONS_H_ 
#define _EXAHYPE_VERTEX_OPERATIONS_H_
#include "exahype/Vertex.h"
#include "peano/grid/Vertex.h"
#include "peano/grid/VertexEnumerator.h"
#include "peano/utils/Globals.h"
namespace exahype { 
      class VertexOperations; 
}
/**
 * NOTE: THIS FILE WAS MODIFIED AND NEEDS TO BE RESTORED AFTER EVERY PDT RUN UNTIL
 * TOBIAS UPDATES DASTGEN.
 */
class exahype::VertexOperations { 
  private: 
    /**
     * One should not instantiate this class as it is a collection of static operations.
     */
    VertexOperations();
  public:
    
    static tarch::la::Vector<TWO_POWER_D,int> readCellDescriptionsIndex(const Vertex& vertex);
    static int readCellDescriptionsIndex(const Vertex& vertex, int index);
    static tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int> readCellDescriptionsIndex(const peano::grid::VertexEnumerator& enumerator, const Vertex* const vertices);
    static void writeCellDescriptionsIndex(Vertex&  vertex, const tarch::la::Vector<TWO_POWER_D,int>& values);
    static void writeCellDescriptionsIndex(Vertex&  vertex, int index, int value);
    static void writeCellDescriptionsIndex(const peano::grid::VertexEnumerator& enumerator, Vertex* const vertices, const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,int>& values);

    static tarch::la::Vector<TWO_POWER_D,void*> readADERDGCellDescriptions(const Vertex& vertex);
    static void* readADERDGCellDescriptions(const Vertex& vertex, int index);
    static tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*> readADERDGCellDescriptions(const peano::grid::VertexEnumerator& enumerator, const Vertex* const vertices);
    static void writeADERDGCellDescriptions(Vertex&  vertex, const tarch::la::Vector<TWO_POWER_D,void*>& values);
    static void writeADERDGCellDescriptions(Vertex&  vertex, int index, void* value);
    static void writeADERDGCellDescriptions(const peano::grid::VertexEnumerator& enumerator, Vertex* const vertices, const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*>& values);

    static tarch::la::Vector<TWO_POWER_D,void*> readFiniteVolumesCellDescriptions(const Vertex& vertex);
    static void* readFiniteVolumesCellDescriptions(const Vertex& vertex, int index);
    static tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*> readFiniteVolumesCellDescriptions(const peano::grid::VertexEnumerator& enumerator, const Vertex* const vertices);
    static void writeFiniteVolumesCellDescriptions(Vertex&  vertex, const tarch::la::Vector<TWO_POWER_D,void*>& values);
    static void writeFiniteVolumesCellDescriptions(Vertex&  vertex, int index, void* value);
    static void writeFiniteVolumesCellDescriptions(const peano::grid::VertexEnumerator& enumerator, Vertex* const vertices, const tarch::la::Vector<TWO_POWER_D_TIMES_TWO_POWER_D,void*>& values);
};
#endif
