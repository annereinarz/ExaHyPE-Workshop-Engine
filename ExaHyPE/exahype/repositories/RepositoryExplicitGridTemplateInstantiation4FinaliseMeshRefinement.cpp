#include "exahype/repositories/Repository.h"
#include "exahype/records/RepositoryState.h"

#include "exahype/State.h"
#include "exahype/Vertex.h"
#include "exahype/Cell.h"

#include "peano/grid/Grid.h"

#include "peano/stacks/CellSTDStack.h"

#include "peano/stacks/VertexSTDStack.h"

 #include "exahype/adapters/MeshRefinement.h" 
 #include "exahype/adapters/MeshRefinementAndPlotTree.h" 
 #include "exahype/adapters/FinaliseMeshRefinement.h" 
 #include "exahype/adapters/FinaliseMeshRefinementOrLocalRollback.h" 
 #include "exahype/adapters/InitialPrediction.h" 
 #include "exahype/adapters/FusedTimeStep.h" 
 #include "exahype/adapters/PredictionRerun.h" 
 #include "exahype/adapters/BroadcastAndDropNeighbourMessages.h" 
 #include "exahype/adapters/RefinementStatusSpreading.h" 
 #include "exahype/adapters/PredictionOrLocalRecomputation.h" 
 #include "exahype/adapters/MergeNeighbours.h" 
 #include "exahype/adapters/UpdateAndReduce.h" 
 #include "exahype/adapters/Prediction.h" 
 #include "exahype/adapters/Correction.h" 


namespace peano {
  namespace grid {
    template class Grid<exahype::Vertex,exahype::Cell,exahype::State, peano::stacks::VertexSTDStack<  exahype::Vertex> ,peano::stacks::CellSTDStack<  exahype::Cell> ,exahype::adapters::FinaliseMeshRefinement>;
  }
}

#include "peano/grid/Grid.cpph"
