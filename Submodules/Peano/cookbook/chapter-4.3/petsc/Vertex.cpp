#include "petsc/Vertex.h"
#include "peano/utils/Loop.h"
#include "peano/grid/Checkpoint.h"


/*
namespace petsc {
  class Vertex;

  *
   * Forward declaration

  class VertexOperations;

  *
   * These are the global vectors that we use to make the adapter communicate
   * with PETSc:

  extern Vec  x;
  extern Vec  rhs;
  extern Mat  A;
}
*/

Vec  petsc::u;
Vec  petsc::f;
Mat  petsc::A;



petsc::Vertex::Vertex():
  Base() { 
//  _vertexData.setIndex(-1);
}


petsc::Vertex::Vertex(const Base::DoNotCallStandardConstructor& value):
  Base(value) { 
  // Please do not insert anything here
}


petsc::Vertex::Vertex(const Base::PersistentVertex& argument):
  Base(argument) {
  // @todo Insert your code here
}


double petsc::Vertex::getU() const {
  double result = 0.0;
  if (_vertexData.getIndex()>=0) {
    PetscInt     indices[] = {_vertexData.getIndex()};
    PetscScalar  values[]  = {0.0};

    VecGetValues(u,1,indices,values);

    result = values[0];
  }
  return result;
}


bool petsc::Vertex::isUnknown() const {
  return _vertexData.getIndex()>=0;
}

void petsc::Vertex::setRhs(double rhs) {
  PetscInt     indices[] = {_vertexData.getIndex()};
  PetscScalar  values[]  = {rhs};
  VecSetValues(f,1,indices,values, INSERT_VALUES);
}
