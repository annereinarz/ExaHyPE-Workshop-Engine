#include "matrixfree/stencil/Stencil.h"


int matrixfree::stencil::getStencilEntryLinearisedIndex( const tarch::la::Vector<DIMENSIONS,int>  stencilEntry) {
  int result = 0;
  int base   = 1;

  for (int d=0; d<DIMENSIONS; d++) {
    result += stencilEntry(d) * base;
    base *= 3;
  }

  return result;
}
