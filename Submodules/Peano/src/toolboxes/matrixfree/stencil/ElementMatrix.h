// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_STENCIl_ELEMENT_MATRIX_H_
#define _MATRIXFREE_STENCIl_ELEMENT_MATRIX_H_


#include "matrixfree/stencil/Stencil.h"
#include "peano/grid/VertexEnumerator.h"


namespace matrixfree {
  /**
   * Derive Element-wise Matrices From Stencils
   *
   * Many components in the Peano framework switch from a stencil notation to
   * element-wise assembly matrices frequently. As both representations (stencils
   * and element-wise matrices) are equivalent, it is reasonable to code only one
   * representation, and to derive the alternative representation automatically.
   * This class provides the required operation.
   *
   * - Stencils are stored within vertices (red lines for a 5-point stencil in
   *   2d, e.g.).
   * - However, sometimes we don't have access to neighbouring cells (big blue
   *   cell, e.g., is one coarse cell where the solver automaton descends to
   *   finer grids)
   * - Consequently, we have to convert the @f$ 2^d @f$ vertex stencils within
   *   each cell into a element-wise assembly matrix @f$ \mathbf{R}^{2^d \times 2^d}@f$.
   *   With this matrix, we then can accumulate the residuals.
   * - PeProt automatically generates a routine that takes the @f$ 2^d @f$
   *   vertices and writes their @f$ 3^d @f$ stencils into one big vector of
   *   length @f$ 2^d \cdot 3^d @f$.
   * - This class provides a routine that transforms this big vector into an
   *   element assembly matrix.
   * - There's also a shortcut operaration that takes only one stencil. In this
   *   case, we assume that the stencil is the same for all @f$ 2^d @f$ adjacent
   *   vertices of the cell.
   *
   * <h1> Example </h1>
   *
   * We have the stencil
   *
   * @f$ \left[ \begin{array}{ccc} -1 & -1 & -1 \\ -1 & 8 & -1 \\ -1 & -1 & -1 \end{array} \right] @f$
   *
   * for (d=2). It is stored as a vector of length nine. Within a cell, we have
   * four vertices. The values u of these vertices can be converted into one
   * vector of length four due to a call of the  operation
   * \code
  tarch::la::Vector<TWO_POWER_D,double> u = MyVertices::readU(...);
   \endcode
   * Such a  operation is generated, if you add the statement
   * \code
    read scalar: U
   \endcode
   * to your Peano specification file. Now, if you wanna compute matrix times
   * vector, you basically compute $Au$ with A being a @f$ 4 \times 4 @f$
   * matrix. A is well-defined by the stencil. In this example, it would be
   *
   * @f$ \left[ \begin{array}{cccc} 2 & -0.5 & -0.5 & -1.0 \\ -0.5 & 2 & -1.0 & -0.5 \\ -0.5 & -1.0 & 2 & -0.5 \\ -1.0 & -0.5 & -0.5 & 2 \end{array} \right] @f$
   *
   * The  methods of this class convert your stencil into the element-wise
   * stiffness matrix. There's two variants of conversion operations:
   *
   * - The simple variant takes one stencil and creates the stiffness matrix. It
   *   resembles the example.
   * - A more flexible variant takes @f$ 2^d @f$ stencils and creates the
   *   element-wise system matrix. This is the method of choice if you have
   *   different stencils on the different vertices.
   *
   *
   * @author Tobias Weinzierl
   */
  namespace stencil {
    /**
     * Derive element-wise matrix from stencil.
     *
     *
     * The operation takes the stencils from the adjacent vertices and derives
     * the parts of them that can be evaluated in one cell. See the first row
     * in the picture below.
     *
     * @image html StencilFactor.png
     *
     * The nine-point stencil is split up between the four adjacent cells. We
     * split up all entries that could be evaluated in multiple cells equally.
     * The diagonal element, e.g., can be evaluated by all four adjacent cells.
     * Therefore, we evaluate it in every cell but divide the contributions by
     * four. If we sum up, we end up with the original stencil.
     *
     * This whole arguing fails if we evaluate stencils along the boundary and
     * if there are unknowns located on the boundary. See the second and third
     * row or preprocessBoundaryStencil() for a detailed discussion.
     *
     * @see namespace description
     * @see matrixfree::stencil::preprocessBoundaryStencil() in StencilFactory.h
     */
     ElementWiseAssemblyMatrix getElementWiseAssemblyMatrix( const Stencil& stencil );

     /**
      * Derive element-wise matrix from stencil.
      *
      * @see namespace description
      * @see matrixfree::stencil::preprocessBoundaryStencil() in StencilFactory.h
      */
     ComplexElementWiseAssemblyMatrix getElementWiseAssemblyMatrix( const ComplexStencil& complexStencil );


    /**
     * Reconstruct stencils from element matrices
     *
     * With getElementWiseAssemblyMatrix() you can construct the element-wise
     * assembly matrix from the stencils. This operation is the inverse. You
     * plug in the assembly matrix and you obtain the corresponding stencils.
     * Hereby, the operation assumes that the stencils are the same for all
     * @f$ 2^d @f$ vertices.
     *
     * Without such an assumption, we were not able to reconstruct the stencils
     * really. The element-wise matrix defines only some stencil entries,
     * whereas the stencils also span neighbouring cells. Only because of the
     * assumption on the stencil uniformity, we can construct all stencil
     * entries. As a result, this operation does not work properly if you have
     * changing stencils (varying coefficients, e.g.).
     */
     Stencil reconstructUniformStencilFragments(const ElementWiseAssemblyMatrix& matrix );

     /**
      * Reconstruct stencils from element matrices
      *
      * Generalised version of reconstructUniformStencilFragments(). It
      * distinguishes the various vertices adjacent to a cell. However, it does
      * not make any assumptions about stencil homogeneity and thus is only
      * able to fill in some stencil entries partially.
      */
     tarch::la::Vector<TWO_POWER_D_TIMES_THREE_POWER_D, double> reconstructStencilFragments(const ElementWiseAssemblyMatrix& matrix );

    /**
     * Derive element-wise matrix from @f$ 2^d @f$ stencils, i.e. this operation
     * takes one big @f$ 2^d \cdot 3^d @f$ vector as argument and returns an
     * @f$ 2^d \times 2^d @f$ matrix.
     *
     * Typical usage if you have data on the heap:
     * <pre>

  VectorOfStencils stencils;
  dfor2(k)
    tarch::la::slice(
      stencils, // into stencils
      tarch::la::Vector<THREE_POWER_D,double>(fineGridVertices[ fineGridVerticesEnumerator(k) ].getStencil()),
      kScalar*THREE_POWER_D
    );
  enddforx

</pre>
     *
     *
     * Please note that this operation does not work properly at the domain
     * boundary: We do divide the contributions among all adjacent cells (see
     * the sketch below; only the upper row, the other two rows are used to
     * illustrate something else).
     *
     * @image html StencilFactory.png
     *
     * Obviously, this splitting is invalid along the domain boundary, where
     * fewer cells are adjacent and thus other scalings have to be used.
     * There's an extended/overloaded version of this routine to handle those
     * cases.
     *
     * @see getElementWiseAssemblyMatrix( const Stencil& stencil );
     */
     ElementWiseAssemblyMatrix getElementWiseAssemblyMatrix( const VectorOfStencils& vectorOfStencils );

     /**
      * @see getElementWiseAssemblyMatrix( const VectorOfStencils& vectorOfStencils )
      */
     ComplexElementWiseAssemblyMatrix getElementWiseAssemblyMatrix( const VectorOfComplexStencils& vectorOfComplexStencils );

     /**
      *
      * I originally thought it would be possible to identify how many
      * neighbours there are from the adjacent vertices' state. However, this
      * is not unique. If we look at the following 3x3 cells (outside and
      * inside)
      *
      * o o i
      * o o i
      * i i i
      *
      * and on the two vertices on the right (there are four real vertices in
      * this ASCII art that are really surrounded by known cells). The upper
      * one has a surrounding pattern of
      *
      * o i
      * o i
      *
      * and thus two inner adjacent cells. In the upper right inner cell, we
      * know that it is boundary. In the cell below, the lower left vertex
      * has exactly the same pattern from inside the cell. However, as its
      * adjacent cells have the state
      *
      * o i
      * i i
      *
      * this one has three inner cells and, thus, is to be scaled differently.
      *
      *
      * <h2>Transition from other operation</h2>
      *
      * If you want to replace your call to getElementWiseAssemblyMatrix()
      * without any inside/outside flags, you simply have to add
      *
      * std::bitset<THREE_POWER_D>(0).flip()
      *
      * as additional argument.
      *
      *
      * @param cellIsOutside Identifies which of the 3^d-1 surrounding cells
      *                      are inside.
      */
     ElementWiseAssemblyMatrix getElementWiseAssemblyMatrix( const VectorOfStencils& vectorOfStencils, const std::bitset<THREE_POWER_D>& cellIsInside );

     /**
      * See non-complex variant.
      */
     ComplexElementWiseAssemblyMatrix getElementWiseAssemblyMatrix( const VectorOfComplexStencils& vectorOfStencils, const std::bitset<THREE_POWER_D>& cellIsInside );

     /**
      * An element-wise assembly matrix is distilled from a sequence of
      * @f$ 2^d @f$ stencils which are in turn @f$ 3^d @f$ double arrays. This
      * operation computes the inverse index mapping. You hand in a row of the
      * matrix plus a column. The operation tells you which entry it is in the
      * original stencil of the rowth vertex.
      *
      * For the 2d variant, I've just hard-coded all entries. This might also
      * be an option for 3d if it turns out that the operation is too slow. For
      * all other dimensions, we realise a generic version of the lookup: We
      * convert both parameters row and col into integer vectors that have
      * either a zero or a one as argument. We then call the overloaded variant
      * of the present function.
      */
     int mapElementMatrixEntryOntoStencilEntry(int row, int col);

     int mapElementMatrixEntryOntoStencilEntry(const tarch::la::Vector<DIMENSIONS,int>& row, const tarch::la::Vector<DIMENSIONS,int>&  col);

     double getDiagonalElement( const ElementWiseAssemblyMatrix& matrix );
     double getDiagonalElement( const Stencil& stencil );

    template<int StencilSize>
     tarch::la::Vector<StencilSize*StencilSize,double> stencilProduct(
      const tarch::la::Vector<StencilSize,double>& a,
      const tarch::la::Vector<StencilSize,double>& b
    );

    template<int StencilSize>
     tarch::la::Vector<StencilSize*StencilSize*StencilSize,double> stencilProduct(
      const tarch::la::Vector<StencilSize,double>& a,
      const tarch::la::Vector<StencilSize,double>& b,
      const tarch::la::Vector<StencilSize,double>& c
    );
  }
}

#include "matrixfree/stencil/ElementMatrix.cpph"

#endif
