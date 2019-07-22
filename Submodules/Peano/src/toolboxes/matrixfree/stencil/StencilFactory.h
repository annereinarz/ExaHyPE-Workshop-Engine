// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_STENCIL_STENCIL_FACTORY_H_
#define _MATRIXFREE_STENCIL_STENCIL_FACTORY_H_


#include "matrixfree/stencil/Stencil.h"

#include <bitset>


namespace matrixfree {
  /**
   * Stencil factory methods
   *
   * Many components in the Peano framework switch from a stencil notation to
   * element-wise assembly matrices. This class provides some operations to
   * construct the stencils. To convert these stencil into element-wise assembly
   * matrices, use the class ElementMatrix.
   *
   * @author Tobias Weinzierl
   */
  namespace stencil {
    /**
     * Preprocess boundary stencils held by boundary vertices
     *
     * If codes use getElementWiseAssemblyMatrix() within cells, this operation
     * decomposes stencils assuming that all @f$ 2^d @f$ adjacent cells are
     * visited. For interior degrees of freedom this works fine. See the top
     * row in the cartoon below.
     *
     * @image html StencilFactor.png
     *
     * It does not hold if we are near to the boundary. See second row left.
     * The six-point stencil corresponds to a homogeneous Neumann condition and
     * shows the stencil that has to be evaluated if the boundary holds a real
     * degree of freedom.
     *
     * If we use getElementWiseAssemblyMatrix(), this stencil is decomposed
     * according to the middle right illustration. It yields the wrong result
     * as there are only cells visited. We could provide specialised versions
     * of getElementWiseAssemblyMatrix() that are evaluated at the boundary.
     * However, it is often more convenient to treat all cells exactly the same
     * way but to modify the stencil entries (red, bottom left) such that
     * getElementWiseAssemblyMatrix() yields the right evaluation in the end.
     *
     * This is exactly the job of this preprocessing routine that accepts the
     * 'real' stencil (top left) plus a bitfield describing whether a boundary
     * runs vertically through the degree domain, e.g. (first entry in
     * boundaryFaceNormals is set as the normal is parallel to the first axis).
     * The routine then upscales the stencil entries such that an evaluation in
     * the end yields the right result.
     *
     * @param boundaryFaceNormals Where is the boundary. The first DIMENSIONS
     *        entry refer to the faces running through the left bottom vertex.
     */
    void preprocessBoundaryStencil( Stencil& stencil, const std::bitset<DIMENSIONS*2>& boundaryFaceNormals );

    /**
     * Exchanges the coordinates of a stencil.
     *
     * Operations like this operation can be used to rotate a stencil, i.e. to
     * derive several stencils from one input stencil.
     */
     Stencil exchangeCoordinates( const Stencil& stencil, int coord0, int coord1 );

    /**
     * @return @f$ [\frac{1}{6}, \frac{2}{3}, \frac{1}{6}] @f$
     */
     tarch::la::Vector<3,double> get1DMassStencil();
    

    /**
     * Stencil is not scaled at all with any mesh width.
     *
     * @return @f$ [-1, 2, -1] @f$
     */
     tarch::la::Vector<3,double> get1DLaplaceStencil();

    /**
     * Stencil is not scaled at all with any mesh width.
     */
     tarch::la::Vector<3,double> get1DIdentity();
     tarch::la::Vector<3,double> get1DLeftIdentity();
     tarch::la::Vector<3,double> get1DRightIdentity();

    /**
     * Stencil is not scaled at all with any mesh width.
     */
     tarch::la::Vector<5,double> get1DLinearInterpolationStencil();

    /**
     * Stencil is not scaled at all with any mesh width.
     */
     tarch::la::Vector<3,double> get1DMeanValueStencil();

    /**
     * Stencil is not scaled at all with any mesh width.
     */
     tarch::la::Vector<3,double> get1DDownwindStencil();
     tarch::la::Vector<3,double> get1DUpwindStencil();
     tarch::la::Vector<3,double> get1DCentralDifferencesStencil();

    /**
     * Makes stencil-product of two stencils:
     *
     * a * b = [a_1, a_2, a_3] o [ b_1, b_2, B_3]
     *         [a_1*b_3 a_2*b_3 a_3*b_3]
     *       = [a_1*b_2 a_2*b_2 a_3*b_2]
     *         [a_1*b_1 a_2*b_1 a_3*b_1]
     */
     tarch::la::Vector<3*3,double> stencilProduct(
            const tarch::la::Vector<3,double>& a,
            const tarch::la::Vector<3,double>& b
            );
     tarch::la::Vector<5*5,double> stencilProduct(
            const tarch::la::Vector<5,double>& a,
            const tarch::la::Vector<5,double>& b
            );

    /**
     * Equals a * (b*c)
     */
     tarch::la::Vector<3*3*3,double> stencilProduct(
            const tarch::la::Vector<3,double>& a,
            const tarch::la::Vector<3,double>& b,
            const tarch::la::Vector<3,double>& c
            );
     tarch::la::Vector<5*5*5,double> stencilProduct(
            const tarch::la::Vector<5,double>& a,
            const tarch::la::Vector<5,double>& b,
            const tarch::la::Vector<5,double>& c
            );

     tarch::la::Vector<3*3*3*3,double> stencilProduct(
            const tarch::la::Vector<3,double>& a,
            const tarch::la::Vector<3,double>& b,
            const tarch::la::Vector<3,double>& c,
            const tarch::la::Vector<3,double>& d
            );
     tarch::la::Vector<5*5*5*5,double> stencilProduct(
            const tarch::la::Vector<5,double>& a,
            const tarch::la::Vector<5,double>& b,
            const tarch::la::Vector<5,double>& c,
            const tarch::la::Vector<5,double>& d
            );

     tarch::la::Vector<3*3*3*3*3,double> stencilProduct(
            const tarch::la::Vector<3,double>& a,
            const tarch::la::Vector<3,double>& b,
            const tarch::la::Vector<3,double>& c,
            const tarch::la::Vector<3,double>& d,
            const tarch::la::Vector<3,double>& e
            );
     tarch::la::Vector<5*5*5*5*5,double> stencilProduct(
            const tarch::la::Vector<5,double>& a,
            const tarch::la::Vector<5,double>& b,
            const tarch::la::Vector<5,double>& c,
            const tarch::la::Vector<5,double>& d,
            const tarch::la::Vector<5,double>& e
            );

    /**
     * Computes the Laplacian.
     *
     * The individual derivatives are scaled with the entries of scaling. For
     * the standard Laplacian, you can use the scaling 1.0. For a scalar
     * material parameter, either pass 1.0 and rescale the result or pass in a
     * vector where all entries hold the material parameter.
     */
     tarch::la::Vector<THREE_POWER_D,double>     getLaplacian(
            const tarch::la::Vector<DIMENSIONS,double>& h,
            const tarch::la::Vector<DIMENSIONS,double>& scaling = 1.0
            );
     tarch::la::Vector<THREE_POWER_D,std::complex<double> >  getLaplacian(
            const tarch::la::Vector<DIMENSIONS,std::complex<double> >& h,
            const tarch::la::Vector<DIMENSIONS,std::complex<double> >& scaling = std::complex<double>(1.0)
            );
     tarch::la::Vector<THREE_POWER_D,std::complex<double> >  getLaplacian(
            const tarch::la::Vector<DIMENSIONS, double >& h,
            const tarch::la::Vector<DIMENSIONS,std::complex<double> >& scaling
            );

     /**
      * Computes a convection operator, defined by the convection coefficients,
      * discretised with the first order upwind scheme.
      *
      * The scheme is stabilisied through a Laplacian, i.e. it is really an
      * upwind scheme and not central differences. However, the operation
      * returns a Finite Difference scheme scaled with the Finite Element
      * mesh widths. For most applications, this should do, though formally the
      * stencil should be derived from piecewise linear shape functions.
      */
     tarch::la::Vector<THREE_POWER_D,double>  getUpwindDiscretisedConvection(
    		 const tarch::la::Vector<DIMENSIONS,double >& h,
    		 const tarch::la::Vector<DIMENSIONS,double>& convCoeff
			 );

     /**
      * Eliminate Neumann BC by adapting the stencil
      *
      * Please note that you often have to call preprocessBoundaryStencil() on
      * the modified stencil afterwards.
      *
      * <h1>Realisation</h1>
      *
      * The realisation is very simple and relies on a central finite
      * difference discretisation. This is the discretisation corresponding to
      * d-linear finite elements. We explain the discretisation at hands of a
      * 2d setup for a vertex that is at the right boundary of the domain. The
      * stencil is given by
      * <pre>
s6  s7  s8       -1   -1   -1
s3  s4  s5   =   -1    8   -1
s0  s1  s2       -1   -1   -1
        </pre>
      *
      * We (virtually) extend the domain by one layer and obtain an additional
      * virtual degree of freedom right of the boundary. Let's denote all the
      * unknowns accessed by the stencil as
      * <pre>
u6  u7  u8
u3  u4  u5
u0  u1  u2
        </pre>
      * Obviously, u2,u5 and u8 are not defined. However, we now apply the
      * Neumann conditions with the rules:
      *
        u2   =  h(0)*derivative + u0
        u5   =  h(0)*derivative + u3
        u8   =  h(0)*derivative + u6
      *
      * We see that the residual evaluation results from an evaluation of the
      * u0,u1,u3,u4,u6,u7 terms plus an additional term
      *
      * h(0)*derivative*(s2+s5+s8) + s2*u0 + s5*u3 + s8*u6
      *
      * This is what the routine does: it modifies the stencil (here
      * s0 \mapsto s0+s2, s3 \mapsto s3+s5 and s6 \mapsto s6+s8) and returns
      * the modified rhs.
      *
      * @param h          cell sizes around the vertex
      * @param derivative Pass zero if you have homogeneous Neumann
      */
     void applyNeumannBC(
       tarch::la::Vector<THREE_POWER_D,double>&      stencil,
       double&                                       rhs,
       const std::bitset<DIMENSIONS*2>&              boundaryFaceNormals,
       const tarch::la::Vector<DIMENSIONS,double>&   h,
       double                                        derivative
     );

    /**
     * Computes the mass matrix.
     *
     * The mass results results from an identity in a PDE that is interpreted
     * in the finite element sense. As such, it is scaled with h^d. And it is
     * not a nodal evaluation but a nine-point stencil (in 2d).
     */
     tarch::la::Vector<THREE_POWER_D,double>                 getMassMatrix(const tarch::la::Vector<DIMENSIONS,double>& h);
     tarch::la::Vector<THREE_POWER_D,std::complex<double> >  getMassMatrix(const tarch::la::Vector<DIMENSIONS,std::complex<double> >& h);


    /**
     * Computes the Helmholtz shift term matrix.
     *
     * The Helmholtz shift is the term that needs to be added to the Laplacian matrix in order to get the full Helmholtz matrix
     */
     tarch::la::Vector<THREE_POWER_D,std::complex<double> >  getHelmholtzShiftMatrix(
            const tarch::la::Vector<DIMENSIONS,double>& h,
            const std::complex<double>& phi
            );

     tarch::la::Vector<THREE_POWER_D,std::complex<double> >  getHelmholtzShiftMatrix(
            const tarch::la::Vector<DIMENSIONS,std::complex<double> >& h,
            const std::complex<double>& phi
            );


    /**
     * Computes the mass matrix.
     *
     * The mass matrix is returned in the finite element form, i.e.~the
     * entries result from an integral over a h. They are scaled with the
     * volume of h. As such it corresponds to the integral over a
     * characteristic function of one cell.
     */
     tarch::la::Vector<THREE_POWER_D,double>                 getIdentity(const tarch::la::Vector<DIMENSIONS,double>& h);
     tarch::la::Vector<THREE_POWER_D,std::complex<double> >  getIdentity(const tarch::la::Vector<DIMENSIONS,std::complex<double> >& h);

    /**
     * Computes the stencil for a d-linear interpolation.
     */
     tarch::la::Vector<FIVE_POWER_D,double> getDLinearInterpolation();

     /**
      * The standard stencil spans 2^d cells adjacent to a vertex. Let (0,0)
      * be the element left or below the current vertex, (1,0) is the one below
      * and right of the vertex, and so forth. The stencil decomposes over all
      * adjacent cells. Such a decomposition is used in
      * getElementWiseAssemblyMatrix() for example to facilitate element-wise
      * matrix-vector products.
      *
      * This operation extracts from the stencil only the fragment that
      * 'belongs' to a certain adjacent cell. It is thus similar to
      * getElementWiseAssemblyMatrix() but realises everything from a
      * stencil point of view rather than with an element-wise language.
      */
     Stencil extractElementStencil( const Stencil& stencil, const tarch::la::Vector<DIMENSIONS,int>&   adjacentCell );
  }
}



#endif
