// This file is part of the Peano project. For conditions of distribution and
// use, please see the copyright notice at www.peano-framework.org
#ifndef _MATRIXFREE_STENCIL_FACTORY_H_
#define _MATRIXFREE_STENCIL_STENCIL_FACTORY_H_


#include "tarch/la/Vector.h"
#include "tarch/la/Matrix.h"
#include "peano/utils/Globals.h"


namespace matrixfree {
  namespace stencil {
    class BSplinesStencilFactory;
  }
}


/**
 * Stencil Factory
 *
 * Many components in the Peano framework switch from a stencil notation to
 * element-wise assembly matrices. This class provides some operations to
 * construct the stencils. To convert these stencil into element-wise assembly
 * matrices, use the class ElementMatrix.
 *
 * @author Tobias Weinzierl
 */
class matrixfree::stencil::BSplinesStencilFactory {
  public:
    /**
     * @return @f$ [\frac{1}{6}, \frac{2}{3}, \frac{1}{6}] @f$
     */
    static tarch::la::Vector<3,double>  get1DMassStencilP1();
    static tarch::la::Vector<5,double>  get1DMassStencilP2();
    static tarch::la::Vector<7,double>  get1DMassStencilP3();
    static tarch::la::Vector<9,double>  get1DMassStencilP4();
    static tarch::la::Vector<11,double> get1DMassStencilP5();
    static tarch::la::Vector<13,double> get1DMassStencilP6();
    static tarch::la::Vector<15,double> get1DMassStencilP7();
    static tarch::la::Vector<17,double> get1DMassStencilP8();

    /**
     * A stencil of order p decomposes into 2p stencils among its support. With
     * the present operation, you can access individual substencils.
     */
    static tarch::la::Vector<3,double>  get1DMassStencilP1(int elementOfSupport);

    /**
     * Spans 1.5 elements in each direction, i.e. 4 in total.
     */
    static tarch::la::Vector<5,double>  get1DMassStencilP2(int elementOfSupport);

    /**
     * Spans 2 elements in each direction.
     */
    static tarch::la::Vector<7,double>  get1DMassStencilP3(int elementOfSupport);

    /**
     * Spans 2.5 elements in each direction, i.e. 6 in total.
     */
    static tarch::la::Vector<9,double>  get1DMassStencilP4(int elementOfSupport);

    /**
     * Stencil is not scaled at all with any mesh width.
     *
     * @return @f$ [-1, 2, -1] @f$
     */
    static tarch::la::Vector<3,double>  get1DLaplaceStencilP1();
    static tarch::la::Vector<5,double>  get1DLaplaceStencilP2();
    static tarch::la::Vector<7,double>  get1DLaplaceStencilP3();
    static tarch::la::Vector<9,double>  get1DLaplaceStencilP4();
    static tarch::la::Vector<11,double> get1DLaplaceStencilP5();
    static tarch::la::Vector<13,double> get1DLaplaceStencilP6();
    static tarch::la::Vector<15,double> get1DLaplaceStencilP7();
    static tarch::la::Vector<17,double> get1DLaplaceStencilP8();

    static tarch::la::Vector<3,double>  get1DLaplaceStencilP1(int elementOfSupport);
    static tarch::la::Vector<5,double>  get1DLaplaceStencilP2(int elementOfSupport);
    static tarch::la::Vector<7,double>  get1DLaplaceStencilP3(int elementOfSupport);
    static tarch::la::Vector<9,double>  get1DLaplaceStencilP4(int elementOfSupport);

    static tarch::la::Vector<THREE_POWER_D,double>   getLaplacianStencilP1( const tarch::la::Vector<DIMENSIONS,double>& scaling = 1.0 );
    static tarch::la::Vector<FIVE_POWER_D,double>    getLaplacianStencilP2( const tarch::la::Vector<DIMENSIONS,double>& scaling = 1.0 );
    static tarch::la::Vector<SEVEN_POWER_D,double>   getLaplacianStencilP3( const tarch::la::Vector<DIMENSIONS,double>& scaling = 1.0 );
    static tarch::la::Vector<NINE_POWER_D,double>    getLaplacianStencilP4( const tarch::la::Vector<DIMENSIONS,double>& scaling = 1.0 );

    static tarch::la::Vector<THREE_POWER_D,double>   getMassStencilP1();
    static tarch::la::Vector<FIVE_POWER_D,double>    getMassStencilP2();
    static tarch::la::Vector<SEVEN_POWER_D,double>   getMassStencilP3();
    static tarch::la::Vector<NINE_POWER_D,double>    getMassStencilP4();

    /**
     *   N means NumberOfInfluencingVerticesAroundCurrentElementAlongEachAxis
     *
     *    p | N
     *   -------
     *    1 | 2
     *    2 | 4
     *    3 | 4
     *    4 | 6
     */
    template <int p, int N>
    static tarch::la::Matrix<N*N,N*N,double>  getElementWiseAssemblyMatrix(
      tarch::la::Vector<2*p+1,double> stencilX[N],
      tarch::la::Vector<2*p+1,double> stencilY[N]
    );


    template <int p, int N>
    static tarch::la::Matrix<N*N*N,N*N*N,double>  getElementWiseAssemblyMatrix(
      tarch::la::Vector<2*p+1,double> stencilX[N],
      tarch::la::Vector<2*p+1,double> stencilY[N],
      tarch::la::Vector<2*p+1,double> stencilZ[N]
    );
};


#include "matrixfree/stencil/BSplinesStencilFactory.cpph"


#endif
