#include "matrixfree/solver/JacobiSmoother.h"
#include "matrixfree/solver/Multigrid.h"
#include "matrixfree/solver/BoxMG.h"
#include "matrixfree/stencil/StencilFactory.h"

#include "peano/utils/Loop.h"
#include "tarch/la/LUDecomposition.h"


namespace {
  /**
   * We solve BoxMG in a reference vertex configuration. See
   * mirrorStencilsOntoReferenceConfiguration(). As a result, we have to mirror
   * the resulting coarse grid stencil back again. This is done in this routine.
   */
  void writeProlongationStencilEntriesFromReferenceConfigurationIntoResult(
    const tarch::la::Vector<THREE_POWER_D, double>&   p,
    tarch::la::Vector<FIVE_POWER_D,double>&           prolongationStencil,
    const tarch::la::Vector<DIMENSIONS,int>&          positionOfCoarseGridVertex
  ) {
    dfor3(src)
      tarch::la::Vector<DIMENSIONS,int> dest;
      for (int d=0; d<DIMENSIONS; d++) {
        dest(d) = positionOfCoarseGridVertex(d)==0 ? 2 + src(d) : 2 - src(d);
      }
      prolongationStencil( peano::utils::dLinearised(dest,5) ) = p(srcScalar);
    enddforx
  }


  /**
   * We do solve BoxMG only for a reference configuration where the affected
   * coarse grid vertex is the one in the bottom left. To be able to do so, we
   * have to mirror all the input stencils until we have them in the reference
   * configuration.
   */
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D, double> mirrorStencilsOntoReferenceConfiguration(
    const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
    const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
  ) {
    tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D, double> s;

    dfor4(destVertex)
      tarch::la::Vector<DIMENSIONS,int> srcVertex;
      for (int d=0; d<DIMENSIONS; d++) {
        srcVertex(d) = positionOfCoarseGridVertex(d)==0 ? destVertex(d) : 3 - destVertex(d);
      }
      dfor3(destIndex)
        tarch::la::Vector<DIMENSIONS,int> srcIndex;
        for (int d=0; d<DIMENSIONS; d++) {
          srcIndex(d) = positionOfCoarseGridVertex(d)==0 ? destIndex(d) : 2 - destIndex(d);
        }
        const int destLinearisedIndex = peano::utils::dLinearised(destVertex,4) * THREE_POWER_D + peano::utils::dLinearised(destIndex,3);
        const int srcLinearisedIndex  = peano::utils::dLinearised(srcVertex,4)  * THREE_POWER_D + peano::utils::dLinearised(srcIndex,3);

        assertion(destLinearisedIndex>=0);
        assertion(srcLinearisedIndex>=0);

        assertion10(destLinearisedIndex<THREE_POWER_D_TIMES_FOUR_POWER_D, destLinearisedIndex, destVertex, srcVertex, destIndex, srcIndex, positionOfCoarseGridVertex, peano::utils::dLinearised(destVertex,4),  peano::utils::dLinearised(destIndex,3), peano::utils::dLinearised(srcVertex,4),  peano::utils::dLinearised(srcIndex,3) );
        assertion10(srcLinearisedIndex<THREE_POWER_D_TIMES_FOUR_POWER_D,  srcLinearisedIndex,  destVertex, srcVertex, destIndex, srcIndex, positionOfCoarseGridVertex, peano::utils::dLinearised(destVertex,4),  peano::utils::dLinearised(destIndex,3), peano::utils::dLinearised(srcVertex,4),  peano::utils::dLinearised(srcIndex,3) );

        s(destLinearisedIndex) = stencils(srcLinearisedIndex);
      enddforx
    enddforx

    return s;
  }
}


void matrixfree::solver::boxmg::computeBoxMGIntergridTransferOperator(
  const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
  tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
  const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
) {
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D, double> s = mirrorStencilsOntoReferenceConfiguration(
    stencils, positionOfCoarseGridVertex
  );

  tarch::la::Vector<THREE_POWER_D, double>                 f(0.0);
  tarch::la::Matrix<THREE_POWER_D, THREE_POWER_D, double>  M, Q, R;
  f(0) = 1.0;

  #if defined(Dim2)
  M    = 1.0,                        0.0,                        0.0,                        0.0,                        0.0,       0.0,       0.0,                        0.0,       0.0,
         s(1*9+0)+s(1*9+3)+s(1*9+6), s(1*9+1)+s(1*9+4)+s(1*9+7), s(1*9+2)+s(1*9+5)+s(1*9+8), 0.0,                        0.0,       0.0,       0.0,                        0.0,       0.0,
         0.0,                        s(2*9+0)+s(2*9+3)+s(2*9+6), s(2*9+1)+s(2*9+4)+s(2*9+7), 0.0,                        0.0,       0.0,       0.0,                        0.0,       0.0,
         s(4*9+0)+s(4*9+1)+s(4*9+2), 0.0,                        0.0,                        s(4*9+3)+s(4*9+4)+s(4*9+5), 0.0,       0.0,       s(4*9+6)+s(4*9+7)+s(4*9+8), 0.0,       0.0,
         0.0,                        0.0,                        0.0,                        s(8*9+0)+s(8*9+1)+s(8*9+2), 0.0,       0.0,       s(8*9+3)+s(8*9+4)+s(8*9+5), 0.0,       0.0,
         s(5*9+0),                   s(5*9+1),                   s(5*9+2),                   s(5*9+3),                   s(5*9+4),  s(5*9+5),  s(5*9+6),                   s(5*9+7),  s(5*9+8),
         0.0,                        s(6*9+0),                   s(6*9+1),                   0.0,                        s(6*9+3),  s(6*9+4),  0.0,                        s(6*9+6),  s(6*9+7),
         0.0,                        0.0,                        0.0,                        s(9*9+0),                   s(9*9+1),  s(9*9+2),  s(9*9+3),                   s(9*9+4),  s(9*9+5),
         0.0,                        0.0,                        0.0,                        0.0,                        s(10*9+0), s(10*9+1), 0.0,                        s(10*9+3), s(10*9+4);
  #elif defined(Dim3)
  M    = 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         s(1*9+0)+s(1*9+3)+s(1*9+6)+s(1*9+9)+s(1*9+12)+s(1*9+15)+s(1*9+18)+s(1*9+21)+s(1*9+24),  // M_21  -> along x-axis
	 s(1*9+1)+s(1*9+4)+s(1*9+7)+s(1*9+10)+s(1*9+13)+s(1*9+16)+s(1*9+19)+s(1*9+22)+s(1*9+25), // M_22
	 s(1*9+2)+s(1*9+5)+s(1*9+8)+s(1*9+11)+s(1*9+14)+s(1*9+17)+s(1*9+20)+s(1*9+23)+s(1*9+26), // M_23
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, // M_2x

         0.0,                                                                                    // M_31
	 s(2*9+0)+s(2*9+3)+s(2*9+6)+s(2*9+9)+s(2*9+12)+s(2*9+15)+s(2*9+18)+s(2*9+21)+s(2*9+24),  // M_32
	 s(2*9+1)+s(2*9+4)+s(2*9+7)+s(2*9+10)+s(2*9+13)+s(2*9+16)+s(2*9+19)+s(2*9+22)+s(2*9+25), // M_33
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, // M_3x

         s(4*9+0)+s(4*9+1)+s(4*9+2)+s(4*9+9)+s(4*9+10)+s(4*9+11)+s(4*9+18)+s(4*9+19)+s(4*9+20),  // M_41  -> along y-axis
	 0.0,                        0.0,
	 s(4*9+3)+s(4*9+4)+s(4*9+5)+s(4*9+12)+s(4*9+13)+s(4*9+14)+s(4*9+21)+s(4*9+22)+s(4*9+23), // M_44
	 0.0,                        0.0,
	 s(4*9+6)+s(4*9+7)+s(4*9+8)+s(4*9+15)+s(4*9+16)+s(4*9+17)+s(4*9+24)+s(4*9+25)+s(4*9+26), // M_47
	 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, // M_4x

	 0.0, 0.0, 0.0,                                                                          // M_51, M_52, M_53
         s(8*9+0)+s(8*9+1)+s(8*9+2)+s(8*9+9)+s(8*9+10)+s(8*9+11)+s(8*9+18)+s(8*9+19)+s(8*9+20),  // M_54
	 0.0,                        0.0,
	 s(8*9+3)+s(8*9+4)+s(8*9+5)+s(8*9+12)+s(8*9+13)+s(8*9+14)+s(8*9+21)+s(8*9+22)+s(8*9+23), // M_57
	 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, // M_5x

         s(16*9+0)+s(16*9+1)+s(16*9+2)+s(16*9+3)+s(16*9+4)+s(16*9+5)+s(16*9+6)+s(16*9+7)+s(16*9+8),        // M_61  -> along z-axis
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         s(16*9+9)+s(16*9+10)+s(16*9+11)+s(16*9+12)+s(16*9+13)+s(16*9+14)+s(16*9+15)+s(16*9+16)+s(16*9+17),// M_6,10
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         s(16*9+18)+s(16*9+19)+s(16*9+20)+s(16*9+21)+s(16*9+22)+s(16*9+23)+s(16*9+24)+s(16*9+25)+s(16*9+26),// M_6,19
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                            // M_6,x

	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                       // M_7,x
         s(32*9+0)+s(32*9+1)+s(32*9+2)+s(32*9+3)+s(32*9+4)+s(32*9+5)+s(32*9+6)+s(32*9+7)+s(32*9+8),         // M_7,10
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         s(32*9+9)+s(32*9+10)+s(32*9+11)+s(32*9+12)+s(32*9+13)+s(32*9+14)+s(32*9+15)+s(32*9+16)+s(32*9+17), // M_7,20
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                            // M_7,x

	 s(5*9+0)+s(5*9+9)+s(5*9+18),                                                                       // M_8,1  -> face with normal along z-axis
	 s(5*9+1)+s(5*9+10)+s(5*9+19),
	 s(5*9+2)+s(5*9+11)+s(5*9+20),
	 s(5*9+3)+s(5*9+12)+s(5*9+21),
	 s(5*9+4)+s(5*9+13)+s(5*9+22),
	 s(5*9+5)+s(5*9+14)+s(5*9+23),
	 s(5*9+6)+s(5*9+15)+s(5*9+24),
	 s(5*9+7)+s(5*9+16)+s(5*9+25),
	 s(5*9+8)+s(5*9+17)+s(5*9+26),
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

	 0.0,                                                                                               // M_9,x  -> face with normal along z-axis
	 s(6*9+0)+s(6*9+9)+s(6*9+18),
	 s(6*9+1)+s(6*9+10)+s(6*9+19),
	 0.0,
	 s(6*9+3)+s(6*9+12)+s(6*9+21),
	 s(6*9+4)+s(6*9+13)+s(6*9+22),
	 0.0,
	 s(6*9+6)+s(6*9+15)+s(6*9+24),
	 s(6*9+7)+s(6*9+16)+s(6*9+25),
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

	 0.0, 0.0, 0.0,                                                                                     // M_10,x  -> face with normal along z-axis
	 s(9*9+0)+s(9*9+9)+s(9*9+18),
	 s(9*9+1)+s(9*9+10)+s(9*9+19),
	 s(9*9+2)+s(9*9+11)+s(9*9+20),
	 s(9*9+3)+s(9*9+12)+s(9*9+21),
	 s(9*9+4)+s(9*9+13)+s(9*9+22),
	 s(9*9+5)+s(9*9+14)+s(9*9+23),
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

	 0.0, 0.0, 0.0, 0.0,                                                                                // M_11,x  -> face with normal along z-axis
	 s(10*9+0)+s(10*9+9)+s(10*9+18),
	 s(10*9+1)+s(10*9+10)+s(10*9+19),
	 0.0,
	 s(10*9+3)+s(10*9+12)+s(10*9+21),
	 s(10*9+4)+s(10*9+13)+s(10*9+22),
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         s(20*9+0)+s(20*9+1)+s(20*9+2), 0.0, 0.0,                                                          // M_12,1 -> face with normal along x-axis
         s(20*9+3)+s(20*9+4)+s(20*9+5), 0.0, 0.0,
         s(20*9+6)+s(20*9+7)+s(20*9+8), 0.0, 0.0,
         s(20*9+9)+s(20*9+10)+s(20*9+11), 0.0, 0.0,
         s(20*9+12)+s(20*9+13)+s(20*9+14), 0.0, 0.0,
         s(20*9+15)+s(20*9+16)+s(20*9+17), 0.0, 0.0,
         s(20*9+18)+s(20*9+19)+s(20*9+20), 0.0, 0.0,
         s(20*9+21)+s(20*9+22)+s(20*9+23), 0.0, 0.0,
         s(20*9+24)+s(20*9+25)+s(20*9+26), 0.0, 0.0,

         0.0, 0.0, 0.0,                                                                                   // M_13,1 -> face with normal along x-axis
         s(24*9+0)+s(24*9+1)+s(24*9+2), 0.0, 0.0,
         s(24*9+3)+s(24*9+4)+s(24*9+5), 0.0, 0.0,
         0.0, 0.0, 0.0,
         s(24*9+9)+s(24*9+10)+s(24*9+11), 0.0, 0.0,
         s(24*9+12)+s(24*9+13)+s(24*9+14), 0.0, 0.0,
         0.0, 0.0, 0.0,
         s(24*9+18)+s(24*9+19)+s(24*9+20), 0.0, 0.0,
         s(24*9+21)+s(24*9+22)+s(24*9+23), 0.0, 0.0,

         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                     // M_14,1 -> face with normal along x-axis
         s(36*9+0)+s(36*9+1)+s(36*9+2), 0.0, 0.0,
         s(36*9+3)+s(36*9+4)+s(36*9+5), 0.0, 0.0,
         s(36*9+6)+s(36*9+7)+s(36*9+8), 0.0, 0.0,
         s(36*9+9)+s(36*9+10)+s(36*9+11), 0.0, 0.0,
         s(36*9+12)+s(36*9+13)+s(36*9+14), 0.0, 0.0,
         s(36*9+15)+s(36*9+16)+s(36*9+17), 0.0, 0.0,

         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                     // M_15,1 -> face with normal along x-axis
         0.0, 0.0, 0.0,
         s(40*9+0)+s(40*9+1)+s(40*9+2), 0.0, 0.0,
         s(40*9+3)+s(40*9+4)+s(40*9+5), 0.0, 0.0,
         0.0, 0.0, 0.0,
         s(40*9+9)+s(40*9+10)+s(40*9+11), 0.0, 0.0,
         s(40*9+12)+s(40*9+13)+s(40*9+14), 0.0, 0.0,

         s(17*9+0)+s(17*9+3)+s(17*9+6),                                                                   // M_16,1 -> face with normal along y-axis
         s(17*9+1)+s(17*9+4)+s(17*9+7),
         s(17*9+2)+s(17*9+5)+s(17*9+8),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         s(17*9+9)+s(17*9+12)+s(17*9+15),
         s(17*9+10)+s(17*9+13)+s(17*9+16),
         s(17*9+11)+s(17*9+14)+s(17*9+17),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         s(17*9+18)+s(17*9+21)+s(17*9+24),
         s(17*9+19)+s(17*9+22)+s(17*9+25),
         s(17*9+20)+s(17*9+25)+s(17*9+26),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         0.0,                                                                                             // M_17,1 -> face with normal along y-axis
         s(18*9+0)+s(18*9+3)+s(18*9+6),
         s(18*9+1)+s(18*9+4)+s(18*9+7),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0,
         s(18*9+9)+s(18*9+12)+s(18*9+15),
         s(18*9+10)+s(18*9+13)+s(18*9+16),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0,
         s(18*9+18)+s(18*9+21)+s(18*9+24),
         s(18*9+19)+s(18*9+22)+s(18*9+25),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                    // M_18,x -> face with normal along y-axis
         s(33*9+0)+s(33*9+3)+s(33*9+6),
         s(33*9+1)+s(33*9+4)+s(33*9+7),
         s(33*9+2)+s(33*9+5)+s(33*9+8),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         s(33*9+9)+s(33*9+12)+s(33*9+15),
         s(33*9+10)+s(33*9+13)+s(33*9+16),
         s(33*9+11)+s(33*9+14)+s(33*9+17),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                                    // M_19,x -> face with normal along y-axis
         0.0,
         s(34*9+0)+s(34*9+3)+s(34*9+6),
         s(34*9+1)+s(34*9+4)+s(34*9+7),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0,
         s(34*9+9)+s(34*9+12)+s(34*9+15),
         s(34*9+10)+s(34*9+13)+s(34*9+16),
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         s(21*9+0),  s(21*9+1),  s(21*9+2),  s(21*9+3),  s(21*9+4),  s(21*9+5),  s(21*9+6),  s(21*9+7),  s(21*9+8), // first inner vertex
         s(21*9+9),  s(21*9+10), s(21*9+11), s(21*9+12), s(21*9+13), s(21*9+14), s(21*9+15), s(21*9+16), s(21*9+17),
         s(21*9+18), s(21*9+19), s(21*9+20), s(21*9+21), s(21*9+22), s(21*9+23), s(21*9+24), s(21*9+25), s(21*9+26),

         0.0,        s(22*9+0),  s(22*9+1),  0.0,        s(22*9+3),  s(22*9+4),  0.0,        s(22*9+6),  s(22*9+7), // second inner vertex
         0.0,        s(22*9+9),  s(22*9+10), 0.0,        s(22*9+12), s(22*9+13), 0.0,        s(22*9+15), s(22*9+16),
         0.0,        s(22*9+18), s(22*9+19), 0.0,        s(22*9+21), s(22*9+22), 0.0,        s(22*9+24), s(22*9+25),

         0.0,        0.0,        0.0,        s(25*9+0),  s(25*9+1),  s(25*9+2),  s(25*9+3),  s(25*9+4),  s(25*9+5),  // third inner vertex
         0.0,        0.0,        0.0,        s(25*9+9),  s(25*9+10), s(25*9+11), s(25*9+12), s(25*9+13), s(25*9+14),
         0.0,        0.0,        0.0,        s(25*9+18), s(25*9+19), s(25*9+20), s(25*9+21), s(25*9+22), s(25*9+23),

         0.0,        0.0,        0.0,        0.0,        s(26*9+0),  s(26*9+1),  0.0,        s(26*9+3),  s(26*9+4),  // fourth inner vertex
         0.0,        0.0,        0.0,        0.0,        s(26*9+9),  s(26*9+10), 0.0,        s(26*9+12), s(26*9+13),
         0.0,        0.0,        0.0,        0.0,        s(26*9+18), s(26*9+19), 0.0,        s(26*9+21), s(26*9+22),

         0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,
         s(37*9+0),  s(37*9+1),  s(37*9+2),  s(37*9+3),  s(37*9+4),  s(37*9+5),  s(37*9+6),  s(37*9+7),  s(37*9+8), // fifth inner vertex
         s(37*9+9),  s(37*9+10), s(37*9+11), s(37*9+12), s(37*9+13), s(37*9+14), s(37*9+15), s(37*9+16), s(37*9+17),

         0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,
         0.0,        s(38*9+0),  s(38*9+1),  0.0,        s(38*9+3),  s(38*9+4),  0.0,        s(38*9+6),  s(38*9+7), // sixth inner vertex
         0.0,        s(38*9+9),  s(38*9+10), 0.0,        s(38*9+12), s(38*9+13), 0.0,        s(38*9+15), s(38*9+16),

         0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,
         0.0,        0.0,        0.0,        s(41*9+0),  s(41*9+1),  s(41*9+2),  s(41*9+3),  s(41*9+4),  s(41*9+5),  // seventh inner vertex
         0.0,        0.0,        0.0,        s(41*9+9),  s(41*9+10), s(41*9+11), s(41*9+12), s(41*9+13), s(41*9+14),

         0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,        0.0,
         0.0,        0.0,        0.0,        0.0,        s(42*9+9),  s(42*9+10), 0.0,        s(42*9+12), s(42*9+13),
         0.0,        0.0,        0.0,        0.0,        s(42*9+18), s(42*9+19), 0.0,        s(42*9+21), s(42*9+22)
         ;

  // @todo raus damit

  M    = 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,

         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0,
         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0;

  #else
  #error Not implemented yet
  #endif

  tarch::la::modifiedGramSchmidt(M, Q, R);
  tarch::la::Vector<THREE_POWER_D, double> p = tarch::la::backSubstitution(R,tarch::la::transpose(Q)*f);

  writeProlongationStencilEntriesFromReferenceConfigurationIntoResult(
    p,
    prolongationStencil,
    positionOfCoarseGridVertex
  );
}


void matrixfree::solver::boxmg::computeSparsifiedSymmetricBoxMGIntergridTransferOperator(
  const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
  tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
  const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
) {
  tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D, double> s = mirrorStencilsOntoReferenceConfiguration(
    stencils, positionOfCoarseGridVertex
  );

  tarch::la::Vector<THREE_POWER_D, double>                 f(0.0);
  tarch::la::Matrix<THREE_POWER_D, THREE_POWER_D, double>  M, Q, R;
  f(0) = 1.0;

  #if defined(Dim2)
  M    = 1.0,                        0.0,                        0.0,                        0.0,                        0.0,       0.0,       0.0,                        0.0,       0.0,
         0.5*( s(1*9+3)+s(0*9+5) ),  s(1*9+4),                   0.5*( s(1*9+5)+s(2*9+3) ),  0.0,                        0.0,       0.0,       0.0,                        0.0,       0.0,
         0.0,                        0.5*( s(2*9+3)+s(1*9+5) ),  s(2*9+4),                   0.0,                        0.0,       0.0,       0.0,                        0.0,       0.0,
         0.5*( s(4*9+1)+s(0*9+7) ),  0.0,                        0.0,                        s(4*9+4),                   0.0,       0.0,       0.5*( s(4*9+7)+s(8*9+1) ),  0.0,       0.0,
         0.0,                        0.0,                        0.0,                        0.5*( s(8*9+1)+s(4*9+7) ),  0.0,       0.0,       s(8*9+4),                   0.0,       0.0,
         0.5*( s(5*9+0)+s(0*9+8) ),  0.5*( s(5*9+1)+s(1*9+7) ),  0.5*( s(5*9+2)+s(2*9+6) ),  0.5*( s(5*9+3)+s(4*9+5) ),  s(5*9+4),  0.5*( s(5*9+5)+s(6*9+3) ),  0.5*( s(5*9+6)+s(8*9+2) ), 0.5*( s(5*9+7)+s(9*9+1) ),  0.5*( s(5*9+8)+s(10*9+0) ),
         0.0,                        0.5*( s(6*9+0)+s(1*9+8) ),  0.5*( s(6*9+1)+s(2*9+7) ),  0.0,                        0.5*( s(6*9+3)+s(5*9+5) ),  s(6*9+4),  0.0,                       0.5*( s(6*9+6)+s(9*9+2) ),  0.5*( s(6*9+7)+s(10*9+1) ),
         0.0,                        0.0,                        0.0,                        0.5*( s(9*9+0)+s(4*9+8) ),  0.5*( s(9*9+1)+s(5*9+7) ),  0.5*( s(9*9+2)+s(6*9+6) ),  0.5*( s(9*9+3)+s(8*9+5) ),            s(9*9+4),  0.5*( s(9*9+5)+s(10*9+3) ),
         0.0,                        0.0,                        0.0,                        0.0,                        0.5*( s(10*9+0)+s(5*9+8) ), 0.5*( s(10*9+1)+s(6*9+7) ), 0.0,      0.5*( s(10*9+3)+s(9*9+5) ), s(10*9+4);
  #elif defined(Dim3)
  assertionMsg(false, "not implemented yet")
  #else
  #error Not implemented yet
  #endif

  tarch::la::modifiedGramSchmidt(M, Q, R);
  tarch::la::Vector<THREE_POWER_D, double> p = tarch::la::backSubstitution(R,tarch::la::transpose(Q)*f);

  writeProlongationStencilEntriesFromReferenceConfigurationIntoResult(
    p,
    prolongationStencil,
    positionOfCoarseGridVertex
  );
}



void matrixfree::solver::boxmg::computeAggregationOperator(
  const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
  tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
  const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
) {
  dfor5(k)
    bool inside = tarch::la::allSmallerEquals(k,3) && tarch::la::allGreaterEquals(k,1);
    prolongationStencil(kScalar) = inside ? 1.0 / FIVE_POWER_D : 0.0;
  enddforx
}


void matrixfree::solver::boxmg::computeInjectionOperator(
  const tarch::la::Vector<THREE_POWER_D_TIMES_FOUR_POWER_D,double>&  stencils,
  tarch::la::Vector<FIVE_POWER_D,double>&                            prolongationStencil,
  const tarch::la::Vector<DIMENSIONS,int>&                           positionOfCoarseGridVertex
) {
  prolongationStencil = tarch::la::Vector<FIVE_POWER_D,double>(0.0);
  prolongationStencil(FIVE_POWER_D/2) = 1.0;
}
