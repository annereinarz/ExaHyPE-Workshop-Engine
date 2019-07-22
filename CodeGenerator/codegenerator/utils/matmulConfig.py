##
# @file This file is part of the ExaHyPE project.
# @author ExaHyPE Group (exahype@lists.lrz.de)
#
# @section LICENSE
#
# Copyright (c) 2016  http://exahype.eu
# All rights reserved.
#
# The project has received funding from the European Union's Horizon 
# 2020 research and innovation programme under grant agreement
# No 671698. For copyrights and licensing, please consult the webpage.
#
# Released under the BSD 3 Open Source License.
# For the full license text, see LICENSE.txt
#
#
# @section DESCRIPTION
#
# Defines a meta data structure for mathematical operations
# (gemm, gemv) for which the assembly code generation back end 
# outputs the implementation.
#


from sys import exit


class MatmulConfig:
    """Specification of a dense matrix-matrix multiplication
    
        C       = alpha  *   A   *    B   + beta  *  C
     (M x N)             (M x K)  (K x N)
     
    for (int it_1 = 0; it_1 < K; it_1++) {
      for (int it_2 = 0; it_2 < N; it_2++) {
        for (int it_3 = 0; it_3 < M; it_3++) {
          C[it_1*LDC+it_3] = alpha * A[it_2*LDA+it_3] * B[it_1*LDB+it_2] + beta * C[it_1*LDC+it_3];
        }
      }
    }
    """

    # dgemm, dgemv, ....
    operationType = ""

    baseroutinename = ""
    
    name = ""
    
    # dimension of the matrices
    M = -1
    N = -1
    K = -1
    
    # leading dimension of A, B, and C 
    LDA = -1
    LDB = -1
    LDC = -1
    
    # scalars
    alpha = 1;  # -1, 1
    beta  = 1;  #  0, 1
    
    # alignment flags
    alignment_A = 0  # 1 aligned, 0 unaligned  
    alignment_C = 0  # 1 aligned, 0 unaligned
    
    # prefetching
    prefetchStrategy = ""
    
    # Constructor
    def __init__(self, M, N, K, LDA, LDB, LDC, alpha, beta, alignment_A, alignment_C, name, prefetchStrategy, operationType="gemm"):
        if((M > LDC) or (K > LDB) or (M > LDA)):
            print("MatmulConfig: Incompatible matrix sizes and leading dimensions")
            exit()
        if(alignment_A not in [0,1]):
            print("MatmulConfig: Something is wrong with the alignment choice of matrix A")
            exit()
        if(alignment_C not in [0,1]):
            print("MatmulConfig: Something is wrong with the alignment choice of matrix C")
            exit()
            
            
        self.M = M
        self.N = N
        self.K = K
        self.LDA = LDA
        self.LDB = LDB
        self.LDC = LDC
        self.alpha = alpha
        self.beta = beta
        self.alignment_A = alignment_A
        self.alignment_C = alignment_C
        self.name = name
        self.prefetchStrategy = prefetchStrategy
        self.baseroutinename = operationType+"_"+str(M)+"_"+str(N)+"_"+str(K)+"_"+name
        

    def __repr__(self):
        return "<%s: %s LDA=%s, LDB=%s, LDC=%s, alpha=%s, beta=%s, alignment_A=%s, alignment_C=%s>" \
             % (self.name, self.baseroutinename, self.LDA, self.LDB, self.LDC, self.alpha, self.beta, self.alignment_A, self.alignment_C)
