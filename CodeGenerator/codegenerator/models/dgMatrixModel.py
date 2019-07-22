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
# Generate the DG matrices used by the solver
# Use pure python matrices operations from Utils
#


from .abstractModelBaseClass import AbstractModelBaseClass

from ..utils import MathsUtils #matrix operation and build functions
import time #for runtime measurements debug print if context["runtimeDebug"]


class DGMatrixModel(AbstractModelBaseClass):

    def generateCode(self):
        #get GL nodes
        wGPN, xGPN = MathsUtils.getGaussLegendre(self.context["nDof"])
    
        padSize = self.context["nDofPad"] - self.context["nDof"]
        self.context["nDofPad_seq"] = range(self.context["nDofPad"])
        self.context["nDofPadTimesnDof_seq"] = range(self.context["nDofPad"]*self.context["nDof"])
        
        start = time.perf_counter()
        # [FLCoeff 0...0]; [FRCoeff 0...0];
        # right now FLCoeff, FRCoeff no pad (gives no benefit w.r.t libxsmm)
        FLCoeff, _ = MathsUtils.baseFunc1d(0.0, xGPN, self.context["nDof"]) #is also F0
        FRCoeff, _ = MathsUtils.baseFunc1d(1.0, xGPN, self.context["nDof"])
        self.context["FLCoeff"] = MathsUtils.vectorPad(FLCoeff, padSize)
        self.context["FRCoeff"] = MathsUtils.vectorPad(FRCoeff, padSize)
        if self.context["runtimeDebug"]:
            print("FRLCoeff: "+str(time.perf_counter()-start))
        
        # Matrices are stored in column major order (so the padding should be on the bottom rows)
        # [ Mat ]
        # [0...0]
        # [0...0]
        start = time.perf_counter()
        # Kxi
        Kxi = MathsUtils.assembleStiffnessMatrix(xGPN, wGPN, self.context["nDof"])
        self.context["Kxi"]   = MathsUtils.matrixPadAndFlatten_ColMajor(Kxi,padSize)
        self.context["Kxi_T"] = MathsUtils.matrixPadAndFlatten_RowMajor(Kxi,padSize) #transpose
        if self.context["runtimeDebug"]:
            print("Kxi: "+str(time.perf_counter()-start))
        
        start = time.perf_counter()
        # iK1_T
        iK1 = MathsUtils.matrixInverse(MathsUtils.assembleK1(Kxi, xGPN, self.context["nDof"]))
        self.context["iK1_T"] = MathsUtils.matrixPadAndFlatten_RowMajor(iK1,padSize) #transpose
        if self.context["runtimeDebug"]:
            print("iK1_T: "+str(time.perf_counter()-start))
        
        start = time.perf_counter()
        # dudx
        MM   = MathsUtils.assembleMassMatrix(xGPN, wGPN, self.context["nDof"])
        dudx = MathsUtils.assembleDiscreteDerivativeOperator(MM,Kxi)
        self.context["dudx"]   = MathsUtils.matrixPadAndFlatten_ColMajor(dudx,padSize)
        self.context["dudx_T"] = MathsUtils.matrixPadAndFlatten_RowMajor(dudx,padSize) #transpose
        if self.context["runtimeDebug"]:
            print("dudx: "+str(time.perf_counter()-start))
        
        start = time.perf_counter()
        #fineGridProjector1d
        fineGridProjector1d_0 = MathsUtils.assembleFineGridProjector1d(xGPN, 0, self.context["nDof"])
        fineGridProjector1d_1 = MathsUtils.assembleFineGridProjector1d(xGPN, 1, self.context["nDof"])
        fineGridProjector1d_2 = MathsUtils.assembleFineGridProjector1d(xGPN, 2, self.context["nDof"])
        self.context["fineGridProjector1d_0"]   = MathsUtils.matrixPadAndFlatten_ColMajor(fineGridProjector1d_0,padSize)
        self.context["fineGridProjector1d_1"]   = MathsUtils.matrixPadAndFlatten_ColMajor(fineGridProjector1d_1,padSize)
        self.context["fineGridProjector1d_2"]   = MathsUtils.matrixPadAndFlatten_ColMajor(fineGridProjector1d_2,padSize)
        
        #fineGridProjector1d_T_weighted
        for i in range(self.context["nDof"]):
            for j in range(self.context["nDof"]):
                fineGridProjector1d_0[i][j] *= wGPN[j]/wGPN[i]/3.0
                fineGridProjector1d_1[i][j] *= wGPN[j]/wGPN[i]/3.0
                fineGridProjector1d_2[i][j] *= wGPN[j]/wGPN[i]/3.0
        self.context["fineGridProjector1d_T_weighted_0"] = MathsUtils.matrixPadAndFlatten_RowMajor(fineGridProjector1d_0,padSize)
        self.context["fineGridProjector1d_T_weighted_1"] = MathsUtils.matrixPadAndFlatten_RowMajor(fineGridProjector1d_1,padSize)
        self.context["fineGridProjector1d_T_weighted_2"] = MathsUtils.matrixPadAndFlatten_RowMajor(fineGridProjector1d_2,padSize)
        if self.context["runtimeDebug"]:
            print("fineGrid: "+str(time.perf_counter()-start))
        
        #generate files         
        self.render("DGMatrices_h.template",   "DGMatrices.h")
        self.render("DGMatrices_cpp.template", "DGMatrices.cpp")
