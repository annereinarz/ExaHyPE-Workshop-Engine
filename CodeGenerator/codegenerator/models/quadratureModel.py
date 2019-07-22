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
# Generate the Quadrature matrices (nodes + weights) used by the solver
# Use pure python matrices operations from Utils
#


from .abstractModelBaseClass import AbstractModelBaseClass

from ..utils import MathsUtils #matrix operation and build functions


class QuadratureModel(AbstractModelBaseClass):

    def generateCode(self):
        if self.context["quadratureType"] == "Gauss-Legendre":
            QuadratureWeights, QuadratureNodes = MathsUtils.getGaussLegendre(self.context["nDof"])
            OtherQuadratureWeights, OtherQuadratureNodes = MathsUtils.getGaussLobatto(self.context["nDof"])
        elif self.context["quadratureType"] == "Gauss-Lobatto":
            raise ValueError("Quadrature type "+self.context["quadratureType"]+" not supported." ) #TODO JMG
        else:
            raise ValueError("Quadrature type "+self.context["quadratureType"]+" not supported." )
        
        
        weightsVector = MathsUtils.vectorPad(QuadratureWeights, self.context["nDofPad"] - self.context["nDof"])
        self.context["weights1"] = weightsVector
        self.context["w1Size"] = len(self.context["weights1"])
        self.context["w1_seq"] = range(self.context["w1Size"])
        if(self.context["nDim"] == 2):
            # weightsVector is QuadratureWeights itself
            weightsVector = MathsUtils.vectorPad(QuadratureWeights, self.controller.getPadSize(len(QuadratureWeights)))
            self.context["weights2"] = weightsVector
            self.context["w2Size"] = len(self.context["weights2"])
            self.context["w2_seq"] = range(self.context["w2Size"])

            # all combinations of two weights, written as an 1D array
            weightsVector = [QuadratureWeights[i] * QuadratureWeights[j] for i in range(self.context["nDof"]) for j in range(self.context["nDof"])]
            weightsVector = MathsUtils.vectorPad(weightsVector, self.controller.getPadSize(len(weightsVector)))
            self.context["weights3"] = weightsVector
            self.context["w3Size"] = len(self.context["weights3"])
            self.context["w3_seq"] = range(self.context["w3Size"])

        elif(self.context["nDim"] == 3):
            # all combinations of two weights, written as an 1D array
            weightsVector = [QuadratureWeights[i] * QuadratureWeights[j] for i in range(self.context["nDof"]) for j in range(self.context["nDof"])]
            weightsVector = MathsUtils.vectorPad(weightsVector, self.controller.getPadSize(len(weightsVector)))
            self.context["weights2"] = weightsVector
            self.context["w2Size"] = len(self.context["weights2"])
            self.context["w2_seq"] = range(self.context["w2Size"])

            # all combination of three weights, written as an 1D array
            weightsVector = [QuadratureWeights[i] * QuadratureWeights[j] * QuadratureWeights[k] for i in range(self.context["nDof"]) for j in range(self.context["nDof"]) for k in range(self.context["nDof"])]
            weightsVector = MathsUtils.vectorPad(weightsVector, self.controller.getPadSize(len(weightsVector)))
            self.context["weights3"] = weightsVector
            self.context["w3Size"] = len(self.context["weights3"])
            self.context["w3_seq"] = range(self.context["w3Size"])
        else:
            print("QuadratureModel: nDim not supported")
        
        # inverse of weight3
        self.context["iweights3"] = [1.0/self.context["weights3"][i] if self.context["weights3"][i] != 0. else 0. for i in self.context["w3_seq"]]
        
        self.context["QuadratureWeights"], self.context["QuadratureNodes"] = QuadratureWeights, QuadratureNodes
        self.context["quadrature_seq"] = range(self.context["nDof"])
        
        if(self.context["useLimiter"]):
            l_padSize = self.context["nDofPad"] - self.context["nDof"]
            uh2lob = MathsUtils.assembleQuadratureConversion(QuadratureNodes, OtherQuadratureNodes, self.context["nDof"]) #TODO JMG adapt when allowing Lobatto as node
            self.context["uh2lob"] = MathsUtils.matrixPadAndFlatten_ColMajor(uh2lob,l_padSize)
            self.context["uh2lobSize"] = len(self.context["uh2lob"])
            self.context["uh2lob_seq"] = range(self.context["uh2lobSize"])
            
            l_padSize = self.context["nDofPad"] - self.context["nDof"]
            dg2fv = MathsUtils.assembleDGToFV(QuadratureNodes, QuadratureWeights, self.context["nDof"], self.context["nDofLim"])
            self.context["dg2fv"] = MathsUtils.matrixPadAndFlatten_ColMajor(dg2fv,l_padSize)
            self.context["dg2fvSize"] = len(self.context["dg2fv"])
            self.context["dg2fv_seq"] = range(self.context["dg2fvSize"])
            
            l_padSize = self.context["nDofLimPad"] - self.context["nDofLim"]
            fv2dg = MathsUtils.assembleFVToDG(dg2fv, QuadratureWeights, self.context["nDof"], self.context["nDofLim"])
            self.context["fv2dg"] = MathsUtils.matrixPadAndFlatten_ColMajor(fv2dg,l_padSize)
            self.context["fv2dgSize"] = len(self.context["fv2dg"])
            self.context["fv2dg_seq"] = range(self.context["fv2dgSize"])

        self.render("Quadrature_h.template",   "Quadrature.h")
        self.render("Quadrature_cpp.template", "Quadrature.cpp")
