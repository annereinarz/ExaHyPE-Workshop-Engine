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
# Generates the code for the AMR
#


from .abstractModelBaseClass import AbstractModelBaseClass

from ..utils import MatmulConfig


class AMRRoutinesModel(AbstractModelBaseClass):    
    
    def generateCode(self):
        self.render("amrRoutines_cpp.template", "amrRoutines.cpp")
        # generates gemms
        if(self.context["useLibxsmm"]):
            self.controller.generateGemms("asm_amrRoutines.c", self.context["matmulConfigs"].values())
    
    
    def buildGemmsConfig(self):
        # define a sequence of matmul configs
        self.context["matmulConfigs"] = {}
        # shortcut
        nVar     = self.context["nVar"]
        nVarPad  = self.context["nVarPad"]
        nData    = self.context["nData"]
        nDataPad = self.context["nDataPad"]
        nDof     = self.context["nDof"]
        nDof2    = nDof*nDof
        nDofPad  = self.context["nDofPad"]
        nDim     = self.context["nDim"]

        # Always overwrite input (no need to set to 0), except if add
        # nDim-1 face projection, inputs are padded
        self.context["matmulConfigs"]["face_Q_x"] =     MatmulConfig(nDataPad, nDof, nDof, nDataPad, nDofPad, nDataPad, 1, 0, 1, 1, "face_Q_x", "nopf", "gemm")
        self.context["matmulConfigs"]["face_F_x"] =     MatmulConfig(nVarPad , nDof, nDof, nVarPad , nDofPad, nVarPad , 1, 0, 1, 1, "face_F_x", "nopf", "gemm")
        if(nDim == 3):
            self.context["matmulConfigs"]["face_Q_y"] = MatmulConfig(nDataPad, nDof, nDof, nDataPad*nDof, nDofPad, nDataPad*nDof, 1, 0, 1, 1, "face_Q_y", "nopf", "gemm")
            self.context["matmulConfigs"]["face_F_y"] = MatmulConfig(nVarPad , nDof, nDof, nVarPad*nDof , nDofPad, nVarPad*nDof , 1, 0, 1, 1, "face_F_y", "nopf", "gemm")
        # nDim volume projection, luh (input/output) is not padded
        self.context["matmulConfigs"]["volume_x"] =      MatmulConfig(nData   , nDof, nDof, nData         , nDofPad, nDataPad     , 1, 0, 0, 1, "volume_x", "nopf", "gemm") # input slice not aligned
        if(nDim==3):
            self.context["matmulConfigs"]["volume_y"] =  MatmulConfig(nDataPad, nDof, nDof, nDataPad*nDof , nDofPad, nDataPad*nDof, 1, 0, 1, 1, "volume_y", "nopf", "gemm")
            self.context["matmulConfigs"]["volume_z"] =  MatmulConfig(nData   , nDof, nDof, nDataPad*nDof2, nDofPad, nData*nDof2  , 1, 0, 1, 0, "volume_z", "nopf", "gemm") # output slice not aligned
            self.context["matmulConfigs"]["volume_z_add"] = MatmulConfig(nData, nDof, nDof, nDataPad*nDof2, nDofPad, nData*nDof2  , 1, 1, 1, 0, "volume_z_add", "nopf", "gemm") # output slice not aligned, add to result
        else:
            self.context["matmulConfigs"]["volume_y"] =  MatmulConfig(nData   , nDof, nDof, nDataPad*nDof , nDofPad, nData*nDof   , 1, 0, 1, 0, "volume_y", "nopf", "gemm") # output slice not aligned
            self.context["matmulConfigs"]["volume_y_add"] = MatmulConfig(nData, nDof, nDof, nDataPad*nDof , nDofPad, nData*nDof   , 1, 1, 1, 0, "volume_y_add", "nopf", "gemm") # output slice not aligned, add to result
