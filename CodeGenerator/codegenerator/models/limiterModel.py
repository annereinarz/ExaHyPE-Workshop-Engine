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
# Generate the converter, used to mix generic and optimized kernels
#


from .abstractModelBaseClass import AbstractModelBaseClass

from ..utils import MatmulConfig


class LimiterModel(AbstractModelBaseClass):
    
    def generateCode(self):
        if(not self.context['useLimiter']):
            return None
        
        self.render("limiter_cpp.template", "limiter.cpp")
        # generates gemms
        if(self.context["useLibxsmm"]):
            self.controller.generateGemms("asm_limiter.c", self.context["matmulConfigs"].values())
    
    
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
        nDofLim  = self.context["nDofLim"]
        nDofLim2 = nDofLim*nDofLim
        nDofLimPad = self.context["nDofLimPad"]
        nDim     = self.context["nDim"]

        # Always overwrite input (no need to set to 0)
        # Project to FV
        self.context["matmulConfigs"]["dg2fv_x"] =     MatmulConfig(nData   , nDofLim, nDof, nData            , nDofPad, nDataPad         , 1, 0, 0, 1, "dg2fv_x", "nopf", "gemm") # input slice not aligned
        if(nDim==3):
            self.context["matmulConfigs"]["dg2fv_y"] = MatmulConfig(nDataPad, nDofLim, nDof, nDofLim*nDataPad , nDofPad, nDofLim*nDataPad , 1, 0, 1, 1, "dg2fv_y", "nopf", "gemm") #M is padded in both input and output
            self.context["matmulConfigs"]["dg2fv_z"] = MatmulConfig(nData   , nDofLim, nDof, nDofLim2*nDataPad, nDofPad, nDofLim2*nData   , 1, 0, 1, 0, "dg2fv_z", "nopf", "gemm") # output slice not aligned
        else:
            self.context["matmulConfigs"]["dg2fv_y"] = MatmulConfig(nData   , nDofLim, nDof, nDofLim*nDataPad , nDofPad, nDofLim*nData    , 1, 0, 1, 0, "dg2fv_y", "nopf", "gemm") # output slice not aligned
        
        # Project to DG
        self.context["matmulConfigs"]["fv2dg_x"] =     MatmulConfig(nData   , nDof, nDofLim, nData         , nDofLimPad, nDataPad      , 1, 0, 0, 1, "fv2dg_x", "nopf", "gemm") # input slice not aligned
        if(nDim==3):
            self.context["matmulConfigs"]["fv2dg_y"] = MatmulConfig(nDataPad, nDof, nDofLim, nDof*nDataPad , nDofLimPad, nDof*nDataPad , 1, 0, 1, 1, "fv2dg_y", "nopf", "gemm") #M is padded in both input and output
            self.context["matmulConfigs"]["fv2dg_z"] = MatmulConfig(nData   , nDof, nDofLim, nDof2*nDataPad, nDofLimPad, nDof2*nData   , 1, 0, 1, 0, "fv2dg_z", "nopf", "gemm") # output slice not aligned
        else:
            self.context["matmulConfigs"]["fv2dg_y"] = MatmulConfig(nData   , nDof, nDofLim, nDof*nDataPad , nDofLimPad, nDof*nData    , 1, 0, 1, 0, "fv2dg_y", "nopf", "gemm") # output slice not aligned
        
        # Project to Lobatto for Min/Max
        self.context["matmulConfigs"]["uh2lob_x"] =           MatmulConfig(nData   , nDof, nDof, nData         , nDofPad, nDataPad      , 1, 0, 0, 1, "uh2lob_x", "nopf", "gemm") # input slice not aligned
        if(nDim==3):
            self.context["matmulConfigs"]["uh2lob_y"] =       MatmulConfig(nDataPad, nDof, nDof, nDof*nDataPad , nDofPad, nDof*nDataPad , 1, 0, 1, 1, "uh2lob_y", "nopf", "gemm") #M is padded in both input and output
            self.context["matmulConfigs"]["uh2lob_z_slice"] = MatmulConfig(nDataPad, nDof, nDof, nDof2*nDataPad, nDofPad, nDataPad      , 1, 0, 1, 1, "uh2lob_z_slice", "nopf", "gemm") # will only write a slice, overwrite
        else:
            self.context["matmulConfigs"]["uh2lob_y_slice"] = MatmulConfig(nDataPad, nDof, nDof, nDof*nDataPad , nDofPad, nDataPad      , 1, 0, 1, 1, "uh2lob_y_slice", "nopf", "gemm") # will only write a slice, overwrite
        
        # Project to FV for Min/Max, reuse previous gem except last one for slice
        if(nDim==3):
            self.context["matmulConfigs"]["dg2fv_z_slice"] = MatmulConfig(nDataPad , nDofLim, nDof, nDofLim2*nDataPad, nDofPad, nDataPad , 1, 0, 1, 1, "dg2fv_z_slice", "nopf", "gemm") # will only write a slice, overwrite
        else:
            self.context["matmulConfigs"]["dg2fv_y_slice"] = MatmulConfig(nDataPad , nDofLim, nDof, nDofLim*nDataPad , nDofPad, nDataPad , 1, 0, 1, 1, "dg2fv_y_slice", "nopf", "gemm") # will only write a slice, overwrite
