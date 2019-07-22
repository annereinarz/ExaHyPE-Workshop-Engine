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
# Generates solver header and source files
#


import copy

from .abstractModelBaseClass import AbstractModelBaseClass
from .codegeneratorModel import CodegeneratorModel


class SolverModel(AbstractModelBaseClass):


    def generateADERDGSolverFiles(self):
        solverTemplates = {
            "user"    : [ (self.context["solver"]+".h"   , "solvers/MinimalADERDGSolverHeader.template"),
                          (self.context["solver"]+".cpp" , "solvers/EmptyADERDGSolverImplementation.template") ],
            "generic" : [ (self.context["solver"]+".h"   , "solvers/ADERDGSolverHeader.template"),
                          (self.context["solver"]+".cpp" , "solvers/ADERDGSolverInCUserCode.template") ],
        }
        solverTemplates["optimised"] = solverTemplates["generic"]
        
        abstractSolverTemplates  = { 
            "user"      :  [],
            "generic"   :  [ (self.context["abstractSolver"]+".cpp" , "solvers/AbstractGenericADERDGSolverImplementation.template"),
                             (self.context["abstractSolver"]+".h"   , "solvers/AbstractGenericADERDGSolverHeader.template") ],
            "optimised" :  [ (self.context["abstractSolver"]+".cpp" , "solvers/AbstractOptimisedADERDGSolverImplementation.template"),
                             (self.context["abstractSolver"]+".h"   , "solvers/AbstractOptimisedADERDGSolverHeader.template") ],
        }
        
        implementation = self.context["implementation"]
        
        paths = [] # path is None if nothing was generated
        if implementation == "optimised":
            codegenModel = CodegeneratorModel()
            _ , self.context["codegeneratorContext"] = codegenModel.generateCode(self.context) #call codegenerator and store context used
        for filePath,template in solverTemplates.get(implementation,[]):
          paths.append(self.render(template,filePath,overwrite=False)[0])
        for filePath,template in abstractSolverTemplates.get(implementation,[]):
          paths.append(self.render(template,filePath)[0])
        
        if implementation != "user":
            paths.append(self.render("variables/VariablesHeader.template",self.context["solver"]+"_Variables.h")[0])
        
        return paths, self.context


    def generateFiniteVolumesSolverFiles(self):
        solverTemplates = {
            "user"    : [ (self.context["solver"]+".h"   , "solvers/MinimalFiniteVolumesSolverHeader.template"),
                          (self.context["solver"]+".cpp" , "solvers/EmptyFiniteVolumesSolverImplementation.template") ],
            "generic" : [ (self.context["solver"]+".h"   , "solvers/GenericFiniteVolumesSolverHeader.template"),
                          (self.context["solver"]+".cpp" , "solvers/GenericFiniteVolumesSolverInCUserCode.template") ],
        }
        
        abstractSolverTemplates  = { 
            "generic"   :  [ (self.context["abstractSolver"]+".cpp" , "solvers/AbstractGenericFiniteVolumesSolverImplementation.template"),
                             (self.context["abstractSolver"]+".h"   , "solvers/AbstractGenericFiniteVolumesSolverHeader.template") ]
        }
       
        implementation = self.context["implementation"]
        
        if implementation=="optimised": # TODO
            print("ERROR: optimised FV kernels not available yet.",file=sys.stderr)
            raise
            
        paths = [] # path is None if nothing was generated
        for filePath,template in solverTemplates.get(implementation,[]):
          paths.append(self.render(template,filePath,overwrite=False)[0])
        for filePath,template in abstractSolverTemplates.get(implementation,[]):
          paths.append(self.render(template,filePath)[0])
        
        if implementation != "user":
            paths.append(self.render("variables/VariablesHeader.template",self.context["solver"]+"_Variables.h")[0])
        
        return paths, self.context


    def generateLimitingADERDGSolverFiles(self):
        solverTemplates = {  }
        abstractSolverTemplates  = { 
            "generic"   :  [ (self.context["solver"]+".cpp" , "solvers/GenericLimiterSolverImplementation.template"),
                             (self.context["solver"]+".h"   , "solvers/GenericLimiterSolverHeader.template") ],
            "optimised" :  [ (self.context["solver"]+".cpp" , "solvers/OptimisedLimiterSolverImplementation.template"),
                             (self.context["solver"]+".h"   , "solvers/OptimisedLimiterSolverHeader.template") ],
        }
        
        implementation = self.context["implementation"]
        
        if implementation=="user": # TODO
            print("ERROR: optimised FV kernels not available (yet).",file=sys.stderr)
            raise
        
        paths = [] # path is None if nothing was generated
        for filePath,template in abstractSolverTemplates.get(implementation,[]):
          paths.append(self.render(template,filePath)[0])
        
        return paths, self.context


    def generateCode(self):
        generators = { 
          "ADER-DG"          : self.generateADERDGSolverFiles,
          "Finite-Volumes"   : self.generateFiniteVolumesSolverFiles,
          "Limiting-ADER-DG" : self.generateLimitingADERDGSolverFiles
        }
       
        return generators[self.context["solverType"]]() # (paths, context)
