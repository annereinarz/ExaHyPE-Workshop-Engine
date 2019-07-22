import os
import copy

from .models import *
from .toolkitHelper import ToolkitHelper

class SolverController:

    def __init__(self, solverSpec, baseContext):
        self.solverSpec = solverSpec
        self.baseContext = baseContext


    def processModelOutput(self, output, contextsList, logger):
        """Standard model output is (paths, context)
        
        Log the path of the generated file (== not None) using the logger and
        store the context used in the contextList
        
        return the context
        """
        paths, context = output
        for path in filter(None, paths):
            logger.info("Generated '"+path+"'")
        if "codegeneratorContext" in context:
            logger.info("Codegenerator used, command line to get the same result: "+context["codegeneratorContext"]["commandLine"])
        contextsList.append(context)
        
        return context


    def run(self, logger):
        solverContextsList = []
        for i,solver in enumerate(self.solverSpec):
            logger.info("Generating solver[%d] = %s..." % (i, solver["name"]))
            if solver["type"]=="ADER-DG": 
                model = solverModel.SolverModel(self.buildADERDGSolverContext(solver))
                solverContext = self.processModelOutput(model.generateCode(), solverContextsList, logger)
            elif solver["type"]=="Finite-Volumes":
                model = solverModel.SolverModel(self.buildFVSolverContext(solver))
                solverContext = self.processModelOutput(model.generateCode(), solverContextsList, logger)
            elif solver["type"]=="Limiting-ADER-DG":
                aderdgContext = self.buildADERDGSolverContext(solver)
                fvContext     = self.buildFVSolverContext(solver)
                context       = self.buildLimitingADERDGSolverContext(solver)
                # modifications
                fvContext["solver"]         = context["FVSolver"]
                fvContext["solverType"]     = "Finite-Volumes"
                fvContext["abstractSolver"] = context["FVAbstractSolver"]
                fvContext["patchSize"]      = 2 * aderdgContext["order"] + 1
                
                aderdgContext["solver"]                 = context["ADERDGSolver"]
                aderdgContext["solverType"]             = "ADER-DG"
                aderdgContext["abstractSolver"]         = context["ADERDGAbstractSolver"]
                aderdgContext["numberOfDMPObservables"] = context["numberOfDMPObservables"]
                aderdgContext["ghostLayerWidth"]        = fvContext["ghostLayerWidth"]
                self.addCodegeneratorPathAndNamespace(aderdgContext) # refresh path and namespace
                
                # generate all solver files
                model = solverModel.SolverModel(fvContext)
                context["fvContext"] = self.processModelOutput(model.generateCode(), [], logger) #don't register context
                model = solverModel.SolverModel(aderdgContext)
                context["aderdgContext"] = self.processModelOutput(model.generateCode(), [], logger) #don't register context
                if "codegeneratorContext" in context["aderdgContext"]:
                    context["codegeneratorContext"] = context["aderdgContext"]["codegeneratorContext"] #move codegencontext one up if it exists
                model = solverModel.SolverModel(context)
                solverContext = self.processModelOutput(model.generateCode(), solverContextsList, logger)
                
            solverContext["plotters"] = []
            for j, plotter in enumerate(solver.get("plotters",[])):
                logger.info("Generating plotter[%d]= %s ,for solver[%d]= %s..." % (j, plotter["name"], i, solver["name"]))
                plotModel = plotterModel.PlotterModel(self.buildPlotterContext(solver,plotter))
                self.processModelOutput(plotModel.generateCode(), solverContext["plotters"], logger)
            
        logger.info("Solver generation... done")
        
        return solverContextsList


    def buildBaseSolverContext(self, solver):
        context = copy.copy(self.baseContext)
        
        context["solverType"]     = solver["type"]
        context["solver"]         = solver["name"]
        context["abstractSolver"] = "Abstract"+context["solver"]
        
        nVar          = ToolkitHelper.count_variables(ToolkitHelper.parse_variables(solver,"variables"))
        nParam        = ToolkitHelper.count_variables(ToolkitHelper.parse_variables(solver,"material_parameters"))
        nGlobalObs    = ToolkitHelper.count_variables(ToolkitHelper.parse_variables(solver,"global_observables"))
        nPointSources = solver["point_sources"] if type(solver.get("point_sources",[])) is int else len(solver.get("point_sources",[]))
        
        context["numberOfVariables"]          = nVar
        context["numberOfParameters"]          = nParam
        context["numberOfMaterialParameters"] = nParam
        context["numberOfGlobalObservables"]  = nGlobalObs
        context["numberOfPointSources"]       = nPointSources
        
        # variables access class
        context["variablesMap"]  = ToolkitHelper.parse_variables(solver,"variables")
        if nParam>0:
            parametersMap = ToolkitHelper.parse_variables(solver,"material_parameters")
            # Increase offset of parameters, they are located directly after variables.
            def increaseOffset(param):
                param["offset"] += nVar
                return param
            parametersMap = [increaseOffset(param) for param in parametersMap]
            context["variablesMap"] += parametersMap
        context["variablesMapSize"] = len(context["variablesMap"])
        context["variables_as_str"] = ToolkitHelper.variables_to_str(solver,"variables")
        context["material_parameters_as_str"]  = ToolkitHelper.variables_to_str(solver,"material_parameters")
        context["global_observables_as_str"]   = ToolkitHelper.variables_to_str(solver,"global_observables")
        
        context["range_0_nVar"]          = range(0,nVar)
        context["range_0_nVarParam"]     = range(0,nVar+nParam)
        context["range_0_nGlobalObs"]    = range(0,nGlobalObs)    # nGlobalObs might be 0
        context["range_0_nPointSources"] = range(0,nPointSources) # nPointSources might be 0
        
        context["namingSchemes"]={} # TODO read from spec
        
        return context


    def buildADERDGSolverContext(self, solver):
        context = self.buildBaseSolverContext(solver)
        context["type"] = "ADER-DG"
        context.update(self.buildADERDGKernelContext(solver["aderdg_kernel"]))
        context.update(self.buildKernelOptimizationContext(solver["aderdg_kernel"], context))
        self.addCodegeneratorPathAndNamespace(context)
        
        context["order"]                  = solver["order"]
        context["numberOfDMPObservables"] = 0 # overwrite if called from LimitingADERDGSolver creation
        
        return context


    def buildLimitingADERDGSolverContext(self, solver):
        context = self.buildBaseSolverContext(solver)
        context["type"] = "Limiting-ADER-DG"
        context["order"]                  = solver["order"]
        context["numberOfDMPObservables"] = solver["limiter"]["dmp_observables"]
        context["implementation"]         = solver["limiter"].get("implementation","generic")
        context["ADERDGSolver"]         = solver["name"]+"_ADERDG"
        context["FVSolver"]             = solver["name"]+"_FV"
        context["ADERDGAbstractSolver"] = "Abstract"+solver["name"]+"_ADERDG"
        context["FVAbstractSolver"]     = "Abstract"+solver["name"]+"_FV"
        
        return context


    def buildFVSolverContext(self, solver):
        context = self.buildBaseSolverContext(solver)
        context["type"] = "Finite-Volumes"
        context.update(self.buildFVKernelContext(solver["fv_kernel"]))
        context["patchSize"] = solver.get("patch_size",-1) # overwrite if called from LimitingADERDGSolver creation
        
        return context


    def buildADERDGKernelContext(self, kernel):
        context = {}
        context["implementation"]          = kernel.get("implementation","generic")
        context["useMaxPicardIterations"]  = kernel.get("space_time_predictor",{}).get("fix_picard_iterations",False)!=False
        context["tempVarsOnStack"]         = kernel.get("allocate_temporary_arrays","heap")=="stack" 
        context["patchwiseAdjust"]         = kernel.get("adjust_solution","pointwise")=="patchwise" 
        context["language"]                = kernel.get("language","C").lower()
        context["basis"]                   = kernel.get("basis","Legendre").lower()
        context["isLinear"]                = not kernel.get("nonlinear",True)
        context["isLinear_s"]              = "true" if not kernel.get("nonlinear",True) else "false"
        context["isNonlinear"]             = kernel.get("nonlinear",True)
        context["linearOrNonlinear"]       = "Linear" if context["isLinear"] else "Nonlinear"
        context["isFortran"]               = kernel.get("language",False)=="Fortran" 
        context["useCERK"]                 = kernel.get("space_time_predictor",{}).get("cerkguess",False)
        context["useSplitCKScalar"]        = kernel.get("space_time_predictor",{}).get("split_ck","disabled") == "scalar"
        context["useSplitCKVect"]          = kernel.get("space_time_predictor",{}).get("split_ck","disabled") == "vectorised"
        context["noTimeAveraging"]         = kernel.get("space_time_predictor",{}).get("notimeavg",False)
        context["noTimeAveraging_s"]       = "true" if kernel.get("space_time_predictor",{}).get("notimeavg",False) else "false"
        context.update(self.buildKernelTermsContext(kernel["terms"]))
        return context


    def buildFVKernelContext(self,kernel):
        context = {}
        ghostLayerWidth = { "godunov" : 1, "musclhancock" : 2 }
        context["finiteVolumesType"]           = kernel["scheme"].replace("robust","")
        context["ghostLayerWidth"]             = ghostLayerWidth[context["finiteVolumesType"]]
        context["useRobustDiagonalLimiting_s"] = "true" if "robust" in kernel["scheme"] else "false"
        context["slopeLimiter"]   = kernel.get("slope_limiter","minmod")

        context["implementation"]  = kernel.get("implementation","generic")
        context["tempVarsOnStack"] = kernel.get("allocate_temporary_arrays","heap")=="stack" 
        context["patchwiseAdjust"] = kernel.get("adjust_solution","pointwise")=="patchwise" 
        context.update(self.buildKernelTermsContext(kernel["terms"]))
        return context


    def buildKernelTermsContext(self,terms):
        context = {}
        context["useFlux"]                  = "flux" in terms or "viscous_flux" in terms
        context["useFlux_s"]                = "true" if context["useFlux"] else "false"
        context["useSource"]                = "source" in terms
        context["useSource_s"]              = "true" if context["useSource"] else "false"
        context["useNCP"]                   = "ncp" in terms
        context["useNCP_s"]                 = "true" if context["useNCP"] else "false"
        context["usePointSources"]          = "point_sources" in terms
        context["usePointSources_s"]        = "true" if context["usePointSources"] else "false"
        context["useMaterialParameters"]    = "material_parameters" in terms
        context["useMaterialParameters_s"]  = "true" if context["useMaterialParameters"] else "false"
        context["useViscousFlux"]           = "viscous_flux" in terms
        context["useViscousFlux_s"]         = "true" if context["useViscousFlux"] else "false"
        
        return context


    def buildPlotterContext(self,solver,plotter):
        context = self.buildBaseSolverContext(solver)

        context["plotter"]          = plotter["name"]
        context["writtenUnknowns"]  = ToolkitHelper.count_variables(ToolkitHelper.parse_variables(plotter,"variables"))
        context["variables_as_str"] = ToolkitHelper.variables_to_str(plotter,"variables")
        context["plotterType"]      = plotter["type"] if type(plotter["type"]) is str else "::".join(plotter["type"])
        
        context["headerPath"]       = os.path.join(context["plotterSubDirectory"],(context["plotter"]+".h"))
        if context["plotterSubDirectory"] != "":
            context["outputPath"]   = os.path.join(context["outputPath"], context["plotterSubDirectory"])

            # This might be the wrong place, but we need to make sure the plotterSubDirectory exists.
            os.makedirs(context["outputPath"], exist_ok=True)
            
        return context


    def buildKernelOptimizationContext(self, kernel, solverContext):
        optimizations = kernel.get("optimised_kernel_debugging",[]) + kernel.get("optimised_terms",[])
        context = {}
        context["useConverter"]       = "converter"        in optimizations
        context["countFlops"]         = "flops"            in optimizations
        context["useFluxVect"]        = "flux_vect"        in optimizations
        context["useFusedSource"]     = "fusedsource"      in optimizations
        context["useFusedSourceVect"] = "fusedsource_vect" in optimizations
        context["useSourceVect"]      = "source_vect"      in optimizations
        context["useNCPVect"]         = "ncp_vect"         in optimizations
        context["useMaterialParametersVect"] = "material_parameters_vect" in optimizations
        
        return context
        
    def addCodegeneratorPathAndNamespace(self, context):
        context["optKernelPath"]      = os.path.join("kernels", context["project"] + "_" + context["solver"])
        context["optNamespace"]       = context["project"] + "::" + context["solver"] + "_kernels::aderdg"
