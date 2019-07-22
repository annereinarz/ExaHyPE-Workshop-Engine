
import sys

# add path to dependencies
from ..configuration import Configuration
sys.path.insert(1, Configuration.pathToCodegenerator)
import codegenerator


class CodegeneratorModel:


    def generateCode(self, solverContext):
        #translation from solverContext to codegeneratorContext
        codegeneratorContext = {
            # Mandatory parameters
            "pathToApplication"  : solverContext["outputPath"],
            "pathToOptKernel"    : solverContext["optKernelPath"],
            "namespace"          : solverContext["optNamespace"],
            "solverName"         : solverContext["project"] + "::" + solverContext["solver"],
            "numberOfVariables"  : solverContext["numberOfVariables"],
            "numberOfParameters" : solverContext["numberOfMaterialParameters"],
            "order"              : solverContext["order"],
            "dimension"          : solverContext["dimensions"],
            "numerics"           : "linear" if solverContext["isLinear"] else "nonlinear",
            "architecture"       : solverContext["architecture"],
            # Optional bool parameters (may set redundant flags and default false flag)
            "useFlux"            : solverContext["useFlux"],
            "useFluxVect"        : solverContext["useFluxVect"],
            "useNCP"             : solverContext["useNCP"],
            "useNCPVect"         : solverContext["useNCPVect"],
            "useSource"          : solverContext["useSource"],
            "useSourceVect"      : solverContext["useSourceVect"],
            "useFusedSource"     : solverContext["useFusedSource"],
            "useFusedSourceVect" : solverContext["useFusedSourceVect"],
            "useMaterialParam"   : solverContext["useMaterialParameters"],
            "useMaterialParamVect" : solverContext["useMaterialParametersVect"],
            "useCERKGuess"       : solverContext["useCERK"],
            "useSplitCKScalar"   : solverContext["useSplitCKScalar"],
            "useSplitCKVect"     : solverContext["useSplitCKVect"],
            "useGaussLobatto"    : solverContext["basis"] == "lobatto",
            # Optional int parameters (may set redundant flags)
            "usePointSources"    : solverContext["numberOfPointSources"] if solverContext["numberOfPointSources"] > 0 else -1,
            "tempVarsOnStack"    : solverContext["tempVarsOnStack"],
            "useLimiter"         : solverContext.get("numberOfDMPObservables", -1), #not set if not limiterSolver
            "ghostLayerWidth"    : solverContext.get("ghostLayerWidth", 0), #not set if not limiterSolver
        }
        
        # call the codegenerator with the given context
        codegeneratorController = codegenerator.Controller(codegeneratorContext)
        codegeneratorController.generateCode()
        
        # if verbose print the associated command line
        codegeneratorContext["commandLine"] = codegeneratorController.commandLine
        
        return solverContext["optKernelPath"], codegeneratorContext
