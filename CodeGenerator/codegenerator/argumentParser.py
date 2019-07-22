#!/bin/env python
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
# ArgumentParser
#
# @note
# requires python3


import argparse
from enum import Enum


class ArgumentParser:
    """Public API
    """
    
    class ArgType(Enum):
        """Types of arguments for the command line API"""
        MandatoryString =1  # name, type, help
        MandatoryInt    =2  # name, type, help
        OptionalBool    =11 # name (will add --), type, help
        OptionalInt     =12 # name (will add --), type, help, default value, metavar

    # List of all expected arguments for the command line or input validation
    args = [
        # mandatory arguments
        ("pathToApplication",   ArgType.MandatoryString, "path to the application as given by the ExaHyPE specification file (application directory as root)"),
        ("pathToOptKernel",     ArgType.MandatoryString, "desired relative path to the generated code (application directory as root)"),
        ("namespace",           ArgType.MandatoryString, "desired namespace for the generated code"),
        ("solverName",          ArgType.MandatoryString, "name of the user-solver"),
        ("numberOfVariables",   ArgType.MandatoryInt,    "the number of quantities"),
        ("numberOfParameters",  ArgType.MandatoryInt,    "the number of parameters (fixed quantities)"),
        ("order",               ArgType.MandatoryInt,    "the order of the approximation polynomial"),
        ("dimension",           ArgType.MandatoryInt,    "the number of spatial dimensions in the simulation (2 or 3)"),
        ("numerics",            ArgType.MandatoryString, "linear or nonlinear"),
        ("architecture",        ArgType.MandatoryString, "the microarchitecture of the target device"),
        # optional arguments
        ("useFlux",             ArgType.OptionalBool,    "enable flux"),
        ("useFluxVect",         ArgType.OptionalBool,    "enable vectorized flux (include useFlux)"),
        ("useNCP",              ArgType.OptionalBool,    "enable non conservative product"),
        ("useNCPVect",          ArgType.OptionalBool,    "enable vectorized non conservative product (include useNCP)"),
        ("useSource",           ArgType.OptionalBool,    "enable source terms"),
        ("useSourceVect",       ArgType.OptionalBool,    "enable vectorized source terms (include useSource)"),
        ("useFusedSource",      ArgType.OptionalBool,    "enable fused source terms (include useSource)"),
        ("useFusedSourceVect",  ArgType.OptionalBool,    "enable vectorized fused source terms (include useFusedSource and useSourceVect)"),
        ("useMaterialParam",    ArgType.OptionalBool,    "enable material parameters"),
        ("useMaterialParamVect",ArgType.OptionalBool,    "enable vectorized material parameters"),
        ("usePointSources",     ArgType.OptionalInt ,    "enable numberOfPointSources point sources", -1, "numberOfPointSources"),
        ("useCERKGuess",        ArgType.OptionalBool,    "use CERK for SpaceTimePredictor inital guess (nonlinear only)"),
        ("useSplitCKScalar",    ArgType.OptionalBool,    "use split Cauchy–Kowalevski formulation (linear only)"),
        ("useSplitCKVect",      ArgType.OptionalBool,    "use split Cauchy–Kowalevski formulation with vect PDE (linear only)"),
        ("useGaussLobatto",     ArgType.OptionalBool,    "use Gauss Lobatto Quadrature instead of Gauss Legendre"),
        ("useLimiter",          ArgType.OptionalInt,     "enable limiter with the given number of observable", -1, "numberOfObservable"),
        ("ghostLayerWidth",     ArgType.OptionalInt,     "use limiter with the given ghostLayerWidth, requires useLimiter option, default = 0", 0, "width"),
        ("tempVarsOnStack",     ArgType.OptionalBool,    "put the big scratch arrays on the stack instead of the heap (you can use ulimit -s to increase the stack size)"),
    ]
    
    @staticmethod
    def parseArgs():
        """Process the command line arguments"""
        parser = argparse.ArgumentParser(description="This is the front end of the ExaHyPE code generator.")
        
        for arg in ArgumentParser.args:
            key = arg[0]
            type = arg[1]
            info = arg[2]
            if   type == ArgumentParser.ArgType.MandatoryString:
                parser.add_argument(key, help=info)
            elif type == ArgumentParser.ArgType.MandatoryInt:
                parser.add_argument(key, type=int, help=info)
            elif type == ArgumentParser.ArgType.OptionalBool:
                parser.add_argument("--"+key, action="store_true", help=info)
            elif type == ArgumentParser.ArgType.OptionalInt:
                parser.add_argument("--"+key, type=int, default=arg[3], metavar=arg[4], help=info)
        
        return vars(parser.parse_args())


    @staticmethod
    def validateInputConfig(inputConfig):
        """Validate a config and add the default value of missing optional arguments"""
        for arg in ArgumentParser.args:
            key  = arg[0]
            type = arg[1]
            #check mandatory and raise error if not set or wrong type
            if   type == ArgumentParser.ArgType.MandatoryString:
                if key not in inputConfig or not isinstance(inputConfig[key], str):
                    raise ValueError("Invalid codegenerator configuration, argument "+key+" missing or of wrong type (string expected)")
            elif type == ArgumentParser.ArgType.MandatoryInt:
                if key not in inputConfig or not isinstance(inputConfig[key], int):
                    raise ValueError("Invalid codegenerator configuration, argument "+key+" missing or of wrong type (int expected)")
            #check optional and set it to default if not set
            elif type == ArgumentParser.ArgType.OptionalBool:
                if key not in inputConfig:
                    inputConfig[key] = False
            elif type == ArgumentParser.ArgType.OptionalInt:
                if key not in inputConfig:
                    inputConfig[key] = arg[3] #default value


    @staticmethod
    def buildCommandLineFromConfig(inputConfig):
        """Build a valid command line for the given config"""
        commandLine = "codegenerator "
        for arg in ArgumentParser.args:
            key  = arg[0]
            type = arg[1]
            # add mandatory parameters
            if   type == ArgumentParser.ArgType.MandatoryString:
                commandLine += inputConfig[key] + " "
            elif type == ArgumentParser.ArgType.MandatoryInt:
                commandLine += str(inputConfig[key]) + " "
            # check optional and add them if set and non default
            elif type == ArgumentParser.ArgType.OptionalBool:
                if key in inputConfig and inputConfig[key]:
                    commandLine += "--" + key + " "
            elif type == ArgumentParser.ArgType.OptionalInt:
                if key in inputConfig and inputConfig[key] != arg[3]:
                    commandLine += "--" + key + " " + str(inputConfig[key]) + " "
        
        return commandLine
