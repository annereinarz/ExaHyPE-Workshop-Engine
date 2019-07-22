#!/usr/bin/env python3

"""
This mimics the classical Frontend of the ExaHyPE toolkit.
"""

import os, sys, argparse, subprocess, datetime, json, logging
from pathlib import Path

# specfile module import
from specfiles import validate, OmniReader

# local import
from .directories import DirectoryAndPathChecker
from .toolkitHelper import BadSpecificationFile
from .toolkitHelper import ToolkitHelper
from .configuration import Configuration
from .solverController import SolverController
from .models import *

from tools import tools

class Controller:
    def header(self):
        info = {
            "gittag": "N/A",
            "year": str(datetime.datetime.now().year)
        }
        try:
           info["gittag"] = subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).strip().decode('ascii')
        except:
           info["gittag"]="N/A"

        return """
    ______           __  __      ____  ______    
   / ____/  ______ _/ / / /_  __/ __ \/ ____/    *************************
  / __/ | |/_/ __ `/ /_/ / / / / /_/ / __/        The ExaHyPE Toolkit v2
 / /____>  </ /_/ / __  / /_/ / ____/ /___           www.exahype.eu
/_____/_/|_|\__,_/_/ /_/\__, /_/   /_____/          Commit: %(gittag)s
                       /____/                    *************************

 This project has received funding from the European Union's Horizon 2020
 research and innovation programme under grant agreement No 671698 (ExaHyPE).

 Copyright (c) %(year)s, All rights reserved
 ExaHyPE is based  on the PDE framework Peano (www.peano-framework.org).
 Released under the BSD 3 Open Source License.
 
""" % info
        # end of header

    def wait_interactive(self, msg):
        if self.interactive:
            print("\n\n\n\n")
            print(msg + "... ok")
            input("<pres Enter>")
            print("\n\n\n\n")
    
    def __init__(self):
        """TODO"""
        # Check the python version according to the configuration
        Configuration.checkPythonVersion()
        
        logging.basicConfig(format="%(filename)s:%(lineno)s(%(funcName)s):%(levelname)s %(message)s")
        self.log = logging.getLogger()

        # init tools
        tools.initRegistry(self.log)

        # parse command line arguments
        args = self.parseArgs()
        
        # set member values from args
        self.specfileName = args.specfile.name # note, this can be something like "<stdin>"
        self.interactive = args.interactive or not args.not_interactive
        self.debug=args.debug
        self.verbose = args.verbose or self.interactive or self.debug
        self.write_json = args.write_json
        self.strict_json = args.strict_json
        
        if self.verbose:
            self.log.setLevel(logging.INFO)
            if self.debug:
                self.log.setLevel(logging.DEBUG)
       
            self.log.info(self.header())
            logging.raiseExceptions = True # development mode
            codegeneratorModel.CodegeneratorModel.logger = self.log #codegeneratorModel will print the associated command line
        else:
            logging.raiseExceptions = False # production mode
        
        inpath = Path(self.specfileName)
        if inpath.exists():
            self.log.info("Read input file %s." % inpath.resolve())
        else:
            self.log.info("Read from stdin (%s)" % str(args.specfile))
        
        rawSpec = self.getSpec(args.specfile, args.format)
        self.spec = self.validateAndSetDefaults(rawSpec, args.validate_only)

        self.runTools(args)
        
    def runTools(self,args):
      """
      Run all tools selected by the user.
      """
      argDict = vars(args)
      previousLevel = self.log.level
      self.log.setLevel(logging.INFO)
      for tool in tools.tools:
         if argDict[tool.id()]:
            tool.run(self.spec)            
      self.log.setLevel(previousLevel)

    def run(self):
        try:
            d = DirectoryAndPathChecker(self.buildBaseContext(), self.log)
        except BadSpecificationFile as e:
            self.log.error("Some directories did not exist and/or could not be created.")
            self.log.exception(e)
            self.log.error("Failure due to bad specification file, cannot continue")
            sys.exit(-4)
        
        self.wait_interactive("validated and configured pathes")
        
        try:
            solverControl = SolverController(self.spec.get("solvers",[]), self.buildBaseContext())
            solverContextsList = solverControl.run(self.log)
        except BadSpecificationFile as e:
            self.log.error("Could not create applications solver classes: %s" % str(e))
            self.log.exception(e)
            sys.exit(-6)
        
        self.wait_interactive("generated application-specific solver and plotter classes")
        
        try:
            # kernel calls
            kernelCalls = kernelCallsModel.KernelCallsModel(self.buildKernelCallsContext(solverContextsList))
            pathToKernelCalls, _ = kernelCalls.generateCode()
            self.log.info("Generated '"+pathToKernelCalls+"'")
        except Exception as e:
            self.log.error("Could not create ExaHyPE's kernel calls: %s" % str(e))
            self.log.exception(e)
            sys.exit(-8)
            
        self.wait_interactive("generated computational kernel calls")
        
        
        try:
            # README
            readme = readmeModel.ReadmeModel(self.buildReadmeContext(solverContextsList))
            pathToReadme, _ = readme.generateCode()
            self.log.info("Generated '"+pathToReadme+"'")
        except Exception as e:
            self.log.error("Could not create ExaHyPE's README: %s" % str(e))
            self.log.exception(e)
            sys.exit(-12)
            
        self.wait_interactive("generated computational README")
        
        try:
            makefile = makefileModel.MakefileModel(self.buildMakefileContext())
            pathToMakefile, _ = makefile.generateCode() #generate Makefile and get path to it
            makefileMessage = makefile.getOutputMessage()
            self.log.info("Generated '"+pathToMakefile+"'")
        except Exception as e:
            self.log.error("Could not create application-specific Makefile: %s" % str(e))
            self.log.exception(e)
            sys.exit(-10)
        
        self.wait_interactive("generated application-specific Makefile")
        
        self.checkEnvVariable()
        self.log.info(makefileMessage)
    
    def parseArgs(self):
        parser = argparse.ArgumentParser(
            description="This is the ExaHyPE toolkit, a small python command line tool for generating the glue code and make system for ExaHyPE applications. It fulfills the purpose of the 'configure' step in some tools and comes before the compilation.",
            epilog="See http://www.exahype.eu and the ExaHyPE Guidebook at http://dev.exahype.eu/guidebook.pdf for help. This is the new python-based toolkit (in action since August 2018). It replaces the older Java-based toolkit. It does not need to be compiled before usage. If you can read this message, you already setup everything to run the toolkit.",
        )
        
        # some mandatory arguments from Toolkit1
        optimized = parser.add_argument_group('Optimized kernels-specific options')
        optimized.add_argument("-c", "--clean-opt",
            help="Clean optimized kernels (only applicable if optimized kernels are used in Specfile)")

        ui = parser.add_argument_group("Toolkit user interface-specific options")
        g = ui.add_mutually_exclusive_group()
        g.add_argument("-i", "--interactive", action="store_true", default=False, help="Run interactively, triggers --verbose.")
        g.add_argument("-n", "--not-interactive", action="store_true", default=True, help="Run non-interactive in non-stop-mode (default)")
        ui.add_argument("-v", "--verbose", action="store_true", help="Be verbose (the new toolkit is off/quiet by default)")
        ui.add_argument("-d", "--debug", action="store_true", default=False, help="Debugging mode, prints more detailed error messages, triggers --verbose.")

        formats = parser.add_argument_group("Specification-file-format specific options")
        formats.add_argument("-o", "--validate-only", action="store_true", default=False, help="Validate input only, don't run the toolkit. Will output the correct JSON if passes.")
        formats.add_argument("-j", "--write-json", action="store_true", default=False, help="Write a JSON file with a similar filename if legacy specification file is read (*.exahype) (default: no)")
        formats.add_argument("-f", "--format", choices=OmniReader.available_readers(), default=OmniReader.any_format_name, help="Specification file format of the input file. 'any' will try all built-in-formats.")
        
        generator = parser.add_argument_group("Generator-specific glue code generation options which should actually be part of the specification files")
        generator.add_argument("-s", "--strict-json", action="store_true", default=False, help="Generate a specification file parser which does not call the toolkit as an external process during runtime but instead only accepts proper JSON as input")

        # add tools
        utils = parser.add_argument_group("Additional tools")
        for tool in tools.tools:
          utils.add_argument("--%s" % tool.id(),help=tool.help(),action="store_true")

        parser.add_argument('specfile',
            type=argparse.FileType('r'),
            help="The specification file to work on (can be .exahype, .exahype2, .json, etc.)")
        
        return parser.parse_args()
    
    
    def getSpec(self, file_handle, file_format=OmniReader.any_format_name):
        try:
            reader = OmniReader(self.log)
            spec = reader.read(file_handle.read(), required_file_format=file_format, filename=self.specfileName)
        except Exception as e:
            self.log.error("Could not read specification file '%s': %s" % (self.specfileName, str(e)))
            self.log.error("In order to fix this problem, please fix the format of your file with the command line flag --format=XXX where XXX is a supported specification file format.")
            if self.debug:
               self.log.exception(e)
            sys.exit(-3)

        # I find this is more a debugging feature...
        if self.specfileName.endswith(".exahype") and self.write_json:
            json_file_name = self.specfileName.replace(".exahype",".exahype2")
            with open(json_file_name, 'w') as outfile:
                json.dump(spec,outfile,indent=2)
                self.log.info("Write JSON file '%s' ... OK" % json_file_name)

        return spec
    
    
    def validateAndSetDefaults(self, spec, validate_only=False):
        """
        Given a specification, validate it  against the JSON-Schema and
        returns the native python data structure, enriched with default values from the schema.
        """
        try:
            spec = validate(spec, set_defaults=True)
        except Exception as e:
            if self.verbose:
                self.log.error("Specification file does not hold a valid ExaHyPE specification, it did not pass the schema validation step. The error message is: %s" % e)
            else:
                msg = str(e); length=len(msg); maxLength=600
                msg = msg[0:min(maxLength,length)]
                if length>maxLength:
                    msg += "\n(Error message cut off after "+str(maxLength)+" characters. Run with -v or -d to see the full message.)"
                self.log.error("Specification file does not hold a valid ExaHyPE specification, it did not pass the schema validation step. The error message is: %s" % 
                msg)
            self.log.exception(e)
            sys.exit(-4)

        if validate_only:
            print(json.dumps(spec, sort_keys=True, indent=4))
            sys.exit(0)
        else:
            return spec


    def buildBaseContext(self):
        """Generate base context from spec with commonly used value"""
        context = {}
        # commonly used paths
        context["outputPath"]           = os.path.join(Configuration.pathToExaHyPERoot, self.spec["paths"]["output_directory"])
        context["exahypePath"]          = os.path.join(Configuration.pathToExaHyPERoot, self.spec["paths"]["exahype_path"])
        context["peanoToolboxPath"]     = os.path.join(Configuration.pathToExaHyPERoot, self.spec["paths"]["peano_kernel_path"])
        context["plotterSubDirectory"]  = self.spec["paths"].get("plotter_subdirectory","").strip()
        
        # commonly used parameters
        context["project"]          = self.spec["project_name"]
        
        context["architecture"]     = self.spec["architecture"]
        context["alignment"]        = Configuration.alignmentPerArchitectures[context["architecture"]]
        
        context["dimensions"]   = self.spec["computational_domain"]["dimension"]
        context["range_0_nDim"] = range(0,context["dimensions"])
        
        context["enableProfiler"] = False # TODO read from spec
        
        return context
    

    def buildMakefileContext(self):
        """Generate context for the Makefile model"""
        context = self.buildBaseContext()
        
        context["architecture"]      = self.spec["architecture"]
        context["compilerFlags"]     = self.spec["compiler_flags"]
        context["linkerFlags"]       = self.spec["linker_flags"]
        context["useSharedMem"]      = "shared_memory" in self.spec;
        context["useDistributedMem"] = "distributed_memory" in self.spec;
        context["useIpcm"]   = False # TODO
        context["useLikwid"] = False # TODO
        context["likwidInc"] = ""    # TODO
        # kernels
        useOptKernel = False
        useFortran   = False
        for solver in self.spec["solvers"]:
            if "aderdg_kernel" in solver:
                useOptKernel = useOptKernel or solver["aderdg_kernel"]["implementation"]=="optimised"
                useFortran   = useFortran or solver["aderdg_kernel"]["language"]=="Fortran"
            if "fv_kernel" in solver:
                useOptKernel = useOptKernel or solver["fv_kernel"]["implementation"]=="optimised"
                useFortran   = useFortran or solver["fv_kernel"]["language"]=="Fortran"
        context["useOptKernel"] = useOptKernel
        context["useFortran"]   = useFortran
        
        return context
    
    
    def buildKernelCallsContext(self, solverContextsList):
        """Generate context for the KernelCalls model"""
        context = self.buildBaseContext()
        context["solvers"] = solverContextsList
        context["codegeneratorContextsList"] = [solverContext["codegeneratorContext"] for solverContext in solverContextsList if "codegeneratorContext" in solverContext]
        context["strictJSON"]            = self.strict_json
        context["specfileName"]          = self.specfileName
        context["specFileAsHex"]         = self.specfileAsHex(self.spec)
        context["externalParserCommand"] = "%s/%s %s" % ( Configuration.pathToExaHyPERoot, "Toolkit/toolkit.sh","--format=any --validate-only")
        
        return context
    
    
    def buildReadmeContext(self, solverContextsList):
        """Generate context for the Readme model"""
        context = self.buildBaseContext()
        context["solverContexts"] = solverContextsList
        
        return context
    
    
    def specfileAsHex(self,spec):
        """
        Given a native python nested dict/list object, dump it as string and then hex-encode that string
        character by character. This is safest way to include something in C++ without dealing with
        character sets or anything.
        """
        text = json.dumps(spec, sort_keys=True, indent=4)
        hex_tokens = [ "0x%02x"%ord(char) for char in text ] + ["0x00"] # null-terminated list of hex numbers
        return ", ".join(hex_tokens)
    
    
    def checkEnvVariable(self):
        """
        Check environment variables as an ultimate step. Should only be called when actually calling the toolkit,
        so it is distinct from the validation step.
        """
        if "shared_memory" in self.spec:
            if not "TBB_INC" in os.environ:
                self.log.warning("environment variable TBB_INC not set but required if code is built with TBB");
            if not "TBB_SHLIB" in os.environ:
                self.log.warning("environment variable TBB_SHLIB not set but required if code is built with TBB");
