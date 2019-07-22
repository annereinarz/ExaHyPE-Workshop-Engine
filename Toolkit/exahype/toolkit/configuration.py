import sys
import os

class Configuration:

    ######################################
    ###### Configuration parameters ######
    ######################################
    # Change them if required

    # absolute path to ExaHyPE's root (we need absolute paths in generated Makefile)
    pathToExaHyPERoot   = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
    
    # absolute path to jinja2
    pathToJinja2        = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "jinja"))
    
    # absolute path to markupsafe (jinja2 dependency)
    pathToMarkupsafe    = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "markupsafe", "src"))
    
    # absolute path to the codegenerator module
    pathToCodegenerator = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "CodeGenerator"))
    
    # absolute path to the specfile module
    pathToSpecfiles    = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    
    alignmentPerArchitectures  = {
        "noarch" : 16,
        "wsm"    : 16,
        "snb"    : 32, 
        "hsw"    : 32, 
        "knc"    : 64, 
        "knl"    : 64,
        "skx"    : 64,
    }


    @staticmethod
    def checkPythonVersion():
        """check version. Python 3.3 required"""
        requiredVersion = (3,3)
        currentVersion  = sys.version_info
        if(requiredVersion > currentVersion):
            sys.exit("Requires Python 3.3 or newer. Abort.")



def checkDependencies():
    """check all dependencies are reachable from the configuration path"""
    # Check jinja
    sys.path.insert(1, Configuration.pathToJinja2)
    sys.path.insert(1, Configuration.pathToMarkupsafe)
    import jinja2
    # Check codegenerator
    sys.path.insert(1, Configuration.pathToCodegenerator)
    import codegenerator
    codegenerator.checkDependencies()
    # Check specfile
    sys.path.insert(1, Configuration.pathToSpecfiles)
    import specfiles
    specfiles.checkDependencies()
    # Remove added path
    sys.path.remove(Configuration.pathToJinja2)
    sys.path.remove(Configuration.pathToMarkupsafe)
    sys.path.remove(Configuration.pathToCodegenerator)
    sys.path.remove(Configuration.pathToSpecfiles)
