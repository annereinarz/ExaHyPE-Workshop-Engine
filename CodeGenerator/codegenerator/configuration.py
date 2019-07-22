import sys
import os

class Configuration:

    ######################################
    ###### Configuration parameters ######
    ######################################
    # Change them if required

    # path to the root of ExaHyPe from this file
    pathToExaHyPERoot          = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))

    # path to the gemm generator from this file
    pathToLibxsmmGemmGenerator = os.path.abspath(os.path.join(pathToExaHyPERoot, "Submodules", "libxsmm", "bin", "libxsmm_gemm_generator"))
    
    # path to jinja2
    pathToJinja2               = os.path.abspath(os.path.join(pathToExaHyPERoot, "Submodules", "jinja"))
    
    # path to markupsafe
    pathToMarkupsafe           = os.path.abspath(os.path.join(pathToExaHyPERoot, "Submodules", "markupsafe"))
    
    # simd size of the accepted architectures
    simdWidth = { "noarch" : 1,
                  "wsm"    : 2,
                  "snb"    : 4,
                  "hsw"    : 4,
                  "knc"    : 8,
                  "knl"    : 8,
                  "skx"    : 8
                }

    # set to false to use standard loops instead of libxsmm
    useLibxsmm = True;
    
    # set to true to print models runtime
    runtimeDebug = False;

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
    # Remove added path
    sys.path.remove(Configuration.pathToJinja2)
    sys.path.remove(Configuration.pathToMarkupsafe)
