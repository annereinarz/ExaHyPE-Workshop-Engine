import sys
import os

class Configuration:

    ######################################
    ###### Configuration parameters ######
    ######################################
    # Change them if required
    
    # absolute path to jinja2
    pathToJinja2      = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "jinja"))
    
    # absolute path to markupsafe (jinja2 dependency)
    pathToMarkupsafe  = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "markupsafe"))
    
    # absolute path to jsonschema
    pathToJSONSchema  = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "jsonschema"))
    
    # absolute path to attr (jsonschema dependency, inside attrs/src)
    pathToAttr        = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "attrs", "src"))
    
    # absolute path to pyrsistent (jsonschema dependency)
    pathToPyrsistent  = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "pyrsistent"))
        
    # absolute path to six (pyrsistent dependency)
    pathToSix         = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "Submodules", "six"))
    
    # absolute path to the schema file
    pathToSchema      = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "exahype-specfile.schema.json"))


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
    # Check jsonschema
    sys.path.insert(1, Configuration.pathToJSONSchema)
    sys.path.insert(1, Configuration.pathToAttr)       #jsonschema dependency
    sys.path.insert(1, Configuration.pathToPyrsistent) #jsonschema dependency
    sys.path.insert(1, Configuration.pathToSix)        #pyrsistent dependency
    import jsonschema
    # Remove added path
    sys.path.remove(Configuration.pathToJinja2)
    sys.path.remove(Configuration.pathToMarkupsafe)
    sys.path.remove(Configuration.pathToJSONSchema)
    sys.path.remove(Configuration.pathToAttr)
    sys.path.remove(Configuration.pathToPyrsistent)
