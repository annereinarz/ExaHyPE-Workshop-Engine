"""
Roughly a 1:1 translation from the Java DirectoryAndPathChecker. It is something one could offload
in theory onto the JSON-Schema, but it is not really something which belongs to a schema.
"""

import os

from .toolkitHelper import BadSpecificationFile

class DirectoryAndPathChecker:


    def __init__(self, paths, log=None):
        self.log = log.getChild(__name__)
        self.check("Peano kernel path", paths["peanoToolboxPath"], required_subdirs=["tarch", "peano"])
        #check("Peano kernel path tarch sources", paths["peanoKernelPath"].joinpath("tarch/"))
        self.check("Peano toolboxes path", paths["peanoToolboxPath"], required_subdirs=["multiscalelinkedcell", "sharedmemoryoracles", "mpibalancing"])
        
        self.check("ExaHyPE path", paths["exahypePath"])
        self.check("Output directory", paths["outputPath"], makedirs=True)


    def check(self, human_readable, path, required_subdirs=[], verbose_show_path=True, makedirs=False):
        valid = os.path.isdir(path)
        human_status = "ok" if valid else "not found"
        if verbose_show_path or not valid:
            human_readable +=  ": " + str(os.path.abspath(path))
        if makedirs:
            if valid:
                human_status = "does exist (will not be overwritten)"
            else:
                os.makedirs(path) # todo, catch exceptions
                human_status = "created"
        
        if self.log:
            self.log.info(human_readable + " ... " + human_status)
        
        if not os.path.isdir(path): # check again in case of makedirs
            raise BadSpecificationFile("%s (relative to directory %s)" % (human_readable, os.getcwd()))
        
        for subdir in required_subdirs:
            self.check("%s holds %s" % (human_readable, subdir), os.path.join(path, subdir), verbose_show_path=False, makedirs=makedirs)

