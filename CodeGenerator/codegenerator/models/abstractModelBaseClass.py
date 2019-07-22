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
# Abstract base class for the Models
#
# provide a render method and the signature for generateCode
#
# copy the provided baseContext as local context (need to hard copy as a model can modify its local context)
#


import copy
import sys
import os

# add path to dependencies
from ..configuration import Configuration
sys.path.insert(1, Configuration.pathToJinja2)
sys.path.insert(1, Configuration.pathToMarkupsafe)

import jinja2

class AbstractModelBaseClass():
    """Base class of a Models
    
    Render and __init__ shouldn't need to be overriden
    
    Override generateCode to implement your model. 
    
    To generate gemms, pass the controller at construction and override 
    buildGemmsConfig() (called at initialization) to put the list of MatMult 
    configurations into the local context. Then generate the gemm using the 
    controler during generateCode()
    """

    def __init__(self, baseContext, baseController=None):
        self.context = copy.copy(baseContext) # copy the given baseContext as base for the local context
        self.controller = baseController      # pointer to the controller to generate gemms or get padding size if needed. None by default (if not needed)
        self.buildGemmsConfig()
    
    
    def buildGemmsConfig(self):
        """Generates the list of MaltMult"""
        pass
    
    
    def generateCode(self):
        """To be overriden
        
        Generate the code by filling self.context and calling 
        self.render(templatePath, outputPath)
        """
        sys.exit("Abstract method") # needs to be overriden
    
    
    # render a template to outputFilename using the given context (default = local context)
    def render(self, templateName, outputFilename, context=None):
        # set default context to local context if none given
        if context == None:
            context = self.context
        loader = jinja2.FileSystemLoader(os.path.realpath(os.path.join(os.path.dirname(__file__),'..','templates')))
        env = jinja2.Environment(loader=loader, trim_blocks=True, lstrip_blocks=True)
        template = env.get_template(templateName)                
        with open(os.path.join(context['pathToOutputDirectory'],outputFilename), 'w') as output:
            output.write(template.render(context))
