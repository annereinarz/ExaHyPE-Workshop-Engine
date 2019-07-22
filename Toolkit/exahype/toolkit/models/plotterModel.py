##
# @file This file is part of the ExaHyPE project.
# @author ExaHyPE Group (exahype@lists.lrz.de)
#
# @section LICENSE
#
# Copyright (c) 2016  http,//exahype.eu
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
# Generates plotter header and source files
#


from .abstractModelBaseClass import AbstractModelBaseClass


class PlotterModel(AbstractModelBaseClass):

    def generateCode(self):
        userDefined = self.context["plotterType"]=="user::defined"
        
        templates = {
            True : [ (self.context["plotter"]+".h"   , "plotters/UserDefinedDeviceHeader.template"),
                     (self.context["plotter"]+".cpp" , "plotters/UserDefinedDeviceImplementation.template") ],
            False: [ (self.context["plotter"]+".h"   , "plotters/UserOnTheFlyPostProcessingHeader.template"),
                     (self.context["plotter"]+".cpp" , "plotters/UserOnTheFlyPostProcessingImplementation.template") ],
          }
        
        paths = [] # path is None if nothing was generated
        for filePath,template in templates.get(userDefined, []):
            paths.append(self.render(template,filePath,overwrite=False)[0]) #only keep the path
        
        return paths, self.context
