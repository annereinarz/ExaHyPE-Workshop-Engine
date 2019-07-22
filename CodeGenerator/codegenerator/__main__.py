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
# Starting point of the code generator
#
# @note
# requires python3

import sys
import os

def main():
    sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__),"..")))
    from codegenerator import Controller
    control = Controller()
    control.generateCode()


if __name__ == "__main__":
    # execute only if run as a script
    main()
