"""
This is the exahype package, part of the toolkit NG.
"""

# Python does not like me when it comes to packages.
# Maybe we can get rid of this...
import sys
from os.path import join, dirname, basename
sys.path.append(join(dirname(__file__),"..","exahype"))

import specfiles
import toolkit
