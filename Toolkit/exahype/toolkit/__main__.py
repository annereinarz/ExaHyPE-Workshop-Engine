#!/usr/bin/env python3

import sys
import os

import resource

def main():
    """"Call the controller and run it"""
    # append directory above to sys.path to be able to load the module
    sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__),"..")))
    # import as module
    from toolkit import Controller
    control = Controller()
    try:
        control.run()
    except Exception as e:
        sys.exit(str(e))

def memoryUsageResource():
    """
    see: http://fa.bianp.net/blog/2013/different-ways-to-get-memory-consumption-or-lessons-learned-from-memory_profiler/
    """
    rusage_denom = 1024.
    mem = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / rusage_denom
    return mem

if __name__ == "__main__":
    # execute only if run as a script
    main()
    #print("Maximum memory usage [MiB]: %1.3f" % memoryUsageResource())

