#!/usr/bin/env python3

import sys,os
import matplotlib
import matplotlib.pyplot as plt
import math
import re

def haveToPrintHelpMessage(argv):
    """
    Check if we have to print a help message.
    """
    result = parseArgument(argv,1)==None
    for arg in argv:
        result = result or ( arg=="-help" or arg=="-h" )
    return result

def printHelpMessage():
    info = \
    """job_statistics.py:
    Plots a histogram from Peano's Job output

    run:

    ./job_statistics.py <file/folder> [output directory]

    file must contain output from an executable compiled with -DTBB_USE_THREADING_TOOLS
    NOTE: check the output is not filtered out by a log filer
    """
    print(info) # correctly indented
    sys.exit()

def parseArgument(argv,i):
    if i<len(argv):
        return argv[i]
    else:
        return None

def plotStatistics(fname, output_folder):
    statsNoOfBackgroundTasks = {}
    statsGrabbedBackgroundTasks = {}
    statsRunningConsumers = {}
    #  10.4573      info         no of background tasks[1]=19029
    base = 2
    with open(fname, 'r') as f:
        for l in f.readlines():
            if "no of background tasks available per consumer run" in l:
                ex = re.compile(r"no of background tasks available per consumer run\[(\d+)\]=(\d+)?")
                match = ex.search(l)
                if match:
                    num_tasks = int(match.group(1))
                    occurences = int(match.group(2))
                    if num_tasks == 0:
                        statsNoOfBackgroundTasks[0] = occurences
                    else:
                        statsNoOfBackgroundTasks_bin = int(math.log(num_tasks, base)) + 1
                        if statsNoOfBackgroundTasks_bin in statsNoOfBackgroundTasks:
                            statsNoOfBackgroundTasks[statsNoOfBackgroundTasks_bin] += occurences
                        else:
                            statsNoOfBackgroundTasks[statsNoOfBackgroundTasks_bin] = occurences
            if "no of background tasks processed per consumer run" in l:
                ex = re.compile(r"no of background tasks processed per consumer run\[(\d+)\]=(\d+)?")
                match = ex.search(l)
                if match:
                    num_tasks = int(match.group(1))
                    occurences = int(match.group(2))
                    if num_tasks == 0:
                        statsGrabbedBackgroundTasks[0] = occurences
                    else:
                        statsNoOfBackgroundTasks_bin = int(math.log(num_tasks, base)) + 1
                        if statsNoOfBackgroundTasks_bin in statsGrabbedBackgroundTasks:
                            statsGrabbedBackgroundTasks[statsNoOfBackgroundTasks_bin] += occurences
                        else:
                            statsGrabbedBackgroundTasks[statsNoOfBackgroundTasks_bin] = occurences
            if "no of running consumers" in l:
                ex = re.compile(r"no of running consumers\[(\d+)\]=(\d+)?")
                match = ex.search(l)
                if match:
                    num_tasks = int(match.group(1))
                    occurences = int(match.group(2))
                    if num_tasks == 0:
                        statsRunningConsumers[0] = occurences
                    else:
                        statsNoOfBackgroundTasks_bin = int(math.log(num_tasks, base)) + 1
                        if statsNoOfBackgroundTasks_bin in statsRunningConsumers:
                            statsRunningConsumers[statsNoOfBackgroundTasks_bin] += occurences
                        else:
                            statsRunningConsumers[statsNoOfBackgroundTasks_bin] = occurences
    if len(statsNoOfBackgroundTasks) == 0:
        print("WARNING: {} doesn't contain TBB_USE_THREADING_TOOLS output: no plot created".format(fname))
        return
    labels = []
    
    BarWidth = 0.2
    
    x = []
    y = []
    for k in range(min(statsNoOfBackgroundTasks.keys()), max(statsNoOfBackgroundTasks.keys())+1):
        x.append(k)
        if k in statsNoOfBackgroundTasks:
            y.append(statsNoOfBackgroundTasks[k])
        else:
            y.append(0)
        if k == 0:
            labels.append(0)
        elif k==1:
            labels.append(r"1".format(base,k))
        else:
            labels.append(r"$<{}^{}$".format(base,k))
    #print "print " + str(x) + "x" + str(y) 
    x = [d+0*BarWidth for d in x]
    plt.bar(x, y, label="Tasks in queue", tick_label=labels,width=BarWidth,log=True)

    plt.ylim( [min(y)/2+1,max(y)*2] ) 

    x = []
    y = []
    for k in range(min(statsGrabbedBackgroundTasks.keys()), max(statsGrabbedBackgroundTasks.keys())+1):
        x.append(k)
        if k in statsGrabbedBackgroundTasks:
            y.append(statsGrabbedBackgroundTasks[k])
        else:
            y.append(0)
    #print "print " + str(x) + "x" + str(y) 
    x = [d+1*BarWidth for d in x]
    plt.bar(x, y, label="Tasks taken",width=BarWidth,log=True)
    
    x = []
    y = []
    for k in range(min(statsRunningConsumers.keys()), max(statsRunningConsumers.keys())+1):
        x.append(k)
        if k in statsRunningConsumers:
            y.append(statsRunningConsumers[k])
        else:
            y.append(0)
    #print "print " + str(x) + "x" + str(y) 
    x = [d+2*BarWidth for d in x]
    plt.bar(x, y, label="Running consumers",width=BarWidth,log=True)
    
    plt.xlabel("Number of tasks/consumers")
    plt.ylabel("Frequency")
    plt.legend()

    if output_folder is None: output_folder="."
    out_file = os.path.join(output_folder, os.path.basename(fname)) + ".job-stats.pdf"
    plt.savefig(out_file, bbox_inches='tight')
    plt.clf()

if __name__ == "__main__":
    if haveToPrintHelpMessage(sys.argv):
        printHelpMessage()
    
    path_name = parseArgument(sys.argv,1)
    output_folder = parseArgument(sys.argv,2)

    if os.path.isfile(path_name):
        plotStatistics(path_name,output_folder)
    elif os.path.isdir(path_name):
        for f in os.listdir(path_name):
            plotStatistics(os.path.join(path_name,f), output_folder)
    else:
        print("ERROR: {} not recognized as a file or folder".format(sys.argv[1]))

    
