#!/usr/bin/env python3

import sys
import json

import specfile1_reader

def parseArgument(i):
  if i<len(sys.argv):
    return sys.argv[i]
  else:
    return None 

file_path = parseArgument(1)

r = specfile1_reader.SpecFile1Reader()
json_obj = r.read(file_path)

print(json_obj)

with open(file_path+"2", "w") as outfile:
    json.dump(json_obj,outfile,indent=2)
