#author: Dominic E. Charrier

import os,collections,math,logging
import argparse

from . import mesh_info

#
# tool definitions
#

class Tool():
  """
  Abstract base class for all tools.
  """
  def __init__(self,log=None):
    if not log:
      logging.basicConfig(format="%(filename)s:%(lineno)s(%(funcName)s):%(levelname)s %(message)s")
      log = logging.getLogger()
      #log.addHandler(logging.StreamHandler()) # to stderr
      log.setLevel(logging.INFO)
    self.log = log.getChild(__name__)

  def id(self):
    """
    Return the tool's id/name.
    """
    return ""

  def help(self):
    """
    Return a help text.
    """
    return ""

  def run(self,spec):
    """
    Run the tool.
    """
    pass

class MeshInfoTool(Tool):
  """
  This tool gives information about the mesh ExaHyPE will construct and suggests
  optimal MPI rank numbers to distribute the coarsest base grid.
  """
  def __init__(self,log=None):
    super().__init__(log)
    self.log.info("registered tool: {} (id='{}')".format(__name__,self.id()))

  def id(self):
    return "mesh_info"

  def help(self):
    return "Mesh Info - Gives information about the mesh ExaHyPE will construct and suggests optimal MPI rank numbers to distribute the coarsest base grid."

  def run(self,spec):
    """
    Create an instance of MeshInfo from the specification file.
    """
    specDomain = spec.get("computational_domain")

    dim                     = specDomain.get("dimension")
    domainOffset            = list(specDomain.get("offset"))[0:dim]
    domainSize              = list(specDomain.get("width"))[0:dim]
    userMeshSize            = mesh_info.getCoarsestMaximumMeshSizeOfAllSolvers(spec.get("solvers",[]))
    outsideCells            = specDomain.get("outside_cells",2)
    outsideCellsLeft        = specDomain.get("outside_cells_left",outsideCells/2)
    oneThirdOfCellsOutside  = specDomain.get("one_third_of_cells_outside",False)
    info = mesh_info.MeshInfo(domainOffset,domainSize,userMeshSize,outsideCells,outsideCellsLeft,oneThirdOfCellsOutside)

    # print user specification
    self.log.info("This is the MeshInfo tool of the toolkit. It does predict what coarse grid ExaHyPE will create.")
    self.log.info("")
    self.log.info("=============================")
    self.log.info("Specification File Parameters")
    self.log.info("=============================")
    self.log.info("NOTE: These settings are read from the specification file.")
    self.log.info("---------------------------------------------------------------------------------------------")
    self.log.info("domain offset          : [ %s ]" % ", ".join([str(i) for i in info.domainOffset]))       
    self.log.info("domain size            : [ %s ]" % ", ".join([str(i) for i in info.domainSize]))
    self.log.info("coarsest user mesh size: %s" % str(info.userMeshSize))
    self.log.info("---------------------------------------------------------------------------------------------")
    self.log.info("bounding box outside cells (per coordinate direction)            : %d" % info.boundingBoxOutsideCells)     
    self.log.info("bounding box outside cells (per coordinate direction, left side) : %d" % info.boundingBoxOutsideCellsLeft)
    self.log.info("is one third of bounding box cells outside                       : %s" % str(info.oneThirdOfBoundingBoxCellsOutside))
   
    # run
    info.deduceCoarseGridInformation()

    self.log.info("")
    self.log.info("=======================================")
    self.log.info("Geometry and Bounding Box Configuration")
    self.log.info("=======================================")
    self.log.info("NOTE: These settings will actually be used during the simulation.")
    self.log.info("---------------------------------------------------------------------------------------------")
    self.log.info("domain offset                                     : [ %s ]" % ", ".join([str(i) for i in info.domainOffset]))       
    self.log.info("domain size (may be scaled if non-cubic domain)   : [ %s ]" % ", ".join([str(i) for i in info.domainSize]))
    self.log.info("coarsest mesh size                                : %s"     % str(info.boundingBoxMeshSize))       
    self.log.info("coarsest mesh level                               : %d"     % info.boundingBoxMeshLevel)       
    self.log.info("(inside) cells on coarsest grid                   : [ %s ]" % ", ".join([str(int(i/info.boundingBoxMeshSize)) for i in info.domainSize]))          
    self.log.info("---------------------------------------------------------------------------------------------")
    self.log.info("bounding box offset                                                   : [ %s ]" % ", ".join([str(i) for i in info.boundingBoxOffset]))
    self.log.info("bounding box size                          (per coordinate direction) : %s" % str(info.boundingBoxSize))          
    self.log.info("bounding box mesh cells                    (per coordinate direction) : %d" % info.boundingBoxMeshCells)
    self.log.info("bounding box outside cells                 (longest edge)             : %d" % info.boundingBoxOutsideCells)
    self.log.info("bounding box outside cells on 'left' side  (longest edge)             : %d" % info.boundingBoxOutsideCellsLeft)      
    self.log.info("is one third of bounding box cells outside (longest edge)             : %s" % str(info.oneThirdOfBoundingBoxCellsOutside))
    self.log.info("---------------------------------------------------------------------------------------------")
    
    # domain decomposition recommendations
    if info.boundingBoxOutsideCells is 0 or\
       (info.boundingBoxOutsideCells is 2 and\
       info.boundingBoxOutsideCellsLeft is 1) or\
       info.oneThirdOfBoundingBoxCellsOutside:
      boundingBoxCells = 3**info.boundingBoxMeshLevel
      ranks = [[] for i in range(1,info.boundingBoxMeshLevel)]
  
      # count inside cells in each coordinate direction
      insideCells = []
      padding = 0 if info.boundingBoxOutsideCells is 0 else 2
      for d in range(0,dim):
        insideCells.append(int(round(info.domainSize[d]/info.boundingBoxMeshSize)))
      for i in range(1,info.boundingBoxMeshLevel):
        level = info.boundingBoxMeshLevel-i
        for d in range(0,dim):
          ranks[level-1].append(math.ceil((insideCells[d]+padding)/3.0**i))
    
      #compute optimal rank counts
      optimalRankNumbers = [1,2]
      for i in range(1,info.boundingBoxMeshLevel):
        level = info.boundingBoxMeshLevel-i
        totalRanks = 1
        for d in range(0,dim):
          totalRanks *= ranks[level-1][d]
        totalRanks +=1 # rank 0 - is administrative
        if level > 1:
           totalRanks+=1 # rank 1 - takes care of load balancing for more than one level
        optimalRankNumbers.append(totalRanks)

      optimalRankNumbers = sorted(optimalRankNumbers)

      self.log.info("")
      self.log.info("====================")
      self.log.info("Domain Decomposition")
      self.log.info("====================")
      self.log.info("NOTE: With the following numbers of ranks, you will obtain load balance on the coarsest grid of the adaptive mesh.")
      self.log.info("------------------------------------------------------------------------------------------------------------------")
      self.log.info("optimal rank numbers: %s" % ( ", ".join([str(i) for i in optimalRankNumbers]) ) )
      self.log.info("------------------------------------------------------------------------------------------------------------------")
#
# registry
#

tools = []

def register(tool):
  global tools
  tools.append(tool)

def initRegistry(log=False):
  register(MeshInfoTool(log))
