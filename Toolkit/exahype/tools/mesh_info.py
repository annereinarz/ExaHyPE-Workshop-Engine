#author: Dominic E. Charrier

import os,sys,math

def getCoarsestMaximumMeshSizeOfAllSolvers(solverSpec):
  """
  Find the coarsest maximum mesh size the user has specified
  for a solver.
  """
  result = 1e20
  for solver in solverSpec:
    result = min( result, solver["maximum_mesh_size"] )
  return result

class MeshInfo:
  def __init__(self,domainOffset,domainSize,userMeshSize,outsideCells,outsideCellsLeft,oneThirdOfCellsOutside):
    """
    Constructor. All arguments are required/

    :type vector: list[float]
    :param vector domainOffset:        offset of the domain   
    :param vector domainSize:          size of the domain
    :param float userMeshSize:        the mesh size the user at least wants
    :param float outsideCells:         number of cells which should be placed outside (per dimension)
    :param float outsideCellsLeft:     number of cells which should be placed outside left of the domain
    :param bool  oneThirdOfCellsOutside if one third of the cells should be placed outside of the domain. Overrules the other scaling options.
    """
    # error handling
    if outsideCellsLeft>outsideCells:
      print("Parameter 'outside_cells_left' must not be greater than 'outside_cells'",file=sys.stderr)
      sys.exit(1)
    self.domainOffset       = domainOffset
    self.domainSize         = domainSize
    self.unscaledDomainSize = domainSize
    self.userMeshSize       = userMeshSize
    self.dim                = len(self.domainOffset)

    self.boundingBoxOutsideCells           = outsideCells           # is overwritten if oneThirdOfCellsOutside is true
    self.boundingBoxOutsideCellsLeft       = outsideCellsLeft       # is overwritten if oneThirdOfCellsOutside is true
    self.oneThirdOfBoundingBoxCellsOutside = oneThirdOfCellsOutside 

    # deduced quantities
    self.scaleBoundingBox       = outsideCells>0 or oneThirdOfCellsOutside
    self.boundingBoxMeshSize    = -1
    self.boundingBoxMeshLevel   = -1
    self.boundingBoxMeshCells   = -1
    self.boundingBoxSize        = -1
    self.boundingBoxOffset      = -1
    self.boundingBoxInsideCells = -1

  def computeCoarsestMeshSizeAndLevel(self):
    """
    Enlarge non-cubic domains such that they are exactly
    resolved by a grid with mesh size meshSize.
    """
    largestExtent = max(self.domainSize)

    level = 0 
    currentMeshSize = 1e20
    while currentMeshSize>self.userMeshSize:
      currentMeshSize = float(largestExtent) / 3.0**level
      level += 1
    level -= 1 # currentMeshSize was computed with previous level (= new level -1 )
    return currentMeshSize, level

  def determineScaledDomainSize(self,meshSize):
    """
    Enlarge non-cubic domains such that they are exactly
    resolved by a grid with mesh size meshSize.
    """
    scaledDomainSize=[0 for i in range(0,self.dim)]    
    for d in range(0,self.dim):
      scaledDomainSize[d] = math.ceil(self.domainSize[d] / meshSize - 1e-11) * meshSize;
    return scaledDomainSize

  def deduceCoarseGridInformation(self):
    """
    From the solver specification and the bounding box scaling options,
    deduce how the coarse grid will look like.
    """
    self.boundingBoxSize                = max(self.domainSize);
    unscaledMeshSize, unscaledMeshLevel = self.computeCoarsestMeshSizeAndLevel();    
    self.boundingBoxMeshLevel           = unscaledMeshLevel;
    self.boundingBoxOffset              = self.domainOffset

    # scale bounding box
    if self.scaleBoundingBox:
      if self.oneThirdOfBoundingBoxCellsOutside:
        self.boundingBoxOutsideCellsLeft = 1

      maxDomainExtent = max(self.domainSize)

      self.boundingBoxMeshSize = -1
      boundingBoxScaling  = 0
      level = unscaledMeshLevel; # level=0 means a single cell
      while self.boundingBoxMeshSize < 0 or\
            self.boundingBoxMeshSize > self.userMeshSize:
        self.boundingBoxMeshCells = 3**level;
        if self.oneThirdOfBoundingBoxCellsOutside:
          self.boundingBoxOutsideCells = self.boundingBoxMeshCells/3 + 2
        self.boundingBoxInsideCells = int(self.boundingBoxMeshCells - self.boundingBoxOutsideCells)
        boundingBoxScaling       = float(self.boundingBoxMeshCells) / float(self.boundingBoxInsideCells)
        self.boundingBoxSize     = boundingBoxScaling * maxDomainExtent
        self.boundingBoxMeshSize = self.boundingBoxSize / self.boundingBoxMeshCells
        level += 1
      self.boundingBoxMeshLevel = level - 1; # decrement result since bounding box was computed using level-1

      for d in range(0,self.dim):
        self.boundingBoxOffset[d] -= self.boundingBoxOutsideCellsLeft*self.boundingBoxMeshSize
    
    else: # not scale bounding box
      self.boundingBoxMeshSize    = unscaledMeshSize
      self.boundingBoxMeshCells   = 3**self.boundingBoxMeshLevel;
      self.boundingBoxInsideCells = self.boundingBoxMeshCells

    self.domainSize = self.determineScaledDomainSize(self.boundingBoxMeshSize)
