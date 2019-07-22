class TreeNode:
  def __init__(self,rank,dim,level):
      self.rank = rank
      self.level = level
      if dim==2:
        self.children = [[[None for x in range(0,3)] for y in range(0,3)] for z in range(0,1)] 
      elif dim==3:
        self.children = [[[None for x in range(0,3)] for y in range(0,3)] for z in range(0,3)] 

  def putChild(self,pos,node):
      self.children[pos[2]][pos[1]][pos[0]] = node

  def getChild(self,pos):
      return self.children[pos[2]][pos[1]][pos[0]]

def buildTreeRecursively(node,parents,offset,volume,level):
  dim = len(volume[0]) 

  activeRanks = 1

  childrenRanks = getChildrenRanks(node.rank,parents)
  for rank in childrenRanks:
    childOffset  = offset[rank] 
    childSize    = volume[rank]
    childLevel   = level[rank]
    parentOffset = offset[node.rank]
    parentSize   = volume[node.rank]
    parentLevel  = level[node.rank]

    levelDelta = childLevel - parentLevel
    
    current       = node
    currentOffset = list(parentOffset) 
    currentSize   = list(parentSize)
    # insert dummy entries with child.rank=node.rank coarser levels
    for dl in range(1,levelDelta): # bound is correct
      pos = [0,0,0]
      for i in range(0,dim):
        pos[i] = int(round((childOffset[i] - currentOffset[i])/childSize[i]))/3**(levelDelta-dl);
      child = current.getChild(pos)
      assert child is None or child.rank is node.rank
      if child is None:
        #print("level=%d, pos=%s: rank=%d" % (parentLevel+dl,str(pos),node.rank))
        child = TreeNode(node.rank,dim,parentLevel+dl)
        current.putChild(pos,child)
      # prepare next iteration
      current = child
      for i in range(0,dim):
        currentOffset[i] = currentOffset[i] + pos[i] * currentSize[i]/3.0
        currentSize  [i] = currentSize[i]/3.0 

    #insert the actual child node on finest level
    pos = [0,0,0]
    for i in range(0,dim):
      pos[i] = int(round((childOffset[i] - currentOffset[i])/childSize[i]));
    #print("level=%d, pos=%s: rank=%d" % (childLevel,str(pos),rank))
    child = TreeNode(rank,dim,childLevel)
    current.putChild(pos,child)
    activeRanks += buildTreeRecursively(child,parents,offset,volume,level)

  return activeRanks

def getChildrenRanks(rank,parents):
  children = []
  for i,x in enumerate(parents):
     if x == rank:
        children.append(i)
  return children

def buildTree(parents,offset,volume,level):
  dim = len(volume[0])
  
  firstMaster = -1
  for rank,parent in enumerate(parents):
    if parent is 0:
       firstMaster = rank
       break

  if len(parents)>1:
    root = TreeNode(firstMaster,dim,level[firstMaster])
    activeRanks = buildTreeRecursively(root,parents,offset,volume,level)
    return root
  else:
    return TreeNode(0,dim)

def computeVolumesOverlapsWork(numberOfRanks,parents,offset,volume,dim,domainoffset,domainsize):
  print "compute volumes, overlaps and work ",
  volumes = [0.0 for x in offset]
  for i in range(0,numberOfRanks):
    print ".",
    if dim==2:
      volumes[i] = volume[i][0]*volume[i][1]
    else:
      volumes[i] = volume[i][0]*volume[i][1]*volume[i][2]

  overlaps = [0.0 for x in offset]
  for i in range(0,numberOfRanks):
    print ".",
    overlaps[i] = 1.0
    for d in range(0,dim):
      left  = 0
      right = 0
      try:
       left  = max(offset[i][d],float(domainoffset[d]))
       right = min(offset[i][d]+volume[i][d],float(domainoffset[d])+float(domainsize[d]))
      except:
       pass
      delta = right-left
      if delta<0:
        print "ERROR for rank " + str(i) +  \
              ": region=" + str(offset[i]) + "x" + str(volume[i]) + \
              ", bonding box=" + str(domainoffset) + "x" + str(domainsize) + \
              ", delta= " + str(delta) 
      overlaps[i] = overlaps[i] * delta
    if overlaps[i]>volume[i]:
      print "ERROR for rank " + str(i) +  \
            ": region=" + str(offset[i]) + "x" + str(volume[i]) + \
            ", bounding box=" + str(domainoffset) + "x" + str(domainsize) + \
            ", volume= " + str(volumes[i]) + ", overlaps=" + str(overlaps[i]) 
  work = [x for x in overlaps]
  for i in range(1,numberOfRanks):
    if work[ parents[i] ]<overlaps[i] and i==1:
      print "INFO: global root seems to delegate all work to another rank and to focus on load balancing. Decrement rank count when plotting performance graphs accordingly." 
    elif work[ parents[i] ]<overlaps[i] and parents[i]>0:
      print "\nWARNING: work of rank " + str(parents[i]) + " will become negative as overlap of rank " + str(i) + " equals " + str(overlaps[i]) 
    work[ parents[i] ] = work[ parents[i] ] - overlaps[i]
  #work[0] = 0.0     # can become negative
  for i in range(0,numberOfRanks):
    if work[i] <0:
      work[i]=0

  if len(volumes)>1:
    volumes[0]=0
    #volumes[1]=0
      
  print " done "
  return (volumes,overlaps,work)
  
  
  
