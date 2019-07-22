import matplotlib

matplotlib.use('Agg') # Must be before importing matplotlib.pyplot or pylab!

import pylab
import networkx 

import performanceanalysis_output
import performanceanalysis_dd


def drawTreeGraph(myGraph):
  pos=networkx.spring_layout(myGraph)
  try:
    from networkx import graphviz_layout
    pos=networkx.graphviz_layout(myGraph,prog='twopi',args='')
  except:
    try:
      pos=networkx.graphviz_layout(myGraph,prog='dot',args='')
    except:
      pos=networkx.spring_layout(myGraph)
      print "fall back to spring layout. Nicer graphs might result if either compatible PyGraphviz or Pydot packages were available"
  networkx.draw_networkx(
    myGraph,
    pos,
    with_labels=True,
    arrows=True,
    node_color='#667766',
    node_size=10,
    alpha=0.2
  )

def plotLogicalTopology(numberOfRanks,inputFileNamePlusPath,parents,levels,offset,volume):
  topologyGraph = networkx.DiGraph()

  GlobalMaster = "global master"
  NodePool     = "pool of\nidle nodes"
  
  topologyGraph.add_node(GlobalMaster)
  topologyGraph.add_node(NodePool)

  for c in range(1,numberOfRanks):
    topologyGraph.add_node(c)
  
  for c in range(1,numberOfRanks):
    myParent = parents[c]
    if myParent<0:
      myParent = NodePool
    if myParent==0:
      myParent = GlobalMaster
    topologyGraph.add_edge(c,myParent)

  pylab.clf()
  pylab.title( "Logical topology" )
  drawTreeGraph(topologyGraph)
  pylab.savefig( inputFileNamePlusPath + ".png", transparent = True, bbox_inches = 'tight', pad_inches = 0, dpi=80 )
  pylab.savefig( inputFileNamePlusPath + ".pdf", transparent = True, bbox_inches = 'tight', pad_inches = 0 )

def plotWorkloadAndResponsibilityDistributionPerRank(numberOfRanks,outputFileName,volumes,overlaps,work):
  print "plot workload distribution per rank",
  pylab.clf()
  ranks = [x for x in range(0,numberOfRanks)]

  print ".",
  _markevery = numberOfRanks
  if numberOfRanks>11:
   _markevery = _markevery / 11
  pylab.plot(ranks, volumes, '-o', label="area of responsibility", markevery=_markevery, color='#000000', markersize=10)

  print ".",
  _markevery = numberOfRanks
  if numberOfRanks>11:
   _markevery = _markevery / 11
  pylab.plot(ranks, overlaps, '-s', label="area of responsibility $\cap \Omega$", markevery=_markevery, color='#ff0000', markersize=10)

  print ".",
  pylab.fill_between(ranks, work, color='#0000bb', alpha=0.4)

  maxLocalWorkToGetExtraLabel = 0
  oldRanksWithLabels  = -1
  ranksWithLabels     = len(work)
  while ranksWithLabels > numberOfRanks/10 and oldRanksWithLabels != ranksWithLabels:
   oldRanksWithLabels = ranksWithLabels
   maxLocalWorkToGetExtraLabel = 0.5 * maxLocalWorkToGetExtraLabel + 0.5 * max(work)
   ranksWithLabels = sum( i>maxLocalWorkToGetExtraLabel for i in work )
 
  for i in range(0,numberOfRanks):
   if work[i]>maxLocalWorkToGetExtraLabel:
     pylab.text(i,volumes[i]+10,str(i))
     pylab.plot([i,i], [0, volumes[i]], '--', color="#000000") 

  ranksWithZeroResponsibility = 0
  while len(volumes)>ranksWithZeroResponsibility and volumes[ranksWithZeroResponsibility]==0.0:
   ranksWithZeroResponsibility = ranksWithZeroResponsibility + 1

  if ranksWithZeroResponsibility>2 and len(volumes)>ranksWithZeroResponsibility:
   pylab.text(ranksWithZeroResponsibility,volumes[ranksWithZeroResponsibility]+20,"ranks_per_node")
   pylab.plot([ranksWithZeroResponsibility,ranksWithZeroResponsibility], [0, volumes[ranksWithZeroResponsibility]], '-', color="#000000") 
 
  try:
   pylab.legend(fontsize=9, framealpha=0.5)
  except:
   # old pylab version
   l = pylab.legend(prop={'size':9})
   l.get_frame().set_alpha(0.5)
  ax = pylab.gca()
  ax.autoscale_view()
  ax.set_yscale('symlog', basey=10)

  pylab.xlabel('rank')  
  pylab.ylabel('$\Omega $')
  pylab.savefig( outputFileName + ".symlog.png", transparent = True )
  pylab.savefig( outputFileName + ".symlog.pdf", transparent = True )


  ax.set_yscale('symlog', basey=10)

  #if numberOfRanks>32:
  # pylab.figure(figsize=(numberOfRanks/10,4))

  ax.set_yscale('linear' )
  pylab.xlabel('rank')  
  pylab.ylabel('$\Omega $')
  pylab.savefig( outputFileName + ".png" )
  pylab.savefig( outputFileName + ".pdf" )
  print "done"

def plotWorkloadAndResponsibilityDistributionPerNode(numberOfRanks,outputFileName,work,nodes):
  print "plot workload distribution per node",
  pylab.clf()

  data = {nodes[i]:0 for i in range(0,numberOfRanks)}
 
  for i in range(0,numberOfRanks):
   print ".",
   data[ nodes[i] ] = data[ nodes[i] ] + work[i]
   
  bars = pylab.bar(range(len(data)), data.values(), align='center')
  pylab.xticks(range(len(data)), data.keys(), rotation=-90, fontsize=6)

  #pylab.axis.xaxis().tick_top() 
  #pylab.subplots_adjust(bottom=2.15)

  for i in range(0,len(bars)):
   nodeName = data.keys()[i]

   oneRankBelongingToThisNode = 0
   while nodes[oneRankBelongingToThisNode]!=nodeName:
     oneRankBelongingToThisNode = oneRankBelongingToThisNode+1
     
   bars[i].set_color( performanceanalysis_dd.getNodeColor(oneRankBelongingToThisNode,nodes) )
   
  ax = pylab.gca()
  ax.autoscale_view()
  ax.set_yscale('log', basey=10)

  pylab.xlabel('rank')  
  pylab.ylabel('$\Omega $')
  pylab.savefig( outputFileName + ".symlog.png", transparent = True )
  pylab.savefig( outputFileName + ".symlog.pdf", transparent = True )

  ax.set_yscale('linear' )
  pylab.xlabel('rank')  
  pylab.ylabel('$\Omega $')
  pylab.savefig( outputFileName + ".png" )
  pylab.savefig( outputFileName + ".pdf" )
  print "done"

def plotMemoryUsagePerRank(outputFileName,memoryUsages):
  '''
  Plot the memory usage per node over time.
  '''
  print "plot memory usage per node",
  pylab.clf()
 
  maxUsage = 0
  minUsage = 10**20 
  for rank in range(0,len(memoryUsages)):
    pylab.scatter(memoryUsages[rank][0],memoryUsages[rank][1], marker='$'+str(rank)+'$', s=100)
    if memoryUsages[rank][1]:
      maxUsage = max(maxUsage, max(memoryUsages[rank][1]))
      minUsage = min(maxUsage, min(memoryUsages[rank][1]))
  
  
  ax = pylab.gca()
  ax.set_ylim([minUsage*0.9,maxUsage*1.1])
 
  pylab.xlabel('time / s')  
  pylab.ylabel('memory usage / MB')
  pylab.savefig( outputFileName + ".png" )
  pylab.savefig( outputFileName + ".pdf" )
  print "done"

def plotWalltimeOverview(outputFileName,beginIterations): 
  pylab.clf()
  DefaultSize = pylab.gcf().get_size_inches()
  #pylab.gcf().set_size_inches( DefaultSize[0]*4, DefaultSize[1] )
  pylab.title( "Walltime" )
  pylab.ylabel( "time per grid sweep [t]=s" )
  pylab.xlabel( "grid sweep" )
  xData = range(0,len(beginIterations))
  yData = [0.0]

  for i in range(1,len(beginIterations)):
    yData.append(beginIterations[i]-beginIterations[i-1])
  pylab.plot(xData, yData, '-',  markersize=10, color='#000066', label='time per traversal on global master' )

  pylab.savefig( outputFileName + ".png", transparent = True, bbox_inches = 'tight', pad_inches = 0, dpi=80 )
  pylab.savefig( outputFileName + ".pdf", transparent = True, bbox_inches = 'tight', pad_inches = 0 )

  ax = pylab.gca()
  ax.autoscale_view()
  ax.set_yscale('symlog', basey=10)

  pylab.savefig( outputFileName + ".symlog.png", transparent = True, bbox_inches = 'tight', pad_inches = 0, dpi=80 )
  pylab.savefig( outputFileName + ".symlog.pdf", transparent = True, bbox_inches = 'tight', pad_inches = 0 )
  
  pylab.gcf().set_size_inches( DefaultSize[0], DefaultSize[1] )
    

def plotHeatMapRecursively(ax,node,parentRank,x,zeta,dx,dzeta,heats,dim,level,plotLevel):
  activeRanks = 0
  if parentRank is not node.rank:
    if plotLevel is None or level is plotLevel:
      activeRanks += 1
      heat=heats[node.rank]
      
      if heat > 1e-12:
        ax.add_patch(matplotlib.patches.Rectangle((x,zeta),dx,dzeta,alpha=1.0, facecolor=(heat,heat,1), edgecolor='black',zorder=2*level))
      else:  
        ax.add_patch(matplotlib.patches.Rectangle((x,zeta),dx,dzeta,alpha=1.0, facecolor='none', edgecolor='black',zorder=2*level))
    if level is plotLevel:
      fontsize=24/(3.0**(dim-1))**(plotLevel-1)
      ax.text(x+dx/2,zeta+dzeta/2, str(node.rank), color='r', weight='bold', fontsize=fontsize, ha='center', va='center', zorder=2*level+1)

  # descend
  izmax=1 if dim is 2 else 3
  for iz in range(0,izmax):
    for iy in range(0,3):
      for ix in range(0,3):
          pos = [ix,iy,iz]
          child = node.getChild(pos)
          if child is not None:
            dx_new    = dx/3.0
            dzeta_new = dzeta/3.0**(dim-1)
            x_new     = x    + ix * dx_new
            zeta_new  = zeta + (iy+3*iz) * dzeta_new
            activeRanks += plotHeatMapRecursively(ax,child,node.rank,x_new,zeta_new,dx_new,dzeta_new,heats,dim,level+1,plotLevel)
          
  return activeRanks

def plotHeatMap(outputFileName,root,treeDepth,works,dim):
  print "plot work heat maps"
  pylab.clf()

  maxWork = max(works)
  heats = works[:]
  for i,h in enumerate(heats):
     heats[i] /= maxWork

  ax = pylab.gca()

  def resetAxes():
    ax.clear()
    ax.set_yscale('linear')
    ax.set_xscale('linear')
    ax.set_xlim([-0.1,3.1])
    ax.set_ylim([-0.1,3**(dim-1)+0.1])
    ax.autoscale_view() 
    for j in range(0,3**(dim-1)+1):
      ax.add_line(matplotlib.lines.Line2D([0,3], [j,j],color='0.2',zorder=-2,linestyle='--',linewidth=1, dashes=(5, 5)))
    for i in range(0,3+1):
      ax.add_line(matplotlib.lines.Line2D([i,i], [0,3**(dim-1)],color='0.2',zorder=-2,linestyle='--',linewidth=1, dashes=(5, 5)))
    
    pylab.xticks(fontsize=14)
    pylab.yticks(fontsize=14)
    pylab.xlabel('$i^{(2)}_{x}$',fontsize=14)  
    if dim is 3:
      pylab.ylabel('$(i^{(2)}_{y}, i^{(2)}_{z})$',fontsize=14)
    else:
      pylab.ylabel('$i^{(2)}_{y}$',fontsize=14)

    ax.xaxis.set_major_locator(matplotlib.ticker.FixedLocator([0.5,1.5,2.5]))
    ax.xaxis.set_major_formatter(matplotlib.ticker.FixedFormatter(['0','1','2']))
    if dim is 3:
      ax.yaxis.set_major_locator(matplotlib.ticker.FixedLocator([0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5]))
      ax.yaxis.set_major_formatter(matplotlib.ticker.FixedFormatter(['(0,0)','(1,0)','(2,0)','(0,1)','(1,1)','(2,1)','(0,2)','(1,2)','(2,2)']))
    else:
      ax.yaxis.set_major_locator(matplotlib.ticker.FixedLocator([0.5,1.5,2.5]))
      ax.yaxis.set_major_formatter(matplotlib.ticker.FixedFormatter(['0', '1', '2']))
    ax.tick_params(axis=u'both', which=u'both',length=0) # hide tick marks
    ax.set_facecolor((0.66, 0.66, 0.66))

  # plot per level
  print "number of active ranks on level %i = %i" % (1,1)
  for plotLevel in range(1,treeDepth+1):
    resetAxes()
    activeRanks = plotHeatMapRecursively(ax,root,0,0.0,0.0,3*1.0,3*3.0**(dim-2),heats,dim,0,plotLevel)

    print "number of active ranks on level %i = %i" % (plotLevel+1,activeRanks)
    pylab.show()
    pylab.savefig( outputFileName+"-level-%i.png" % (plotLevel+1), bbox_inches='tight', pad_inches=0.05 )
    pylab.savefig( outputFileName+"-level-%i.pdf" % (plotLevel+1), bbox_inches='tight', pad_inches=0.05 )

  # plot alltogether
  resetAxes()
  activeRanks = plotHeatMapRecursively(ax,root,0,0.0,0.0,3*1.0,3*3.0**(dim-2),heats,dim,0,None)
  print "number of total active ranks = %i" % (activeRanks)

  pylab.savefig( outputFileName+"-all.png", bbox_inches='tight', pad_inches=0.05 )
  pylab.savefig( outputFileName+"-all.pdf", bbox_inches='tight', pad_inches=0.05 )
  print "done"
