import sys
import re
import pylab 
import os
import numpy



from scipy.integrate import quad



def B0(x,i):
  if x<=i or x>i+1:
    return 0.0
  else:
    return 1.0
    
def BNormalised(x,i,p):
  if p==0:
    return B0(x,i)
  else:
    return (x-i)/p*BNormalised(x,i,p-1) + (i+p+1-x)/p*BNormalised(x,i+1,p-1)
    
def B(x,i,p):
  x = x+0.5+p/2.0
  return BNormalised(x,i,p)

    
def dBdx(x,i,p):
  x = x+0.5+p/2.0
  if (p==1):
    return (BNormalised(x,i,p-1)-BNormalised(x,i-1,p-1)) 
  else:
    return (p-1)*(BNormalised(x,i,p-1)-BNormalised(x,i+1,p-1)) 



Accuracy = 1000
#MaxOrder = 9
MaxOrder = 5


pylab.clf()


xData = numpy.linspace( -1.0, 1.0, Accuracy )
filename = "shapes-0"
yData = [B(x,0,0) for x in xData]
pylab.plot(xData, yData, "-", color="#000000", markersize=10, label="b-spline", markevery=1)
pylab.savefig( filename + ".png")
pylab.savefig( filename + ".pdf")



for order in range(1,MaxOrder):
  xData = numpy.linspace( -order, order, Accuracy )
  filename = "shapes-" + str(order)

  yData = [B(x,0,order) for x in xData]
  pylab.plot(xData, yData, "-", color="#000000", markersize=10, markevery=1)

  pylab.savefig( filename + ".png")
  pylab.savefig( filename + ".pdf")



for order in range(1,MaxOrder):
  pylab.clf()

  xData = numpy.linspace( -order-0.5, order+0.5, Accuracy )
  filename = "shape-plus-derivative-" + str(order)

  yData = [B(x,0,order) for x in xData]
  pylab.plot(xData, yData, "-", color="#000000", markersize=10, markevery=1)

  yData = [dBdx(x,0,order) for x in xData]
  pylab.plot(xData, yData, "--", color="#0000ff", markersize=10, markevery=1)

  pylab.savefig( filename + ".png")
  pylab.savefig( filename + ".pdf")



for order in range(1,MaxOrder):
  pylab.clf()
  xData = numpy.linspace( -order-1, order+1, Accuracy )
  filename = "shapes-and-tests-" + str(order)

  for i in range(-order-1,order+2):
    yData = [B(x,i,order) for x in xData]
    pylab.plot(xData, yData, "-", color="#000000", markersize=10, markevery=1)
    yData = [dBdx(x,i,order) for x in xData]
    pylab.plot(xData, yData, "--", color="#0000ff", markersize=10, markevery=1)
  
  
  massStencil = [0.0 for x in range(2*order+1)]
  laplaceStencil = [0.0 for x in range(2*order+1)]
  for shape in range(2*order+1):
    def integrand(x):
      return B(x,0,order)*B(x,shape-order,order)
    
    massStencil[shape] = quad(integrand, -order-1, order+1)[0] # accuracy doesn't matter here, so skip second return argument

    def integrand(x):
      return dBdx(x,0,order)*dBdx(x,shape-order,order)
    
    laplaceStencil[shape] = quad(integrand, -order-1, order+1)[0] 

    
  numpy.set_printoptions(precision=4)
  print " "
  print "--------"
  print "stencils for order " + str(order) + ":" 
  print "--------"
  print "M=" + str(massStencil)
  print "A=" + str(laplaceStencil)
  print " "

  pylab.text(-order-1.0,1,"M=" + str(massStencil) )
  pylab.text(-order-1.0,0.8,"A=" + str(laplaceStencil) )
  
  pylab.ylim([0,1])
  #ax.text(3, 8, 'boxed italics text in data coords', style='italic',
  #      bbox={'facecolor':'red', 'alpha':0.5, 'pad':10})

  pylab.savefig( filename + ".png")
  pylab.savefig( filename + ".pdf")
  
  
  numberOfElements = order+1
  if (order%2==0):
    numberOfElements = numberOfElements+1
    
    
  for element in range(0,numberOfElements):
    localMassStencil = [0.0 for x in range(2*order+1)]

    def integrand(x):
      return B(x,0,order)*B(x,shape-order,order)

    for shape in range(2*order+1):
      leftIntegrationBoundary  = -numberOfElements/2+element
      localMassStencil[shape] = quad(integrand, leftIntegrationBoundary, leftIntegrationBoundary+1)[0] # accuracy doesn't matter here, so skip second return argument

    print "M(local," + str(element) + ")=" + str(localMassStencil)
  
  
  print " "


  for element in range(0,numberOfElements):
    localLaplaceStencil = [0.0 for x in range(2*order+1)]

    def integrand(x):
      return dBdx(x,0,order)*dBdx(x,shape-order,order)
    
    for shape in range(2*order+1):
      leftIntegrationBoundary  = -numberOfElements/2+element
      localLaplaceStencil[shape] = quad(integrand, leftIntegrationBoundary, leftIntegrationBoundary+1)[0] 

    print "A(local," + str(element) + ")=" + str(localLaplaceStencil)
    