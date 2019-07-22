/**
 * This file is part of the ExaHyPE project.
 * Copyright (c) 2016  http://exahype.eu
 * All rights reserved.
 *
 * The project has received funding from the European Union's Horizon 
 * 2020 research and innovation programme under grant agreement
 * No 671698. For copyrights and licensing, please consult the webpage.
 *
 * Released under the BSD 3 Open Source License.
 * For the full license text, see LICENSE.txt
 **/

/* This is a hack to enable PingPongTests also when built in Release mode:
 * Enable Assertions for this compile context (=this cpp file) only.
 */
#ifndef Asserts
#define Asserts
#endif

#include "tarch/logging/Log.h"
#include "tarch/tests/TestCaseRegistry.h"
#include "tarch/logging/CommandLineLogger.h"
#include "tarch/logging/LogFilterFileReader.h"
#include "tarch/parallel/Node.h"

#include "peano/peano.h"

#include "exahype/main.h"
#include "exahype/parser/Parser.h"
#include "exahype/Vertex.h"
#include "exahype/runners/Runner.h"

#include "kernels/KernelCalls.h"

#include "kernels/GaussLegendreQuadrature.h"
#include "kernels/DGMatrices.h"

#include <vector>
#include <cstdlib> // getenv, exit
#include <iostream>
#include <cstdio>

/**
 * The ping pong test has to be triggered by main very very early. There should
 * be no other message in the MPI subsystem.
 */
int exahype::pingPongTest() {
  bool correct = true;
  tarch::logging::Log _log("exahype");
  logInfo( "pingPongTest()", "start ping pong test .... if test fails, please retranslate with -DnoPackedRecords" );
  
  #if defined(Parallel)
  exahype::Vertex::initDatatype();
  exahype::Vertex sendVertex[5];

  if (tarch::parallel::Node::getInstance().getNumberOfNodes()>1) {
    if (tarch::parallel::Node::getInstance().getRank()==0) {
      sendVertex[0].setPosition( tarch::la::Vector<DIMENSIONS,double>(2.0), 4);
      sendVertex[0].setAdjacentRank( 0, 10 );
      sendVertex[0].setAdjacentRank( 1, 11 );
      sendVertex[0].setAdjacentRank( 2, 12 );
      sendVertex[0].setAdjacentRank( 3, 13 );
      sendVertex[1].setPosition( tarch::la::Vector<DIMENSIONS,double>(3.0), 5);
      sendVertex[1].setAdjacentRank( 0, 20 );
      sendVertex[1].setAdjacentRank( 1, 21 );
      sendVertex[1].setAdjacentRank( 2, 22 );
      sendVertex[1].setAdjacentRank( 3, 23 );
      sendVertex[2].setPosition( tarch::la::Vector<DIMENSIONS,double>(4.0), 6);
      sendVertex[2].setAdjacentRank( 0, 30 );
      sendVertex[2].setAdjacentRank( 1, 31 );
      sendVertex[2].setAdjacentRank( 2, 32 );
      sendVertex[2].setAdjacentRank( 3, 33 );

      sendVertex[0].send(1,100,false,Vertex::MPIDatatypeContainer::ExchangeMode::Blocking);
      sendVertex[0].send(1,100,true,Vertex::MPIDatatypeContainer::ExchangeMode::Blocking);
      MPI_Send( sendVertex, 3, exahype::Vertex::MPIDatatypeContainer::Datatype, 1, 100, tarch::parallel::Node::getInstance().getCommunicator() );
    }
    if (tarch::parallel::Node::getInstance().getRank()==1) {
      exahype::Vertex receivedVertex;

      receivedVertex.receive(0,100,false,Vertex::MPIDatatypeContainer::ExchangeMode::Blocking);
      assertion1( receivedVertex.getLevel()==4, receivedVertex.toString() );
      assertion1( receivedVertex.getX()(0)==2.0, receivedVertex.toString() );
      assertion1( receivedVertex.getX()(1)==2.0, receivedVertex.toString() );
      correct &=  receivedVertex.getLevel()==4 && receivedVertex.getX()(0)==2.0 && receivedVertex.getX()(1)==2.0;
      #ifdef Dim3
      assertion1( receivedVertex.getX()(2)==2.0, receivedVertex.toString() );
      correct &=  receivedVertex.getX()(2)==2.0;
      #endif


      receivedVertex.receive(0,100,true,Vertex::MPIDatatypeContainer::ExchangeMode::Blocking);
      assertion1( receivedVertex.getLevel()==4, receivedVertex.toString() );
      assertion1( receivedVertex.getX()(0)==2.0, receivedVertex.toString() );
      assertion1( receivedVertex.getX()(1)==2.0, receivedVertex.toString() );
      correct &=  receivedVertex.getLevel()==4 && receivedVertex.getX()(0)==2.0 && receivedVertex.getX()(1)==2.0;
      #ifdef Dim3
      assertion1( receivedVertex.getX()(2)==2.0, receivedVertex.toString() );
      correct &=  receivedVertex.getX()(2)==2.0;
      #endif

      exahype::Vertex receivedVertices[5];
      MPI_Status status;
      MPI_Recv( receivedVertices, 3, exahype::Vertex::MPIDatatypeContainer::Datatype, 0, 100, tarch::parallel::Node::getInstance().getCommunicator(), &status );
      assertion3( receivedVertices[0].getLevel()==4,  receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      assertion3( receivedVertices[0].getX()(0)==2.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      assertion3( receivedVertices[0].getX()(1)==2.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      correct &=  receivedVertices[0].getLevel()==4 && receivedVertices[0].getX()(0)==2.0 && receivedVertices[0].getX()(1)==2.0;
      #ifdef Dim3
      assertion3( receivedVertices[0].getX()(2)==2.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      correct &=  receivedVertices[0].getX()(2)==2.0;
      #endif

      assertion3( receivedVertices[1].getLevel()==5,  receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      assertion3( receivedVertices[1].getX()(0)==3.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      assertion3( receivedVertices[1].getX()(1)==3.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      correct &=  receivedVertices[1].getLevel()==5 && receivedVertices[1].getX()(0)==3.0 && receivedVertices[1].getX()(1)==3.0;
      #ifdef Dim3
      assertion3( receivedVertices[1].getX()(2)==3.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      correct &=  receivedVertices[1].getX()(2)==3.0;
      #endif

      assertion3( receivedVertices[2].getLevel()==6,  receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      assertion3( receivedVertices[2].getX()(0)==4.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      assertion3( receivedVertices[2].getX()(1)==4.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      correct &=  receivedVertices[2].getLevel()==6 && receivedVertices[2].getX()(0)==4.0 && receivedVertices[2].getX()(1)==4.0;
      #ifdef Dim3
      assertion3( receivedVertices[2].getX()(2)==4.0, receivedVertices[0].toString(), receivedVertices[1].toString(), receivedVertices[2].toString() );
      correct &=  receivedVertices[2].getX()(2)==4.0;
      #endif
    }
    MPI_Barrier( tarch::parallel::Node::getInstance().getCommunicator() );

    if (tarch::parallel::Node::getInstance().getRank()==0) {
      //exahype::Vertex* heapVertices = new exahype::Vertex[5];
      exahype::Vertex heapVertices[5];
      MPI_Recv( heapVertices, 3, exahype::Vertex::MPIDatatypeContainer::Datatype, 1, 1, tarch::parallel::Node::getInstance().getCommunicator(), MPI_STATUS_IGNORE );

      assertion3( heapVertices[0].getLevel()==4,  heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      assertion3( heapVertices[0].getX()(0)==2.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      assertion3( heapVertices[0].getX()(1)==2.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      correct &=  heapVertices[0].getLevel()==4 && heapVertices[0].getX()(0)==2.0 && heapVertices[0].getX()(1)==2.0;
      #ifdef Dim3
      assertion3( heapVertices[0].getX()(2)==2.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      correct &=  heapVertices[0].getX()(2)==2.0;
      #endif

      assertion3( heapVertices[1].getLevel()==5,  heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      assertion3( heapVertices[1].getX()(0)==3.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      assertion3( heapVertices[1].getX()(1)==3.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      correct &=  heapVertices[1].getLevel()==5 && heapVertices[1].getX()(0)==3.0 && heapVertices[1].getX()(1)==3.0;
      #ifdef Dim3
      assertion3( heapVertices[1].getX()(2)==3.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      correct &=  heapVertices[1].getX()(2)==3.0;
      #endif

      assertion3( heapVertices[2].getLevel()==6,  heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      assertion3( heapVertices[2].getX()(0)==4.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      assertion3( heapVertices[2].getX()(1)==4.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      correct &=  heapVertices[2].getLevel()==6 && heapVertices[2].getX()(0)==4.0 && heapVertices[2].getX()(1)==4.0;
      #ifdef Dim3
      assertion3( heapVertices[2].getX()(2)==4.0, heapVertices[0].toString(), heapVertices[1].toString(), heapVertices[2].toString() );
      correct &=  heapVertices[2].getX()(2)==4.0;
      #endif
    }
    if (tarch::parallel::Node::getInstance().getRank()==1) {
      exahype::Vertex* heapVertices = new exahype::Vertex[5];
      heapVertices[0].setPosition( tarch::la::Vector<DIMENSIONS,double>(2.0), 4);
      heapVertices[1].setPosition( tarch::la::Vector<DIMENSIONS,double>(3.0), 5);
      heapVertices[2].setPosition( tarch::la::Vector<DIMENSIONS,double>(4.0), 6);
      MPI_Send( heapVertices, 3, exahype::Vertex::MPIDatatypeContainer::Datatype, 0, 1, tarch::parallel::Node::getInstance().getCommunicator() );
      delete[] heapVertices;
    }
    MPI_Barrier( tarch::parallel::Node::getInstance().getCommunicator() );
  }

  logInfo( "pingPongTest()", " ping pong test ok" );
  #else
  logError("pingPongTest()", "Cannot run MPI ping pong test since this built is not parallel.");
  correct = false;
  #endif
  return correct ? EXIT_SUCCESS : -1;
}
