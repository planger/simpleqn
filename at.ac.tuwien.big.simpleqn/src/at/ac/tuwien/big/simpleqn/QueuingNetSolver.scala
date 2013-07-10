/*
 * Copyright (c) 2013 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package at.ac.tuwien.big.simpleqn

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

protected class QueuingNetSolver(val services: List[Service], val jobs: List[Job]) {

  var currentArrivalTime = (Int.MaxValue /: jobs) { (curMin, job) => Math.min(curMin, job.arrivalTime) }
  var requestStack = mutable.ListBuffer[Request]()

  def solve = {
    initializeRequestStack
    initializeArrivalTimes
    processRequests
  }

  private def initializeArrivalTimes {
    jobs foreach { j => j.requests.head.myArrivalTime = j.arrivalTime }
  }

  private def processRequests {
    while (haveRequestsToProcess) {
      for (request <- requestsOfCurrentArrivalTime) {
        request.service.queue += request
        request.computeLeavingQueueTime
        updateArrivalOfNextRequest(request)
        updateCurrentArrivalTimeInJob(request)
        setProcessed(request)
      }
      updateCurrentArrivalTime
    }
  }

  private def haveRequestsToProcess = {
    !requestStack.isEmpty
  }

  private def requestsOfCurrentArrivalTime = {
    (ListBuffer[Request]() /: jobsWithCurrentArrivalTime) { (list, job) =>
      list ++ job.requests.filter { currentArrivalTime == _.arrivalTime }
    }
  }

  private def initializeRequestStack: mutable.ListBuffer[Request] = {
    requestStack ++= allRequests
  }

  private def allRequests = {
    (ListBuffer[Request]() /: jobs) {
      _ ++ _.requests
    }
  }

  private def jobsWithCurrentArrivalTime = {
    jobs filter { _.currentArrivalTime == currentArrivalTime }
  }

  private def updateArrivalOfNextRequest(request: Request) = {
    val nextRequest = request.nextRequestInJob
    if (nextRequest.isDefined)
      nextRequest.get.myArrivalTime = request.leavingServiceTime
  }
  
  private def updateCurrentArrivalTimeInJob(request: Request) {
    request.job.currentArrivalTime = request.leavingServiceTime
  }

  private def setProcessed(request: Request) {
    requestStack -= request
  }
  
  private def updateCurrentArrivalTime {
    currentArrivalTime = minCurrentArrivalTimeOfIncompleteJob
  }

  private def minCurrentArrivalTimeOfIncompleteJob = {
    (Int.MaxValue /: jobs) { (currentMin, job) =>
      if (isCompleted(job))
        Math.min(currentMin, job.currentArrivalTime)
      else
        currentMin
    }
  }

  private def isCompleted(job: Job): Boolean = {
    requestStack.contains(job.requests.last)
  }

}