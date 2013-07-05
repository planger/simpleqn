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

class Request(val job: Job, val service: Service, val serviceTime: Int) {

  def this(job: Job, service: Service) = this(job, service, service.serviceTime)
  
  var cachedPastResidenceTime = -1
  var cachedWaitingTime = -1

  def net = job.net
  def isClosed = net.isClosed
  def jobRequests = job.requests
  def index = jobRequests.indexOf(this)
  def previousIndex = index - 1
  def nextIndex = index + 1

  def previousRequestInJob = {
    if (this != jobRequests.head)
      Option(jobRequests(previousIndex))
    else
      None
  }

  def nextRequestInJob = {
    if (this != jobRequests.last)
      Option(jobRequests(nextIndex))
    else
      None
  }

  private def earlierOtherRequests() = {
    val requests = service.sortByArrivalTime(service.requests)
    requests.slice(0, requests.indexOf(this))
  }

  private def arrivalTimeDifference = {
    if (haveEarlierRequests) {
      arrivalTime - earlierOtherRequests.last.arrivalTime
    } else 0
  }

  private def haveEarlierRequests = {
    !earlierOtherRequests.isEmpty
  }
  
  def waitingTime(): Int = {
    if (!isClosed && cachedWaitingTime < 0) {
      cachedWaitingTime = computeWaitingTime
    }
    cachedWaitingTime
  }

  def computeWaitingTime: Int = {
    if (earlierOtherRequests.length < 1) 0
    else {
      val time = (0 /: earlierOtherRequests) { (time, prevReq) =>
        time + prevReq.waitingTime
      } + earlierOtherRequests.last.serviceTime
      if (arrivalTimeDifference < time) time - arrivalTimeDifference else 0
    }
  }

  def residenceTime = {
    waitingTime + serviceTime
  }

  def arrivalTime = {
    job.arrivalTime + pastResidenceTime
  }

  def leavingQueueTime = {
    arrivalTime + waitingTime
  }

  def leavingServiceTime = {
    arrivalTime + residenceTime
  }

  def pastResidenceTime(): Int = {
    if (!isClosed && cachedPastResidenceTime < 0) {
      cachedPastResidenceTime = computePastResidenceTime
    }
    cachedPastResidenceTime
  }

  private def computePastResidenceTime(): Int = {
    if (previousRequestInJob.isDefined) {
      val prevRequest = previousRequestInJob.get
      prevRequest.pastResidenceTime + prevRequest.waitingTime + prevRequest.serviceTime
    } else 0
  }

  def processingAt(time: Int) = {
    time >= leavingQueueTime && time < leavingServiceTime
  }

  def waitingAt(time: Int) = {
    time >= arrivalTime && time < leavingQueueTime
  }

}
