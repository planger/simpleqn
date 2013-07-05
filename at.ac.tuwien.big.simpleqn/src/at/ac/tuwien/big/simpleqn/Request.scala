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

  def previousRequestInJob = {
    if (this != jobRequests.head)
      Option(jobRequests(jobRequests.indexOf(this) - 1))
    else
      None
  }

  def nextRequestInJob = {
    if (this != jobRequests.last)
      Option(jobRequests(jobRequests.indexOf(this) + 1))
    else
      None
  }

  private def earlierOtherRequest() = {
    // TODO what if a request comes at the same time?
    val earlierOtherRequests = service.requests.filter(r => r != this && r.arrivalTime <= arrivalTime)
    if (!earlierOtherRequests.isEmpty) {
      Option(earlierOtherRequests.last)
    } else {
      None
    }
  }

  private def arrivalTimeDifference = {
    val earlierOtherReq = earlierOtherRequest
    if (earlierOtherReq.isDefined) {
      arrivalTime - earlierOtherRequest.get.arrivalTime
    } else 0
  }

  def waitingTime(): Int = {
    if (!isClosed || cachedWaitingTime < 0) {
      cachedWaitingTime = computeWaitingTime
    }
    cachedWaitingTime
  }

  def computeWaitingTime: Int = {
    val earlierOtherReq = earlierOtherRequest
    if (earlierOtherReq.isEmpty) 0
    else {
      val time = earlierOtherRequest.get.waitingTime + earlierOtherRequest.get.serviceTime - arrivalTimeDifference
      if (time < 0) 0 else time
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
    if (!isClosed || cachedPastResidenceTime < 0) {
      cachedPastResidenceTime = computePastResidenceTime
    }
    cachedPastResidenceTime
  }

  private def computePastResidenceTime(): Int = {
    var pResTime = 0;
    for (r <- job.requests.slice(0, job.requests.indexOf(this))) {
      pResTime += r.residenceTime
    }
    pResTime
  }

  def processingAt(time: Int) = {
    time >= leavingQueueTime && time < leavingServiceTime
  }

  def waitingAt(time: Int) = {
    time >= arrivalTime && time < leavingQueueTime
  }

}
