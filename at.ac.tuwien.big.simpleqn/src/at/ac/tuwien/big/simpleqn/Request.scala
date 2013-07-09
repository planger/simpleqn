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
  def index = job.requests.indexOf(this)
  
  def requestId() = {
    job.jobId + "R[" + index + "]-" + this.hashCode()
  }

  def previousRequestsInJob = {
    jobRequests.slice(0, index)
  }

  def previousRequestInJob = {
    if (this != jobRequests.head)
      Option(jobRequests(index - 1))
    else
      None
  }

  def nextRequestInJob = {
    if (this != jobRequests.last)
      Option(jobRequests(index + 1))
    else
      None
  }

  private def earlierOtherRequest() = {
    // TODO fix stack overflow
    val myArrivalTime = arrivalTime
    val earlierOtherRequests = service.requests.filter {
      r => r != this && isBefore(r, myArrivalTime)
    }.sortWith { (r1, r2) =>
      val r1ArrivalTime = r1.arrivalTime
      val r2ArrivalTime = r2.arrivalTime
      if (r1ArrivalTime < r2ArrivalTime) {
        true
      } else if (r1ArrivalTime == r2ArrivalTime) {
        r1.hashCode() < r2.hashCode()
      } else {
        false
      }
    }
    if (!earlierOtherRequests.isEmpty) {
      Option(earlierOtherRequests.last)
    } else {
      None
    }
  }

  private def isBefore(otherRequest: Request, myArrivalTime: Int) = {
    if (otherRequest.minArrivalTime <= myArrivalTime) {
      val otherArrivalTime = otherRequest.arrivalTime
      if (otherArrivalTime < myArrivalTime) {
        true
      } else if (otherArrivalTime == myArrivalTime) {
        val isBefore = otherRequest.hashCode() < this.hashCode()
        isBefore
      } else {
        false
      }
    } else false
  }

  def waitingTime(): Int = {
    if (!isClosed || cachedWaitingTime < 0) {
      cachedWaitingTime = computeWaitingTime
    }
    cachedWaitingTime
  }

  def computeWaitingTime: Int = {
    val otherReq = earlierOtherRequest
    if (!otherReq.isEmpty) {
      val time = otherReq.get.waitingTime + otherReq.get.serviceTime - arrivalTimeDifference
      if (time < 0) 0 else time
    } else 0
  }
  
  private def arrivalTimeDifference = {
    val otherReq = earlierOtherRequest
    if (otherReq.isDefined) {
      arrivalTime - otherReq.get.arrivalTime
    } else 0
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

  def minArrivalTime = {
    job.arrivalTime + pastServiceTime
  }

  def pastServiceTime = {
    (0 /: previousRequestsInJob) {
      _ + _.serviceTime
    }
  }

  def pastResidenceTime(): Int = {
    if (!isClosed || cachedPastResidenceTime < 0) {
      cachedPastResidenceTime = computePastResidenceTime
    }
    cachedPastResidenceTime
  }

  private def computePastResidenceTime(): Int = {
    (0 /: previousRequestsInJob) {
      _ + _.residenceTime
    }
  }

  def processingAt(time: Int) = {
    time >= leavingQueueTime && time < leavingServiceTime
  }

  def waitingAt(time: Int) = {
    time >= arrivalTime && time < leavingQueueTime
  }

}
