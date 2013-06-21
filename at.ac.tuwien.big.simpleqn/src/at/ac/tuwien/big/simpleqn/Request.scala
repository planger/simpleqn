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

class Request(val job: Job, val service: Service) {

  val net = job.net

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

  private def ealierOtherRequests() = {
    val requests = service.requests
    requests.slice(0, requests.indexOf(this))
  }

  private def waitingTimeBehind(request: Request) = {
    request.waitingTime - arrivalTimeDifference(request)
  }

  private def arrivalTimeDifference(request: Request) = {
    if (arrivalTime > request.arrivalTime) {
      arrivalTime - request.arrivalTime
    } else {
      request.arrivalTime - arrivalTime
    }
  }

  def waitingTime: Int = {
    val earlierRequests = ealierOtherRequests
    if (earlierRequests.length < 1)
      0
    else
      (0 /: earlierRequests) { _ + waitingTimeBehind(_) } + serviceTime
  }

  def serviceTime = {
    service.serviceTime
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
    if (previousRequestInJob.isDefined) {
      val prevRequest = previousRequestInJob.get
      prevRequest.pastResidenceTime + prevRequest.waitingTime + prevRequest.serviceTime
    } else 0
  }
  
  def isProcessingAt(time: Int) = {
    time >= leavingQueueTime && time <= leavingServiceTime
  }

}
