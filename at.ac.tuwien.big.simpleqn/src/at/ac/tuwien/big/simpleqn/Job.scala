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

import scala.collection.mutable.HashSet
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

class Job(val arrivalTime: Int, val net: QueuingNet) {
  net.jobs += this

  val requests = new MutableList[Request]

  private def totalValueOfRequests(value: Request => Int) = {
    (0 /: requests) { _ + value(_) }
  }

  private def maxValueOfRequests(value: Request => Int) = {
    (0 /: requests) { (maxValue, request) => Math.max(maxValue, value(request)) }
  }

  def request(service: Service): Request = {
    request(service, service.serviceTime)
  }

  def request(service: Service, serviceTime: Int) = {
    val request = new Request(this, service, serviceTime)
    requests += request
    service.addRequest(request)
    request
  }

  def waitingAt(time: Int) = {
    requests.exists { _.waitingAt(time) }
  }

  def processingAt(time: Int) = {
    requests.exists { _.processingAt(time) }
  }

  def overallServiceTime: Int = {
    totalValueOfRequests { _.serviceTime }
  }

  def overallWaitingTime = {
    totalValueOfRequests { _.waitingTime }
  }

  def overallResidenceTime = {
    overallServiceTime + overallWaitingTime
  }

  def averageWaitingTime = {
    overallWaitingTime / requests.size.toDouble
  }

  def maxWaitingTime = {
    maxValueOfRequests { _.waitingTime }
  }

  def arrivedBefore(time: Int) = {
    arrivalTime <= time
  }

  def latestLeavingRequest = {
    requests.foldLeft(requests.head) { (latestRequest, currentRequest) =>
      if (latestRequest.leavingServiceTime >= currentRequest.leavingServiceTime)
        latestRequest
      else
        currentRequest
    }
  }

  def completionTime = {
    latestLeavingRequest leavingServiceTime
  }

  def serviceAt(time: Int): Option[Service] = {
    val request = requests.find {
      request => request.waitingAt(time) || request.processingAt(time)
    }
    if (request.isDefined) {
      val service = request.get.service
      Option(service)
    } else {
      None
    }
  }

}
