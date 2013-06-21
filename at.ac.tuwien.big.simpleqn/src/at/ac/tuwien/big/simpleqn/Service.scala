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

import scala.collection.mutable.MutableList
import scala.reflect.internal.util.Collections
import scala.collection.mutable.ListBuffer
import scala.Immutable

class Service(val name: String, val serviceTime: Int, val net: QueueingNet) {
  net.services += this

  private val _requests = new ListBuffer[Request]

  protected[simpleqn] def addRequest(request: Request) {
    _requests += request
  }

  def requests: List[Request] = {
    sortByArrivalTime(_requests.toList)
  }

  def requestsOf(job: Job) = {
    requests filter (job == _.job)
  }

  def jobQueueAt(time: Int) = {
    requestQueueAt(time) collect { case request => request.job }
  }

  def requestQueueAt(time: Int): List[Request] = {
    sortByArrivalTime(requests
      filter (_.arrivalTime <= time)
      filter (_.leavingQueueTime > time))
  }

  def requestQueueLengthAt(time: Int) = {
    requestQueueAt(time).length
  }

  def sortByArrivalTime(requestList: List[Request]) = {
    requestList sortWith (_.arrivalTime < _.arrivalTime)
  }

  def averageQueueLength(range: Range) = {
    (0 /: range) { _ + requestQueueLengthAt(_) } / range.length.toDouble
  }

  def maxQueueLength(range: Range) = {
    (0 /: range) { (max, time) => Math.max(max, requestQueueLengthAt(time)) }
  }

  def busyAt(time: Int) = {
    requests.exists(_.isProcessingAt(time))
  }

  def busyTime(range: Range) = {
    (0 /: range) { (busyTime, time) => busyTime + net.countIf(busyAt(time)) }
  }

  def idleTime(range: Range) = {
    range.length - busyTime(range)
  }

  def utilization(range: Range): Double = {
    busyTime(range) / range.length.toDouble
  }

  def utilization: Double = {
    utilization(0 to net.completionTime)
  }

}
