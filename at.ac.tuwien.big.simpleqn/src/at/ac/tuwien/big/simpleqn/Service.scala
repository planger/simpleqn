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

class Service(val name: String, val serviceTime: Int) {

  private val _requests = new ListBuffer[Request]
  
  private def countIf(bool: Boolean) = {
    if (bool) 1 else 0
  }

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
    val queueAtTime = requests filter { request =>
      time >= request.arrivalTime && time < request.leavingQueueTime
    }
    sortByArrivalTime(queueAtTime)
  }

  def requestQueueLengthAt(time: Int) = {
    requestQueueAt(time).length
  }

  def sortByArrivalTime(requestList: List[Request]) = {
    requestList sortWith { (r1, r2) =>
      if (r1.arrivalTime != r2.arrivalTime) {
        r1.arrivalTime < r2.arrivalTime
      } else {
        r1.job.arrivalTime < r2.job.arrivalTime
      }
    }
  }

  def avgQueueLength(range: Range) = {
    (0 /: range) { _ + requestQueueLengthAt(_) } / range.length.toDouble
  }

  def maxQueueLength(range: Range) = {
    (0 /: range) { (max, time) => Math.max(max, requestQueueLengthAt(time)) }
  }

  def busyAt(time: Int) = {
    requests exists { _.processingAt(time) }
  }

  def busyTime(range: Range) = {
    (0 /: range) { (busyTime, time) => busyTime + countIf(busyAt(time)) }
  }

  def idleTime(range: Range) = {
    range.length - busyTime(range)
  }

  def utilization(range: Range): Double = {
    busyTime(range) / range.length.toDouble
  }

}
