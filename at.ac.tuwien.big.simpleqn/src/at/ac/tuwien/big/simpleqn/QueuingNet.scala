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
import scala.collection.JavaConversions

class QueuingNet(val services: List[Service]) {
  
  private var _isComputationDone = false
  
  protected[simpleqn] val jobs = new mutable.HashSet[Job]
  
  def sortedJobs = {
    jobs.toList.sortBy(_.arrivalTime)
  }
  
  def close() {
    new QueuingNetSolver(services, sortedJobs).solve
    _isComputationDone = true
  }
  
  def isClosed = _isComputationDone
  
  def open() {
    _isComputationDone = false
  }

  def this(serviceList: java.util.List[Service]) = {
    this(JavaConversions.asScalaBuffer(serviceList).toList)
  }

  def latestCompletingJob = {
    jobs.foldLeft(jobs.head) { (latestJob, currentJob) =>
      if (latestJob.completionTime >= currentJob.completionTime)
        latestJob
      else
        currentJob
    }
  }

  def completionTime = {
    latestCompletingJob.completionTime
  }

  def busyTime(range: Range) = {
    (0 /: range) { (busyTime, time) =>
      busyTime + countIf(services.exists(_.busyAt(time)))
    }
  }
    
  private def countIf(bool: Boolean) = {
    if (bool) 1 else 0
  }

  def completedJobs: List[Job] = {
    completedJobs(0 until completionTime)
  }

  def completedJobs(toTime: Int): List[Job] = {
    completedJobs(0 to toTime)
  }

  def completedJobs(range: Range): List[Job] = {
    jobs filter { job =>
      job.arrivalTime >= range.start && job.completionTime <= range.end
    } toList
  }

  def utilization(range: Range): Double = {
    busyTime(range) / range.length.toDouble
  }

  def utilization: Double = {
    utilization(0 until completionTime)
  }

  def throughput(range: Range): Double = {
    completedJobs(range).length / range.length.toDouble
  }

  def throughput: Double = {
    throughput(0 until completionTime)
  }

  def debugPrint {
    debugPrintScale
    for (j <- sortedJobs) { debugPrint(j) }
  }

  private def debugPrintScale {
    (0 to completionTime) foreach { i => print("|     " + i + "     ") }
    println("|")
  }

  private def debugPrint(job: Job) {
    (0 until job.arrivalTime) foreach { i =>
      val fill = i.toString.replaceAll(".", " ")
      print("|" + ".." + fill + "      ..")
    }
    
    (job.arrivalTime to completionTime) foreach { i =>
      
      val fill = i.toString.replaceAll(".", " ")
      val fillAll = fill + "        "
      val service = job.serviceAt(i)
      var serviceName = if (service.isDefined) { service.get.name } else { fillAll }
      if (serviceName.length > fillAll.length) {
        serviceName = serviceName.subSequence(serviceName.length - fillAll.length, serviceName.length).toString()
      } else {
        serviceName = serviceName + fillAll.substring(serviceName.length)
      }
      
      if (i >= job.completionTime) print("|.." + fillAll)      
      else if (job.waitingAt(i)) print("|w@" + serviceName)
      else if (job.processingAt(i)) print("|b@" + serviceName)
      
    }
    println("|")
  }

}
