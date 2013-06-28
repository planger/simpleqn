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

class QueuingNet() {

  protected[simpleqn] val services = new mutable.MutableList[Service]
  protected[simpleqn] val jobs = new mutable.HashSet[Job]

  protected[simpleqn] def countIf(bool: Boolean) = {
    if (bool) 1 else 0
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
    for (j <- jobs) { debugPrint(j) }
  }

  private def debugPrintScale {
    (0 to completionTime) foreach { i => print("|  " + i + "  ") }
    println("|")
  }

  private def debugPrint(job: Job) {
    (0 until job.arrivalTime) foreach { i => print("|" + ".." + i + "..") }
    (job.arrivalTime to completionTime) foreach { i =>
      if (i >= job.completionTime) print("|.." + i + "..")
      else if (job.waitingAt(i)) print("|w " + i + " w")
      else if (job.processingAt(i)) print("|b " + i + " b")
    }
    println("|")
  }

}
