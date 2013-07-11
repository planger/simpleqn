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

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.Random
import at.ac.tuwien.big.simpleqn.strategies.BalancingStrategy
import at.ac.tuwien.big.simpleqn.strategies.ScalingStrategy

class ScalingBalancer(name: String, serviceTime: Int, balancingStrategy: BalancingStrategy, scalingStrategy: ScalingStrategy)
  extends Balancer(name, serviceTime, balancingStrategy) {

  scalingStrategy.balancer = this
  for (i <- 1 to scalingStrategy.numberOfServices.start) { addService(0) }

  override def availableServices(time: Int) = {
    super.availableServices(time).filter {
      _ match {
        case node: ScaledServiceNode => node.isActive(time)
        case _ => false
      }
    }
  }

  override def addToQueue(request: Request) {
    val currentTime = request.leavingQueueTime
    val availServices = availableServices(currentTime)
    if (shouldScaleOut(request, availServices))
      scaleOut(currentTime)
    if (shouldScaleIn(request, availServices))
      scaleIn(currentTime, serviceToScaleIn(availServices))
    super.addToQueue(request)
  }

  private def shouldScaleOut(request: Request, availServices: List[Service]) = {
    scalingStrategy.shouldScaleOut(request, availServices) && availServices.size + 1 < scalingStrategy.numberOfServices.end
  }

  private def shouldScaleIn(request: Request, availServices: List[Service]) = {
    scalingStrategy.shouldScaleIn(request, availServices) && availServices.size > scalingStrategy.numberOfServices.start
  }

  def serviceToScaleIn(services: List[Service]) = {
    services(Random.nextInt(services.length))
  }

  private def scaleIn(time: Int, service: Service) {
    service match {
      case node: ScaledServiceNode => node.availableTo = time
    }
  }

  private def scaleOut(currentTime: Int) {
    addService(currentTime + scalingStrategy.startUpTime)
  }

  private def addService(availableFrom: Int) {
    val newService = new ScaledServiceNode(nextServiceId, serviceTime, availableFrom)
    addService(newService)
  }

  private def nextServiceId = {
    name + "_" + (services.length + 1).toString
  }

}