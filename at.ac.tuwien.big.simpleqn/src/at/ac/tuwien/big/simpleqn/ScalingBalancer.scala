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

  for (i <- scalingStrategy.numberOfServices) yield { scaleOut(0) }

  override def availableServices(time: Int) = {
    super.availableServices(time).filter {
      _ match {
        case node: ScaledServiceNode => node.isActive(time)
        case _ => false
      }
    }
  }

  override def addRequest(request: Request) = {
    val currentTime = request.arrivalTime
    val availServices = availableServices(currentTime)
    if (scalingStrategy.shouldScaleOut(request, availServices))
      scaleOut(currentTime)
    if (scalingStrategy.shouldScaleIn(request, availServices))
      scaleIn(currentTime, serviceToScaleIn(availServices))
    super.addRequest(request)
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
    val availableFrom = currentTime + scalingStrategy.startUpTime
    val newService = new ScaledServiceNode(name + nextServiceId, serviceTime, availableFrom)
    addService(newService)
  }

  private def nextServiceId = {
    "_" + (services.length + 1).toString
  }

}