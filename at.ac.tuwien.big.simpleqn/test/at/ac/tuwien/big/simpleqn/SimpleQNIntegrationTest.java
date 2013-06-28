/*
 * Copyright (c) 2013 Vienna University of Technology.
 * All rights reserved. This program and the accompanying materials are made 
 * available under the terms of the Eclipse Public License v1.0 which accompanies 
 * this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Philip Langer - initial API and implementation
 */
package at.ac.tuwien.big.simpleqn;

import junit.framework.TestCase;
import scala.collection.immutable.Range;

public class SimpleQNIntegrationTest extends TestCase {

	public void testBasicQN() {
		QueuingNet net = new QueuingNet();
		Service service1 = new Service("Service1", 1, net);
		Service service2 = new Service("Service2", 2, net);
		Service service3 = new Service("Service3", 3, net);
		Service service4 = new Service("Service4", 4, net);

		Job job1 = new Job(1, net);
		Job job2 = new Job(2, net);

		// The order in which requests are added within one job matters!
		Request requestJob1Service1 = job1.requestService(service1);
		Request requestJob1Service2 = job1.requestService(service2);
		Request requestJob1Service4 = job1.requestService(service4);

		Request requestJob2Service1 = job2.requestService(service1);
		Request requestJob2Service3 = job2.requestService(service3);
		Request requestJob2Service4 = job2.requestService(service4);
				
		// assert overall service time
		assertEquals(7, job1.overallServiceTime());
		assertEquals(8, job2.overallServiceTime());

		// assert requests at service centers
		assertEquals(2, service1.requests().size());
		assertEquals(1, service2.requests().size());
		assertEquals(1, service3.requests().size());
		assertEquals(2, service4.requests().size());
		assertEquals(1, service1.requestsOf(job1).size());
		assertTrue(service1.requestsOf(job1).contains(requestJob1Service1));
		assertEquals(1, service2.requestsOf(job1).size());
		assertTrue(service2.requestsOf(job1).contains(requestJob1Service2));
		assertEquals(0, service3.requestsOf(job1).size());
		assertEquals(1, service4.requestsOf(job1).size());
		assertTrue(service4.requestsOf(job1).contains(requestJob1Service4));

		// assert prev and next request
		assertEquals(requestJob1Service1, requestJob1Service2
				.previousRequestInJob().get());
		assertEquals(requestJob1Service2, requestJob1Service4
				.previousRequestInJob().get());
		assertTrue(requestJob1Service1.previousRequestInJob().isEmpty());
		assertEquals(requestJob1Service2, requestJob1Service1
				.nextRequestInJob().get());
		assertEquals(requestJob1Service4, requestJob1Service2
				.nextRequestInJob().get());
		assertTrue(requestJob1Service4.nextRequestInJob().isEmpty());

		// assert waiting times
		assertEquals(0, requestJob1Service1.waitingTime());
		assertEquals(0, requestJob1Service2.waitingTime());
		assertEquals(0, requestJob1Service4.waitingTime());
		// 0sec waiting time because job2 starts 1sec later than job1, service1
		// requires 1sec -> when job2 arrives, job1 has been finished already
		assertEquals(0, requestJob2Service1.waitingTime());
		assertEquals(0, requestJob2Service3.waitingTime());
		// 2sec waiting time because job2 starts 1sec later than job1, and
		// job2 arrives at service4 2sec later than job1 (service 3 takes 1sec
		// longer than service2, job2 looses another second). Service4 takes
		// 4sec and job2 arrives after job1 has been processed for 2sec already.
		assertEquals(2, requestJob2Service4.waitingTime());

		// assert residence times
		assertEquals(1, requestJob1Service1.residenceTime());
		assertEquals(2, requestJob1Service2.residenceTime());
		assertEquals(4, requestJob1Service4.residenceTime());
		assertEquals(1, requestJob2Service1.residenceTime());
		assertEquals(3, requestJob2Service3.residenceTime());
		assertEquals(6, requestJob2Service4.residenceTime());

		// assert arrival times
		assertEquals(1, requestJob1Service1.arrivalTime());
		assertEquals(2, requestJob1Service2.arrivalTime());
		assertEquals(4, requestJob1Service4.arrivalTime());
		assertEquals(2, requestJob2Service1.arrivalTime());
		assertEquals(3, requestJob2Service3.arrivalTime());
		assertEquals(6, requestJob2Service4.arrivalTime());

		// assert leaving times
		assertEquals(2, requestJob1Service1.leavingServiceTime());
		assertEquals(4, requestJob1Service2.leavingServiceTime());
		assertEquals(8, requestJob1Service4.leavingServiceTime());
		assertEquals(3, requestJob2Service1.leavingServiceTime());
		assertEquals(6, requestJob2Service3.leavingServiceTime());
		assertEquals(3, requestJob2Service3.leavingQueueTime());
		assertEquals(12, requestJob2Service4.leavingServiceTime());
		assertEquals(8, requestJob2Service4.leavingQueueTime());

		assertEquals(7, job1.overallServiceTime());
		assertEquals(0, job1.overallWaitingTime());
		assertEquals(7, job1.overallResidenceTime());
		assertEquals(0, job1.maxWaitingTime());

		assertEquals(8, job2.overallServiceTime());
		assertEquals(10, job2.overallResidenceTime());
		assertEquals(2, job2.overallWaitingTime());
		assertEquals(2, job2.maxWaitingTime());

		// assert queue at time
		assertEquals(0, service1.requestQueueAt(0).size());
		assertEquals(1, service4.requestQueueAt(6).size());
		assertEquals(1, service4.requestQueueAt(7).size());
		assertEquals(0, service4.requestQueueAt(8).size());

		// assert average/max queue length in a time interval
		assertEquals(0.0, service1.averageQueueLength(range(0, 20)));
		assertEquals(1.0, service4.averageQueueLength(range(6, 7)));
		assertEquals(0.5, service4.averageQueueLength(range(7, 8)));
		assertEquals(0, service1.maxQueueLength(range(0, 20)));
		assertEquals(0, service4.maxQueueLength(range(0, 4)));
		assertEquals(1, service4.maxQueueLength(range(0, 20)));

		// assert completion time
		assertEquals(8, job1.completionTime());
		assertEquals(12, job2.completionTime());
		assertEquals(12, net.completionTime());

		// assert idle time, busy time, and utilization
		assertEquals(1, service1.idleTime(range(0, 0)));
		assertEquals(0, service1.idleTime(range(1, 2)));
		assertEquals(1, service1.busyTime(range(0, 1)));
		assertEquals(2, service1.busyTime(range(1, 2)));
		assertEquals(0.5, service1.utilization(range(0, 1)));
		assertEquals(1.0, service1.utilization(range(1, 2)));
		assertEquals(1, net.busyTime(range(0, 1)));
		assertEquals(0.5, net.utilization(range(0, 1)));
		assertEquals(1.0, net.utilization(range(1, 2)));
		assertEquals(1.0, net.utilization(range(1, 11)));

		// assert number of completed jobs and throughput
		assertEquals(2, net.completedJobs().size());
		assertEquals(1, net.completedJobs(8).size());
		assertEquals(0, net.completedJobs(7).size());
		assertEquals(2d / 12d, net.throughput());
	}

	public void testJobDependentServiceTime() {
		QueuingNet net = new QueuingNet();
		Service service1 = new Service("Service1", 2, net);
		Service service2 = new Service("Service2", 2, net);

		Job job1 = new Job(1, net);
		Job job2 = new Job(2, net);
		Job job3 = new Job(3, net);
		Job job4 = new Job(4, net);

		Request reqJob1Service1 = job1.requestService(service1, 3);
		Request reqJob1Service2 = job1.requestService(service2, 3);
		Request reqJob2Service1 = job2.requestService(service1, 4);
		Request reqJob2Service2 = job2.requestService(service2);
		Request reqJob3Service1 = job3.requestService(service1, 1);
		Request reqJob3Service2 = job3.requestService(service2, 5);
		Request reqJob4Service1 = job4.requestService(service1);
		Request reqJob4Service2 = job4.requestService(service2, 4);
		
		assertEquals(2, reqJob2Service1.waitingTime());
		assertEquals(0, reqJob2Service2.waitingTime());

		assertEquals(3, reqJob1Service1.serviceTime());
		assertEquals(3, reqJob1Service2.serviceTime());
		assertEquals(4, reqJob2Service1.serviceTime());
		assertEquals(2, reqJob2Service2.serviceTime());
		assertEquals(1, reqJob3Service1.serviceTime());
		assertEquals(5, reqJob3Service2.serviceTime());
		assertEquals(2, reqJob4Service1.serviceTime());
		assertEquals(4, reqJob4Service2.serviceTime());
		
		assertEquals(6, job1.overallResidenceTime());
		assertEquals(0, job1.overallWaitingTime());
		assertEquals(7, job1.completionTime());
		
		assertEquals(2, job2.overallWaitingTime());
		assertEquals(10, job2.completionTime());
		assertEquals(8, job2.overallResidenceTime());
		
		assertEquals(15, job3.completionTime());
		assertEquals(6, job3.overallWaitingTime());
		assertEquals(12, job3.overallResidenceTime());
		
		assertEquals(9, job4.overallWaitingTime());
		assertEquals(15, job4.overallResidenceTime());
		assertEquals(19, job4.completionTime());
	}

	private Range range(int from, int to) {
		// Range is inclusive
		return new Range(from, to + 1, 1);
	}

}
