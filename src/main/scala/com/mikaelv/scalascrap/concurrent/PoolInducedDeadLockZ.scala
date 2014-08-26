package com.mikaelv.scalascrap.concurrent

import scala.concurrent._
import duration._
import java.util.concurrent.Executors
import scala.collection.concurrent.TrieMap
import scala.annotation.tailrec
import scalaz.concurrent.Future

/**
 * [[http://blog.jessitron.com/2014/01/fun-with-pool-induced-deadlock.html]]
 */
object PoolInducedDeadLockZ extends App {


  val n = Runtime.getRuntime.availableProcessors
  val taskCount = 30000
  implicit val pool = Executors.newCachedThreadPool()

  val receivedMessages = TrieMap[Int, String]()

  def sendMessage(taskId: Int) = Future {
    //println(s"${Thread.currentThread()} - taskId: $taskId map.put")
    receivedMessages.put(taskId, "Hi!")
  }.runAsync(_ => ())


  @tailrec
  def waitForMessage(taskId: Int, delay: Int, count: Int): Boolean = {
    if (receivedMessages.get(taskId).isEmpty && count > 0) {
      //blocking { Thread.sleep(delay) }
      Thread.sleep(delay)
      waitForMessage(taskId, delay, count-1)
    } else {
      count > 0
    }

  }


  val start = System.nanoTime()
  val futures = Range(0, taskCount).map { taskId =>
    Future {
      //println(s"${Thread.currentThread()} - taskId: $taskId start")
      sendMessage(taskId)
      val complete = waitForMessage(taskId, 100, 10)
      if (taskId % 1000 == 0) println (s"$taskId - number of threads: ${Thread.activeCount()}")
      //println(s"${Thread.currentThread()} - taskId: $taskId complete: $complete")
    }
  }

  Future.gatherUnordered(futures).run
  //Await.result(Future.sequence(futures), 5.seconds)
  val duration = (System.nanoTime() - start) / 1000000
  println(s"Done in $duration ms")
  //ecn.shutdown()
}
