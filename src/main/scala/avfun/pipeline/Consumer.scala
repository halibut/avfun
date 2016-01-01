package avfun.pipeline

import java.util.concurrent.ArrayBlockingQueue

trait Consumer[T] {
  private[pipeline] val maxQueueSize = 10
  private val _inputQueue = new ArrayBlockingQueue[T](maxQueueSize)
  
  def put(input:T) = _inputQueue.put(input)

  def get:T = _inputQueue.take
  
  def queueDepth:Int = _inputQueue.size()
  
  def queueDepths:Seq[Int] = Seq(queueDepth)
}