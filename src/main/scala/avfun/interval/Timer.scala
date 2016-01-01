package avfun.interval

/**
 * This interface defines a mechanism for executing a function on a fixed interval.
 * 
 */
trait Timer {
  var interval:Int
  
  private var _intFunc:Option[()=>Unit] = None
  
  def executeIntervalFunction:Unit = _intFunc.foreach(f => f())
  
  def setIntervalFunction(intFunc:()=>Unit){
    _intFunc = Some(intFunc)
  }
  
  def play
  def pause
  def stop
  
  def isStarted:Boolean
  def isPaused:Boolean
  def isStopped:Boolean
}