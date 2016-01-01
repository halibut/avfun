package avfun.interval

/**
 * A Java Thread-based timer that tries to be as accurate as Thread.sleep will allow
 * which is typically in the 1-10 millisecond range (depending on OS and load of the system)
 * 
 * This Timer will do its best to ensure that any time-drift is corrected for within 1 interval 
 * as long as the user-defined function can finish executing within that interval.
 * 
 * If the user-defined function takes longer than the interval, then it will keep repeating
 * the function indefinitely until either the timer is stopped or until the user function
 * completes within the interval, at which point it will continue to try to minimize time-drift
 * once again.
 * 
 */
class ThreadBasedTimer() extends Timer {
  
  private val _timer = this
  
  override var interval:Int = 1000
  
  var _thread:Option[TimerThread] = None
  
  def play: Unit = {
    if(!_thread.isDefined){
      _thread = Some(createAndStartTimerThread)
    }
    _thread.foreach { x => x.paused = false}
  }
  
  def pause: Unit = {
    _thread.foreach { x => x.paused = true}
  }

  def stop: Unit = {
    _thread.foreach { x => x.running = false }
    _thread = None
    System.out.println("Stopped timer thread.")
  }
  
  def isPaused: Boolean = { _thread.map(_.paused).getOrElse(false) }
  def isStarted: Boolean = { _thread.map(!_.paused).getOrElse(false) }
  def isStopped: Boolean = { !_thread.isDefined }
  
  private def createAndStartTimerThread:TimerThread = {
    val t = new TimerThread()
    t.start
    System.out.println("Started timer thread.")
    t
  }
  
  class TimerThread extends Thread("Timer Thread"){
    var running = true
    var paused = false
    var lastKnownAccurateFrame:Long = 0
    var accurateFrames:Long = 0
    
    override def run(){
      lastKnownAccurateFrame = System.currentTimeMillis()
      
      while(running){
        
        if(paused){
          Thread.sleep(100)
          accurateFrames = 0
          lastKnownAccurateFrame = System.currentTimeMillis()
        }
        else{
          val beforeTime = System.currentTimeMillis()
          try{
            executeIntervalFunction
          }
          catch{
            case e:Exception => {
              _timer.stop
              throw e
            } 
          }
          val afterTime = System.currentTimeMillis()
          
          val deltaTime = afterTime - beforeTime
          if(deltaTime < interval){
            accurateFrames += 1
            val expectedStartNext = lastKnownAccurateFrame + (accurateFrames * interval)
            val diffToExpected = expectedStartNext - afterTime
            
            if(diffToExpected > 0){
              Thread.sleep(diffToExpected)
            }
          }
          else{
            accurateFrames = 0
            lastKnownAccurateFrame = afterTime
          }
        }
                  
      }

    }
  }


}