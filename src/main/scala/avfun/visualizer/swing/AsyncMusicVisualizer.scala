package avfun.visualizer.swing

import avfun.visualizer.MusicVisualizer
import avfun.viz.Canvas2D
import avfun.visualizer.AudioFrameData

class AsyncMusicVisualizer(val musicVisualizer:MusicVisualizer) extends MusicVisualizer {

  private val _processThread = new Thread(){
    private var _frameData:AudioFrameData = null
    var running = true;
    override def run():Unit = {
      while(running){
        processData
      }
    }
    
    private def processData{
      this.synchronized{
        this.wait()
      }
       
      this.synchronized{
        if(_frameData != null){
          musicVisualizer.onFrameData(_frameData)
          _frameData = null
        }
      }
    }
    
    def addFrameData(frameData:AudioFrameData):Unit = {
      this.synchronized{
        _frameData = frameData
        this.notify()
      }
    }
  }
  
  override def setCanvas(canvas:Canvas2D):Unit = {
    musicVisualizer.setCanvas(canvas)
  }
  
  def requiredSamplesPerFrame: Int = { musicVisualizer.requiredSamplesPerFrame }

  def onRegistered: Unit = { 
		  musicVisualizer.onRegistered
      _processThread.start()
  }
  def onDeregistered: Unit = { 
    musicVisualizer.onDeregistered
    _processThread.running = false
  }
  
  def drawFrame(frameData: AudioFrameData, canvas: Canvas2D): Unit = { 
    musicVisualizer.drawFrame(frameData, canvas) 
  } 
  
  /**
   * processes the frame data in another thread
   */
  override def onFrameData(frameData: AudioFrameData): Unit = {
    _processThread.addFrameData(frameData)
//    _processThread.notify()
  }
  
  
}