package avfun.visualizer

import avfun.audio.stream.AudioStream
import avfun.viz.Canvas2D
import avfun.audio.stream.StreamData
import avfun.audio.AudioOutputDevice
import avfun.interval.Timer
import avfun.audio.stream.BufferedAudioStream
import scala.collection.mutable.ArrayBuffer

trait MusicVisualizer extends AudioFrameListener {
  
  def requiredSamplesPerFrame:Int
  private var _canvas:Option[Canvas2D] = None
  
  private val _drawFrameListeners:ArrayBuffer[VideoFrameListener] = new ArrayBuffer()
  def frameListener_+=(listenerFunc:VideoFrameListener):Unit = this._drawFrameListeners += listenerFunc
  
  def drawFrame(frameData:AudioFrameData,canvas:Canvas2D):Unit 
  
  def onFrameData(frameData: AudioFrameData): Unit = {
    _canvas.foreach { canvas => 
      drawFrame(frameData, canvas)
      
      val buffer = canvas.getByteBuffer
      
      _drawFrameListeners.foreach{ l => 
        l.onFrameData(VideoFrameData(canvas.colorBufferType, buffer, frameData.endOfData))
      }
    }
  }
  
  def setCanvas(canvas:Canvas2D):Unit = {
    this._canvas = Some(canvas)
  }

}



