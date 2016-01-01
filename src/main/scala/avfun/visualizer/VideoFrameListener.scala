package avfun.visualizer

import avfun.viz.Canvas2D
import avfun.viz.ColorBufferType

trait VideoFrameListener {
  //def framesPerSecond:Int
  def onFrameData(frameData:VideoFrameData):Unit
}

case class VideoFrameData(
    val colorBufferType:ColorBufferType,
    val buffer:Array[Byte], 
    val endOfData:Boolean)