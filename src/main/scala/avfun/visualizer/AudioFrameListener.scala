package avfun.visualizer

import avfun.audio.stream.StreamData

trait AudioFrameListener {
  
  def requiredSamplesPerFrame:Int
  
  def onFrameData(frameData:AudioFrameData):Unit

  def onRegistered:Unit
  
  def onDeregistered:Unit
}


