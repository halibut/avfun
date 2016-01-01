package avfun.audio

import avfun.audio.stream.StreamData

trait AudioOutputDevice {
  val channels:Int
  val samplesPerSecond:Int
  def latencySamples:Int
  
  def play(streamData:StreamData):Unit
  
  def stop:Unit
}