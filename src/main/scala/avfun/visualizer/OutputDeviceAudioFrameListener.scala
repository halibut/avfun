package avfun.visualizer

import avfun.audio.AudioOutputDevice

class OutputDeviceAudioFrameListener(val outputDevice:AudioOutputDevice) extends AudioFrameListener {
  
  override def onFrameData(frameData: AudioFrameData): Unit = {
    outputDevice.play(frameData.audioData)
  }

  override val requiredSamplesPerFrame: Int = 0

  def onDeregistered: Unit = {
    outputDevice.stop
  }

  def onRegistered: Unit = {
    //do nothing
  }
}