package avfun.visualizer

trait AudioFrameProducer{
  protected var _audioFrameListeners = List[AudioFrameListener]()
  def addAudioFrameListener(listener:AudioFrameListener):Unit = {
    _audioFrameListeners :+= listener
    listener.onRegistered
  }
  def removeAudioFrameListener(listener:AudioFrameListener):Unit = {
    val newListeners = _audioFrameListeners.filterNot{l => 
      if(l == listener){
        l.onDeregistered
        true
      }
      else{
        false
      }
    }
    _audioFrameListeners = newListeners
  }
  
  def maxBufferedAudioSamples(minSamplesPerFrame:Int):Int = {
    val listenerSamples = _audioFrameListeners.map(_.requiredSamplesPerFrame).toSeq
    val allReqSamples = listenerSamples :+ minSamplesPerFrame
    allReqSamples.max
  }
  
  def sendAudioFrameToListeners(frameData:AudioFrameData){
    _audioFrameListeners.foreach{l => l.onFrameData(frameData) }
  }
}