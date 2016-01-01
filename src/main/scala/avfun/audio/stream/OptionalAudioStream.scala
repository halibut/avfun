package avfun.audio.stream

/**
 * A special kind of audio stream that will always return data when the read() method is invoked.
 * It also has the ability to wrap an underlying stream. The underlying stream can be switched,
 * which will cause the data retrieved from the read() method to use said stream.
 * 
 * This gives the flexibility to switch the underlying stream without affecting anything that
 * depends on an active stream. During the switch, there will simply be zero (quiet) data returned
 * from this stream.
 */
class OptionalAudioStream(override val samplesPerSecond:Int, override val channels:Int)  extends AudioStream {
  
  private var _innerStream:Option[AudioStream] = None
  private val _lock = new AnyRef

  def read(samples: Int): Option[StreamData] = {
    val stream = _innerStream
    
    if(stream.isDefined){
      val readSamples = stream.get.read(samples)
      if(readSamples.isEmpty){
        removeStream
        Some(StreamData.Empty(samples, channels))
      }
      else{
        val samps = readSamples.get.toNChannelStream(channels)
        Some(samps.extendBy(samples - samps.samples))
      }
    } 
    else{
      Some(StreamData.Empty(samples, channels))
    }  
  }

  def close: Unit = {
    removeStream
  }

  private def removeStream:Unit = {
    val stream = _innerStream
    _innerStream = None
    
    stream.foreach{ s=> s.close }
  }
  
  def setStream(stream:AudioStream):Unit = {
    removeStream
    _innerStream = Some(stream)
  }
  
}