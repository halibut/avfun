package avfun.audio.stream

import java.util.Arrays

/**
 * A wrapper around another AudioStream where an additional amount of samples
 * can be buffered from the underlying stream.
 * 
 * The read() method will advance the underlying stream by the specified samples
 * while the new getBufferedSamples() method will retrieve up to bufferedSamples
 * number of already-buffered samples, but will not advance the underlying stream.
 * 
 */
class BufferedAudioStream(val as:AudioStream, val bufferedSamples:Int) extends AudioStream {
  val channels: Int = as.channels
  val samplesPerSecond: Int = as.samplesPerSecond
  
  private var _streamClosed = false
  private var _bufferedSamples = 0
  private var _streamPosition:Option[Float] = None
  private val _buffer = new Array[Array[Float]](channels)
  for(i <- 0 until channels){
    _buffer(i) = new Array[Float](bufferedSamples)
  }
  
  private def readIntoBuffer(samples:Int):Unit = {
    val newData = as.read(samples)
    
    if(newData.isDefined){
      val data = newData.get
    
      _streamPosition = data.streamPosition
      
      //println(s"${new java.util.Date().getTime} - Read ${data.samples} samples.")
      
      val remainingSpace = bufferedSamples - _bufferedSamples
      val prevBuffered = _bufferedSamples
      
      if(data.samples > remainingSpace){
        val diff = data.samples - remainingSpace
        
        (0 until channels) foreach { i =>
          System.arraycopy(_buffer(i), diff, _buffer(i), 0, prevBuffered - diff)
          System.arraycopy(data.channelData(i), 0, _buffer(i), prevBuffered - diff, data.samples)
        }
        _bufferedSamples = bufferedSamples
      }
      else{
        (0 until channels) foreach { i =>
          System.arraycopy(data.channelData(i), 0, _buffer(i), prevBuffered, data.samples)
        }
        _bufferedSamples = prevBuffered + data.samples
      }
    }
    else{
      _streamClosed = true
    }
    
  }
  
  def getBufferedSamples(samples:Int):Option[StreamData] = {
    if(_streamClosed){
      None
    }
    else{
      val toRead = math.min(samples, _bufferedSamples)
      
      val readSamples = (0 until channels) map { i =>
        Arrays.copyOf(_buffer(i), toRead)
      }
      Some(StreamData(toRead, readSamples, _streamPosition))
    }
  }

  def read(samples: Int):Option[StreamData] = {
    if(_streamClosed){
      None
    }
    else if(_bufferedSamples == 0){
      readIntoBuffer(bufferedSamples)
      getBufferedSamples(samples)
    }
    else{
      readIntoBuffer(samples)
      getBufferedSamples(samples)
    }
  }
  
  def close:Unit = as.close

}