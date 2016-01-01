package avfun.audio.stream

/**
 * Represents a stream of audio data. Contains basic information about the audio data
 * such as the number of channels and samples per second, as well as methods to read
 * data from the stream. 
 */
trait AudioStream {
  val channels:Int
  val samplesPerSecond:Int
  
  def read(samples:Int):Option[StreamData]
  def close:Unit
  
}

/**
 * A chunk of audio data. Contains the number of samples in the chunk
 * and a IndexedSeq of Float Arrays containing the audio data for each channel.
 */
case class StreamData(val samples:Int, val channelData:IndexedSeq[Array[Float]]){
  
  /**
   * Extends the amount of samples in the StreamData by extending zeros to
   * the right side of the channelData arrays.
   * 
   * If additionalSamples <= 0, then no extension will take place 
   */
  def extendBy(additionalSamples:Int):StreamData = {
    if(additionalSamples <= 0){
      this
    }
    else{
      val newCData = channelData.map{ d =>
        val newD = new Array[Float](samples + additionalSamples)
        System.arraycopy(d, 0, newD, 0, samples)
        newD
      }
      
      StreamData(samples + additionalSamples, newCData)
    }
  }
  
  /**
   * Combines all channels of the StreamData into a single channel
   * by averaging all the channels and returning a StreamData 
   * with just one channel
   */
  def toMonoStream:StreamData = {
    val channels = channelData.size
    if(channels == 1){
       this 
    }
    else{
      val newData = new Array[Float](samples)
      if(channels == 0){
        StreamData(samples, Array(newData))
      }
      else{
        channelData.foreach{cd =>
          var i = 0
          while(i < samples){
            newData(i) += cd(i)
            i += 1
          }
        }
        
        val mult = 1f / channels
        var i = 0
        while(i < samples){
          newData(i) *= mult
          i += 1
        }
        
        StreamData(samples, Array(newData))
      }
    }
  }

  def toStereoStream:StreamData = {
    val channels = channelData.size
    if(channels == 2){
       this 
    }
    else if (channels == 1){
      toMonoStream.toNChannelStream(2)
    }
    else{
      toNChannelStream(2)
    }
  }

  def toNChannelStream(n: Int):StreamData = {
    val thisChannels = this.channelData.size
    if(n == thisChannels){
      this
    }
    else{
      n match {
        case 1 => this.toMonoStream
        case _ => {
          val monoData = this.toMonoStream.channelData(0)
          val channelData = new Array[Array[Float]](n)
          for(i <- 0 until n){
            channelData(i) = monoData
          }
          StreamData(this.samples, channelData)
        }
      }
    }
  }
}

object StreamData{
  
  def Empty(samples:Int, channels:Int):StreamData = {
    val cData = 0 until channels map{ i =>
      new Array[Float](samples)
    }
    
    StreamData(samples, cData)
  }
}
