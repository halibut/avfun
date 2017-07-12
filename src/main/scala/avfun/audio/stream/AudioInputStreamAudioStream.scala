package avfun.audio.stream

import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioFormat.Encoding

class AudioInputStreamAudioStream(is:AudioInputStream) extends AudioStream {
  
  val af = is.getFormat
  val encoding = af.getEncoding
  val channels = af.getChannels;
  val sampleSizeBytes = af.getFrameSize / channels
  val bigEndian = af.isBigEndian()
  
  val samplesPerSecond:Int = af.getSampleRate.toInt
  
  val totalSamples = Some(is.getFrameLength.toInt)
  private var readSamples = 0

  def resetStream = {
    is.reset()
    readSamples = 0
  }
  
  def closeStream = {
    is.close()
  }
  
  def close: Unit = {
    closeStream
  }
  
  private val (_bDir, _bOffset) = if(!bigEndian){
    (-1,sampleSizeBytes)
  }
  else{
    (1,0)
  }
  
  val maxSignedVal = AudioInputStreamAudioStream maxSignedValForBytes(sampleSizeBytes-1)
  val divAmount = AudioInputStreamAudioStream divMaxSignedValForBytes(sampleSizeBytes-1)
  
  val decodeFunc:(Array[Byte],Int)=>Float = encoding match {
    case Encoding.PCM_SIGNED => decodeSampleToFloat_PCM_Signed
    case Encoding.PCM_UNSIGNED => decodeSampleToFloat_PCM_Unsigned
    case Encoding.PCM_FLOAT => throw new UnsupportedOperationException(s"Encoding type ${encoding} is not supported.")
    case _ => throw new UnsupportedOperationException(s"Encoding type ${encoding} is not supported.")
  }
  
  def read(samples:Int):Option[StreamData] = {
    val bufferSize = samples * af.getFrameSize
     
    val buffer = new Array[Byte](bufferSize)
    
    val read = is.read(buffer)
    
    val framesRead = read / af.getFrameSize
    
    readSamples += framesRead
    
    val chData = (0 until channels).map(i => new Array[Float](framesRead)).toIndexedSeq
    
    var curFrame = 0
    var byteInd = 0
    while(curFrame < framesRead){
      var curChannel = 0
      while(curChannel < channels){
        val channel = chData(curChannel)
        
        val sampleVal = decodeFunc(buffer,byteInd)
                
        channel(curFrame) = sampleVal
        
        byteInd += sampleSizeBytes
        curChannel += 1
      }
      curFrame += 1
    }
    
    if(framesRead == 0){
      None
    }
    else{
      Some(new StreamData(framesRead, chData, (if(totalSamples.getOrElse(0)!=0) StreamSourceInfo(totalSamples, Some(readSamples)) else StreamSourceInfo.InfinitStream)))
    }
    
  }
  
  def readOrEmptyData[T](samples:Int):StreamData = {
    read(samples).getOrElse{
      val chData = (0 until channels).map(i => new Array[Float](0)).toIndexedSeq
      new StreamData(0, chData, StreamSourceInfo.InfinitStream)
    }
  }
  
  private def decodeSampleToFloat_PCM_Unsigned(bytes:Array[Byte], startInd:Int): Float = {
    val lVal = if(_bDir > 1){
      AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(bytes, startInd, sampleSizeBytes, false)  
    }
    else{
      val arr = bytes.slice(startInd,startInd + _bOffset).reverse
      AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(arr, 0 ,sampleSizeBytes, false)
    }
    (lVal - maxSignedVal) * divAmount
  }
  
  private def decodeSampleToFloat_PCM_Signed(bytes:Array[Byte], startInd:Int): Float = {
    val lVal = if(_bDir > 1){
      AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(bytes, startInd, sampleSizeBytes, true)  
    }
    else{
      val arr = bytes.slice(startInd,startInd + _bOffset).reverse
      AudioInputStreamAudioStream.decode_PCM_Sample_ToLong(arr, 0 ,sampleSizeBytes, true)
    }
    (lVal) * divAmount
  }

}

object AudioInputStreamAudioStream{
  val MAX_INT_UNSIGNED:Long = Int.MaxValue + Int.MaxValue
  val MAX_INT_SIGNED:Long = Int.MaxValue
  
  def maxSampleValue(as:AudioInputStreamAudioStream): Long = {
    require(1 <= as.sampleSizeBytes && as.sampleSizeBytes <= 4, "Sample size must be between 1 and 4 bytes.")
    
    var max:Long = 1L << (as.sampleSizeBytes * 8)
    
    as.encoding match {
      case Encoding.PCM_SIGNED => max  
      case Encoding.PCM_UNSIGNED => max >> 1
      case _ => throw new UnsupportedOperationException(s"Encoding type ${as.encoding} is not supported.")
    }
  }
  
  val maxSignedValForBytes = Array[Long](
      0x000000000000007F,
      0x0000000000007FFF,
      0x00000000007FFFFF,
      0x000000007FFFFFFF
  )
  val maxUnsignedValForBytes = maxSignedValForBytes.map(signedMax => signedMax << 1 | 1 )
  
  val divMaxSignedValForBytes = maxSignedValForBytes.map(signedMax => 1.0f / signedMax)
  val divMaxUnsignedValForBytes = maxUnsignedValForBytes.map(unsignedMax => 1.0f / unsignedMax)
  
  
  def decode_PCM_Sample_ToLong(bytes:Array[Byte],startInd:Int,numBytes:Int,signed:Boolean):Long = {
    var ind = startInd
    val endInd = startInd + numBytes
    
    var longValue = if(signed) bytes(ind).toLong else (bytes(ind) & 0xFF).toLong 
    ind+=1
    
    while(ind < endInd){
      longValue = (longValue << 8) | (bytes(ind) & 0xFF)
      
      ind+=1
    }
    longValue
  }
  
}


