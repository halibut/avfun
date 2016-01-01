package avfun.audio

import javax.sound.sampled.TargetDataLine
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.DataLine
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.Line
import javax.sound.sampled.SourceDataLine
import avfun.convert.DataStreamUtils
import scala.collection.mutable.ArrayBuffer

class TargetLineAudioOutputDevice(val bufferSamples:Int = 1470, threaded:Boolean = true) extends AudioOutputDevice {
  
  /**
   * Stereo. If underlying audio data is not stereo, it will be converted.
   */
  override val channels: Int = 2
  
  /**
   * 44.1 kHz (CD quality sampling rate)
   */
  override val samplesPerSecond:Int = 44100
  
  override val latencySamples: Int = bufferSamples
  
  private lazy val _runnable = {
    val runnable = new TargetLineAudioOutputDeviceRunnable(bufferSamples)
    if(threaded){
      val thread = new Thread(runnable)
      thread.setName("Audio Output Thread")
      thread.setDaemon(true)
      thread.start()
    }
    runnable
  }
  
  override def play(streamData: avfun.audio.stream.StreamData): Unit = {
    _runnable.addToBuffer(streamData.toStereoStream)
    
    if(!threaded){
      _runnable.playBuffereData
    }
  }
  
  override def stop:Unit = {
    _runnable.stop
    
    if(!threaded){
      _runnable.closeLine
    }
    
  }

}

class TargetLineAudioOutputDeviceRunnable(bufferSamples:Int) extends Runnable{
  import TargetLineAudioOutputDevice._
  
  /**
   * 16 bit sample range
   */
  val bytesPerSample = 2
  
  val channels = 2
  
  /**
   * Stereo, 44.1 kHz (CD quality), 16 bit, big-endian, signed PCM
   * This is a very common audio format that should work on almost all modern audio drivers
   */
  val audioFormat = new AudioFormat(44100, bytesPerSample * 8, 2, true, true);
  
  //Lazily create and open a line to start writing audio data to the speakers
  private lazy val _outputLine = {
    val line = AudioSystem.getSourceDataLine(audioFormat)
    val _bufferSizeBytes = bufferSamples * channels * bytesPerSample;
    line.open(audioFormat, _bufferSizeBytes);
    line.start();
    line
  }
  
  private val _channels = new Array[ArrayBuffer[Float]](2)
  _channels(0) = new ArrayBuffer[Float]()
  _channels(1) = new ArrayBuffer[Float]()
  
  private var _running = true
  
  def addToBuffer(streamData: avfun.audio.stream.StreamData){
    this.synchronized{
      val sData = streamData.toStereoStream.channelData
      _channels(0) ++= sData(0)
      _channels(1) ++= sData(1)
    }
  }
  
  def stop{
    _running = false;
  }
  
  def closeLine{
    _outputLine.close();
  }
  
  def playBuffereData{
    this.synchronized{
      
      val left = _channels(0)
      val right = _channels(1)
      val length = left.length
      
      val frameSize = 4
      val bytes = new Array[Byte](length * frameSize)
      
      var i = 0
      while(i < length){
        write2ByteSignedPCM_BigEndian(left(i), bytes, i * frameSize)
        write2ByteSignedPCM_BigEndian(right(i), bytes, i * frameSize + 2)
        
        i+=1
      }
        
      _outputLine.write(bytes, 0, bytes.length)
      
      _channels.foreach(ch => ch.clear() )
    }
  }
  
  override def run(){
    while(_running){
      playBuffereData
    }
  }
}

object TargetLineAudioOutputDevice{
  val maxSignedVal2Bytes = DataStreamUtils.maxSignedValForBytes(1) 
  
  def getCompatibleLines(aFormat:AudioFormat):Array[Line.Info] = {
    AudioSystem.getTargetLineInfo(new DataLine.Info(classOf[TargetDataLine], aFormat))
  }
  
  def write2ByteSignedPCM_BigEndian(fVal:Float, byteArray:Array[Byte], offset:Int){
    val longVal:Long = (fVal * maxSignedVal2Bytes).toLong 
    
    val b1Val:Long = (longVal & 0x0000FF00) >> 8
    val b2Val:Long = longVal & 0x000000FF
    
    byteArray(offset) = b1Val.asInstanceOf[Byte]
    byteArray(offset+1) = b2Val.asInstanceOf[Byte]
  }
  
}