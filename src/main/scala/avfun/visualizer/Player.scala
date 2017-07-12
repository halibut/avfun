package avfun.visualizer

import avfun.audio.stream.StreamData
import avfun.audio.stream.OptionalAudioStream
import avfun.interval.Timer
import avfun.audio.AudioOutputDevice
import avfun.interval.ThreadBasedTimer
import scala.collection.mutable.ArrayBuffer
import avfun.audio.stream.BufferedAudioStream
import avfun.interval.Timer
import avfun.audio.stream.AudioInputStreamAudioStream
import javax.sound.sampled.AudioInputStream
import java.io.File
import javax.sound.sampled.AudioSystem
import java.util.Date
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import avfun.audio.stream.AudioStream
import avfun.audio.stream.StreamSourceInfo

abstract class Player extends AudioFrameProducer {
  import Player.logger
  
  val framerate:Int = 25
  val millisPerFrame = 1000 / framerate
  val samplesPerSecond = 44100
  
  val minSamplesPerFrame = samplesPerSecond / framerate 
  
  private val _as = new OptionalAudioStream(samplesPerSecond, 2)
  private var _bufferedAS:BufferedAudioStream = null
  
  protected def timer:Timer
  
  private lazy val _timer:Timer = {
    val t = this.timer
    t.interval = millisPerFrame
    t.setIntervalFunction(timerIntervalFunction)
    t
  }
    
  private def timerIntervalFunction():Unit = { 
    if(_bufferedAS == null){
      _bufferedAS = new BufferedAudioStream(_as, maxBufferedAudioSamples(minSamplesPerFrame))
      
      //Read some data into the buffer
      _bufferedAS.resetUnderlyingStream()
      _bufferedAS.getBufferedSamples(minSamplesPerFrame)
    }
    
    val t1 = new Date().getTime;
    
    val numSamples = _bufferedAS.bufferedSamples
    
    val frameSamples:Option[(IndexedSeq[Array[Float]], StreamSourceInfo)] = _bufferedAS.read(numSamples).map{data =>
      if(data.samples < numSamples){
        val channelData = (0 until _as.channels) map { ch =>
          val arr = new Array[Float](numSamples)
          System.arraycopy(data.channelData(ch), 0, arr, 0, data.samples)
          arr
        }
        (channelData, data.streamSourceInfo)
      }
      else{
        (data.channelData, data.streamSourceInfo)
      }
    }
    
    val frameData = frameSamples match {
      case Some((sd:IndexedSeq[Array[Float]], streamSourceInfo:StreamSourceInfo)) => {
        AudioFrameData(framerate, minSamplesPerFrame, samplesPerSecond, _as.channels, StreamData(numSamples, sd, streamSourceInfo), false, streamSourceInfo.getStreamPosition) 
      }
      case None =>{
        val data = (0 until _as.channels) map { ch => new Array[Float](numSamples) }
        AudioFrameData(framerate, minSamplesPerFrame, samplesPerSecond, _as.channels, StreamData(numSamples, data, StreamSourceInfo.InfinitStream), true, None)
      }
    }
    
    sendAudioFrameToListeners(frameData)
    
    val t2 = new Date().getTime();
    //logger.debug(s"Completed frame. Time: ${t2-t1} ms.  Audio samples read: ${frameData.audioData.samples}")
  }
  
  def play = {
    _timer.play
  }
  
  def pause = {
    _timer.pause
  }
  
  def isPaused:Boolean = {
    _timer.isPaused
  }
  
  def isPlaying:Boolean = {
    _timer.isStarted
  }
  
  def changeAudioSource(streamSource:AudioStream):Unit = {
    _as.setStream(streamSource)
   
    if(_bufferedAS != null) {
      //Read some data into the buffer
      _bufferedAS.resetUnderlyingStream()
      _bufferedAS.getBufferedSamples(minSamplesPerFrame)
    }
  }
}

object Player {
  val logger = LoggerFactory.getLogger(classOf[Player]);
}
