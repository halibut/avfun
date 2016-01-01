package avfun.video.experiments

import avfun.video.BasicCanvasWindow
import java.awt.Graphics2D
import javax.sound.sampled.AudioInputStream
import java.io.FileInputStream
import java.io.File
import javax.sound.sampled.AudioSystem
import java.awt.Color
import avfun.video.AnimatedCanvasWindow
import scala.collection.mutable.ArrayBuffer
import org.jtransforms.fft.FloatFFT_1D
import avfun.audio.HanningWindow
import avfun.audio.FFT
import avfun.audio.stream.AudioInputStreamAudioStream

object AudioToImage extends AnimatedCanvasWindow("Sound Test", 1600, 500, 30) { 
  
  this.stopAnim
  
  val wavFile = new File("electric hymn.wav")
  var stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 

  val frameRateDiv = (30 / fps).toInt
  
  val fftSamples = 1024
  val audioFrameRate = stream.af.getFrameRate.toInt
  val displaySamples = Math.max(fftSamples, audioFrameRate) 
  
  val samplesPerFrame = (audioFrameRate / fps).toInt / frameRateDiv
  
  val scaleX = sx.toFloat / displaySamples.toFloat
  
  var data = new ArrayBuffer[Float](displaySamples)
  val fft = new FFT(fftSamples, new HanningWindow(fftSamples)) ;
  var fftData = new Array[Float](fftSamples)
  
  var realFps = 0f
  
  
  def update(ds:Float){
    realFps = 1f / ds
    
    
    data = data.takeRight(displaySamples-samplesPerFrame)
    
    while(data.size < displaySamples){
      val leftOver = displaySamples - data.size
      val readData = stream.readOrEmptyData(leftOver).channelData(0) 
      data ++= readData
      
      if(readData.length < leftOver){
        stream.closeStream
        stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 
      }
    }
    
    fftData = fft.transform(data.take(fftSamples).toArray)
  }
  
  
  def paintCanvas(g: Graphics2D): Unit = {
    implicit val implGraphics = g 
    
    g.setColor(Color.WHITE)
    g.fillRect(0,0,sx,sy)
    
    g.setColor(Color.BLACK)
    g.drawString(s"Fps: ${realFps}", 100, 100)
    
//    g.translate(0,sy / 2)
    
    g.setColor(Color.red)
    g.drawLine(0,0,sx,0)
    
    g.setColor(Color.black)
    var lastPoint = (0,0)
//    data.zipWithIndex.foreach{case (amp,ind) =>
//      val (x,y) = ((ind * scaleX).toInt, (amp * sy *.5).toInt)
//      g.drawLine(lastPoint._1, lastPoint._2, x,y)
//      lastPoint = (x,y)
//    }

    //g.translate(0,sy / 2)
    
    val yoffset = sy / 2
    
    val logBase = 1.5
    val maxLogX = Math.log(fftSamples / 2) / Math.log(logBase)
    val scaleLogX = sx / maxLogX
    
    lastPoint = (0,0)
    var i = 0
    while(i < fftSamples / 2){
      val real = fftData(i * 2)
      val im = fftData(i * 2 + 1)
      val mag = Math.sqrt(real*real + im*im) 
      val logX = Math.log(i+1) / Math.log(logBase)
      val (x,y) = ((logX * scaleLogX).toInt, (-mag).toInt)
      
      g.setColor(Color.green)
      g.drawLine(lastPoint._1, yoffset + lastPoint._2, x, yoffset + y)
      lastPoint = (x,y)
//      g.fillRect(, , 4, 4)
//      g.setColor(Color.green)
//      g.fillRect(i, (real * sy * .5).toInt, 4, 4)
//      g.setColor(Color.red)
//      g.fillRect(i, (im * sy * .5).toInt, 4, 4)
      i+=1
    }
  }
  
  def drawOval(x:Int, y:Int, width:Int, height:Int)(implicit g:Graphics2D):Unit = {
    g.translate(x-width/2, y-height/2)
    g.drawOval(0, 0, width, height)
    g.translate(width/2-x, height/2-y)
  } 
  
  this.startAnim
}