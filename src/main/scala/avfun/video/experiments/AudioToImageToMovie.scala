package avfun.video.experiments

import java.awt.Graphics2D
import avfun.audio.stream.AudioInputStreamAudioStream
import java.io.File
import javax.sound.sampled.AudioSystem
import java.awt.Color
import avfun.video.AnimatedCanvasWindow
import scala.collection.mutable.ArrayBuffer
import avfun.video.ffmpeg.CanvasRecorder
import scala.swing.event.WindowClosing

object AudioToImageToMovie extends AnimatedCanvasWindow("Sound Test", 200, 200, 15, true, false) { 
  
  this.stopAnim
  
  val wavFile = new File("test3.wav")
  var stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 

  val displaySamples = stream.af.getFrameRate.toInt / 2
  
  val samplesPerFrame = (stream.af.getFrameRate.toInt / fps).toInt
  
  val scaleX = sx.toFloat / displaySamples.toFloat
  
  var data = new ArrayBuffer[Float](displaySamples)
  
  var realFps = 0f
  
  //val fileName = "test.avi"
  val fileName = "test.gif"
  
  val file = new File(fileName)
  if(file.exists()){
    file.delete();
  }
  
  val recorder = new CanvasRecorder(fileName,this.ui,"D:/ffmpeg/bin/ffmpeg")
  
  reactions += {
    case e:WindowClosing => recorder.stop()
    case _ =>
  }
  
  
  this.startAnim
  
  def update(ds:Float){
    realFps = 1f / ds
    
    val readData = stream.readOrEmptyData(samplesPerFrame).channelData(0) 
    data ++= readData
      
    if(readData.length < samplesPerFrame){
      val leftOver = samplesPerFrame - readData.length
      stream.closeStream
      
      stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 
      
      
      data ++= stream.readOrEmptyData(leftOver).channelData(0)
    }
    
    data = data.takeRight(displaySamples)
  }
  
  
  def paintCanvas(g: Graphics2D): Unit = {
    implicit val implGraphics = g 
    
    g.setColor(Color.WHITE)
    g.fillRect(0,0,sx,sy)
    
    g.setColor(new Color(4,144,244))
    g.fillRect(0,0,1,1)
    
    g.translate(0,sy / 2)
    
    g.setColor(Color.GREEN)
    var lastPoint = (0,0)
    data.zipWithIndex.foreach{case (amp,ind) =>
      val (x,y) = ((ind * scaleX).toInt, (amp * sy *.5).toInt)
      g.drawLine(lastPoint._1, lastPoint._2, x,y)
      lastPoint = (x,y)
    }
    
    g.setColor(Color.BLUE)
    g.drawString(s"Fps: ${realFps}", 10, -sy / 2 + 10 )
    
    g.setColor(Color.red)
    g.drawLine(0,0,sx,0)
  }
  
  def drawOval(x:Int, y:Int, width:Int, height:Int)(implicit g:Graphics2D):Unit = {
    g.translate(x-width/2, y-height/2)
    g.drawOval(0, 0, width, height)
    g.translate(width/2-x, height/2-y)
  } 
  
  
}