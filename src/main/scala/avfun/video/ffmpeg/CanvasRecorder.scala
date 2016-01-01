package avfun.video.ffmpeg

import scala.swing.Panel
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.awt.image.ColorModel
import java.awt.image.WritableRaster
import java.awt.image.BufferedImageOp
import java.awt.image.ImageObserver
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import scala.swing.event.UIElementResized
import scala.swing.event.Event
import scala.swing.Reactor
import avfun.video.CanvasDrawn
import java.lang.ProcessBuilder.Redirect
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import avfun.video.Canvas
import java.io.BufferedOutputStream
import javax.imageio.ImageIO
import java.io.File
import java.util.Arrays
import avfun.pipeline.Pipeline

class CanvasRecorder(fileName:String, cv:Canvas, ffmpegCmd:String = "ffmpeg", framerate:Int = 30) extends Reactor {
  
  this.listenTo(cv)
  
  val width = cv.width
  val height = cv.height
  
  val ffmpegProcBuilder = new ProcessBuilder()
    .command(ffmpegCmd,
        "-f", "rawvideo",
        "-pixel_format", "bgr32",
        //"-pixel_format", "bmp_pipe",
        "-video_size", s"${width}x${height}",
        "-framerate", s"${framerate}",
        "-i", "pipe:",
        //"-c:v", "mpeg4", "-vtag", "xvid", "-q:v", "8",
        "-c:v", "libx264", "-pix_fmt", "yuv420p", "-preset", "slow", "-crf", "22",
        fileName)
  
  ffmpegProcBuilder.directory(new File("./"))
  
  println(ffmpegProcBuilder.command())
  
  val ffmpegProc = ffmpegProcBuilder.start();
  val stdIn = ffmpegProc.getOutputStream;
  val stdInStream = new BufferedOutputStream(stdIn)
  var stopped = false
  
  val outputThread = new Thread(){
    this.setDaemon(false)
    override def run(){
      val buffer = new Array[Byte](2048)
      var read = ffmpegProc.getErrorStream.read(buffer)
      while(read > 0){
        System.out.write(buffer,0,read)
        read = ffmpegProc.getErrorStream.read(buffer)
      }
    }
  }.start()
  
  val pipeline = Pipeline(2)(sendImage)
  val outputQueue = pipeline.start()
  outputQueue.blocking = false
  
  listenTo(cv)
  
  reactions += {
    case e:CanvasDrawn => pipeline.put(e.canvas)
  }
  
  val canvasBuffer = new Array[Int](width * height * 3)
  val outputBuffer = new Array[Byte](width * height * 4)
  Arrays.fill(outputBuffer, 0.toByte)
  
  def sendImage(img:BufferedImage){
    if(!stopped){
      img.getData.getPixels(0, 0, width, height, canvasBuffer)
      
      var i = 0;
      var o = 0;
      var length = canvasBuffer.length
      while(i < length){
        outputBuffer(o) = canvasBuffer(i).toByte
        
        i += 1
        o += 1
        if(i % 3 == 0){
          o += 1
        }
      }
      
      stdInStream.write(outputBuffer)
    }
    else{
      stdInStream.close()
    }
    
  }
  
  def stop(){
    stopped = true
  }
  
  
}
