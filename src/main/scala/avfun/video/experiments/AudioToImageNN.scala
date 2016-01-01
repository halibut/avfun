package avfun.video.experiments

import java.awt.Color
import java.awt.Graphics2D
import java.awt.image.RescaleOp
import java.io.File
import java.io.FileOutputStream

import scala.swing.event.MouseClicked
import scala.swing.event.WindowClosing

import avfun.audio.stream.AudioInputStreamAudioStream
import avfun.nnviz.nn.FastNetwork
import avfun.nnviz.nn.NeuralNetwork
import avfun.video.AnimatedCanvasWindow
import avfun.video.ffmpeg.CanvasRecorder
import javax.sound.sampled.AudioSystem


object AudioToImageNN extends AnimatedCanvasWindow("Sound Test", 1000, 500, 24, true, false) { 
  
  this.stopAnim
  
  val song = "electric hymn"
  
  val wavFile = new File(song+".wav")
  var stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 

  val samplesPerFrame = (stream.af.getFrameRate.toInt / fps).toInt
  val sampleHistory = 500
  val sampleBuffer = samplesPerFrame + sampleHistory
  
  val data = new Array[Float](sampleBuffer)
  
  def cleanupFile(name:String){
    val file = new File(name)
    if(file.exists()){
      file.delete();
    }
  }
  
  
  val inputs = 0 until sampleHistory map(i => i.toString)
  val outputs = Seq("x","y","r","g","b","symType","size")
  //val net = Network.random(inputs,outputs,100,1000)
  //val slowNet = Network.random(inputs,outputs,500,5000)
  val net = FastNetwork.random(inputs.size,outputs.size,500,5000)
  val networkFile = "out/"+song+"-network.txt"
  cleanupFile(networkFile)
  val fos = new FileOutputStream("out/"+song+"-network.txt")
  NeuralNetwork.serialize(net,fos)
  fos.close()
  
  //val net = new FastNetwork(slowNet)
  
  
  val videoFile = "out/"+song+".avi"
  cleanupFile(videoFile)
  val recorder = new CanvasRecorder(videoFile,this.ui,"D:/ffmpeg/bin/ffmpeg", fps.toInt)
  
  listenTo(ui.mouse.clicks)
  
  reactions += {
    case e:WindowClosing => recorder.stop()
    case e:MouseClicked => recorder.stop()
    case _ =>
  }
  
  this.startAnim
  
  val halfx = sx / 2
  val halfy = sy / 2
  
  def update(ds:Float){
    val readData = stream.readOrEmptyData(samplesPerFrame).channelData(0) 
    
    //Copy last frame's data to the end of the array
    System.arraycopy(data,0,data,samplesPerFrame,sampleHistory)
    //Get this frame's data from the input stream
    System.arraycopy(readData,0,data,samplesPerFrame-readData.length,readData.length)
    
    if(readData.length < samplesPerFrame){
      val leftOver = samplesPerFrame - readData.length
      stream.closeStream
      
      stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 
      
      val nReadData = stream.readOrEmptyData(leftOver).channelData(0)
      System.arraycopy(nReadData,0,data,0,nReadData.length)
      recorder.stop()
    }

  }
  
  val colorScale = 0.9f
  
  val scales = Array(colorScale, colorScale, colorScale);
  val offsets = Array(0f,0f,0f);
  val rop = new RescaleOp(scales, offsets, null);
  
  val smult = math.max(1,(sx/300).toInt)
  
  
  def paintCanvas(gr: Graphics2D): Unit = {
    implicit val implGraphics = gr 
   
    gr.drawImage(this.ui.getImage, rop, 0, 0);
  
    var i = samplesPerFrame-1
    while(i >= 0){
      
      val inputs = new Array[Float](sampleHistory)
      var j = 0
      while(j < sampleHistory){
        inputs(j) = data(i + sampleHistory - j)
        j+=1
      }
      
      val res = net.calc(inputs)
      
      val r = math.min(1f,math.max(0f,res(0)*.5f+.5f))
      val g = math.min(1f,math.max(0f,res(1)*.5f+.5f))
      val b = math.min(1f,math.max(0f,res(2)*.5f+.5f))
      val x = (res(3)*halfx).toInt
      val y = (res(4)*halfy).toInt
      val symType = (res(5)*8+8).toInt
      val size = (res(6)*smult+smult).toInt
      
      val size2 = 2 * size
      val xstart = x-size 
      val ystart = y-size
      
      gr.setColor(new Color(r,g,b))
//      gr.fillRect(halfx + xstart, halfy + ystart, size2, size2)
      if((symType & 0x0001) == 0x0001){
        gr.fillRect(halfx + xstart, halfy + ystart, size2, size2)
      }
      if((symType & 0x0002) == 0x0002){
        gr.fillRect(halfx + xstart, halfy - ystart, size2, size2)
      }
      if((symType & 0x004) == 0x0004){
        gr.fillRect(halfx - xstart, halfy + ystart, size2, size2)
      }
      if((symType & 0x008) == 0x0008){
        gr.fillRect(halfx - xstart, halfy - ystart, size2, size2)
      }
      
      i-=1
    }

  }
  
}