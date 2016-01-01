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
import avfun.pipeline.Pipeline
import avfun.video.AnimatedCanvasWindow
import avfun.video.ffmpeg.CanvasRecorder
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem


object AudioToImageNNPixelOutputs extends AnimatedCanvasWindow("Sound Test", 1200, 800, 8, true, true) { 
  
  this.stopAnim
  
  val song = "electric hymn"
//  val song = "test2"
  
  val wavFile = new File(song+".wav")
  var stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 

  val samplesPerFrame = (stream.af.getFrameRate.toInt / fps).toInt
  val intermediateNeurons = 5000
  val sampleBuffer = samplesPerFrame
  
  val data = new Array[Float](sampleBuffer)
  
  def cleanupFile(name:String){
    val file = new File(name)
    if(file.exists()){
      file.delete();
    }
  }
  
  val samps = 100 // samplesPerFrame
  val xpixels = 50
  val ypixels = 50
  
  val inputs = (0 until samps map(i => i.toString))
  val outputs = 0 until (xpixels*ypixels*3) map (i => i.toString)
  //val net = Network.random(inputs,outputs,100,1000)
  //val slowNet = Network.random(inputs,outputs,intermediateNeurons,(inputs.size + outputs.size + intermediateNeurons)*40, (-.5f,.5f))
  val net = FastNetwork.random(inputs.size,outputs.size,intermediateNeurons,(inputs.size + outputs.size + intermediateNeurons)*40, (-.5f,.5f))
  val networkFile = "out/"+song+"-network.txt"
  cleanupFile(networkFile)
  val fos = new FileOutputStream("out/"+song+"-network.txt")
  //Network.serialize(slowNet,fos)
  NeuralNetwork.serialize(net,fos)
  fos.close()
  
  //val net = new FastNetwork(slowNet)
  
  
  val videoFile = "out/"+song+".avi"
  cleanupFile(videoFile)
  val recorder = new CanvasRecorder(videoFile,this.ui,"D:/ffmpeg/bin/ffmpeg", fps.toInt)
  
  listenTo(ui.mouse.clicks)
  
  reactions += {
    case e:WindowClosing => record = false //recorder.stop()
    case e:MouseClicked => record = false //recorder.stop()
    case _ =>
  }
  
  val halfx = sx / 2
  val halfy = sy / 2
  
  val colorScale = 0.75f
  val scales = Array(colorScale, colorScale, colorScale);
  val offsets = Array(0f,0f,0f);
  val rop = new RescaleOp(scales, offsets, null);
  
  val smult = math.max(1,(sx/200).toInt)
  
  final class GInst(val r:Float,val g:Float, val b:Float)
  
  final case class FrameInfo(val ind:Int, val recording:Boolean)
  
  var record = true
  
  val pipeline = Pipeline{frameInfo:FrameInfo => 
    val readData = stream.readOrEmptyData(samplesPerFrame).channelData(0) 
    
    //Get this frame's data from the input stream
    System.arraycopy(readData,0,data,0,readData.length)
    
    if(readData.length < samplesPerFrame){
      val leftOver = samplesPerFrame - readData.length
      stream.closeStream
      
      stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile)) 
      
      val nReadData = stream.readOrEmptyData(leftOver).channelData(0)
      System.arraycopy(nReadData,0,data,0,nReadData.length)
      record = false
    }
    (data,frameInfo)
  }
  .sendTo{case ((dat:Array[Float], frameInfo:FrameInfo)) =>
    val res = net.calc(dat.takeRight(samps))
    //val res = net.calc(Array(0.1f))
    (res, frameInfo)
  }
  
  val outputQueue = pipeline.start()
  
  new Thread{
    override def run(){
      var i = 0
      while(true){
        pipeline.put(FrameInfo(i, record))
        i+=1
      }
    }
  }.start()
  
  
  var frameNo = 0
  def update(ds:Float){
    frameNo += 1
    
    if(frameNo % fps.toInt == 0){
      println(frameNo + " - " + pipeline.queueDepths)
    }
  }
  
  
  def paintCanvas(gr: Graphics2D): Unit = {
    
//    println(outputQueue != null)
    
    val (instArr,frameInfo) = outputQueue.get
    if(!frameInfo.recording){
      recorder.stop()
    }
    
    //gr.drawImage(this.ui.getImage, rop, 0, 0);
    
    val dx = sx / xpixels
    val dy = sy / ypixels
    
    var xInd = 0
    while(xInd < xpixels){
      var yInd = 0
      while(yInd < ypixels){
        val r = Math.min(1.0f, Math.max(0.0f, 0.5f + .5f * instArr((xInd * ypixels + yInd) * 3)))
        val g = Math.min(1.0f, Math.max(0.0f, 0.5f + .5f * instArr((xInd * ypixels + yInd) * 3 + 1)))
        val b = Math.min(1.0f, Math.max(0.0f, 0.5f + .5f * instArr((xInd * ypixels + yInd) * 3 + 2)))
        
        gr.setColor(new Color(r,g,b))
        gr.fillRect(xInd*dx, yInd*dy, dx, dy)
        
        yInd+=1
      }
      xInd += 1
    }

  }
  
  this.startAnim
  
}