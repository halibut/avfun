package avfun.video.experiments

import java.awt.Color
import java.awt.Graphics2D
import java.awt.image.RescaleOp
import java.io.File
import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date

import scala.swing.event.MouseClicked
import scala.swing.event.WindowClosing

import avfun.audio.stream.AudioInputStreamAudioStream
import avfun.audio.FFT
import avfun.audio.HanningWindow
import avfun.nnviz.nn.FastNetwork
import avfun.nnviz.nn.NeuralNetwork
import avfun.pipeline.Pipeline
import avfun.util.FileUtil
import avfun.video.AnimatedCanvasWindow
import avfun.video.ffmpeg.CanvasRecorder
import javax.sound.sampled.AudioInputStream
import javax.sound.sampled.AudioSystem

object AudioToMovieRun extends AnimatedCanvasWindow("Sound Test", 200, 200, 20, true, false){

  this.stopAnim

  //File information
  val song = "electric hymn"
  val wavFile = new File(song+".wav")
  val date = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date())
  val networkFile = s"out/lots/${song}-${date}.network.txt"
  val videoFile = s"out/lots/${song}-${date}.mp4"
  
  //Delete output files if they exist already
  FileUtil.deleteIfExists(networkFile)
  FileUtil.deleteIfExists(videoFile)
  
  //Various tweakable parameters
  val fadeScale = 0.9f
  val intermediateNeurons = 100
  val fftSize = 256
  
  
  //Data arrays/buffers
  var stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile))
  val samplesPerFrame = (stream.af.getFrameRate.toInt / fps).toInt  
  val sampleBuffer = samplesPerFrame + fftSize
  val data = new Array[Float](sampleBuffer)
  val inputSize = fftSize / 2
  
  //Neural Network Information
  val net = {
    val inputs = 0 until (inputSize) map(i => i.toString)
    val outputs = Seq("x","y","r","g","b","symType","size")
    //val slowNet = Network.random(inputs,outputs,intermediateNeurons,(inputSize+intermediateNeurons)*20, (-.5f,.5f))
    val net = FastNetwork.random(inputs.size,outputs.size,intermediateNeurons,(inputSize+intermediateNeurons)*20, (-.5f,.5f))
    
    //Serialize this network in case we want to save it for later
    val fos = new FileOutputStream(networkFile)
    //Network.serialize(slowNet,fos)
    NeuralNetwork.serialize(net,fos)
    fos.close()
    
    //new FastNetwork(slowNet)
    net
  }
  
  //Screen capture
  val recorder = new CanvasRecorder(videoFile,this.ui,"D:/ffmpeg/bin/ffmpeg", fps.toInt)
  var record = true
  
  
  listenTo(ui.mouse.clicks)
  reactions += {
    case e:WindowClosing => record = false //recorder.stop()
    case e:MouseClicked => record = false //recorder.stop()
    case _ =>
  }
  
  
  //Calculated values
  val halfx = sx / 2
  val halfy = sy / 2
  val scales = Array(fadeScale, fadeScale, fadeScale);
  val offsets = Array(0f,0f,0f);
  val rop = new RescaleOp(scales, offsets, null);
  val sizeMult = math.max(1,(sx/200).toInt)
  
  
  //Classes for carrying information through the processing pipeline
  final class GInst(val r:Float,val g:Float, val b:Float, val x:Float, val y:Float, val symType:Float, val size:Float)
  final case class FrameInfo(val ind:Int, val recording:Boolean)
  
  //Signal processing pipeline
  val fft = new FFT(fftSize, new HanningWindow(fftSize)) ;
  val audioSignalPipeline = Pipeline(4){in:(Array[Float],Int) =>
    val (samples,offset) = in
    val amp0 = (1f + samples(offset)) * .5f
    
    val fftArr = fft.transform(samples, offset)
    
    val netInputs = new Array[Float](inputSize)
    var i = 0
    while(i < inputSize){
      val real = fftArr(i * 2)
      val im = fftArr(i * 2 + 1)
      val amp = Math.sqrt(real * real + im * im)
      netInputs(i) = amp.toFloat / inputSize.toFloat
      i+=1
    }
    netInputs
  }
  .sendTo{netInputs:Array[Float] => 
    net.calc(netInputs)
  }
  val audioSignalOutput = audioSignalPipeline.start(samplesPerFrame*2)
  
  
  val pipeline = Pipeline{frameInfo:FrameInfo => 
    //Copy last frame's data to the end of the array
    System.arraycopy(data,0,data,samplesPerFrame,fftSize)
    var totalRead = fftSize
    
    //Read data from the audio stream until it is we have enough samples for the frame    
    while(totalRead < (fftSize + samplesPerFrame)){
      val toRead = samplesPerFrame + fftSize - totalRead
      val readData = stream.readOrEmptyData(toRead).channelData(0)
      System.arraycopy(readData,0,data,totalRead,readData.length)
      totalRead += readData.length
      
      if(readData.length < toRead){
        stream.closeStream
        stream = new AudioInputStreamAudioStream(AudioSystem.getAudioInputStream(wavFile))
        record = false
      }
    }
    
    (data,frameInfo)
  }
  .sendTo{case ((dat:Array[Float], frameInfo:FrameInfo)) =>
    
    //Start the image processing pipeline. Each sample for the current frame
    var i = 0
    while(i < samplesPerFrame){
      audioSignalPipeline.put(dat, i)
      i+=1
    }
    
    val ginsts = new Array[GInst](samplesPerFrame)
    
    i = 0
    while(i < samplesPerFrame){
      val res = audioSignalOutput.get
      ginsts(i) = new GInst(res(0),res(1),res(2),res(3),res(4),res(5),res(6)) 
      i+=1
    }
    
    (ginsts, frameInfo)
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
      
      System.exit(0)
    }
    
    gr.drawImage(this.ui.getImage, rop, 0, 0);
    
    var i = 0
    while(i < instArr.length){
      val inst = instArr(i)
      
      val r = Math.min(1f,Math.max(0f,inst.r*.5f+.5f))
      val g = Math.min(1f,Math.max(0f,inst.g*.5f+.5f))
      val b = Math.min(1f,Math.max(0f,inst.b*.5f+.5f))
      val x = (inst.x*halfx).toInt
      val y = (inst.y*halfy).toInt
      val symType = (inst.symType*16+16).toInt
      val size = (inst.size*sizeMult+sizeMult).toInt
      
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
      
      i+=1
    }

  }
  
  this.startAnim
}