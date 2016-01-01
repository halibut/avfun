package avfun.visualizer.symmetric

import avfun.viz.Canvas2D
import avfun.visualizer.AudioFrameData
import avfun.visualizer.MusicVisualizer
import avfun.nnviz.nnga.FastNetworkOrganismDefinition
import avfun.nnviz.nnga.FastNetworkOrganismDefinition._
import avfun.audio.FFT
import avfun.audio.HanningWindow
import avfun.nnviz.ga.Organism
import avfun.nnviz.nn.FastNetwork
import java.util.Date
import java.util.Arrays
import org.slf4j.LoggerFactory

class SymmetricVisualizer(val org:Organism[SymmetricVisualizerNetworkDef.type]) extends MusicVisualizer {
  import SymmetricVisualizer._
  
  override val requiredSamplesPerFrame: Int = math.max(numInputs, fftInputSamples)
  
  val net:FastNetwork = org.organismDefintion.expressPhenotype(org)(SymmetricVisualizerNetworkDef.networkConv)
  
  val fft = new FFT(fftInputSamples, new HanningWindow(fftInputSamples)) ;
  
  private var _globalRot = 0f
  
  def drawFrame(frameData: AudioFrameData, canvas: Canvas2D): Unit = {
    val t1 = new Date().getTime
    
    //Cause the image to constantly fade to black
    canvas.imgMult(0.95f,0.95f,0.95f)
    
    val t2mult = new Date().getTime
    
    
    //Get audio data and do a FFT on it
    val sampleData = frameData.audioData.toMonoStream.channelData(0)
    val fftData = fft.transform(sampleData)
    val spectrumData = FFT.getSpectrumForAudio(fftData, fft)
    val inputData = new Array[Float](numInputs)
    System.arraycopy(spectrumData, 0, inputData, 0, fftOutputSamples)
    
    val t3fft = new Date().getTime
    
    //For each set of audio samples run the neural network
    val outputs = new Array[Array[Float]]((sampleData.length-samplesPerDot) / samplesPerDot)
    var i = 0
    while(i < outputs.length){
      var j = 0
      while(j < samplesPerDot){
        inputData(fftOutputSamples+j) = sampleData(i*samplesPerDot+j)
        j+=1
      }
              
      outputs(i) = net.calc(inputData)
      i+=1
    }

    val t4netCalc = new Date().getTime
    
    //For each set output, draw a dot (or set of radially symmetrical dots)
    i = 0
    while(i < outputs.length-1){
      var j = 0
      while(j < samplesPerDot){
        val out = linterp(outputs(i), outputs(i+1), j.toFloat / samplesPerDot.toFloat)
        drawDot(canvas, out)
        j+=1
      }
      i+=1
    }
    
    val t5draw = new Date().getTime
    
    logger.debug(s"Total frame time: ${t5draw-t1}.  Img Mult: ${t2mult-t1}.  FFT: ${t3fft-t2mult}.  Net Calc: ${t4netCalc-t3fft}.  Calc/Dot: ${(t4netCalc-t3fft)/samplesPerDot}.  Draw: ${t5draw-t4netCalc}.")
  }
  
  private def linterp(arr1:Array[Float], arr2:Array[Float], x:Float):Array[Float] = {
    val values = new Array[Float](arr1.length)
    var i = 0
    while (i < arr1.length){
      values(i) = arr1(i) + (arr2(i)-arr1(i)) * x
      i+=1
    }
    values
  }

  private def drawDot(canvas:Canvas2D, arr: Array[Float]) = {
    _globalRot += 0.01f * (arr(7) * invTwoPi).toFloat / fftInputSamples
    
    //radius = between -1 and 1
    val radius = (1f - math.abs(arr(0)))
    val xMult = radius * canvas.width / 2f
    val yMult = radius * canvas.height / 2f
    
    //angle (in radians) from -2 Pi to 2 Pi
    val thetaRads = _globalRot + (arr(1)+1) * SymmetricVisualizer.twoPi
    
    //RGB color elements
    val r = (arr(2)+1) * .5f
    val g = (arr(3)+1) * .5f
    val b = (arr(4)+1) * .5f
    
    //Number of times to draw this dot between 1 and ${SymmetricVisualizer.maxSymmetry}
    val symTimes = math.ceil((arr(5)+1) * halfSymmetry).toInt
    //angle (in radians) offset between dots
    val symRads = 1 / (symTimes * SymmetricVisualizer.twoPi)
    
    //Size of dot between 1 and 3
    val size = math.floor((arr(6)+1)*3).toInt
    val xSize = size
    val ySize = size
    val xOffset = xSize / 2
    val yOffset = ySize / 2
    
    //Set the color to draw
    canvas.setColor(r, g, b)
    
    //Draw each radially symmetrical dot
    var i = 0
    while(i < symTimes){
      val angle = thetaRads + i * symRads
      val x =  canvas.width / 2 + (xMult * math.cos(angle)).toInt
      val y =  canvas.height / 2 + (yMult * math.sin(angle)).toInt
      
      canvas.fillRect(x - xOffset, y - yOffset, xSize, ySize)
      
      i+=1
    }
  }

  def onDeregistered: Unit = {
    //Do nothing
  }

  def onRegistered: Unit = {
    //Do nothing
  }
  
}

object SymmetricVisualizer{
  val logger = LoggerFactory.getLogger(classOf[SymmetricVisualizer])
  
  val twoPi = math.Pi * 2
  val invTwoPi = 1 / (twoPi)
  
  val maxSymmetry = 8
  val halfSymmetry = maxSymmetry *.5f
  
  val fftInputSamples = 128
  val fftOutputSamples = fftInputSamples / 2 
  
  val samplesPerDot = 20
  val numInputs = fftOutputSamples + samplesPerDot
  val outputNames = Seq("radius","theta","r","g","b","symType","size","rotAmount")
  
  val numHiddenNeurons = 500
  
  val connectionWeightRange = 0.425f
  val numConnections = (numInputs + outputNames.size + numHiddenNeurons) * 20
}

object SymmetricVisualizerNetworkDef extends FastNetworkOrganismDefinition(
      "Symmetrical Music Visualizer Neural Network",
      SymmetricVisualizer.numInputs,
      SymmetricVisualizer.outputNames.size,
      SymmetricVisualizer.numHiddenNeurons,
      SymmetricVisualizer.numConnections){
  
  implicit val networkConv = implicitlyGetFastNetworkConverter(this)
  
  override def minFloatVal:Float = -SymmetricVisualizer.connectionWeightRange
  override def maxFloatVal:Float = SymmetricVisualizer.connectionWeightRange
}