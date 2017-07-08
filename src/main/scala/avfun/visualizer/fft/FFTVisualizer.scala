package avfun.visualizer.fft

import avfun.viz.Canvas2D
import avfun.audio.FFT
import avfun.audio.HanningWindow
import avfun.visualizer.AudioFrameData
import avfun.visualizer.MusicVisualizer
import scala.collection.mutable.ArrayBuffer

class FFTVisualizer(fftInputSamples:Int, displayBarsPerChannel:Int, barWidth:Int = 3) extends MusicVisualizer {
  val fftOutputSamples = fftInputSamples / 2 
  
  val samplesPerDisplayBar = fftOutputSamples.toFloat / displayBarsPerChannel.toFloat
  
  override val requiredSamplesPerFrame: Int = fftInputSamples
  
  val fftWindow = new HanningWindow(fftInputSamples)
  val fft = new FFT(fftInputSamples, fftWindow) ;
  
  def drawFrame(frameData: AudioFrameData, canvas: Canvas2D): Unit = {
    //Cause the image to constantly fade to white
    //canvas.imgAdd(0.3f,0.3f,0.3f)
    canvas.imgMult(0.85f,0.95f,0.95f)
//    canvas.setColor(1f,1f,1f)
//    canvas.fillRect(0,0,canvas.width,canvas.height)
    
    
    val sData = frameData.audioData.toStereoStream
    
//     = new Array[Float](fftInputSamples)
//    System.arraycopy(sData.channelData(0), 0, leftSamples, 0, fftInputSamples)
    val leftSamples = fft.transform(sData.channelData(0))
    val leftSpec = FFT.getSpectrumForAudio(leftSamples, fft)
    
//    new Array[Float](fftInputSamples)
//    System.arraycopy(, 0, rightSamples, 0, fftInputSamples)
    val rightSamples = fft.transform(sData.channelData(1))
    val rightSpec = FFT.getSpectrumForAudio(rightSamples, fft)
    
    val lines = fftInputSamples / 2

    val halfWidth = canvas.width / 2
    
    val accumLeft = ArrayBuffer[Float]()
    val accumRight = ArrayBuffer[Float]()
    
    for{
      i <- 0 until lines
    }{
      accumLeft += leftSpec(i)
      accumRight += rightSpec(i)
      
      if(accumLeft.size >= samplesPerDisplayBar) {
        val avgLeft = accumLeft.sum / accumLeft.size.toFloat
        val avgRight = accumRight.sum / accumRight.size.toFloat
        
        val xOffset = (canvas.width * (i.toFloat / lines) / 2).toInt
        val xLeft = halfWidth - xOffset
        val xRight = halfWidth + xOffset
  //      val logx = (canvas.width * .25 * (4+math.log(i.toFloat / lines))).toInt
  //      val logy = canvas.height - (canvas.height.toFloat *.125f * (4+math.log(leftDec(i))).toFloat).toInt
        val yLeft = canvas.height - (canvas.height * avgLeft).toInt
        val yRight = canvas.height - (canvas.height * avgRight).toInt
        
        canvas.setColor(1f,leftSpec(i),leftSpec(i))
        canvas.fillRect(xLeft, yLeft, barWidth, canvas.height - yLeft)
        canvas.setColor(1f,rightSpec(i),rightSpec(i))
        canvas.fillRect(xRight, yRight, barWidth, canvas.height - yRight)
        
        accumLeft.clear()
        accumRight.clear()
      }
      
    }
    
    canvas.setColor(.5f,.5f,.5f)
    for(i <- 0 until 10){
      val vHeight = i * canvas.height / 10
      canvas.fillRect(0, vHeight, canvas.width, 1)
    }
  }

  def onDeregistered: Unit = {
    //Do nothing
  }

  def onRegistered: Unit = {
    //Do nothing
  }
  
  

}