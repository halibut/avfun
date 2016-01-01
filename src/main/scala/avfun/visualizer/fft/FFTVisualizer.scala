package avfun.visualizer.fft

import avfun.viz.Canvas2D
import avfun.audio.FFT
import avfun.audio.HanningWindow
import avfun.visualizer.AudioFrameData
import avfun.visualizer.MusicVisualizer

class FFTVisualizer extends MusicVisualizer {
  val fftInputSamples = 256
  val fftOutputSamples = fftInputSamples / 2 
  
  override val requiredSamplesPerFrame: Int = fftInputSamples
  
  val fft = new FFT(fftInputSamples, new HanningWindow(fftInputSamples)) ;
  
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
    
    val lines = fftInputSamples / 2

    var oldX = 0
    var oldY = canvas.height
    var oldLogX = 0
    var oldLogY = canvas.height
    
    for(i <- 0 until lines){
      //val x = i * canvas.width / lines
      //val y = canvas.height - (leftDec(i) * canvas.height).toInt
      
//      canvas.setColor(.8f,0f,0f)
//      canvas.drawLine(oldX, oldY, x, y)
//      canvas.fillRect(x, y, 2, 2)
      
      val logx = (canvas.width * .25 * (4+math.log(i.toFloat / lines))).toInt
//      val logy = canvas.height - (canvas.height.toFloat *.125f * (4+math.log(leftDec(i))).toFloat).toInt
      val logy = canvas.height - (canvas.height * leftSpec(i)).toInt
      
      canvas.setColor(1f,.5f,.5f)
      canvas.drawLine(oldLogX, oldLogY, logx, logy)
      canvas.setColor(1f,.8f,.8f)
      canvas.fillRect(logx, logy, 3, canvas.height - logy)
      
      //oldX = x
      //oldY = y
      oldLogX = logx
      oldLogY = logy
    }
    
//    canvas.setColor(0f,0f,.5f)
//    for(i <- 0 until lines){
//      val x = i * canvas.width / lines
//      val y = canvas.height - (rightSamples(i) * canvas.height).toInt
//      canvas.fillRect(x, y, 2, 2)
//    }
    
    
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