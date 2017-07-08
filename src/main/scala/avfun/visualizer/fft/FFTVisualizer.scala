package avfun.visualizer.fft

import avfun.viz.Canvas2D
import avfun.audio.FFT
import avfun.audio.HanningWindow
import avfun.visualizer.AudioFrameData
import avfun.visualizer.MusicVisualizer
import scala.collection.mutable.ArrayBuffer

class FFTVisualizer(fftInputSamples:Int, displayBarsPerChannel:Int, barWidth:Int = 3) extends MusicVisualizer {
  val fftOutputSamples = fftInputSamples / 2 
  
  private val displayBarsLeft = new Array[Float](displayBarsPerChannel)
  private val displayBarsRight = new Array[Float](displayBarsPerChannel)
  
  val samplesPerDisplayBar = fftOutputSamples.toFloat / displayBarsPerChannel.toFloat
  
  override val requiredSamplesPerFrame: Int = fftInputSamples
  
  val fftWindow = new HanningWindow(fftInputSamples)
  val fft = new FFT(fftInputSamples, fftWindow) ;
  
  var timeSamples = Seq[(Long,Long,Long)]()
  
  def drawFrame(frameData: AudioFrameData, canvas: Canvas2D): Unit = {
    val t1 = System.currentTimeMillis()
    
    val freqPerBucket = frameData.samplesPerSecond.toFloat / fftOutputSamples
    
    //Cause the image to constantly fade to black
    canvas.imgMult(0.85f,0.95f,0.95f)
    
    val sData = frameData.audioData.toStereoStream
    
    val leftSamples = fft.transform(sData.channelData(0))
    //val leftSpec = FFT.getSpectrumForAudio(leftSamples, fft)
    FFT.getLogSpectrumForAudio(leftSamples, fft, displayBarsLeft)
    
    val rightSamples = fft.transform(sData.channelData(1))
    //val rightSpec = FFT.getSpectrumForAudio(rightSamples, fft)
    FFT.getLogSpectrumForAudio(rightSamples, fft, displayBarsRight)
    
    val t2 = System.currentTimeMillis()
    
    val lines = fftInputSamples / 2

    val halfWidth = canvas.width / 2
    
    val accumLeft = ArrayBuffer[Float]()
    val accumRight = ArrayBuffer[Float]()
    
    var curBar = 0
    
    for{
      //i <- 0 until lines
      i <- 0 until displayBarsPerChannel
    }{
      val leftBucketVal = displayBarsLeft(i)
      val rightBucketVal = displayBarsRight(i)
      
      val xOffset = (canvas.width * (i.toFloat / displayBarsPerChannel) / 2).toInt
      val xLeft = halfWidth - xOffset
      val xRight = halfWidth + xOffset
//      val logx = (canvas.width * .25 * (4+math.log(i.toFloat / lines))).toInt
//      val logy = canvas.height - (canvas.height.toFloat *.125f * (4+math.log(leftDec(i))).toFloat).toInt
      val yLeft = canvas.height - (canvas.height * leftBucketVal).toInt
      val yRight = canvas.height - (canvas.height * rightBucketVal).toInt
      
      canvas.setColor(1f,leftBucketVal,leftBucketVal)
      canvas.fillRect(xLeft, yLeft, barWidth, canvas.height - yLeft)
      canvas.setColor(1f,rightBucketVal,rightBucketVal)
      canvas.fillRect(xRight, yRight, barWidth, canvas.height - yRight)
      
//      val curBarLogVal = math.log10((curBar+1).toDouble / displayBarsPerChannel)  
//      val curBucketLogVal = math.log10((i+1).toDouble / lines.toDouble)
//      
//      if(curBucketLogVal <= curBarLogVal) {
//        accumLeft += leftSpec(i)
//        accumRight += rightSpec(i)
//      }
//      else {
//        curBar+=1
//        if(accumLeft.size > 0) {
//          val avgLeft = accumLeft.sum // / accumLeft.size.toFloat
//          val avgRight = accumRight.sum // / accumRight.size.toFloat
//          
//          val xOffset = (canvas.width * (i.toFloat / lines) / 2).toInt
//          val xLeft = halfWidth - xOffset
//          val xRight = halfWidth + xOffset
//    //      val logx = (canvas.width * .25 * (4+math.log(i.toFloat / lines))).toInt
//    //      val logy = canvas.height - (canvas.height.toFloat *.125f * (4+math.log(leftDec(i))).toFloat).toInt
//          val yLeft = canvas.height - (canvas.height * avgLeft).toInt
//          val yRight = canvas.height - (canvas.height * avgRight).toInt
//          
//          canvas.setColor(1f,leftSpec(i),leftSpec(i))
//          canvas.fillRect(xLeft, yLeft, barWidth, canvas.height - yLeft)
//          canvas.setColor(1f,rightSpec(i),rightSpec(i))
//          canvas.fillRect(xRight, yRight, barWidth, canvas.height - yRight)
//          
//          accumLeft.clear()
//          accumRight.clear()
//        }
//      }
      
    }
    
    canvas.setColor(.5f,.5f,.5f)
    for(i <- 0 until 10){
      val vHeight = i * canvas.height / 10
      canvas.fillRect(0, vHeight, canvas.width, 1)
    }
    
    val t3 = System.currentTimeMillis()
    
    timeSamples = timeSamples :+ (t2-t1, t3-t2, t3-t1)
    if(timeSamples.size > 60) {
      println(s"Avg FFT/Analysis Time: ${timeSamples.map(_._1).sum / 60.0} ms")
      println(s"Avg Drawing Time: ${timeSamples.map(_._2).sum / 60.0} ms")
      println(s"Avg Total Time: ${timeSamples.map(_._3).sum / 60.0} ms")
      timeSamples = Seq()
    }
  }

  def onDeregistered: Unit = {
    //Do nothing
  }

  def onRegistered: Unit = {
    //Do nothing
  }
  
  

}