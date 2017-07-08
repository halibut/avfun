package avfun.audio

import org.jtransforms.fft.FloatFFT_1D

class FFT(val sampleSize:Int, val windowFunction:WindowFunction) {

  private val _fft = new FloatFFT_1D(sampleSize)
  
  private val _resultArr = new Array[Float](sampleSize * 2)
  
  def transform(samples:Array[Float]):Array[Float] = {
    transform(samples, 0)
  }
  
  def transform(samples:Array[Float], offset:Int):Array[Float] = {
    var i = 0
    while(i < sampleSize){
      _resultArr(i) = samples(i + offset)
      _resultArr(sampleSize + i) = 0f
      i+=1
    }
    
    windowAndTransform
  }
  
  private def windowAndTransform():Array[Float] = {
    windowFunction.window(_resultArr)
    
    _fft.realForward(_resultArr)
    
    _resultArr.clone()
  }
  
}

object FFT{
  
  def getMagnitudes(freqDomainData:Array[Float], fft:FFT):Array[Float] = {
    val outputSamples = fft.sampleSize / 2
    val outputSamplesDiv = 1f / outputSamples.toFloat
    val output = new Array[Float](outputSamples)
    
    var i = 0
    while(i < outputSamples){
      val real = freqDomainData(i * 2)
      val im = freqDomainData(i * 2 + 1)
      val amp = Math.sqrt(real * real + im * im)
      output(i) = amp.toFloat
      i+=1
    }
    
    output
  }
  
  /**
   * This method will take an array in the frequency domain and convert it into a 
   * array of scaled spectrum values, where each value in the spectrum corresponds
   * to a frequency bucket, but is scaled (logarithmically) to output values between
   * 0 and 1.
   * 
   * The resulting array is useful for visualizations or as input into other frequency
   * analysis tools.
   */
  def getSpectrumForAudio(freqDomainData:Array[Float], fft:FFT, minDecibel:Int = -60, maxDecibel:Int = -8):Array[Float] = {
    val outputSamples = fft.sampleSize / 2
    val outputSamplesDiv = 1f / fft.sampleSize.toFloat
    val output = new Array[Float](outputSamples)
    
    val dbRange = -minDecibel
    
    var i = 0
    while(i < outputSamples){
      val real = freqDomainData(i * 2)
      val im = freqDomainData(i * 2 + 1)
      val amp = real * real + im * im
      val power = 2f * math.sqrt(amp).toFloat * outputSamplesDiv
      
      val scaledDb = 1f + ((20f * math.log10(power).toFloat - maxDecibel) / dbRange)
      
      output(i) = math.min(1f, math.max(0f, scaledDb))
      
//      val power = 10 * math.log10(1 + amp)
//      val logScaled = .125f * (4+math.log(power)).toFloat 
//      output(i) = math.min(1f, math.max(0f, logScaled))
      i+=1
    }
    
    output
  }
  
  /**
   * This method will take an array in the frequency domain and convert it into a 
   * array of scaled spectrum values, where each value in the spectrum corresponds
   * to a frequency bucket, but is scaled (logarithmically) to output values between
   * 0 and 1.
   * 
   * The resulting array is useful for visualizations or as input into other frequency
   * analysis tools.
   */
  def getLogSpectrumForAudio(freqDomainData:Array[Float], fft:FFT, outputBins:Array[Float], minDecibel:Int = -60, maxDecibel:Int = -1):Unit = {
    val outputSamples = fft.sampleSize / 2
    val outputSamplesDiv = 1f / fft.sampleSize.toFloat
    
    val dbRange = -minDecibel
    
    var bin = 0
    outputBins(0) = 0f
    
    var i = 0
    while(i < outputSamples){
      val real = freqDomainData(i * 2)
      val im = freqDomainData(i * 2 + 1)
      val amp = real * real + im * im
      val power = 2f * math.sqrt(amp).toFloat * outputSamplesDiv
      
      val curBinLogVal = math.log10((bin+1).toDouble / outputBins.length)  
      val curBucketLogVal = math.log10((i+1).toDouble / outputSamples)
      
      if(curBinLogVal >= curBucketLogVal) {
        outputBins(bin) += power
      }
      else {
        outputBins(bin) = 1f + ((20f * math.log10(outputBins(bin)).toFloat - maxDecibel) / dbRange)
        outputBins(bin) = math.min(1f, math.max(0f, outputBins(bin)))
        
        if(bin < outputBins.length-1) {
          bin+=1
          outputBins(bin) = power
        } 
      }
      i+=1
    }
    
    if(bin == outputBins.length-1) {
      outputBins(bin) = 1f + ((20f * math.log10(outputBins(bin)).toFloat - maxDecibel) / dbRange)
      outputBins(bin) = math.min(1f, math.max(0f, outputBins(bin)))
    }
  }
}