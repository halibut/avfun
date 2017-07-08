package avfun.audio

class HanningWindow(dataLength:Int) extends WindowFunction{

  val cosMult = ((2f * Math.PI) / dataLength).toFloat
  
  private val windowValues = new Array[Float](dataLength)
  (0 until dataLength).foreach{ i =>
    windowValues(i) = 0.5f - math.cos(i * cosMult).toFloat 
  }
  
  override def window(data:Array[Float]){
    var i = 0
    while(i < dataLength){
      //data(i) = data(i) * 0.5f * (1.0f - Math.cos(i * cosMult).toFloat)
      data(i) = data(i) * windowValues(i)
      i+=1
    }
  }
}