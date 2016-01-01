package avfun.audio

class HanningWindow(dataLength:Int) extends WindowFunction{

  val cosMult = ((2f * Math.PI) / (dataLength - 1)).toFloat
  
  override def window(data:Array[Float]){
    var i = 0
    while(i < dataLength){
      data(i) = data(i) * 0.5f * (1.0f - Math.cos(i * cosMult).toFloat) 
      i+=1
    }
  }
}