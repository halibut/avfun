package avfun.viz

import java.util.Arrays

class SimpleSoftwareCanvas2D(override val width:Int, override val height:Int) extends Canvas2D {
  
  val colorBufferType = ColorBufferType.RGBByteArray
  
  //Stores R,G,B values as byte array
  private val _buffer = new Array[Byte](width * height * 3)
  private val _color = Array[Byte](0, 0, 0)
  
  private def clampColor(value:Int):Byte = {
    math.max(0, math.min(value, 255)).toByte
  }
  
  private def clampColor(value:Float):Byte = {
    math.max(0, math.min((value * 256f).toInt, 255)).toByte
  }
  
  def setColor(r: Float, g: Float, b: Float): Unit = {
    _color(0) = clampColor(r)
    _color(1) = clampColor(g)
    _color(2) = clampColor(b)
  }
  
  def setColor(r: Int, g: Int, b: Int): Unit = {
    _color(0) = clampColor(r)
    _color(1) = clampColor(g)
    _color(2) = clampColor(b)
  }

  def getByteBuffer: Array[Byte] = {
    Arrays.copyOf(_buffer, _buffer.length)
  }

  def fillRect(x: Int, y: Int, w: Int, h: Int): Unit = {
    if(w < 0){
      fillRect(x + w, y, -w, h)
    }
    else if(h < 0){
      fillRect(x, y + h, w, -h)
    }
    else{
      val startX = math.max(0, math.min(x, width))
      val startY = math.max(0, math.min(y, height))
      val endX = math.max(0, math.min(x+w, width))
      val endY = math.max(0, math.min(y+h, height))
      
      if(startX < width && startY < height && endX >= 0 && endY >= 0){
        var i = startX
        while(i < endX){
          val rowOffset = i * height
          var j = startY
          while(j < endY){
            System.arraycopy(_color, 0, _buffer, (rowOffset + j) * 3, 3)
            j += 1
          }
          i += 1
        }
        
      }
    }
  }

  def imgMult(r: Float, g: Float, b: Float): Unit = {
    val colors = Array(clampColor(r), clampColor(g), clampColor(b))
    
    var i = 0
    while(i < width*height){
      var k = 0
      while(k < 3){
        val pixInd = i*3 + k 
        val colVal = _buffer(pixInd)
        _buffer(pixInd) = clampColor(colVal * colors(k))
        k += 1
      }
      i += 1
    }
  }
  def imgAdd(arg: Float, arg1: Float, arg2: Float): Unit = ???
  
  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Unit = ???



}

