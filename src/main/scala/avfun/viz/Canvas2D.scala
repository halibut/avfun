package avfun.viz

trait Canvas2D {
  val width:Int
  val height:Int
  
  def colorBufferType:ColorBufferType
  
  def setColor(r:Int, g:Int, b:Int)
  def setColor(r:Float, g:Float, b:Float)
  def fillRect(x:Int, y:Int, w:Int, h:Int)
  def drawLine(x1:Int, y1:Int, x2:Int, y2:Int)
  
  def imgAdd(arg: Float, arg1: Float, arg2: Float)
  def imgMult(r:Float, g:Float, b:Float)
  
  def getByteBuffer:Array[Byte]

  
}

object Canvas2D{
  def swapRandB(in:Array[Byte]):Array[Byte] = {
    val outBuffer = new Array[Byte](in.length)
    
    var i = 0
    val l = in.length
    while(i < l){
      outBuffer(i * 3) = in(i*3 + 2)
      outBuffer(i * 3 + 1) = in(i*3 + 1)
      outBuffer(i * 3 + 2) = in(i*3)
      
      i+=1
    }
    
    outBuffer
  }
}

sealed trait ColorBufferType
object ColorBufferType{
  
  case object RGBByteArray extends ColorBufferType
  case object BGRByteArray extends ColorBufferType
  
}