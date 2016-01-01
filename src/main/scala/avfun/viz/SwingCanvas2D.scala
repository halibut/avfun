package avfun.viz

import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import java.awt.image.RescaleOp
import java.awt.image.DataBufferByte

class SwingCanvas2D(override val width:Int, override val height:Int) extends Canvas2D {
  
  val colorBufferType = ColorBufferType.BGRByteArray
  
  private var _img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
  private var _imgGraphics = _img.createGraphics()

  def imgMult(r: Float, g: Float, b: Float): Unit = {
    val scales = Array(b, g, r);
    val offsets = Array(0f,0f,0f);
    val rop = new RescaleOp(scales, offsets, null);
    
    _imgGraphics.drawImage(_img, rop, 0, 0);
  }
  
  def imgAdd(r: Float, g: Float, b: Float): Unit = {
    val scales = Array(1f, 1f, 1f);
    val offsets = Array(b, g, r);
    val rop = new RescaleOp(scales, offsets, null);
    
    _imgGraphics.drawImage(_img, rop, 0, 0);
  }

  def fillRect(x: Int, y: Int, w: Int, h: Int): Unit = {
    _imgGraphics.fillRect(x, y, w, h)
  }

  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Unit = {
	  _imgGraphics.drawLine(x1, y1, x2, y2)
  }

  def getByteBuffer: Array[Byte] = {
    val pixels = (_img.getRaster().getDataBuffer()).asInstanceOf[DataBufferByte].getData()
    pixels
  }

  def setColor(r: Float, g: Float, b: Float): Unit = {
    _imgGraphics.setColor(new Color(b, g, r))
  }

  def setColor(r: Int, g: Int, b: Int): Unit = {
    _imgGraphics.setColor(new Color(b, g, r))
  }
  
  def drawDirect(drawFunc:(Graphics2D)=>Unit):Unit = {
    drawFunc(_imgGraphics)
  }

  
}