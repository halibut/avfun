package avfun.video

import scala.swing.Panel
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.awt.image.ColorModel
import java.awt.image.WritableRaster
import java.awt.image.BufferedImageOp
import java.awt.image.ImageObserver
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import scala.swing.event.UIElementResized
import scala.swing.event.Event
import avfun.viz.Canvas2D

abstract class Canvas(val width:Int,val height:Int) extends Panel {
  preferredSize = new Dimension(width,height) 
  
  def draw(g:Graphics2D):Unit;
  
  var syncWithWindow = true
  
  private var _img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  private var _imgGraphics = _img.createGraphics()
  
  private val imgOp = new AffineTransformOp(new AffineTransform(), null)
  
  def renderCanvas() = {
    val beforeTxForm = _imgGraphics.getTransform()
    draw(_imgGraphics)
    _imgGraphics.setTransform(beforeTxForm)
    
    
    publish(CanvasDrawn(_img))
    
    if(syncWithWindow){
      this.repaint() 
    }
    
  }
  
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    
    g.drawImage(_img, imgOp, 0, 0)
  }
  
  
  def getImage = _img
  
}

case class CanvasDrawn(canvas:BufferedImage) extends Event