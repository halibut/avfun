package avfun.visualizer.swing

import scala.swing.Panel
import avfun.visualizer.VideoFrameListener
import java.awt.Color
import java.awt.Dimension
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import avfun.visualizer.VideoFrameData
import avfun.viz.ColorBufferType
import avfun.viz.Canvas2D
import javax.swing.SwingUtilities

class VideoFrameListenerPanel(val cWidth:Int, val cHeight:Int) extends Panel with VideoFrameListener{

  background = Color.BLACK
  preferredSize = new Dimension(cWidth, cHeight)
  var img = new BufferedImage(cWidth, cHeight, BufferedImage.TYPE_3BYTE_BGR)
  
  def setImageData(bgrByteArray:Array[Byte]){
    img.getRaster.setDataElements(0,0,cWidth,cHeight,bgrByteArray)
  }
  
  override def paint(g:Graphics2D):Unit = {
    super.paint(g)
    val actDims = this.size
    val x = (actDims.getWidth - cWidth).toInt / 2
    val y = (actDims.getHeight - cHeight).toInt / 2
    g.drawImage(img, x, y, null)
  }
  
  override def onFrameData(frameData:VideoFrameData):Unit = {
    frameData.colorBufferType match {
      case ColorBufferType.BGRByteArray => setImageData(frameData.buffer)
      case ColorBufferType.RGBByteArray => setImageData(Canvas2D.swapRandB(frameData.buffer))
    }
    this.repaint()
  }
}