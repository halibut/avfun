package avfun.video.experiments

import avfun.video.BasicCanvasWindow
import java.awt.Graphics2D
import java.awt.Color

object Exp1 extends BasicCanvasWindow("Experiment 1", 500, 500) { 
  

  def paintCanvas(g: Graphics2D): Unit = {
    implicit val implGraphics = g 
    
    g.setColor(Color.WHITE)
    g.fillRect(0,0,sx,sy)
    
    g.setColor(Color.BLACK)
    // g.translate(200, 150)
    // g.drawOval(0, 0, 100, 200)
   g.translate(sx/2, sy/2)
   g.scale(1, -1)
    drawOval(0, 0, 200, 400)
    drawOval(0, 0, 49, 300)
    drawOval(0,100,20, 30)
  }
  
  def drawOval(x:Int, y:Int, width:Int, height:Int)(implicit g:Graphics2D):Unit = {
    g.translate(x-width/2, y-height/2)
    g.drawOval(0, 0, width, height)
    g.translate(width/2-x, height/2-y)
  } 
  
  
}