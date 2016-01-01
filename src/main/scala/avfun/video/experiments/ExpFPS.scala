package avfun.video.experiments

import java.awt.Graphics2D
import avfun.video.AnimatedCanvasWindow

object ExpFPS extends AnimatedCanvasWindow("FPS counter", 500, 500, 60) { 
  
  
  var realFps = 0f
  
  this.startAnim
    
  def update(seconds:Float){
    realFps = 1f / seconds
  }  
  
  def paintCanvas(g: Graphics2D): Unit = {
    implicit val implGraphics = g 
    
    g.clearRect(0, 0, this.sx, this.sy)
    g.drawString(s"Fps: ${realFps}", 100, 100)
  }
  
}