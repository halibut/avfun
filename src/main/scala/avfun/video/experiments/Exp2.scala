package avfun.video.experiments

import avfun.video.BasicCanvasWindow
import java.awt.Graphics2D

object Exp2 extends BasicCanvasWindow("Experiment 2", 500, 500) { 
  
  val thisWindow = this
  
    var x = 0f
  
    var speed = 50f
    var speed2 = 30f
    var speed3 = 5f
    var height = 300f
    var width = 180f
    
    var totalTime = 0f
    
  def update(seconds:Float){
    totalTime += seconds
      
    x += seconds * speed
        
    if(x > 3 || x < -3)
      speed = -speed 
      
    height += seconds * speed2
    
    if(height > 350 || height < 300)
      speed2 = -speed2
      
    width += seconds *speed3
    
//    if(width > 210 || width < 150)
//      speed3 = -speed3
      
    width = 180f + 30f * Math.sin(speed3 * totalTime).toFloat;
  }  
  
  def paintCanvas(g: Graphics2D): Unit = {
    implicit val implGraphics = g 
    
    // g.translate(200, 150)
    // g.drawOval(0, 0, 100, 200)
   g.translate(sx/2, sy/2)
   g.scale(1, -1)
    drawOval(0, 0, width.toInt, 400)
    drawOval(0, 0, 49, height.toInt)
    drawOval(x.toInt,100,20, 30)
  }
  
  def drawOval(x:Int, y:Int, width:Int, height:Int)(implicit g:Graphics2D):Unit = {
    g.translate(x-width/2, y-height/2)
    g.drawOval(0, 0, width, height)
    g.translate(width/2-x, height/2-y)
  } 
  
  new Thread(){
    val fps = 60;
    
    override def run(){
      while(true){
        update(1.0f / fps)
        
        thisWindow.ui.repaint()
        Thread.sleep(1000/fps)
      }
    }
  }.start()
}