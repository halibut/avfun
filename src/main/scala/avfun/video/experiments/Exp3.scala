package avfun.video.experiments

import avfun.video.BasicCanvasWindow
import java.awt.Graphics2D

object Exp3 extends BasicCanvasWindow("Experiment 3", 500, 500) { 
  
  val thisWindow = this
  
  var oval1 = new Oval(0,0,200,400)
  var oval2 = new Oval(0, 0, 49, 300)
  var oval3 = new Oval(0,100,20, 30)
  
    var speed = 30f
    var speed2 = 10f
    var speed3 = 5f
//    var height = 300f
//    var width = 180f
    
    var totalTime = 0f
    
  def update(seconds:Float){
    totalTime += seconds
      
    
    oval3.x = 3 * Math.sin(speed * totalTime).toFloat;
//    if(oval3.x > 3 || oval3.x < -3)
//      speed = -speed 
    
    oval2.height = 300f + 20 * Math.sin(speed2 * totalTime).toFloat;

    oval1.width = 180f + 30 * Math.sin(speed3 * totalTime).toFloat;

    
//    if(width > 210 || width < 150)
//      speed3 = -speed3

  }  
  
  def paintCanvas(g: Graphics2D): Unit = {
    implicit val implGraphics = g 
    
    // g.translate(200, 150)
    // g.drawOval(0, 0, 100, 200)
   g.translate(sx/2, sy/2)
   g.scale(1, -1)
//    drawOval(0, 0, width.toInt, 400)
//    drawOval(0, 0, 49, height.toInt)
//    drawOval(x.toInt,100,20, 30)
   
   oval1.draw
   oval2.draw
   oval3.draw
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