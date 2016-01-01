package avfun.video

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point, geom}
import scala.swing.SimpleSwingApplication
import java.awt.Dimension

abstract class AnimatedCanvasWindow(winTitle:String,val sx:Int=500,val sy:Int=500, 
    val fps:Float=30f, val constantFrameLength:Boolean=false, val realTime:Boolean = true) extends SimpleSwingApplication {
  
  
  def update(dSeconds:Float)
  def paintCanvas(g: Graphics2D):Unit
  
  private val fpsMillis = 1000 / fps
  private val _thisWindow = this
  private var _running = false
  
//  if(!realTime){
//    this.ui.syncWithWindow = false
//  }
  
  private val _animThread = new Thread(){
    private var time = 0L
    private var dTime = 0f
    override def run(){
      time = System.nanoTime()
      dTime = 0f
      
      while(true){
        val prevTime = time
        time = System.nanoTime()
        
        val dNanos = (time - prevTime) 
        
        dTime = dNanos / 1000000000f
        
        if(_running){
          val dt = if(!constantFrameLength) dTime else fpsMillis / 1000f
        	update(dt)
        
          ui.renderCanvas()
        }
        
        val dRenderNanos = System.nanoTime() - time
        val sleepTime = fpsMillis - (dRenderNanos / 1000000f)  
        
        
        if(realTime && sleepTime > 0){
          Thread.sleep(sleepTime.toInt)
        }
      }
    }
  }
  
  ui.syncWithWindow = true
  _animThread.start()
  
  def stopAnim = _running = false
  def startAnim = _running = true
  
  
  lazy val ui = new Canvas(sx,sy) {
    override def draw(g:Graphics2D){
      paintCanvas(g)
    }
  }

  def top = new MainFrame {
    title = winTitle
    contents = ui
  }
}