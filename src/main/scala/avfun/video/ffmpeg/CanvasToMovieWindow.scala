package avfun.video.ffmpeg

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point, geom}
import scala.swing.SimpleSwingApplication
import java.awt.Dimension

abstract class CanvasToMovieWindow(winTitle:String,val sx:Int=500,val sy:Int=500, val fps:Float=60f) extends SimpleSwingApplication {
  
  
  def update(dSeconds:Float)
  def paintCanvas(g: Graphics2D):Unit
  
  private val fpsMillis = 1000 / fps
  private val _thisWindow = this
  private var _running = false
  
  
  
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
        	update(dTime)
        	
        	_thisWindow.ui.repaint()
        
        }
        
        val dRenderNanos = System.nanoTime() - time
        val sleepTime = fpsMillis - (dRenderNanos / 1000000f)  
        
        
        if(sleepTime > 0){
          Thread.sleep(sleepTime.toInt)
        }
      }
    }
  }
  _animThread.start()
  
  def stopAnim = _running = false
  def startAnim = _running = true
  
  lazy val ui = new Panel {
    background = Color.white
    preferredSize = new Dimension(sx,sy) 

    focusable = true
    
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      
      paintCanvas(g)
    }
  }

  def top = new MainFrame {
    title = winTitle
    contents = ui
  }
}