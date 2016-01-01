package avfun.video

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel}
import scala.swing.event._
import java.awt.{Color, Graphics2D, Point, geom}
import scala.swing.SimpleSwingApplication
import java.awt.Dimension

abstract class BasicCanvasWindow(winTitle:String,val sx:Int=500,val sy:Int=500) extends SimpleSwingApplication {
  
  def paintCanvas(g: Graphics2D):Unit
  
//  lazy val ui = new Panel {
//    background = Color.white
//    preferredSize = new Dimension(sx,sy) 
//
//    focusable = true
//    
//    override def paintComponent(g: Graphics2D) = {
//      super.paintComponent(g)
//      
//      paintCanvas(g)
//    }
//  }
  
  lazy val ui = new Canvas(sx,sy){
    override def draw(g:Graphics2D){
      paintCanvas(g)
    }
  }

  def top = new MainFrame {
    title = winTitle
    contents = ui
  }
  
  ui.renderCanvas()
}