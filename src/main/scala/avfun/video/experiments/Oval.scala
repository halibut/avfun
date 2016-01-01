package avfun.video.experiments

import java.awt.Graphics2D

class Oval(var x:Float, var y:Float, var width:Float, var height:Float) {

  def draw(implicit g:Graphics2D){
    g.translate(x-width/2, y-height/2)
    g.drawOval(0, 0, width.toInt, height.toInt)
    g.translate(width/2-x, height/2-y)
  }
}