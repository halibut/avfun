package avfun.visualizer.info

import avfun.visualizer.AudioFrameListener
import scala.swing.Panel
import avfun.visualizer.AudioFrameData
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D

class SongProgressBar extends Panel with AudioFrameListener{
  override val requiredSamplesPerFrame: Int = 0
  
  private var _songLengthInSamples = 0
  private var _songPositionInSamples = 0
  private var _songPosition = 0f
  
  private var _onSongProgress: Option[(Float)=>Unit] = None
  
  private var _progWidth:Int = 0
    
  def onFrameData(frameData:AudioFrameData):Unit = {
    _songPositionInSamples += frameData.samplesPerFrame
    _songPosition = if(_songLengthInSamples == 0) 0.0f else _songPositionInSamples.toFloat / _songLengthInSamples.toFloat
    
    val newWidth = math.round(size.getWidth * _songPosition).toInt
    
    //Only redraw if the progress in pixels has changed
    if(newWidth != _progWidth){
      this.repaint()
    }

    _onSongProgress.foreach(pf => pf(_songPosition))
  }
  def onRegistered:Unit = {}
  def onDeregistered:Unit = {}
  
  background = new Color(1.0f,1.0f,1.0f)
  maximumSize = new Dimension(800, 20)
  
  val rectColor = new Color(0.1f, 0.1f, 0.1f)
  override def paintComponent(g:Graphics2D):Unit = {
    super.paintComponent(g);
    val height = size.getHeight
    val width = size.getWidth
    
    val newWidth = math.round(size.getWidth * _songPosition).toInt
    _progWidth = newWidth
    
    g.setColor(rectColor)
    g.fillRect(0,0, newWidth, height.toInt)
  }
  
  def reset(songLengthInSamples:Int, songPositionInSamples:Int = 0):Unit = {
    _songPositionInSamples = songPositionInSamples
    _songLengthInSamples = songLengthInSamples
  }
  
  def onSongProgress(f:(Float)=>Unit):Unit = {
    _onSongProgress = Some(f)
  }
}