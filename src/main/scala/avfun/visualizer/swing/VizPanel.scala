package avfun.visualizer.swing

import avfun.viz.SwingCanvas2D
import avfun.visualizer.MusicVisualizer
import avfun.visualizer.AudioFrameProducer

class VizPanel(val cWidth:Int, val cHeight:Int, val audioFrameProducer:AudioFrameProducer){
  
  val panel:VideoFrameListenerPanel = new VideoFrameListenerPanel(cWidth, cHeight)
  lazy val canvas:SwingCanvas2D = new SwingCanvas2D(cWidth, cHeight)
  private var _viz:Option[MusicVisualizer] = None
  
  def changeVisualizer(viz:MusicVisualizer):Unit = {
    _viz.foreach{ v => audioFrameProducer.removeAudioFrameListener(v) }
    
    if(viz != null){
      viz.setCanvas(canvas)
      viz.frameListener_+=(panel)
      
      val aViz = new AsyncMusicVisualizer(viz)
      _viz = Some(aViz)
      audioFrameProducer.addAudioFrameListener(aViz)
    }
    else{
      _viz = None
    }
  }
}