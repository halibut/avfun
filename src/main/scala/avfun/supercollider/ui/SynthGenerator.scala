package avfun.supercollider.ui

import scala.swing.SimpleSwingApplication
import scala.swing.Frame
import scala.swing.MainFrame
import scala.swing.SplitPane
import avfun.interval.ThreadBasedTimer
import avfun.visualizer.symmetric.SymmetricVisualizer
import avfun.viz.SwingCanvas2D
import avfun.nnviz.ga.Organism
import avfun.visualizer.symmetric.SymmetricVisualizerNetworkDef
import scala.swing.Orientation
import scala.swing.Panel
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Dimension
import java.awt.image.BufferedImage
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.event.ButtonClicked
import avfun.viz.ColorBufferType
import avfun.viz.Canvas2D
import avfun.audio.TargetLineAudioOutputDevice
import java.io.File
import avfun.audio.stream.AudioInputStreamAudioStream
import javax.sound.sampled.AudioSystem
import avfun.visualizer.fft.FFTVisualizer
import avfun.visualizer.MusicVisualizer
import avfun.visualizer.OutputDeviceAudioFrameListener
import avfun.visualizer.Player
import avfun.visualizer.VideoFrameData
import avfun.visualizer.VideoFrameListener
import scala.swing.GridPanel
import avfun.visualizer.AudioFrameProducer
import avfun.cfg.ConfigUtil
import avfun.visualizer.swing.VideoFrameListenerPanel
import avfun.visualizer.swing.AsyncMusicVisualizer
import avfun.visualizer.swing.VizPanel

object SynthGenerator extends SimpleSwingApplication {  
  
  val appConfig = ConfigUtil.load()
  
  val drumsLoc = "c:/tmp/sc/orgs/drums"
  val synthtsLoc = "c:/tmp/sc/orgs/synths"
  
  
  
  
  var curSong = -1
  
  val player = new Player(){
    override def timer = new ThreadBasedTimer
  }
  player.addAudioFrameListener(new OutputDeviceAudioFrameListener(new TargetLineAudioOutputDevice()))
  
  val cWidth = 400
  val cHeight = 300
  
  val p1Panel = new VizPanel(cWidth, cHeight, player)
  val p2Panel = new VizPanel(cWidth, cHeight, player)
  val cPanel = new VizPanel(cWidth, cHeight, player)
  val specPanel = new VizPanel(cWidth, cHeight, player)
  
  var p1Org = newOrganism
  var p2Org = newOrganism
  var cOrg = p1Org.organismDefintion.mate(p1Org, p2Org)
  
  lazy val ui = new SplitPane(Orientation.Vertical){
    this.leftComponent = new BoxPanel(Orientation.Vertical){
      this.contents += new Button("Play"){ reactions += { case c:ButtonClicked => player.play } }
      this.contents += new Button("Pause"){ reactions += { case c:ButtonClicked => player.pause } }
      this.contents += new Button("Next Song"){ reactions += { case c:ButtonClicked => nextSong } }
      this.contents += new Button("New Child"){ reactions += { case c:ButtonClicked => newChild } }
      this.contents += new Button("New Parent 1"){ reactions += { case c:ButtonClicked => newParent1 } }
      this.contents += new Button("Child to Parent 1"){ reactions += { case c:ButtonClicked => childToParent1 } }
      this.contents += new Button("New Parent 2"){ reactions += { case c:ButtonClicked => newParent2 } }
      this.contents += new Button("Child to Parent 2"){ reactions += { case c:ButtonClicked => childToParent2 } }
    }
    this.rightComponent = new GridPanel(2,2){
      this._contents += p1Panel.panel
      this._contents += p2Panel.panel
      this._contents += cPanel.panel
      this._contents += specPanel.panel
    }
    
    //Initialize with the first song
    nextSong
    
    //Initialize panels
    specPanel.changeVisualizer(new FFTVisualizer(256, 40))
    p1Panel.changeVisualizer(new SymmetricVisualizer(p1Org))
    p2Panel.changeVisualizer(new SymmetricVisualizer(p2Org))
    cPanel.changeVisualizer(new SymmetricVisualizer(cOrg))
    
  }

  override def top:Frame = new MainFrame {
    title = "Audio Visualizer"
    contents = ui
  }
  
  def nextSong = {
    
  }
 
  def newOrganism:Organism[SymmetricVisualizerNetworkDef.type] = {
    SymmetricVisualizerNetworkDef.randomize
  }

  def newChild:Unit = {
    cOrg = p1Org.organismDefintion.mate(p1Org, p2Org)
    cPanel.changeVisualizer(new SymmetricVisualizer(cOrg))
  }
  
  def newParent1:Unit = {
    p1Org = newOrganism
    p1Panel.changeVisualizer(new SymmetricVisualizer(p1Org))
    newChild
  }
  
  def childToParent1:Unit = {
    p1Org = cOrg
    p1Panel.changeVisualizer(new SymmetricVisualizer(p1Org))
    newChild
  }
  
  def newParent2:Unit = {
    p2Org = newOrganism
    p2Panel.changeVisualizer(new SymmetricVisualizer(p2Org))
    newChild
  }
  
  def childToParent2:Unit = {
    p2Org = cOrg
    p2Panel.changeVisualizer(new SymmetricVisualizer(p2Org))
    newChild
  }
  
}

