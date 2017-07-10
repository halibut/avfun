package avfun.musicgen

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
import avfun.visualizer.swing.VizPanel
import java.nio.file.Files
import scala.swing.ScrollPane
import scala.swing.Label
import scala.swing.TextArea
import javax.swing.Box
import scala.swing.Swing
import scala.swing.event.AdjustingEvent
import scala.swing.Component
import scala.swing.GridBagPanel
import avfun.visualizer.AudioFrameListener
import avfun.visualizer.AudioFrameListener
import avfun.visualizer.AudioFrameData
import java.awt.Graphics
import java.awt.Font
import avfun.visualizer.info.SongProgressBar
import avfun.visualizer.info.SongNotesPlayerVisualizer

object MusicGenUI extends SimpleSwingApplication {  
  
  val songGeneratorConfig = SongGeneratorConfig.getConfigFromFile("./song-generator-config.json")
  
  val songUtil = new SongGeneratorUtil(
    baseDir = songGeneratorConfig.projectDir, songs = 20, 
    songStructureOrganisms = 20, patternOrganisms = 20, synthOrganisms = 40, instrumentPatternOrganisms = 40  
  )

  var songs:GenSongs = null
  var curSong = -1
  
  val songPositionPanel = new SongProgressBar()
  songPositionPanel.onSongProgress{ p => if(p > 1.0f) this.nextSong() }
  
  val songNotesPanel = new SongNotesPlayerVisualizer()
  
  val player = new Player(){
    override def timer = new ThreadBasedTimer
  }
  player.addAudioFrameListener(new OutputDeviceAudioFrameListener(new TargetLineAudioOutputDevice()))
  player.addAudioFrameListener(songPositionPanel)
  player.addAudioFrameListener(songNotesPanel)
  
  val cWidth = 600
  val cHeight = 200
  
  val specPanel = new VizPanel(cWidth, cHeight, player)
  
  lazy val songListPanel = new BoxPanel(Orientation.Vertical)
  lazy val songInfoPanel = new BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension(400,0)
  }
  
  lazy val ui = new SplitPane(Orientation.Vertical){
    this.leftComponent = new SplitPane(Orientation.Horizontal){ 
      this.topComponent = new GridBagPanel(){
        minimumSize = new Dimension(100,40)
        preferredSize = new Dimension(120,40)
        maximumSize = new Dimension(2000,80)
        
        this.add(new Button("Play"){ 
            reactions += { case c:ButtonClicked => player.play }
            maximumSize = new Dimension(800, 20)
          },
          new Constraints{ grid = (0,0); anchor = GridBagPanel.Anchor.LineStart; fill = GridBagPanel.Fill.Horizontal; weightx = 1.0}
        )
        this.add(new Button("Pause"){ 
            reactions += { case c:ButtonClicked => player.pause }
            maximumSize = new Dimension(800, 20)
          },
          new Constraints{ grid = (1,0); fill = GridBagPanel.Fill.Horizontal; weightx = 1.0}
        )
        this.add(
          songPositionPanel,
          new Constraints{ grid = (0,1); gridwidth = 2; anchor = GridBagPanel.Anchor.LineStart; fill = GridBagPanel.Fill.Horizontal; weightx = 1.0}
        )
      }
      this.bottomComponent = new ScrollPane(){
        this.contents = songListPanel
      }
    }
    this.rightComponent = new BoxPanel(Orientation.Horizontal){
      this._contents += songInfoPanel
      this._contents += new BoxPanel(Orientation.Vertical){
        songNotesPanel.maximumSize = new Dimension(1000,800)
        this._contents += songNotesPanel
        
        specPanel.panel.maximumSize = new Dimension(cWidth, cHeight)
        this._contents += specPanel.panel 
      }
    }
    
    //init song list
    initializeSongList()
    
    //Initialize with the first song
    setSong(0)
    
    //Initialize panels
    specPanel.changeVisualizer(new FFTVisualizer(1024,50,2))
    
  }

  override def top:Frame = new MainFrame {
    title = "Audio Visualizer"
    contents = ui
  }
  
  def nextSong() = {
    setSong((curSong+1) % songs.songs.size)
  }
  
  def setSong(ind:Int) = {
    if(curSong != ind) {
      curSong = ind
      
      songListPanel.contents.zipWithIndex.foreach { case(songButton,ind) =>
        setSelectedStyle(songButton, ind == curSong)
      }
        
      val songFilePath = songUtil.getFullSongPath(songs.songs(curSong))
      val file = songFilePath.toFile
      val audioInputStream = AudioSystem.getAudioInputStream(file)
      println(s"Song sample rate: ${audioInputStream.getFormat.getSampleRate}")
      val songLength = ((audioInputStream.getFrameLength / audioInputStream.getFormat.getFrameRate) * audioInputStream.getFormat.getSampleRate).ceil.toInt
      
      val songLengthFrames = audioInputStream.getFrameLength
      
      println(s"Song Length Method 1: ${songLength}")
      println(s"Song Length Method 2: ${songLengthFrames}")
      
      songPositionPanel.reset(songLength)
      songNotesPanel.reset(songUtil.getSongDef(songs.songs(curSong)), songLength)
      
      player.changeAudioSource(new AudioInputStreamAudioStream(audioInputStream))
      
      updateSongInfoPanel(songs.songs(curSong))
      
    }
  }
  
  def setSelectedStyle(c:Component, selected:Boolean):Unit = {
    if(selected) {
      c.background = new Color(1.0f,1.0f,1.0f)
      c.foreground = new Color(0.0f,0.0f,0.0f)
    }
    else {
      c.background = new Color(0.1f,0.1f,0.1f)
      c.foreground = new Color(0.9f,0.9f,0.9f)
    }
  }
  
  def updateSongInfoPanel(songInfo:GenSongInfo):Unit = {
    songInfoPanel.contents.clear()
    
    songInfoPanel.contents += new Label(s"Song: ${songInfo.songFile}")
    songInfoPanel.contents += new GridBagPanel(){
      minimumSize = new Dimension(200,10)
      preferredSize = new Dimension(2000,40)
      maximumSize = new Dimension(2000,40)
      
      val keep = songInfo.keep.getOrElse(false)
      
      add(new Button("Discard") {
          reactions += {case c:ButtonClicked => updateSongInfo(songInfo.copy(keep = Some(false)))}
          setSelectedStyle(this, !keep) 
          maximumSize = new Dimension(40, 400)
        },
        new Constraints{
          grid = (0,0)
          anchor = GridBagPanel.Anchor.LineStart
          fill = GridBagPanel.Fill.Both
          weightx = 1.0
        }
      )
      add(new Button("Keep") {
          reactions += {case c:ButtonClicked => updateSongInfo(songInfo.copy(keep = Some(true)))}
          setSelectedStyle(this, keep)
          maximumSize = new Dimension(40, 400)
        },
        new Constraints{
          grid = (1,0)
          anchor = GridBagPanel.Anchor.LineStart
          fill = GridBagPanel.Fill.Both
          weightx = 1.0
        }
      )
    }
    songInfoPanel.contents += new BoxPanel(Orientation.Vertical){
      contents += new BoxPanel(Orientation.Horizontal) { 
        contents += new Label(s"Overall"){
          minimumSize = new Dimension(100,30)
        }
        contents += Swing.HGlue
        contents += ratingComponent(songInfo.overallRank, (newRank) => updateSongInfo(songInfo.copy(overallRank = Some(newRank))))
      }
      
      contents += new BoxPanel(Orientation.Horizontal) { 
        contents += new Label(s"Song Structure"){
          minimumSize = new Dimension(100,30)
        }
        contents += Swing.HGlue
        contents += ratingComponent(songInfo.songStructureRank, (newRank) => updateSongInfo(songInfo.copy(songStructureRank = Some(newRank))))
      }
      
      contents += new BoxPanel(Orientation.Horizontal) { 
        contents += new Label(s"Note Patterns"){
          minimumSize = new Dimension(100,30)
        }
        contents += Swing.HGlue
        contents += ratingComponent(songInfo.instrumentPatternsRank, (newRank) => updateSongInfo(songInfo.copy(instrumentPatternsRank = Some(newRank))))
      }
      
      contents += new BoxPanel(Orientation.Horizontal) { 
        contents += new Label(s"Synths"){
          minimumSize = new Dimension(100,30)
        }
        contents += Swing.HGlue
        contents += ratingComponent(songInfo.synthsRank, (newRank) => updateSongInfo(songInfo.copy(synthsRank = Some(newRank))))
      }
      
      contents += new BoxPanel(Orientation.Horizontal) { 
        contents += new Label(s"Pattern Structure"){
          minimumSize = new Dimension(100,30)
        }
        contents += Swing.HGlue
        contents += ratingComponent(songInfo.patternStructureRank, (newRank) => updateSongInfo(songInfo.copy(patternStructureRank = Some(newRank))))
      }
    }
    songInfoPanel.contents += new ScrollPane(){
      val songDef = songUtil.getSongDef(songInfo)
      val noteInfo = songDef.notes.map{ synth =>
        s"""Synth: ${synth._1}
        |  SynthDef: 
        |    ${synth._2.prettyPrint().split("\n").mkString("\n      ")}
        |  Instruments: ${synth._3.zipWithIndex.map(i => "instrument-"+i._2+": "+i._1).mkString("\n    ", "\n    ","")}
        """.stripMargin
      }
      
      val songLength = songDef.notes.map(_._3).flatten.map(_.length).max * songDef.noteLengthMult
      val songMinutes = songLength.toInt / 60
      val songSeconds = songLength - songMinutes*60
      val songSecondsStr = if(songSeconds<10) "0"+songSeconds else ""+songSeconds
      
      contents = new TextArea() {
        text = s"""Song: ${songInfo.songFile}
          |BPM: ${songDef.bpm}
          |Length: ${songMinutes}:${songSecondsStr}
          |
          |  ${noteInfo.mkString("\n  ")}
          |""".stripMargin
        editable = false
        peer.setCaretPosition(0)
        peer.setFont(new Font("monospaced", Font.PLAIN, 12));
      }
    }
    
    songInfoPanel.revalidate()
  }
  
  def updateSongInfo(updatedSong:GenSongInfo):Unit = {
    songs = songs.copy(songs = songs.songs.map{ s =>
      if(s.songFile == updatedSong.songFile) {
        updatedSong
      }
      else {
        s
      }
    })
    
    
    songUtil.saveCurrentGenSongs(songs)
    
    updateSongInfoPanel(updatedSong)
  }
  
  def ratingComponent(rating: Option[Int], onUpdate:(Int)=>Unit):Panel = {
    new BoxPanel(Orientation.Horizontal){
      contents ++= (1 to 5).toSeq.map{ buttonInd =>
        new Button(s"${buttonInd}"){
          setSelectedStyle(this, rating.getOrElse(0) >= buttonInd)
          
          reactions += { 
            case c:ButtonClicked => 
              onUpdate(buttonInd)
          } 
          minimumSize = new Dimension(50,20)
          preferredSize = new Dimension(50,30)
          maximumSize = new Dimension(50,30)
        }
      }
    }
  }
  
  def initializeSongList() = {
    val genInfo = songUtil.getCurrentGenSongs()
    
    songs = genInfo
    
    songListPanel.contents.clear()
    songs.songs.zipWithIndex.foreach{ case (songInfo,ind) =>
      songListPanel.contents += new Button(songInfo.songFile){ 
        reactions += { 
          case c:ButtonClicked => 
            setSong(ind)
        } 
        maximumSize = new Dimension(800, 20)
      }
    }
    
    
    setSong(0)
  }
  
  

}
