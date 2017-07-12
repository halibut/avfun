package avfun.visualizer.info

import avfun.visualizer.AudioFrameListener
import scala.swing.Panel
import avfun.visualizer.AudioFrameData
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import avfun.supercollider.model.SongDef
import avfun.supercollider.model.NoteEvent
import avfun.supercollider.model.PatternDef
import avfun.supercollider.MusicScale
import avfun.supercollider.MusicScale.OctaveStartIndicator
import avfun.supercollider.MusicScale.OctaveNoIndicator
import java.awt.Font

class SongNotesPlayerVisualizer(val displayPlayedBars:Float = 1f, val displayFutureBars:Float = 6f, latencyInSamples:Int = 0)  extends Panel with AudioFrameListener{
  override val requiredSamplesPerFrame: Int = 0
  
  
  private var _songDef:SongDef = null
  private var _songLengthInSamplesFromSongDef = 0f
  //private var _songLengthInSamples = 0
  //private var _songPositionInSamples = 0
  private var _songPosition = 0f
  
  private var _maxNote = 100
  private var _minNote = 1
  
  private var _notesRemaining:Seq[(PatternDef, Seq[NoteEvent])] = Seq()
    
  def onFrameData(frameData:AudioFrameData):Unit = {
    val songPositionInSamples = (frameData.audioData.streamSourceInfo.samplesRead.getOrElse(0)-latencyInSamples)
    val maxSongLengthSamples = frameData.audioData.streamSourceInfo.totalSamples.getOrElse(0)
    println(s"${frameData.audioData.streamSourceInfo}")
    _songPosition = if(maxSongLengthSamples == 0) 0.0f else songPositionInSamples.toFloat / math.min(maxSongLengthSamples.toFloat, _songLengthInSamplesFromSongDef.toFloat)
    //_songPosition = frameData.streamPosition.getOrElse(0f)
    
    if(_songDef != null) {
      _notesRemaining = _notesRemaining.map{ case(patternData, notes) => 
        val currentTime = patternData.length * _songPosition
        val keepTime = patternData.length * _songPosition - displayPlayedBars
        val newNotes = notes.dropWhile(noteEvent => (noteEvent.time+noteEvent.dur) < keepTime)
        
        (patternData, newNotes)
      }
    }
    
    this.repaint()
  }
  def onRegistered:Unit = {}
  def onDeregistered:Unit = {}
  
  background = new Color(1.0f,1.0f,1.0f)
  maximumSize = new Dimension(800, 20)
  val instColors = Array[Color](
    new Color(0.5f, 0f, 0f),
    new Color(0f, 0.5f, 0f),
    new Color(0f, 0f, 0.5f),
    new Color(0.5f, 0.5f, 0f),
    new Color(0.5f, 0f, 0.5f),
    new Color(0f, 0.5f, 0.5f)
  )
  
  val whiteKeyColor = new Color(1.0f, 1.0f, 1.0f)
  val blackKeyColor = new Color(0.8f, 0.8f, 0.8f)
  
  override def paintComponent(g:Graphics2D):Unit = {
    super.paintComponent(g);
    val height = size.getHeight
    val width = size.getWidth
    
    val songLengthInBars = if(_songDef != null) _songDef.notes.headOption.flatMap(_._3.headOption.map(_.length)).getOrElse(1f) else 1
    val barsPosition = songLengthInBars * _songPosition
    val currentBar = math.floor(barsPosition)
    
    val noteRange = _maxNote - _minNote +1
    val noteHeight = math.max(height / noteRange.toFloat, 2f)
    
    
    val fontHeight = math.min(20, (noteHeight*.75).toInt)
    g.setFont(this.peer.getFont.deriveFont(Font.PLAIN, math.min(20, (noteHeight*.75).toInt)))
    
    //Draw keys
    for(i <- _minNote to _maxNote) {
      val noteStartStr = MusicScale.getMidiNoteStr(i, OctaveNoIndicator)
      if(noteStartStr.indexOf('#') >= 0){
        g.setColor(blackKeyColor)
      }
      else {
        g.setColor(whiteKeyColor)
      }
      val laneStart = (height - (i-_minNote)*noteHeight).toInt
      g.fillRect(0,laneStart, width.toInt, -noteHeight.toInt)
      
      g.setColor(Color.BLACK)
      g.drawLine(0,laneStart, width.toInt, laneStart)
      g.drawString(MusicScale.getMidiNoteStr(i, OctaveStartIndicator), 0, laneStart-(0.25*fontHeight).toInt)
    }
    
    val totalBarsDisplayed = displayPlayedBars+displayFutureBars
    
    val instNoteHeight = math.max(4, noteHeight / _notesRemaining.size)
    //Draw Notes
    for{
      inst <- 0 until _notesRemaining.size
    } {
      
      val instColor = instColors(inst % instColors.size)
      g.setColor(instColor)
      val pattern = _notesRemaining(inst)
      for(note <- pattern._2.takeWhile(ne => (ne.time + ne.dur) < (barsPosition + displayFutureBars + 1))) {
        val midiNote = MusicScale.freqToMidi(note.freq)
        val x = ((displayPlayedBars + note.time - barsPosition) * width/totalBarsDisplayed).toInt
        val length = ((note.dur) * width/totalBarsDisplayed).toInt
        val laneStart = (height - (midiNote-_minNote)*noteHeight).toInt
        g.fillRect(x, laneStart + (instNoteHeight * inst).toInt, length, instNoteHeight.toInt)
        g.fillRect(x, laneStart + (instNoteHeight * inst).toInt - (instNoteHeight/4).toInt, 5, (1.5*instNoteHeight).toInt)
      }
    }
    
    //Draw timing info
    g.setColor(Color.BLACK)
    g.fillRect((width*displayPlayedBars/(totalBarsDisplayed)).toInt, 0, -4, height.toInt)
    val barPosition = barsPosition - currentBar
    for(i <- 0 until math.ceil(totalBarsDisplayed).toInt) {
      g.fillRect((i*width/totalBarsDisplayed + (displayPlayedBars-barPosition)*width/totalBarsDisplayed).toInt, 0, -2, height.toInt)
    }
    
  }
  
  def reset(songDef:SongDef, songLengthInSamples:Int, songPositionInSamples:Int = 0):Unit = {
    //_songPositionInSamples = songPositionInSamples
    //_songLengthInSamples = songLengthInSamples
    
    _songDef = songDef
    
    val lastOfAllNotes = _songDef.notes.flatMap(_._3.map(_.notes.lastOption)).filter(_.isDefined).map(n => n.get.time + n.get.dur).max
    _songLengthInSamplesFromSongDef = (lastOfAllNotes * _songDef.noteLengthMult * 44100).toInt
    
    var maxFreq = 0f
    var minFreq = 100000f
    
    _notesRemaining = songDef.notes.map( _._3 ).flatten.map{ patDef =>
      val patDefMaxNote = patDef.notes.maxBy(_.freq)
      val patDefMinNote = patDef.notes.minBy(_.freq)
      
      if(patDefMaxNote.freq > maxFreq) {
        maxFreq = patDefMaxNote.freq
      }
      if(patDefMinNote.freq < minFreq) {
        minFreq = patDefMinNote.freq
      }
      
      (patDef, patDef.notes)
    }
     
    if(minFreq<50000) {
      _minNote = MusicScale.freqToMidi(minFreq)-2
    }
    else {
      _minNote = 0
    }
    
    if(maxFreq> 0) {
      _maxNote = MusicScale.freqToMidi(maxFreq)+2
    }
    else {
      _maxNote = _minNote+20
    }
    
  }
  
}