import avfun.audio.TargetLineAudioOutputDevice

import javax.sound.sampled.TargetDataLine
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.DataLine
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.Line
import javax.sound.sampled.Port


object tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val afwav = new AudioFormat(44100, 16, 2, true, true);
                                                  //> afwav  : javax.sound.sampled.AudioFormat = PCM_SIGNED 44100.0 Hz, 16 bit, st
                                                  //| ereo, 4 bytes/frame, big-endian
  val afinfo = new DataLine.Info(classOf[SourceDataLine], afwav);
                                                  //> afinfo  : javax.sound.sampled.DataLine.Info = interface SourceDataLine suppo
                                                  //| rting format PCM_SIGNED 44100.0 Hz, 16 bit, stereo, 4 bytes/frame, big-endia
                                                  //| n
  
  val line = AudioSystem.getLine(afinfo).asInstanceOf[SourceDataLine]
                                                  //> line  : javax.sound.sampled.SourceDataLine = com.sun.media.sound.DirectAudio
                                                  //| Device$DirectSDL@2bbf1be2
  val lineInfo = line.getLineInfo                 //> lineInfo  : javax.sound.sampled.Line.Info = interface SourceDataLine support
                                                  //| ing 8 audio formats, and buffers of at least 32 bytes
  val lineFormat = line.getFormat                 //> lineFormat  : javax.sound.sampled.AudioFormat = PCM_SIGNED 44100.0 Hz, 16 bi
                                                  //| t, stereo, 4 bytes/frame, big-endian
  val lineBufferSize = line.getBufferSize         //> lineBufferSize  : Int = 88200
  
  val infos = AudioSystem.getSourceLineInfo(afinfo)
                                                  //> infos  : Array[javax.sound.sampled.Line.Info] = Array(interface SourceDataLi
                                                  //| ne supporting 8 audio formats, and buffers of at least 32 bytes, interface S
                                                  //| ourceDataLine supporting 8 audio formats, and buffers of at least 32 bytes, 
                                                  //| interface SourceDataLine supporting 8 audio formats, and buffers of at least
                                                  //|  32 bytes, interface SourceDataLine supporting 8 audio formats, and buffers 
                                                  //| of at least 32 bytes, interface SourceDataLine supporting 8 audio formats, a
                                                  //| nd buffers of at least 32 bytes, interface SourceDataLine supporting 8 audio
                                                  //|  formats, and buffers of at least 32 bytes, interface SourceDataLine support
                                                  //| ing 8 audio formats, and buffers of at least 32 bytes, interface SourceDataL
                                                  //| ine supporting 8 audio formats, and buffers of at least 32 bytes, interface 
                                                  //| SourceDataLine supporting 8 audio formats, and buffers of at least 32 bytes,
                                                  //|  interface SourceDataLine supporting 8 audio formats, and buffers of at leas
                                                  //| t 32 bytes)
  
  infos.map(i => i.getLineClass)                  //> res0: Array[Class[_]] = Array(interface javax.sound.sampled.SourceDataLine, 
                                                  //| interface javax.sound.sampled.SourceDataLine, interface javax.sound.sampled.
                                                  //| SourceDataLine, interface javax.sound.sampled.SourceDataLine, interface java
                                                  //| x.sound.sampled.SourceDataLine, interface javax.sound.sampled.SourceDataLine
                                                  //| , interface javax.sound.sampled.SourceDataLine, interface javax.sound.sample
                                                  //| d.SourceDataLine, interface javax.sound.sampled.SourceDataLine, interface ja
                                                  //| vax.sound.sampled.SourceDataLine)
  
  

}