package avfun.supercollider.integ

import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.TimeUnit
import java.nio.charset.Charset
import sun.nio.cs.US_ASCII
import java.io.BufferedOutputStream
import java.io.OutputStream
import avfun.supercollider.model.SynthDef
import java.util.Date
import avfun.supercollider.SCSynthWriter
import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.FileWriter
import avfun.supercollider.SynthOrganism
import avfun.supercollider.SynthOrganism.SynthOrganismDef
import avfun.supercollider.model.SongDef
import avfun.supercollider.SCScoreWriter

object SCSoundRecorder {
  
  private val EXEC_MARKER = ">>>CMD FINISHED>>>"
  
  def writeDefFile(synthDef:SynthDef, name:String)(implicit c:SCClient):Unit = {
    
    val endCmd = "\""+EXEC_MARKER+"\".postln;"
    
    val cmd = s"""(
      ${SCSynthWriter.writeSynthDef(synthDef, name, true)}.writeDefFile;
      ${endCmd}
      )"""
    
    c.sendCommandSynchronously(cmd, EXEC_MARKER)
  }
  
  def recordSong(songDef:SongDef, name:String, outDir:File)(implicit c:SCClient):Unit = {
    
    val nameWithoutExt = name.takeWhile(_ != '.')
    
    val songFile = new File(outDir, nameWithoutExt+".wav")
    
    if(songFile.exists()) {
      songFile.delete()
    }
    
    val outputOscFile = new File(outDir, nameWithoutExt+".osc")
    
    val songFileStr = songFile.getAbsolutePath.replaceAllLiterally("\\", "\\\\")
    val outputOscFileStr = outputOscFile.getAbsolutePath.replaceAllLiterally("\\", "\\\\")
    
    for((name,synthDef,_) <- songDef.notes) {
      writeDefFile(synthDef, name)
    }

    val cmd = s"""(
      var score, options;
      
      score = ${SCScoreWriter.writeScore(songDef)};
      
      TempoClock.default.tempo = 1;
      
      options = ServerOptions.new;
      options.numOutputBusChannels = 2;
      
      Score.recordNRT(score, "${outputOscFileStr}", "${songFileStr}", nil, headerFormat:"WAV", options: options);
      
      )"""
    
    c.sendCommandSynchronously(cmd, "RESULT = 0")
    
    outputOscFile.delete()
    
  }
  
  def main(args:Array[String]):Unit = {
    val scDir = new File("C:/Program Files (x86)/SuperCollider-3.6.6/")
    
    implicit val scClient = new SCClient(scDir)
    
    scClient.start(System.out)
    scClient.waitForBoot();
    
    for(i <- 0 until 4) {
      var synthDef = SynthOrganism.getSynthDefinition(SynthOrganismDef.randomize)
      
      while(!synthDef.isValid) {
        synthDef = SynthOrganism.getSynthDefinition(SynthOrganismDef.randomize)
      }
      
      writeDefFile(synthDef, "test-synth-"+i)
    }
    
    scClient.quit();
    
  }
  
}
