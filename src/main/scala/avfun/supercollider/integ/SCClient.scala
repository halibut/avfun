package avfun.supercollider.integ

import java.io.File
import java.lang.ProcessBuilder.Redirect
import java.util.concurrent.TimeUnit
import java.nio.charset.Charset
import sun.nio.cs.US_ASCII
import java.io.BufferedOutputStream
import java.io.OutputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.io.PrintStream
import scala.annotation.tailrec
import java.io.BufferedWriter
import java.io.FileWriter
import java.util.Date

class SCClient(val scLangDir:File, val tmpDir:File = new File("/tmp/")) {
  
  private var _sclangProcess:Option[Process] = None;
  private var _msgs:Seq[String] = Seq()
  
  private val Ctrl_C:Int = 3 
  private val EXEC = Array[Byte](27.toByte, 13.toByte)
  
  def start(out:PrintStream, args:String*):Unit = {
    quit()
    
    if(!tmpDir.exists()) {
      tmpDir.mkdirs()
    }
    
    val sclangExe = new File(scLangDir, "sclang.exe").getAbsolutePath 
    val workingDir = scLangDir.getAbsolutePath
    
    val pb = new ProcessBuilder()
      .command(sclangExe, "-i", "avfun", "-d", workingDir)
    
    pb.redirectErrorStream(true)
      
    val proc = pb.start()
    
    val client = this
    
    val outputThread = new Thread(){
      this.setDaemon(false)
      override def run(){
        val r = new BufferedReader(new InputStreamReader(proc.getInputStream))
        
        var line = r.readLine()
        while(line != null) {
          out.println(line)
          client.synchronized{
            _msgs = _msgs :+ line
          }
          line = r.readLine()
        }
      }
    }
    
    outputThread.start();
    _sclangProcess = Some(proc)
    
    waitForOutput("Welcome to SuperCollider")
  }
  
  def waitForOutput(str:String):Unit = {
    _waitForOutput(str)
  }
  
  @tailrec
  private def _waitForOutput(str:String):Unit = {
    val ln = _msgs.headOption
    val matched = ln match {
      case Some(line) => {
        this.synchronized{
          _msgs = _msgs.tail
        }
        line.contains(str)
      }
      case None => {
        Thread.sleep(100)
        false
      }
    }
    
    if(!matched) {
      _waitForOutput(str)
    }
  }
  
  def waitForBoot() {
    sendCommandSynchronously("s.waitForBoot(onComplete: {\">>>BOOT COMPLETE>>>\".postln;});", "BOOT COMPLETE")
  }
  
  def sendCommand(cmd:String):Unit = {
    _sclangProcess.foreach{ case c =>
      
      //If this is a multi-line command, then write it to a file and execute the file
      if(cmd.count(_ == '\n') > 0) {
        val tmpFileName = "sc-cmd-"+(new Date().getTime())+".osc"
        val tmpFile = new File(tmpDir, tmpFileName)
        
        val w = new BufferedWriter(new FileWriter(tmpFile))
        w.write(cmd)
        w.flush()
        w.close()
        
        println("Executing multi-line command. - "+tmpFileName)
        
        val path = tmpFile.getAbsolutePath.replaceAllLiterally("\\", "\\\\")
        sendCommand("this.executeFile(\""+path+"\");")
        
        val cleanup = new Thread() {
          override def run(){
            Thread.sleep(10000)
            tmpFile.delete()
          }
        }
        cleanup.setDaemon(false)
        cleanup.start()
        
      }
      else {
        println(cmd)
        
        val os = new BufferedOutputStream(c.getOutputStream())
        os.write(cmd.getBytes)
        os.write(EXEC)
        os.flush();
      }
    }
  }
  
  def sendCommandSynchronously(cmd:String, expectedOutput:String):Unit = {
    sendCommand(cmd)
    waitForOutput(expectedOutput)
  }
  
  def quit():Unit = {
    _sclangProcess.foreach{ case c =>
      sendCommandSynchronously("0.exit;", "cleaning up OSC")
      c.waitFor(3000, TimeUnit.MILLISECONDS)
      c.destroy()
    }
    
    _sclangProcess = None
  }
  
}

object TestScClient {
  
  def main(args:Array[String]):Unit = {
    
    val c = new SCClient(new File("C:/Program Files (x86)/SuperCollider-3.6.6/"))
    
    c.start(System.out);
    
    Thread.sleep(2000)
    c.sendCommand("s.boot;")
    
    Thread.sleep(3000)
    c.sendCommand("{Pan2.ar(SinOsc.ar(440), 0)}.play;")
    
    Thread.sleep(1000)
    c.sendCommand("s.freeAll;");
    
    Thread.sleep(1000)
    c.quit();
    
  }
  
}