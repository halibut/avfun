package avfun.supercollider

import avfun.supercollider.SynthOrganism.SynthOrganismDef
import avfun.nnviz.ga.OrganismSerializer
import avfun.nnviz.ga.FileSystemPersistence
import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import avfun.nnviz.ga.Organism

object SCSynthWriter {
  
  def writeSynthDef(synthDef:SynthDef, synthName:String, withComments:Boolean):String = {
    val sb = new StringBuilder
    
    sb.append("SynthDef(\"").append(synthName).append("\", { arg out = 0, freq = 440, amp = 0.75, pan = 0, gate = 1;\n")
    
    //Get the declared 
    val oscWithInd = synthDef.ugens.zipWithIndex.filter(_._1.isDefined).map(x => (x._2, x._1.get))
    
    if(!oscWithInd.isEmpty) {
      //Print out the variable declarations for the oscillators
      sb.append("  var ").append(oscWithInd.map("osc"+_._1).mkString(", ")).append(";\n")
      sb.append("  var ").append(oscWithInd.map("osc"+_._1+"Filter").mkString(", ")).append(";\n")
    }
    
    //Print out some more variable declarations
    sb.append("  var osc, adjFreq, adjAmp;\n")
    
    //Set freq and amp variables to be used by the ugens
    sb.append("  adjFreq = freq;\n");
    sb.append("  adjAmp = amp;\n");
    
    //Optional frequency vibrato
    synthDef.lfoFreq.foreach{lfo =>
      sb.append("  adjFreq = freq + (").append(lfo.amount * 20f).append(" * SinOsc.ar(").append(lfo.speed).append(", Rand(0, 2.0)));\n")
    }
    
    //Optional amplitude (level) vibrato
    synthDef.lfoAmp.foreach{lfo =>
      sb.append("  adjAmp = amp + (").append(lfo.amount).append(" * SinOsc.ar(").append(lfo.speed).append(", Rand(0, 2.0)));\n")
    }
    
    for((ind, ugenDef) <- oscWithInd) {
      sb.append("\n")
      val oscName = "osc"+ind;
      if(withComments){
        sb.append("  //UGen-").append(ind).append(" ").append(printUGenComment(ugenDef)).append("\n")
      }
      sb.append("  ").append(oscName).append(" = ").append(printUGenDefinition(ugenDef)).append(";\n")
      
      ugenDef.filterType.foreach { filter  =>
        sb.append("  ").append(oscName).append("Filter = ").append(printUGenFilterDefinition(ugenDef, oscName)).append(";\n")
        val amtOrig = math.abs(ugenDef.vars(0))
        val amtFilter = math.abs(ugenDef.vars(1))
        val totalAmp = math.max(amtOrig, amtFilter).toFloat
        sb.append("  ").append(oscName).append(" = Mix([").append(oscName).append(" * ").append(amtOrig / totalAmp)
          .append(", ").append(oscName).append("Filter * ").append(amtFilter / totalAmp).append("]);\n")
      }
//      sb.append("  ").append(oscName).append(" = ").append(oscName).append(" * ").append(printUGenEnvDefinition(ugenDef)).append(";\n")
    }
    
    sb.append("\n")
    sb.append("  osc = adjAmp * Mix([").append(oscWithInd.map("osc"+_._1).mkString(", ")).append("]);\n")
    sb.append("  osc = osc * ").append(printUGenEnvDefinition(oscWithInd.head._2)).append(";\n")
    sb.append("\n")
    sb.append("  Out.ar(out, Pan2.ar(osc, 0));\n")
    sb.append("})");
        
    sb.toString
  }
  
  private def printUGenComment(ugenDef:UGenDef):String = {
    s"${ugenDef}"
  }
  
  private def printUGenDefinition(ugenDef:UGenDef):String = {
    val amp = ugenDef.amp;
    val phase = 0.5f + 0.5f * ugenDef.vars(0)
    val width = 0.5f + 0.5f * ugenDef.vars(1)
    val freqMult = ugenDef.freqMult
    
    val freqDef = s"(adjFreq * ${freqMult})"
    
    ugenDef.uGenType % 7 match {
      case 0 => {
        s"${amp} * SinOsc.ar(${freqDef}, ${phase})"
      }
      case 1 => {
        s"${amp} * Pulse.ar(${freqDef}, ${phase})"
      }
      case 2 => {
        s"${amp} * Saw.ar(${freqDef})"  
      }
      case 3 => {
        s"${amp} * LFTri.ar(${freqDef}, ${4f * phase})"  
      }
      case 4 => {
        s"${amp} * VarSaw.ar(${freqDef}, ${phase}, ${width})"
      }
      case 5 => {
        val start = 5f + 50f + ugenDef.vars(0) * 50f
        val end = 5f + 50f + ugenDef.vars(1) * 50f 
        val time = 0.01f + 1f + ugenDef.vars(2) * 1f
        s"${amp} * Blip.ar(${freqDef} , Line.kr(${start},${end},${time},doneAction: 2))"
      }
      case 6 => {
        val formFreqMul = math.max(1, ugenDef.vars.slice(0, 4).map(x => if (x > 0.0f) 1 else 0).sum) 
        val bwFreqMul = formFreqMul * math.max(1, ugenDef.vars.slice(4, 8).map(x => if (x > 0.0f) 1 else 0).sum)
        
        s"${amp} * Formant.ar(${freqDef}, ${formFreqMul} * adjFreq, ${bwFreqMul} * adjFreq)"
      }
    }
  }
  
  private def printUGenFilterDefinition(ugenDef:UGenDef, oscName:String):String = {
    s"${oscName}"
  }
  
  private def printUGenEnvDefinition(ugenDef:UGenDef):String = {
    val v = ugenDef.vars
    val a = v(4) * 0.5f + 0.5f
    val d = v(5) * 0.5f + 0.5f
    val s = v(6) * 0.5f + 0.5f
    val r = v(7) * 0.5f + 0.5f
    
    ugenDef.envelopeType match {
      case EnvelopeASDR() => {
        s"EnvGen.ar(Env.adsr(${a * 0.25f}, ${d * 0.25f}, ${s * 0.5f}, ${r * .5f + .25f}, 1), gate, doneAction: 2)"
      }
      case EnvelopePerc() => {
        s"EnvGen.ar(Env.perc(${a * 0.05f}, ${d + 0.1f}), gate, doneAction: 2)"
      }
    }
  }
  
  def getSynthFromFile(file:File):Organism[SynthOrganismDef.type] = {
    val orgSerializer = new OrganismSerializer()
    
    val orgfis = new FileInputStream(file)
    try{
      val chroms = orgSerializer.deserializeOrganismCoromosomes(SynthOrganismDef, orgfis) 
      val org = new Organism(0L, chroms, None, None)(SynthOrganismDef)
      org
    }
    finally{
      orgfis.close()
    }
  }
  
  def getSynthFromSavedDefs():Organism[SynthOrganismDef.type] = {
    val synthFiles = new File("sc-orgs/synthdef").listFiles()
    val f1 = synthFiles((math.random * synthFiles.length).toInt)
    val f2 = synthFiles((math.random * synthFiles.length).toInt)
    
    println(s"//Synth - ${f1.getName} + ${f2.getName}")
    
    val o1 = getSynthFromFile(f1)
    val o2 = getSynthFromFile(f2)
    
    SynthOrganismDef.mate(o1, o2)
  }
  
  def getGoodSynthFromSavedDefs():(Organism[SynthOrganismDef.type],SynthDef) = {
    var org:Organism[SynthOrganismDef.type] = null
    var synthDef = SynthDef(None, None, Seq(), Seq())
    
    while(synthDef.ugens.filter(_.isDefined).size == 0) {
      org = getSynthFromSavedDefs()
      synthDef = SynthOrganism.getSynthDefinition(org)
    }
    
    (org,synthDef)
  }
  
  def saveSynthDef(name:String, org:Organism[SynthOrganismDef.type]):Unit = {
    val orgSerializer = new OrganismSerializer()
    val orgFile = new File("sc-orgs/synthdef", name+".org")
    if(orgFile.exists()){
      orgFile.delete()
    }
    
    val orgfos = new FileOutputStream(orgFile)
    try{
      orgSerializer.serializeOrganismChromosomes(org.chromosomes, orgfos)
    }
    finally{
      orgfos.close()
    }
  }
  
  def main(args:Array[String]){
    
    for(i <- 1 until 5) {
      val (org,synthDef) = getGoodSynthFromSavedDefs()
      saveSynthDef("n-"+i, org)
      println(writeSynthDef(synthDef, "inst"+i, true)+".add;")
    }
    
  }
}