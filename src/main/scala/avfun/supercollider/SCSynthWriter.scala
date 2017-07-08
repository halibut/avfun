package avfun.supercollider

import avfun.supercollider.SynthOrganism.SynthOrganismDef
import avfun.nnviz.ga.OrganismSerializer
import avfun.nnviz.ga.FileSystemPersistence
import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import avfun.nnviz.ga.Organism

import avfun.supercollider.model._

object SCSynthWriter {
  
  def writeSynthDef(synthDef:SynthDef, synthName:String, withComments:Boolean):String = {
    val longestUGen = synthDef.longestUGen.get.envelopeType.minTime
    val hasSustain = synthDef.ugens.filter(_.isDefined).map(_.get).exists(_.envelopeType == EnvelopeADSR)
    
    val sb = new StringBuilder
    
    sb.append("SynthDef(\"").append(synthName).append("\", { arg out = 0, freq = 440, amp = 0.75, pan = 0, gate = 1, timeScale = 1;\n")
    
    //Get the declared 
    val oscWithInd = synthDef.ugens.zipWithIndex.filter(_._1.isDefined).map(x => (x._2, x._1.get))
    
    if(!oscWithInd.isEmpty) {
      //Print out the variable declarations for the oscillators
      sb.append("  var ").append(oscWithInd.map("osc"+_._1).mkString(", ")).append(";\n")
      sb.append("  var ").append(oscWithInd.map("osc"+_._1+"Env").mkString(", ")).append(";\n")
      sb.append("  var ").append(oscWithInd.map("osc"+_._1+"Filter").mkString(", ")).append(";\n")
    }
    
    //Print out some more variable declarations
    sb.append("  var osc, adjFreq, adjAmp;\n")
    
    //Set freq and amp variables to be used by the ugens
    sb.append("  adjFreq = freq;\n");
    sb.append("  adjAmp = amp;\n");
    
    //Optional frequency vibrato
    synthDef.lfoFreq.foreach{lfo =>
      sb.append("  adjFreq = adjFreq + (").append(lfo.amount * 20f).append(" * SinOsc.ar(").append(lfo.speed).append(", Rand(0, 2.0)));\n")
    }
    
    //Optional amplitude (level) vibrato
    synthDef.lfoAmp.foreach{lfo =>
      val amp = lfo.amount
      sb.append("  adjAmp = (adjAmp - ").append(amp * 0.5f)
        .append(") + (").append(amp * 0.5f)
        .append(" * SinOsc.ar(").append(lfo.speed).append(", Rand(0, 2.0)));\n")
    }
    
    for((ind, ugenDef) <- oscWithInd) {
      sb.append("\n")
      val oscName = "osc"+ind;
      if(withComments){
        sb.append("  //UGen-").append(ind).append(" ").append(printUGenComment(ugenDef)).append("\n")
      }
      sb.append("  ").append(oscName).append(" = ").append(printUGenDefinition(ugenDef)).append(";\n")
      sb.append("  ").append(oscName).append("Env = ").append(printUGenEnvDefinition(ugenDef, longestUGen)).append(";\n")
      
      ugenDef.filterType.foreach { filter  =>
        sb.append("  ").append(oscName).append("Filter = ").append(printUGenFilterDefinition(ugenDef, oscName)).append(";\n")
        val amtOrig = 1f - filter.filterAmt
        val amtFilter = filter.filterAmt
        val totalAmp = math.max(amtOrig, amtFilter).toFloat * .75f
        sb.append("  ").append(oscName).append(" = Mix([(").append(oscName).append(" * ").append(math.min(1f, amtOrig / totalAmp))
          .append("), (").append(oscName).append("Filter * ").append(math.min(1f, amtFilter / totalAmp)).append(")]);\n")
      }
      
      sb.append("  ").append(oscName).append(" = ")
        .append(ugenDef.amp).append(" * ").append(oscName).append(" * ").append(oscName).append("Env;\n")
    }
    
    sb.append("\n")
    sb.append("  osc = adjAmp * Mix([").append(oscWithInd.map("osc"+_._1).mkString(", ")).append("]);\n")
    //sb.append("  osc = osc * ").append(printUGenEnvDefinition(oscWithInd.head._2)).append(";\n")
    sb.append("\n")
    sb.append("  Out.ar(out, Pan2.ar(osc, pan));\n")
    sb.append("})");
        
    sb.toString
  }
  
  private def printUGenComment(ugenDef:UGenDef):String = {
    s"${ugenDef}"
  }
  
  private def printUGenDefinition(ugenDef:UGenDef):String = {
    val amp = ugenDef.amp;
    
    val freqDef = ugenDef.freqControl match {
      case sf:StaticFreq => s"${sf.freq}"
      case df:DynamicFreq => s"(adjFreq * ${df.totalFreqMult})"
    }
    
    ugenDef.uGenType match {
      case UGenSin(phase) => {
        s"SinOsc.ar(${freqDef}, ${phase})"
      }
      case UGenPulse(phase) => {
        s"Pulse.ar(${freqDef}, ${phase})"
      }
      case UGenSaw() => {
        s"Saw.ar(${freqDef})"  
      }
      case UGenLFTri(phase) => {
        s"LFTri.ar(${freqDef}, ${4f * phase})"  
      }
      case UGenVarSaw(phase, width) => {
        s"VarSaw.ar(${freqDef}, ${phase}, ${width})"
      }
      case UGenBlip(start, end, time) => {
        s"Blip.ar(${freqDef} , Line.kr(${start},${end},${time},doneAction: 0))"
      }
      case UGenFormant(formFreqMul, bwFreqMul) => {
        s"Formant.ar(${freqDef}, ${formFreqMul} * ${freqDef}, ${bwFreqMul} * ${freqDef})"
      }
      case UGenWhiteNoise() => {
        s"WhiteNoise.ar(1)"
      }
      case _ => {
        "//Not a valid uGen #"
      }
    }
  }
  
  private def printUGenFilterDefinition(ugenDef:UGenDef, oscName:String):String = {
    val freqDef = ugenDef.freqControl match {
      case sf:StaticFreq => s"${sf.freq}"
      case df:DynamicFreq => s"(adjFreq * ${df.totalFreqMult})"
    }
    
    val filter = ugenDef.filterType.get
    
    def relFreq(relative:Boolean, freq:Float, followEnv:Option[Float]):String = {
      val freqStr = if(relative) s"(${freqDef} + ${freq})" else s"${math.abs(freq)}"
      
      followEnv match {
        case None => freqStr
        case Some(f) => s"${freqStr} * (1 + (${oscName}Env * ${f}))" 
      }
    }
    
    filter match {
      case LowPassFilter(freq, relative, followEnv, _) => 
        s"LPF.ar(${oscName}, ${relFreq(relative, freq, followEnv)})"
      case HighPassFilter(freq, relative, followEnv, _) => 
        s"HPF.ar(${oscName}, ${relFreq(relative, -freq, followEnv)})"
      case BandPassFilter(lowFreq, highFreq, relative, followEnv, _) => 
        s"LPF.ar(HPF.ar(${oscName}, ${relFreq(relative, lowFreq, followEnv)}), ${relFreq(relative, -highFreq, followEnv)})"
      case FormletFilter(resFreq, attackTime, decayTime, _) =>
        s"Formlet.ar(${oscName}, ${freqDef} * ${resFreq}, ${attackTime}, ${decayTime})"
      case MoogFilter(cutoffFreq, resAmt, followEnv, _) => 
        s"MoogFF.ar(${oscName}, ${relFreq(true, cutoffFreq, followEnv)}, ${resAmt})"
      case _ => "//Not a valid Filter"
    }
  }
  
  private def printUGenEnvDefinition(ugenDef:UGenDef, minTime:Float):String = {
    val timeDiff = minTime - ugenDef.envelopeType.minTime
    
    val doneAction = if(ugenDef.envelopeType.hasSustain) {
      2
    } else if(!ugenDef.envelopeType.hasSustain && timeDiff <= 0f) {
      2
    } else {
      0
    }
    
    ugenDef.envelopeType match {
      case adsr:EnvelopeADSR => {
        val a = adsr.attackTime
        val d = adsr.decayTime
        val s = adsr.sustainLevel
        val r = adsr.releaseTime
    
        s"EnvGen.ar(Env.adsr(${a}, ${d}, ${s}, ${r + timeDiff}, 1), gate, timeScale: timeScale, doneAction: ${doneAction})"
      }
      case perc:EnvelopePerc => {
        val a = perc.attackTime
        val r = perc.releaseTime
        
        s"EnvGen.ar(Env.perc(${a}, ${r}), gate, timeScale: timeScale, doneAction: ${doneAction})"
      }
      case _ => {
        "//Not a valid Envelope"
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
    //val synthdefsDir = new File("sc-orgs/synthdef")
    
    val synthFiles = new File("/tmp/sc/orgs/synths").listFiles()
    
    val o1 = if(synthFiles.length == 0) {
      SynthOrganismDef.randomize
    } else {
      getSynthFromFile(synthFiles((math.random * synthFiles.length).toInt))
    }
    
    val o2 = if(synthFiles.length < 10) {
      SynthOrganismDef.randomize
    } else {
      getSynthFromFile(synthFiles((math.random * synthFiles.length).toInt))
    }
        
    SynthOrganismDef.mate(o1, o2)
  }
  
  def getGoodSynthFromSavedDefs():(Organism[SynthOrganismDef.type],SynthDef) = {
    var org:Organism[SynthOrganismDef.type] = null
    var synthDef = SynthDef(None, None, Seq(), UGenVars())
    
    while(!synthDef.isValid 
        || synthDef.ugens.filter(_.isDefined).exists(_.get.uGenType.isInstanceOf[UGenWhiteNoise])
        || synthDef.ugens.filter(_.isDefined).exists(_.get.filterType.map(_.isInstanceOf[HighPassFilter]).getOrElse(false))
        || synthDef.ugens.filter(_.isDefined).exists(_.get.filterType.map(_.isInstanceOf[BandPassFilter]).getOrElse(false))
        || synthDef.ugens.filter(_.isDefined).exists(_.get.freqControl.isInstanceOf[StaticFreq])) {
      org = getSynthFromSavedDefs()
      synthDef = SynthOrganism.getSynthDefinition(org)
    }
    
    (org,synthDef)
  }
  
  def saveSynthDef(name:String, org:Organism[SynthOrganismDef.type], dir:String = "/tmp/sc/orgs/synths"):Unit = {
    val orgSerializer = new OrganismSerializer()
    val orgFile = new File(dir, name+".org")
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
    
//    println(writeSynthDef(SynthDefs.Drums.kick, "kick", true)+".add;")
//    println(writeSynthDef(SynthDefs.Drums.openHat, "open-hat", true)+".add;")
//    println(writeSynthDef(SynthDefs.Drums.closedHat, "closed-hat", true)+".add;")
//    println(writeSynthDef(SynthDefs.Drums.snare, "snare", true)+".add;")
//    println(writeSynthDef(SynthDefs.Drums.clap, "clap", true)+".add;")
    
    for(i <- 1 until 5) {
      val (org,synthDef) = getGoodSynthFromSavedDefs()
      saveSynthDef("n-"+i, org)
      println(writeSynthDef(synthDef, "inst"+i, true)+".add;")
    }
    
  }
}