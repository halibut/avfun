package avfun.supercollider

import avfun.nnviz.ga.Organism
import avfun.nnviz.ga.Chromosome
import avfun.supercollider.SynthOrganism.SynthChromosomeDef
import avfun.supercollider.SynthOrganism.UGenChromosomeDef
import avfun.supercollider.SynthOrganism.SynthOrganismDef

import avfun.supercollider.model._

import scala.collection.Seq

object SynthDefs {
  
  object Drums {
        
    val kick = 
      SynthDef(None, None, Seq(
        Some(UGenDef(UGenSin(), 1f, StaticFreq(60f), EnvelopePerc(0.05f, 0.5f), None)),
        Some(UGenDef(UGenWhiteNoise(), 1f, StaticFreq(1500f), EnvelopePerc(0.001f, 0.05f), Some(LowPassFilter(1500f)))),
        None,
        None)
        , UGenVars())
      
    
    val closedHat = 
      SynthDef(None, None, Seq(
        Some(UGenDef(UGenWhiteNoise(), 1f, StaticFreq(4000), EnvelopePerc(0.001f, 0.1f), Some(BandPassFilter(2000f, 6000f)))),
        None,
        None,
        None)
        , UGenVars())

    
    val openHat = 
      SynthDef(None, None, Seq(
        Some(UGenDef(UGenWhiteNoise(), 1f, StaticFreq(4000), EnvelopePerc(0.001f, 0.3f), Some(BandPassFilter(2000f, 6000f)))),
        None,
        None,
        None)
        , UGenVars())
      
    
    val snare = 
      SynthDef(None, None, Seq(
        Some(UGenDef(UGenPulse(), 1f, StaticFreq(100f), EnvelopePerc(0.001f, 0.2f), Some(LowPassFilter(1000f)))),
        Some(UGenDef(UGenWhiteNoise(), 1f, StaticFreq(1500f), EnvelopePerc(0.001f, 0.2f), Some(BandPassFilter(500f, 1000f)))),
        None,
        None)
        , UGenVars())

        
    val clap = 
      SynthDef(None, None, Seq(
        Some(UGenDef(UGenWhiteNoise(), 1f, StaticFreq(7500), EnvelopePerc(0.001f, 0.6f), Some(BandPassFilter(7500, 15000f)))),
        None,
        None,
        None)
        , UGenVars())
      

  }
  
}