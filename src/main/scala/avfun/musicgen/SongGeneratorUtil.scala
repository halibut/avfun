package avfun.musicgen

import avfun.supercollider.PatternStructureOrganism.PatternStructureOrganismDef
import avfun.nnviz.ga.Organism

import java.io.File
import rapture.json._
import rapture.json.jsonBackends.spray._
import rapture.json.formatters.humanReadable._
import java.nio.file.Paths
import java.nio.file.Files
import avfun.nnviz.ga.OrganismSerializer
import avfun.nnviz.ga.OrganismSerializer
import java.io.ByteArrayInputStream
import avfun.nnviz.ga.OrganismDef
import java.io.ByteArrayOutputStream
import java.nio.file.Path
import avfun.nnviz.ga.OrganismDef
import avfun.supercollider.SynthOrganism.SynthOrganismDef
import avfun.supercollider.model.UGenWhiteNoise
import avfun.supercollider.model.HighPassFilter
import avfun.supercollider.model.BandPassFilter
import avfun.supercollider.model.StaticFreq
import avfun.supercollider.InstrumentPatternOrganism.InstrumentPatternOrganismDef
import avfun.supercollider.model.SongDef
import avfun.supercollider.SongStructureOrganism.SongStructureOrganismDef
import avfun.nnviz.nn.FastNetwork
import avfun.supercollider.model.PatternDef
import avfun.supercollider.model.NoteEvent
import avfun.supercollider.integ.SCClient
import avfun.supercollider.integ.SCSoundRecorder
import avfun.supercollider._
import avfun.supercollider.model.SynthDef

class SongGeneratorUtil(baseDir:String, songs:Int, songStructureOrganisms:Int, patternOrganisms:Int, synthOrganisms:Int, instrumentPatternOrganisms:Int) {
  
  private val basePath = Paths.get(baseDir)
  if(!basePath.toFile().exists()) {
    basePath.toFile().mkdirs()
  }
  
  private val genInfoFile = "gen-info.json"
  private val songStructFitFile = "song-structure-fitness.json"
  private val patternStructFitFile = "pattern-structure-fitness.json"
  private val synthDefFitFile = "synth-def-fitness.json"
  private val instrumentPatternFitFile = "instrument-pattern-fitness.json"
  
  private val songsPath = basePath.resolve("songs")
  if(!songsPath.toFile().exists()) {
    songsPath.toFile.mkdirs();
  }
  
  private val topSongsPath = basePath.resolve("top-songs")
  if(!topSongsPath.toFile().exists()) {
    topSongsPath.toFile.mkdirs();
  }
  
  private val songStructsPath = basePath.resolve("song-struct-organisms")
  if(!songStructsPath.toFile().exists()) {
    songStructsPath.toFile().mkdirs()
  }
  private val patternStructsPath = basePath.resolve("pattern-struct-organisms")
  if(!patternStructsPath.toFile().exists()) {
    patternStructsPath.toFile().mkdirs()
  }
  private val synthDefsPath = basePath.resolve("synth-def-organisms")
  if(!synthDefsPath.toFile().exists()) {
    synthDefsPath.toFile().mkdirs()
  }
  private val instrumentPattensPath = basePath.resolve("instrument-pattern-organisms")
  if(!instrumentPattensPath.toFile().exists()) {
    instrumentPattensPath.toFile().mkdirs()
  }
  
  private val organismSerializer = new OrganismSerializer
  
  private def randSongPart():PatternType = {
    val rndInd = (math.random * 500).toInt % 5
    
    val songPart = rndInd match {
        case 0 => Intro()
        case 1 => Outro()
        case 2 => Melody()
        case 3 => Chorus()
        case 4 => Break()
      }
    
    songPart
  }
  
  private def randomSongStructureOrganisms(count:Int):Seq[Organism[SongStructureOrganismDef.type]] = {
    for(i <- 0 until count) yield {
      var o = SongStructureOrganismDef.randomize
      while(!validSongStructure(o)) {
        o = SongStructureOrganismDef.randomize
      }
      o
    }
  }
  
  private def validSongStructure(o:Organism[SongStructureOrganismDef.type]):Boolean = {
    var songStructure = SongStructureOrganism.getSongStructure(o)
    
    val parts = Seq(songStructure.hasBreak, songStructure.hasChorus, songStructure.hasIntro, songStructure.hasOutro)
      .filter(_ == true).size
    
    (parts >= 4 
        && Seq(2, 3, 4, 5, 6).exists(_ == songStructure.noteDiv) 
        && songStructure.instruments >= 3 && songStructure.instruments <= 6)
  }
  
  private def patternStructureFitnessByInst(o:Organism[PatternStructureOrganismDef.type]):Float = {
      val notes = (0 until 20).map{ i =>
        val songNoteDiv = 4;
        val inst = (math.random * 16).toInt
        val instType = inst % 4;
        val songPart = randSongPart();
        
        val pInput = PatternInput(songPart, inst, (math.random * 16).toInt, songNoteDiv)
        val ps = PatternStructureOrganism.getPatternStructure(o, pInput)
        
        instType match {
          case 0 => (if(ps.noteDiv == songNoteDiv)   1 else 0)  + (if(ps.noteOffsetRange <= 12) 1 else 0)
          case 1 => (if(ps.noteDiv == songNoteDiv)   1 else 0)  + (if(ps.noteOffsetRange <= 6) 1 else 0)
          case 2 => (if(ps.noteDiv > songNoteDiv)    1 else 0)  + (if(ps.noteOffsetRange >= 12) 1 else 0)
          case 3 => (if(ps.noteDiv >= songNoteDiv*2) 1 else 0)  + (if(ps.noteOffsetRange >= 12) 1 else 0)
        }
      }
      notes.sum
  }
  
  private def patternStructureFitnessByVariation(o:Organism[PatternStructureOrganismDef.type]):Float = {
      val notes = (0 until 20).map{ i =>
        val songPart = randSongPart();
        val pInput = PatternInput(songPart, (math.random * 16).toInt, (math.random * 16).toInt, (math.random * 16).toInt)
        
        val ps = PatternStructureOrganism.getPatternStructure(o, pInput)
        (ps.noteDiv, ps.noteOffsetRange)
      }
      
      notes.sliding(2).map(s => (if(s(0)._1 != s(1)._1) 1 else 0) + (if(s(0)._2 != s(1)._2) 1 else 0)).sum
  }
  
  private def randomPatternStructureOrganisms(count:Int):Seq[Organism[PatternStructureOrganismDef.type]] = {
    val trainedOrgs = FastOrganismTrainer.train(PatternStructureOrganismDef, Math.max(count, 30), 25){ o =>
  
      patternStructureFitnessByInst(o) +
        patternStructureFitnessByVariation(o)
    }
    
    trainedOrgs.take(count)
  }
  
  private def randomSynthOrganisms(count:Int):Seq[Organism[SynthOrganismDef.type]] = {
    for(i <- 0 until count) yield {
      var o = SynthOrganismDef.randomize
      while(!validSynthOrganism(o)) {
        o = SynthOrganismDef.randomize
      }
      o
    }
  }
  private def validSynthOrganism(o:Organism[SynthOrganismDef.type]):Boolean = {
    val synthDef = SynthOrganism.getSynthDefinition(o)
    synthDef.isValid && !(
        synthDef.ugens.filter(_.isDefined).exists(_.get.uGenType.isInstanceOf[UGenWhiteNoise])
          || synthDef.ugens.filter(_.isDefined).exists(_.get.filterType.map(_.isInstanceOf[HighPassFilter]).getOrElse(false))
          || synthDef.ugens.filter(_.isDefined).exists(_.get.filterType.map(_.isInstanceOf[BandPassFilter]).getOrElse(false))
          || synthDef.ugens.filter(_.isDefined).exists(_.get.freqControl.isInstanceOf[StaticFreq])
    )
  }

  private def randomInstrumentPatternOrganisms(count:Int):Seq[Organism[InstrumentPatternOrganismDef.type]] = {
    val trainedOrgs = FastOrganismTrainer.train(InstrumentPatternOrganismDef, Math.max(count, 30), 25){ o =>
    val notes = (0 until 20).map{ i =>
      val songPart = randSongPart();
      
      val pInput = PatternInput(songPart, (math.random * 16).toInt, (math.random * 16).toInt, (math.random * 16).toInt)
      val structure = PatternStructureOrganism.getPatternStructure(PatternStructureOrganismDef.randomize, pInput).copy(noteOffsetRange = 16)
      
      val pattern = InstrumentPatternOrganism.getPattern(o, structure, pInput)
      
      val numNotesOn = pattern.notes.filter(_.isDefined).size
      
      val notesOn = numNotesOn.toFloat / pattern.notes.size.toFloat
      
      val sliding4NoteDiff = 
        pattern.notes.filter(_.isDefined).map(_.get._2).sliding(4).map{x => x.distinct.size.toFloat }.sum / 4f
        
      val arpDiff = 
        pattern.notes.filter(_.isDefined).map(_.get._2).sliding(2).map{x =>
          if(x.size == 2) {
            val diff = math.abs(x(1) - x(0))
            if(diff == 1 || diff == 2) {
              1f
            }
            else if(diff == 3 || diff == 4) {
              0.5f
            }
            else if(diff != 0) {
              0.25f
            }
            else {
              0f
            }
          }
          else {
            0f
          }
        }.sum
      
      
      (sliding4NoteDiff + arpDiff) / pattern.notes.size.toFloat
    }
    
    notes.sum
    }
    
    trainedOrgs.take(count)
  }
  
  private def loadOrganismFromFile[D <: OrganismDef](path:Path, fileName:String, orgDef:D): Organism[D] = {
    val patStructFile = new ByteArrayInputStream(Files.readAllBytes(path.resolve(fileName)))
    val chromosomes = organismSerializer.deserializeOrganismCoromosomes(orgDef, patStructFile)

    val organism = new Organism[D](
      id = 0L,
      chromosomes = chromosomes, 
      None, None)(orgDef)
      
    organism
  }
  
  private def getRandomOrganism[D <: OrganismDef](fitFile:String, orgsPath:Path, orgDef:D):(String,Organism[D]) = {
    val fitStore = getFitnessStore(fitFile)
    val ind = Math.max(0, Math.min((Math.random() * fitStore.orgsAndFitnesses.size).toInt, fitStore.orgsAndFitnesses.size-1)) 
    val orgFile = fitStore.orgsAndFitnesses(ind).org
    
    val organism = loadOrganismFromFile(orgsPath, orgFile, orgDef)
    
    (orgFile, organism)
  }
  
  private def saveOrganismToFile[D <: OrganismDef](path:Path, fileName:String, org:Organism[D]):Unit  = {
    val bos = new ByteArrayOutputStream();
    organismSerializer.serializeOrganismChromosomes(org.chromosomes, bos)
    bos.close()
    
    Files.write(path.resolve(fileName), bos.toByteArray())
  }
  
  private def deleteOrganismFile(path:Path, fileName:String):Unit  = {
    Files.deleteIfExists(path.resolve(fileName))
  }
  
  private def createNextGenOrganisms[D <: OrganismDef](fitFile:String, orgsPath:Path, gen:Int, count:Int, keep:Int, 
      orgDef:D, mateFunc:(Organism[D],Organism[D])=>Organism[D], ifMissing:() => Seq[Organism[D]],
      validFunc: (Organism[D])=>Boolean):Unit = {
    
    val orgs = getFitnessStore(fitFile).orgsAndFitnesses.sortBy(_.fit).reverse
    val newOrgs = if(orgs.isEmpty) {
      val randomOrgs = ifMissing().zipWithIndex.map{case (org, ind) =>
        val name = s"g${gen}-i${ind}.organism"
        saveOrganismToFile(orgsPath, name, org)
        OrganismAndFitness(name, 0)
      }
      randomOrgs
    }
    else{
      val (keepOrgs, killOrgs) = orgs.splitAt(5)
      
      val matedOrgs = for(i <- 0 until (count-keepOrgs.size)) yield {
        var org:Organism[D] = null
        
        while(org == null || !validFunc(org)) {
          val position1 = Math.pow(Math.random(), 2)
          val ind1 = Math.max(0, Math.min(orgs.size-1, (position1 * orgs.size).toInt))
          val orgFileName1 = orgs(ind1).org
          
          val position2 = Math.pow(Math.random(), 2)
          val ind2 = Math.max(0, Math.min(orgs.size-1, (position2 * orgs.size).toInt))
          val orgFileName2 = orgs(ind2).org
          
          val p1 = loadOrganismFromFile(orgsPath, orgFileName1, orgDef)
          val p2 = loadOrganismFromFile(orgsPath, orgFileName2, orgDef)
          
          org = mateFunc(p1,p2)
        }
        
        val name = s"g${gen}-i${i}.organism"
        saveOrganismToFile(orgsPath, name, org)
        OrganismAndFitness(name, 0)
      }
      
      killOrgs.foreach{ case OrganismAndFitness(org,fit) =>
        deleteOrganismFile(orgsPath, org)
      }
      
      keepOrgs ++ matedOrgs
    }
    
    writeFitnessStore(fitFile, OrganismFitnessStore(newOrgs))
  }
  
  private def createAllNextGenOrganisms(gen:Int, keep:Int):Unit = {
    createNextGenOrganisms(songStructFitFile, songStructsPath, gen, songStructureOrganisms, keep, SongStructureOrganismDef,
        SongStructureOrganismDef.mate,
        () => randomSongStructureOrganisms(songStructureOrganisms),
        (o:Organism[SongStructureOrganismDef.type]) => validSongStructure(o)
        
    )
    
    createNextGenOrganisms(patternStructFitFile, patternStructsPath, gen, patternOrganisms, keep, PatternStructureOrganismDef,
        PatternStructureOrganismDef.mate,
        () => randomPatternStructureOrganisms(patternOrganisms), 
        (o:Organism[PatternStructureOrganismDef.type]) => true
    )
    
    createNextGenOrganisms(synthDefFitFile, synthDefsPath, gen, synthOrganisms, keep, SynthOrganismDef,
        SynthOrganismDef.mate,
        () => randomSynthOrganisms(synthOrganisms),
        (o:Organism[SynthOrganismDef.type]) => validSynthOrganism(o)
    )
    
    createNextGenOrganisms(instrumentPatternFitFile, instrumentPattensPath, gen, instrumentPatternOrganisms, keep, InstrumentPatternOrganismDef,
        InstrumentPatternOrganismDef.mate,
        () => randomInstrumentPatternOrganisms(instrumentPatternOrganisms),
        (o:Organism[InstrumentPatternOrganismDef.type]) => true
    )
  }
  
  private def generateSongWithCurrentOrganisms(name:String):(SongDef, GenSongInfo) = {
    
    val (songOrgName, songOrg) = getRandomOrganism(songStructFitFile, songStructsPath, SongStructureOrganismDef)
    val songStructure = SongStructureOrganism.getSongStructure(songOrg)
   
    val synths = Math.min(4, songStructure.instruments)
    
    val instrumentPatterns = for(i <- 0 until songStructure.instruments) yield {
      val (instPatternOrgName, instPatternOrg) = getRandomOrganism(instrumentPatternFitFile, instrumentPattensPath, InstrumentPatternOrganismDef)
      
      (instPatternOrgName, InstrumentPatternOrganismDef.expressPhenotype(instPatternOrg))
    }
    
    val (patternOrgName, patternOrg) = getRandomOrganism(patternStructFitFile, patternStructsPath, PatternStructureOrganismDef)
    
    val synthOrgs = (0 until synths).map{ synthInd => 
      getRandomOrganism(synthDefFitFile, synthDefsPath, SynthOrganismDef)
    }
    
    val scale = new MajorScale(4, MusicScale.A + songStructure.scaleOffsetFromA4)
    val rootNote = scale.originNote
    
    val numInstruments = instrumentPatterns.size
    
    val songSynthPatternsInput = (0 until synthOrgs.size).map{ synthInd => 
      val (synthFileName, synthOrg) = synthOrgs(synthInd)
      
      val cleanSynthFileName = synthFileName.takeWhile(_ != '.').replaceAll("[-_ ]", "")      
      val synthName = "inst"+cleanSynthFileName
      
      val synthDef = SynthOrganism.getSynthDefinition(synthOrg)
      
      (synthInd, synthName, synthDef)
    }
    
    val songDef = createSongDef(songStructure, patternOrg, instrumentPatterns, songSynthPatternsInput)
    
    val genSongInfo = GenSongInfo(
      songFile = name,
      songStructureOrg = songOrgName,
      patternStructureOrg = patternOrgName,
      synthOrgs = synthOrgs.map(_._1),
      instrumentPatternOrgs = instrumentPatterns.map(_._1),
      overallRank = None,
      songStructureRank = None,
      patternStructureRank = None,
      synthsRank = None,
      instrumentPatternsRank = None,
      keep = None
    )
    
    (songDef, genSongInfo)
  }
  
  private def createSongDef(songStructure:SongStructure, patternOrg:Organism[PatternStructureOrganismDef.type], instrumentPatterns:Seq[(String,FastNetwork)], songSynthPatternsInput:Seq[(Int, String, SynthDef)]):SongDef = {
    val scale = new MajorScale(4, MusicScale.A + songStructure.scaleOffsetFromA4)
    val rootNote = scale.originNote
    
    val numInstruments = songStructure.instruments
    
    val synths = Math.min(4, songStructure.instruments)
    
    val songSynthPatterns = songSynthPatternsInput.map{ case(synthInd, synthName, synthDef) => 
      val instPatterns = for{
        insInd <- 0 until numInstruments;
        if (insInd % synths) == synthInd
      } yield {
        getInstrumentMelody(insInd, instrumentPatterns(insInd)._2, scale, rootNote, patternOrg, songStructure)
      }
      
      (synthName, synthDef, instPatterns)
    }
    
    var songDef = SongDef(songStructure.bpm, songStructure.noteDiv, songSynthPatterns)
    
    val dense = songDef.notes.map(_._3.map(p => p.notes.size / p.length).max).max / songDef.noteLengthMult
    if(dense > 8f) {
      songDef = songDef.copy(bpm = songDef.bpm / 2)
    }
    songDef
  }
  
  def getSongDef(genSongInfo:GenSongInfo):SongDef = {
    val songStructOrg = loadOrganismFromFile(songStructsPath, genSongInfo.songStructureOrg, SongStructureOrganismDef)
    val songStructure = SongStructureOrganism.getSongStructure(songStructOrg)
    
    val patternOrg = loadOrganismFromFile(patternStructsPath, genSongInfo.patternStructureOrg, PatternStructureOrganismDef)
    
    val synthOrgs = genSongInfo.synthOrgs.map{ s =>
      (s, loadOrganismFromFile(synthDefsPath, s, SynthOrganismDef))
    }
    
    val instrumentPatterns = genSongInfo.instrumentPatternOrgs.map{ p =>
      val org = loadOrganismFromFile(instrumentPattensPath, p, InstrumentPatternOrganismDef)
      (p, InstrumentPatternOrganismDef.expressPhenotype(org))
    }
    
    val songSynthPatternsInput = (0 until synthOrgs.size).map{ synthInd => 
      val (synthFileName, synthOrg) = synthOrgs(synthInd)
      
      val cleanSynthFileName = synthFileName.takeWhile(_ != '.').replaceAll("[-_ ]", "")      
      val synthName = "inst"+cleanSynthFileName
      
      val synthDef = SynthOrganism.getSynthDefinition(synthOrg)
      
      (synthInd, synthName, synthDef)
    }
    
    createSongDef(songStructure, patternOrg, instrumentPatterns, songSynthPatternsInput)
    
  }
  
  
  private def genPattern(
    instrumentInd: Int, bars:Int, repeat:Int, 
    patternType: PatternType, scale: MusicScale, rootNote:Int, songNoteDiv:Int,
    patternNet:FastNetwork,
    patternStructureOrg:Organism[PatternStructureOrganismDef.type]) : PatternDef = {
    
    val length = (bars*repeat).toFloat
    var noteTime = 0f
    
    //Create a pattern for each bar and then append them together
    val barPatterns = for(b <- 0 until bars) yield {
      val patternInput = PatternInput(patternType, b+1, instrumentInd, songNoteDiv)
      val patternStructure = PatternStructureOrganism.getPatternStructure(patternStructureOrg, patternInput);
      val pattern = InstrumentPatternOrganism.getPatternFromNet(patternNet, patternStructure, patternInput)
      val lenMult = patternStructure.noteLength
      
      val notePattern = pattern.notes.zipWithIndex.filter(_._1.isDefined).map(o=>(o._1.get, o._2)).map{ case ((len,midiNote),ind) =>
        val realLen = len * lenMult;
        val freq = MusicScale.midiToFreq(scale.getOffset(rootNote, midiNote))
        NoteEvent(noteTime + ind * lenMult, freq, realLen, 0f) 
      }
      
      noteTime += 1f
      
      notePattern
    }
    
    val patternLength = bars.toFloat
    
    val repeated = for(r <- 0 until repeat; p <- barPatterns) yield (p, r)
    
    val timeCorrectedRepeated = repeated.map(p => p._1.map(n => n.copy(time = n.time + (p._2*patternLength)))).flatten
    
    val uncondensed = timeCorrectedRepeated
    
    if(!uncondensed.isEmpty) {
      //Combine \\rests if they are next to each other
      //Remove useless rests and notes (that have duration of 0.0
      var condensed = Seq[NoteEvent](uncondensed.head)
      for(ne <- uncondensed.tail) {
        val last = condensed.last
        if(last.freq == ne.freq) {
          val noteStartDiff = last.time - ne.time
          val prevNoteLenDiff = noteStartDiff - last.dur
          if(-0.0001f < prevNoteLenDiff && prevNoteLenDiff <= 0.0001f) {
            condensed = condensed.dropRight(1) :+ last.copy(dur = last.dur + noteStartDiff)
          }
          else {
            condensed :+= ne
          }
        }
        else {
          condensed :+= ne
        }
      }
    
      PatternDef(condensed, (bars*repeat).toFloat)
    }
    else {
      PatternDef(uncondensed, (bars*repeat).toFloat)
    }
  }
  
  private def getInstrumentMelody(insInd:Int, instrumentPattern:FastNetwork,
      scale:MusicScale, rootNote:Int, patternOrg:Organism[PatternStructureOrganismDef.type],
      songStructure:SongStructure):PatternDef = {
    val ins = instrumentPattern
    val noteDiv = songStructure.noteDiv
    
    val intro = genPattern(insInd, 4, 2, Intro(), scale, rootNote, noteDiv, ins, patternOrg)
    val melody1 = genPattern(insInd, 8, 2, Melody(), scale, rootNote, noteDiv, ins, patternOrg)
    val chorus1 = genPattern(insInd, 8, 2, Chorus(), scale, rootNote, noteDiv, ins, patternOrg)
    val melody2 = melody1
    val break = genPattern(insInd, 16, 1, Break(), scale, rootNote, noteDiv, ins, patternOrg)
    val chorus2 = chorus1
    val outro = genPattern(insInd, 4, 2, Outro(), scale, rootNote, noteDiv, ins, patternOrg)
    
    
    val fullSongForIns = Seq(intro, melody1, chorus1, melody2, break, chorus2, outro)
      .reduceLeft((p,x) => p append x)
    
    fullSongForIns
  }
  
  def createNextGenSongs(scClient:SCClient):Unit = {
    val genSongInfo = getGenSongInfo(genInfoFile)
    if(genSongInfo.gen > 0){
      require(genSongInfo.songs.map(_.overallRank).forall(_.isDefined), "You have not ranked the songs!")
      
      updateOrganismFitnesses(genSongInfo.songs)
    }
    
    //Save the "good" songs
    genSongInfo.songs.filter(_.keep.getOrElse(false)).foreach{song =>
      Files.copy(
          songsPath.resolve(song.songFile),
          topSongsPath.resolve(song.songFile)
      )
    }
    
    //Delete the other songs
    genSongInfo.songs.foreach{ song =>
      Files.deleteIfExists(songsPath.resolve(song.songFile))
    }
    
    val nextGen = genSongInfo.gen+1
    
    //Generate a new generation of song organisms
    createAllNextGenOrganisms(nextGen, 5)
    
    //Record the songs
    val s = for( i <- 0 until songs) yield {
      val songName = s"gen-${nextGen}-song-${i+1}.wav"
      val (songDef, songInfo) = generateSongWithCurrentOrganisms(songName)
      
      SCSoundRecorder.recordSong(songDef, songName, songsPath.toFile)(scClient)
      
      songInfo
    }
    
    //Save song metadata
    val newGenSongInfo = GenSongs(nextGen, s)
    writeGenSongInfo(genInfoFile, newGenSongInfo)
  }
  
  def updateOrganismFitnesses(ratedSongs:Seq[GenSongInfo]):Unit = {
    {
      val songStructDiffs = ratedSongs.groupBy(s => s.songStructureOrg).map{ case (songStruct, songs) =>
        songStruct -> songs.map(s => s.overallRank.getOrElse(0) + s.songStructureRank.getOrElse(0)).sum
      }
      val songStructFitStore = getFitnessStore(songStructFitFile)
      val updatedSongStructFitStore = updateFitnessStore(songStructFitStore, songStructDiffs)
      writeFitnessStore(songStructFitFile, updatedSongStructFitStore)
    }
    {
      val patStructDiffs = ratedSongs.groupBy(s => s.patternStructureOrg).map{ case (patternStruct, songs) =>
        patternStruct -> songs.map(s => s.overallRank.getOrElse(0) + s.patternStructureRank.getOrElse(0)).sum
      }
      val patFitStore = getFitnessStore(patternStructFitFile)
      val updatedPatFitStore = updateFitnessStore(patFitStore, patStructDiffs)
      writeFitnessStore(patternStructFitFile, updatedPatFitStore)
    }
    
    {
      val synthDiffs = ratedSongs
        .map(s => s.synthOrgs.map(sy => (sy, s.overallRank.getOrElse(0) + s.synthsRank.getOrElse(0))))
        .flatten
        .groupBy(s => s._1)
        .map{ case (synth, ranks) =>
          synth -> ranks.map(_._2).sum
        }
      val synthFitStore = getFitnessStore(synthDefFitFile)
      val updatedSynthFitStore = updateFitnessStore(synthFitStore, synthDiffs)
      writeFitnessStore(synthDefFitFile, updatedSynthFitStore)
    }
    
    {
      val instrumentPatDiffs = ratedSongs
        .map(s => s.instrumentPatternOrgs.map(sy => (sy, s.overallRank.getOrElse(0) + s.instrumentPatternsRank.getOrElse(0))))
        .flatten
        .groupBy(s => s._1)
        .map{ case (pat, ranks) =>
          pat -> ranks.map(_._2).sum
        }
      val instFitStore = getFitnessStore(instrumentPatternFitFile)
      val updatedInstPatFitStore = updateFitnessStore(instFitStore, instrumentPatDiffs)
      writeFitnessStore(instrumentPatternFitFile, updatedInstPatFitStore)
    }
  }
  
  private def getFitnessStore(file:String):OrganismFitnessStore = {
    val fitStoreFile = basePath.resolve(file)
    
    if(!fitStoreFile.toFile().exists()) {
      OrganismFitnessStore(Seq())
    }
    else{
      val fitStoreStr = new String(Files.readAllBytes(fitStoreFile))
      val fitStore = Json.parse(fitStoreStr).as[OrganismFitnessStore]
      fitStore
    }
  }
  
  private def writeFitnessStore(file:String, fitStore:OrganismFitnessStore):Unit = {
    val fitStoreFile = basePath.resolve(file)
    
    val fitSToreJson = Json(fitStore)
    
    Files.write(fitStoreFile, Json.format(fitSToreJson).getBytes)
    
  }
  
  private def updateFitnessStore(store:OrganismFitnessStore, updatedFits:Map[String, Int]):OrganismFitnessStore = {
    OrganismFitnessStore(
      store.orgsAndFitnesses.map{ case OrganismAndFitness(org, fit) =>
        OrganismAndFitness(org, fit + updatedFits.get(org).getOrElse(0))
      }
    )
  }
  
  private def getGenSongInfo(file:String):GenSongs = {
    val fitStoreFile = basePath.resolve(file)
    
    if(!fitStoreFile.toFile().exists()) {
      GenSongs(0, Seq())
    }
    else{
      val genSongsStr = new String(Files.readAllBytes(fitStoreFile))
      val genSongs = Json.parse(genSongsStr).as[GenSongs]
      genSongs
    }
  }
  
  def writeGenSongInfo(file:String, genSongs:GenSongs):Unit = {
    val genSongInfoFile = basePath.resolve(file)
    
    val genSongInfoFileJson = Json(genSongs)
    
    Files.write(genSongInfoFile, Json.format(genSongInfoFileJson).getBytes)
  }
  
  def getCurrentGenSongs():GenSongs = {
    getGenSongInfo(genInfoFile)
  }
  
  def saveCurrentGenSongs(genSongs:GenSongs):Unit = {
    val genFile = basePath.resolve(genInfoFile)
    val tmpFile = basePath.resolve(genInfoFile+".bak")
    Files.copy(genFile, tmpFile)
    
    try{
      writeGenSongInfo(genInfoFile, genSongs)
    }
    catch{
      case t:Throwable => {
        Files.copy(tmpFile, genFile)
      }
    }
    finally{
      Files.deleteIfExists(tmpFile)
    }
  }
  
  def getFullSongPath(songInfo:GenSongInfo):Path = {
    songsPath.resolve(songInfo.songFile)
  }
  
}

case class GenSongs( gen:Int, songs:Seq[GenSongInfo])

case class GenSongInfo(
    songFile:String,
    songStructureOrg:String,
    patternStructureOrg:String,
    synthOrgs:Seq[String],
    instrumentPatternOrgs:Seq[String],
    overallRank:Option[Int],
    songStructureRank:Option[Int],
    patternStructureRank:Option[Int],
    synthsRank:Option[Int],
    instrumentPatternsRank:Option[Int],
    keep:Option[Boolean]
)

case class OrganismAndFitness(org:String, fit:Int)

case class OrganismFitnessStore(
    orgsAndFitnesses:Seq[OrganismAndFitness]
)