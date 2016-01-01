package avfun.nnviz.ga

import java.io.OutputStream
import java.io.InputStream
import java.io.BufferedOutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.BufferedReader
import java.io.BufferedInputStream
import java.io.InputStreamReader
import java.util.regex.Pattern
import java.io.File
import java.io.FilenameFilter
import avfun.util.FileUtil
import java.io.FileInputStream
import java.io.FileOutputStream
import avfun.util.StreamUtil

class FileSystemPersistence[T <: OrganismDef](
    val baseDir:String, override val serializer:OrganismSerializer, 
    override val organismDef:T) extends SerializeablePersistence[T] {
  
  import FileSystemPersistence._
  import Generation.{OrganismMetaData,GenerationMetaData}
  
  private val _baseDir = new File(baseDir)
  
  if(!_baseDir.exists()){
    _baseDir.mkdir()
  }
  else{
    require(_baseDir.isDirectory(), s"Expected baseDir (${baseDir}) to be a directory.")
  }
  
  def getMaxOrganismId: Long = {
    val newestGen = getNewestGenerationMetadata
    
    newestGen
      .map(x => x.organisms.map(y => y.id).max)
      .getOrElse(0L)
  }

  def getNewestGenerationMetadata: Option[GenerationMetaData] = {
    val files = _baseDir.list(GenDirectoryFilter)
    
    if(files.length == 0){
      None
    }
    else{
      val last = files.sorted.last
      
      val genMetaFile = new File(_baseDir,last+"/"+GEN_METADATA_FILENAME)
      val fis = new FileInputStream(genMetaFile)
      try{
        Some(serializer.deserializeGenerationData(fis))
      }finally{
        fis.close()
      }
    }
  }
  
  private def pad(num:Int) = {
    var str = num.toString()
    while(str.length() < 10){
      str = "0" + str
    }
  }
  
  private def pad(num:Long) = {
    var str = num.toString()
    while(str.length() < 10){
      str = "0" + str
    }
  }
  
  private def getMetadataFile(genId:Int):File = {
    val genDir = getGenerationDir(genId)
    new File(genDir, GEN_METADATA_FILENAME)
  }
  
  private def getGenerationDir(id:Int):File = {
    val padded = pad(id)
    new File(_baseDir, "/"+GEN_DIR_PREFIX+padded)
  }
  
  private def getOrgFilePrefix(id:Long):String = {
    ORG_FILE_PREFIX + pad(id)
  }

  def saveGeneration(generation: Generation[oType]): Unit = {
    val genDir = getGenerationDir(generation.generationId)
    if(genDir.exists()){
      FileUtil.deleteRecursive(genDir)
    }
    genDir.mkdir()
    
    saveGenerationMetadata(Generation.GenMetadata(generation))
    
    for(organism <- generation.getOrganisms){
      saveOrganism(organism, generation)
    }
  }

  def saveOrganism(organism: Organism[oType], generation: Generation[oType]): Unit = {
    val genDir = getGenerationDir(generation.generationId)
    val orgFile = new File(genDir, getOrgFilePrefix(organism.id) + ORG_FILE_SUFFIX)
    if(orgFile.exists()){
      orgFile.delete()
    }
    
    val orgfos = new FileOutputStream(orgFile)
    try{
      serializer.serializeOrganismChromosomes(organism.chromosomes, orgfos)
    }
    finally{
      orgfos.close()
    }
  }

  def getNewestGeneration: Option[Generation[oType]] = {
    val newestMeta = getNewestGenerationMetadata
    
    newestMeta.map{meta =>
      val genDir = getGenerationDir(meta.id)
      
      val orgs = for(org <- meta.organisms) yield {
        val orgFile = new File(genDir, ORG_FILE_PREFIX+org.id)
        val orgIs = new FileInputStream(orgFile)
        try{
          val chromosomes = serializer.deserializeOrganismCoromosomes(organismDef, orgIs)
          (new Organism[oType](org.id, chromosomes, org.p1Id, org.p2Id)(organismDef),org)
        }
        finally{
          orgIs.close()
        }
      }
      
      new Generation[oType](meta.id, orgs.map(x => x._1 -> x._2.fitness))
    }
  }

  def saveGenerationMetadata(generationMetadata: GenerationMetaData): Unit = {
    val metadataFile = getMetadataFile(generationMetadata.id)
    
    if(metadataFile.exists()){
      metadataFile.delete()
    }
    
    val metafos = new FileOutputStream(metadataFile)
    try{
      serializer.serializGenerationData(generationMetadata, metafos)
    }
    finally{
      metafos.close()
    }
  }

  def attachDataToOrganism(organism: OrganismMetaData, gen: GenerationMetaData, attachName: String, is: InputStream): Unit = {
    val genDir = getGenerationDir(gen.id)
    val file = new File(genDir, getOrgFilePrefix(organism.id) + attachName)
    if(file.exists()){
      file.delete()
    }
    
    val fos = new FileOutputStream(file)
    try{
      StreamUtil.copyData(is, fos)
    }
    finally{
      fos.close()
    }
  }

  def getAttachedData(organism: OrganismMetaData, gen: GenerationMetaData, attachName: String, os: OutputStream):Boolean = {
    val genDir = getGenerationDir(gen.id)
    val file = new File(genDir, getOrgFilePrefix(organism.id) + attachName)
    if(file.exists()){
      val fis = new FileInputStream(file)
      try{
        StreamUtil.copyData(fis, os)
      }
      finally{
        fis.close()
      }
      true
    }
    else{
      false
    }
  }
  
}

object FileSystemPersistence{
  private val GEN_DIR_PREFIX = "gen-"
  private val ORG_FILE_PREFIX = "organism-"
  private val ORG_FILE_SUFFIX = ".genetic_code"
  private val GEN_METADATA_FILENAME = "_metadata.txt"
  
  private object GenDirectoryFilter extends FilenameFilter {
    def accept(file: File, name: String): Boolean = {
      file.isDirectory() && file.getName.startsWith(GEN_DIR_PREFIX)
    }
  }  
  
}
