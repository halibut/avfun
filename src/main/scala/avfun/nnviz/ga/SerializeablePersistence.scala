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
import java.io.ByteArrayOutputStream
import scala.reflect.classTag
import scala.reflect.ClassTag
import avfun.util.ZipUtil
import java.io.ByteArrayInputStream
import avfun.util.StreamUtil


trait SerializeablePersistence[T <: OrganismDef] extends Persistence[T]{
  
  def serializer:OrganismSerializer
}

class OrganismSerializer{
  private val chromosomeZipEntryName = "chromosomeData" 
  
  def getSerializerForChromosomeType(tp:ClassTag[_]):Option[ChromosomeSerializer] = {
    import NumericExtImplicits._
    
    tp match {
      case t if t == ClassTag.Float => Some(new NumericChromosomeSerializer[Float])
      case t if t == ClassTag.Int => Some(new NumericChromosomeSerializer[Int])
      case _ => None
    }
  }
  
  def serializeOrganismChromosomes(chromosomes:Seq[Chromosome[_]], os:OutputStream):Unit = {
    val bos = new ByteArrayOutputStream
    val w = new OutputStreamWriter(bos)
    
    w.write(s"Chromosomes: ${chromosomes.size}\n")
    
    for(chrom <- chromosomes) {
      val cdef = chrom.definition
      val serializer = getSerializerForChromosomeType(cdef.chromosomeDataType).getOrElse{
        throw new IllegalStateException(s"Cannot find serializer for chromosome of type: ${cdef.chromosomeDataType.runtimeClass.getName}")
      }
      w.write(s"Chromosome[${cdef.chromosomeDataType.runtimeClass.getSimpleName}](${cdef.numGenes})\n")
      for(gene <- chrom.genes){
        w.write(serializer.serialize(gene)+"\n")
      }
    }
    w.flush()
    bos.close()
    
    val bis = new ByteArrayInputStream(bos.toByteArray())
    ZipUtil.zip(bis, chromosomeZipEntryName, os)
//    StreamUtil.copyData(bis, os)
    
  }
  
  private val chromosomesDefPattern = Pattern.compile("^\\s*Chromosomes:\\s*(\\d+)\\s*$")
  private val chromosomeDefPattern = Pattern.compile("^\\s*Chromosome\\[\\s*(\\S+)\\s*]\\(\\s*(\\d+)\\s*\\)\\s*$")
  
  def deserializeOrganismCoromosomes(organismDef:OrganismDef, is:InputStream):Seq[Chromosome[_]] = {
    val bos = new ByteArrayOutputStream
    ZipUtil.unzipSingleEntryToOutputStream(is, chromosomeZipEntryName, bos)
//    StreamUtil.copyData(is,bos)
    bos.close()
    
    val bis = new ByteArrayInputStream(bos.toByteArray())
    val r = new BufferedReader(new InputStreamReader(bis))
    
    val defLine = r.readLine()
    val defLineMatch = chromosomesDefPattern.matcher(defLine)
    val numChromosomes = if(defLineMatch.matches()){
      defLineMatch.group(1).toInt
    }
    else{
      throw new IllegalStateException(s"Chromosome data input stream must start with: Chromosomes: numChromosomes, but found ${defLine}")
    }
    
    var line:String = null
    var chromosomes = Seq[Chromosome[_]]()
    var curChromosome:Seq[Any] = null
    var curChromosomeDef:ChromosomeDef[Any] = null
    var tailChromosomeDefs:Seq[ChromosomeDef[Any]] = organismDef.chromosomeDefs.asInstanceOf[Seq[ChromosomeDef[Any]]]
    var curClassSerializer:ChromosomeSerializer = null
    
    do{
      line = r.readLine()
      
      if(line != null && !line.isEmpty()){
        val chomosomeDefMatch = chromosomeDefPattern.matcher(line)
        if(chomosomeDefMatch.matches()){
          if(curChromosome != null){
            chromosomes :+= new Chromosome[Any](curChromosomeDef,curChromosome)
          }
          curChromosomeDef = tailChromosomeDefs.head
          tailChromosomeDefs = tailChromosomeDefs.tail
          curChromosome = Seq[Any]()
          curClassSerializer = getSerializerForChromosomeType(curChromosomeDef.chromosomeDataType).getOrElse{
            throw new IllegalStateException(s"Cannot find serializer for chromosome of type: ${curChromosomeDef.chromosomeDataType.runtimeClass.getName}")
          }
        }
        else if(curChromosomeDef != null){
          curChromosome :+= curClassSerializer.deserialize(line)
        }
      }
      
    }while(line != null)
    
    if(curChromosome != null){
      chromosomes :+= new Chromosome[Any](curChromosomeDef,curChromosome)
    }
      
    chromosomes
  }

  def serializGenerationData(generation:Generation.GenerationMetaData, os:OutputStream):Unit = {
    val w = new OutputStreamWriter(os)
    w.write("Generation: "+generation.id+"\n")    
    
    for(organism <- generation.organisms){
      val fitness = organism.fitness
      val p1Id = organism.p1Id.map(_.toString()).getOrElse("")
      val p2Id = organism.p2Id.map(_.toString()).getOrElse("")
      
      w.write(s"Organism[${organism.id}](${fitness}) - Parents[${p1Id},${p2Id}]\n")
    }
    w.flush()
  }
  
  private val genLinePattern = Pattern.compile("^\\s*Generation:\\s*(\\d+)\\s*$")
  private val orgLinePattern = Pattern.compile("^\\s*Organism\\[\\s*(\\d+)\\s*\\]\\(\\s*(\\d+\\.\\d+)\\s*\\)\\s*-\\s*Parents\\[\\s*(\\d*)\\s*,\\s*(\\d*)\\s*\\]\\s*$") 
  
  def deserializeGenerationData(is:InputStream):Generation.GenerationMetaData = {
    val r = new BufferedReader(new InputStreamReader(is))
    
    var line = r.readLine()
    val genMatch = genLinePattern.matcher(line)
    val genId = if(genMatch.matches()){
      genMatch.group(1).toInt
    }
    else{
      throw new IllegalStateException(s"Generation data input stream must start with: Generation: id, but found ${line}")
    }
    
    var organismMetadata = Seq[Generation.OrganismMetaData]()
    
    do{
      line = r.readLine()
      
      val orgMatch = orgLinePattern.matcher(line)
      if(orgMatch.matches()){
        val id = orgMatch.group(1).toLong
        val fitness = orgMatch.group(2).toFloat
        
        val p1 = orgMatch.group(3)
        val p1Id:Option[Long] = if(p1.isEmpty()) None else Some(p1.toLong)
        
        val p2 = orgMatch.group(4)
        val p2Id:Option[Long] = if(p2.isEmpty()) None else Some(p2.toLong)
        
        organismMetadata :+= Generation.OrganismMetaData(id, fitness, p1Id, p2Id)
      }
      
    }while(line != null)
      
    Generation.GenerationMetaData(genId, organismMetadata)
  }
}

trait ChromosomeSerializer{
  def serialize(any:Any):String
  def deserialize(s:String):Any
}

class NumericChromosomeSerializer[T](implicit val numExt:NumericExt[T]) extends ChromosomeSerializer{
  override def serialize(t:Any):String = {
    t.toString()
  }
  override def deserialize(s:String):Any = {
    numExt.fromString(s)
  }
}

