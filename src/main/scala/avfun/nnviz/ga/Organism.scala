package avfun.nnviz.ga

import java.util.concurrent.atomic.AtomicLong

case class Organism[D <: OrganismDef](
    val id:Long,
    val chromosomes:Seq[Chromosome[_]], 
    val p1Id:Option[Long], val p2Id:Option[Long])
    (implicit val organismDefintion:D){
  
  val chromDefs = organismDefintion.chromosomeDefs
  
  require(chromosomes.size == chromDefs.size, s"Not the right number of chromosomes. Found [${chromosomes.size}] Required [${chromDefs.size}].")
  (chromDefs, chromosomes).zipped.toSeq.zipWithIndex.foreach{ case ((cDef,c),ind) =>
    require(c.definition == cDef, s"Not the same chromosome definition at chromosome with index ${ind}")
    require(c.genes.size == cDef.numGenes, s"Not the right number of genes in chromosome: ${cDef.name}. Found [${c.genes.size}] Required [${cDef.numGenes}].")
    
  }
  
  for(chromDef <- organismDefintion.chromosomeDefs){
    
  }
  
  def getChromosome[T](chromosomeDef:ChromosomeDef[T]):Chromosome[T] = {
    chromosomes
      .find(_.definition == chromosomeDef)
      .get.asInstanceOf[Chromosome[T]]
  }
  
  def getChromosomes[T](chromosomeDef:ChromosomeDef[T]):Seq[Chromosome[T]] = {
    chromosomes
      .filter(_.definition == chromosomeDef)
      .map(_.asInstanceOf[Chromosome[T]])
  }
}


trait OrganismDef{
  val curId = new AtomicLong(0L)
  def name:String
  def chromosomeDefs:Seq[ChromosomeDef[_]]
  
  def resetMaxId(maxId:Long){
    curId.set(maxId)
  }
  def getId:Long = {
    curId.incrementAndGet()
  }
  
  def expressPhenotype[P](organism:Organism[this.type])(implicit conversion:PhenotypeExpression[this.type,P]):P = {
    conversion.expressPhenotype(organism)
  }
  
  
  def mate(o1:Organism[this.type],o2:Organism[this.type]):Organism[this.type] = {
    val zippedChroms = o1.chromosomes.zip(o2.chromosomes)
    val defZippedChroms = chromosomeDefs.zip(zippedChroms)
    
    val chromosomes:Seq[Chromosome[_]] = defZippedChroms map { case (d,(c1,c2)) =>
      val newChrom = mateChromosome[Any](d.asInstanceOf[ChromosomeDef[Any]],c1.asInstanceOf[Chromosome[Any]],c2.asInstanceOf[Chromosome[Any]])
      newChrom
    }
    
    Organism(getId, chromosomes, Some(o1.id), Some(o2.id))(this)
  } 
  
  private def mateChromosome[T](d:ChromosomeDef[T], c1:Chromosome[T],c2:Chromosome[T]):Chromosome[T] = {
    d.mate(c1,c2)
  }
  
  def randomize:Organism[this.type] = {
    val chromosomes:Seq[Chromosome[_]] = chromosomeDefs map { cd =>
      cd.randomize
    }
    
    Organism(getId, chromosomes, None, None)(this)
  }
}