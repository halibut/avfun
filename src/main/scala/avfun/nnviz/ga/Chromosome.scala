package avfun.nnviz.ga

import scala.reflect.ClassTag
import scala.reflect.classTag

case class Chromosome[T](val definition:ChromosomeDef[T], val genes:Seq[T]) {
  
}

abstract class ChromosomeDef[T : ClassTag] {
  val chromosomeDataType = classTag[T]
  
  def name:String
  def numGenes:Int
  
  def mate(c1: Chromosome[T],c2: Chromosome[T]):Chromosome[T]
  def randomize:Chromosome[T]
}

abstract class NumericChromosomeDef[T : ClassTag](implicit val num:Numeric[T], val numExt:NumericExt[T]) extends ChromosomeDef[T]{
  import num._
  
  def minVal:T
  def maxVal:T
  def range:T = maxVal-minVal
  def crossoverTimes:Int
  
  def mutateMagnitude:T
  def mutateRatio:Float
  
  def mate(c1:Chromosome[T], c2: Chromosome[T]):Chromosome[T] = {
    val crossovers = ((1+crossoverTimes) * math.random).toInt
    val crossoverPoints:Seq[Int] = (((0 until crossovers) map (i=> (math.random * numGenes).toInt ) toSeq ) sorted ) :+ numGenes
    
    var gPairs = (c1.genes, c2.genes).zipped.toSeq
    
    var selectc1 = true
    val crossOverGenes = crossoverPoints.map{crossoverInd =>
      val (first,last) = gPairs.splitAt(crossoverInd)
      gPairs = last
      val genesPart = first.map(g => if(selectc1) g._1 else g._2).toSeq
      selectc1 = !selectc1
      genesPart
    }
    .flatten
    
    val min = minVal
    val max = maxVal
    val newGenes = crossOverGenes.map{ g =>
      if(mutateRatio < math.random){
        val newVal = g + numExt.fromDouble( (2 * math.random -1) * mutateMagnitude.toDouble() )
        
        val clamped = num.max(num.min(newVal , max), min)
        clamped
      }
      else{
        g
      }
    }
    
    Chromosome(this, newGenes)
  }
  
  protected def validRandomValue:T = {
    val randMult = math.random * range.toDouble()
    
    val rand = minVal + numExt.fromDouble(randMult)
    
    rand
  }
  
  def randomize:Chromosome[T] = {
    val genes:Seq[T] = ((0 until numGenes) map (i => validRandomValue)) toSeq
    
    Chromosome(this, genes)
  }
  
}

trait NumericExt[T]{
  def fromDouble(d:Double):T
  def fromString(string:String):T
}

object NumericExtImplicits{
  implicit object FloatNumericExt extends NumericExt[Float]{
    def fromDouble(d:Double):Float = {
      d.toFloat
    }
    def fromString(s:String):Float = {
      s.toFloat
    }
  }
  
  implicit object IntNumericExt extends NumericExt[Int]{
    def fromDouble(d:Double):Int = {
      d.toInt
    }
    def fromString(s:String):Int = {
      s.toInt
    }
  }
}