package avfun.nnviz.ga

import org.scalatest.FunSuite
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream

class SerializeablePersistenceTests extends FunSuite {
  import NumericExtImplicits._
  
  val c1Def = new NumericChromosomeDef[Float]{
    def name:String = "chromosome 1"
    def numGenes:Int = 20
    def minVal:Float = -1.0f
    def maxVal:Float = 1.0f
    def crossoverTimes:Int = 1
    def mutateMagnitude:Float = 0.5f
    def mutateRatio:Float = 0.25f
  }
  
  val c2Def = new NumericChromosomeDef[Int]{
    def name:String = "chromosome 2"
    def numGenes:Int = 30
    def minVal:Int = 0
    def maxVal:Int = 10
    def crossoverTimes:Int = 1
    def mutateMagnitude:Int = 5
    def mutateRatio:Float = 0.25f
  }
  
  val c3Def = new NumericChromosomeDef[Float]{
    def name:String = "chromosome 3"
    def numGenes:Int = 10
    def minVal:Float = -1.0f
    def maxVal:Float = 1.0f
    def crossoverTimes:Int = 3
    def mutateMagnitude:Float = 0.5f
    def mutateRatio:Float = 0.25f
  }
  
  val orgDef = new OrganismDef{
    def name:String = "Test organism"
    val chromosomeDefs:Seq[ChromosomeDef[_]] = Seq(c1Def,c2Def,c3Def)
  }
  
  test("Serialize and Deserialize Organism"){
    
    val organism = orgDef.randomize
    
    val serializer = new OrganismSerializer()
    val serOS = new ByteArrayOutputStream
    serializer.serializeOrganismChromosomes(organism.chromosomes, serOS)
    
    val deserIS = new ByteArrayInputStream(serOS.toByteArray())
    val testChromosomes = serializer.deserializeOrganismCoromosomes(orgDef, deserIS)
    
    assert(organism.chromosomes.size === testChromosomes.size)
    val zipped = (organism.chromosomes,testChromosomes).zipped
    
    zipped.foreach{case (oc,tc) =>
      assert(oc.genes.size === tc.genes.size)
      
      (oc.genes,tc.genes).zipped.foreach{case (o,t) =>
        assert(o === t)
      }
    }
    
  }
}