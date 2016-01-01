package avfun.nnviz.nn

import org.scalatest.FunSuite
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream

/**
 * @author Kyle
 */
class NetworkTests extends FunSuite {
  import FastNetwork.Conn
  
  test("Serialize and Deserialize Simple Network"){
    val net = new FastNetwork(2,1,1,Seq(
        new Conn(0,2,.1f),
        new Conn(1,2,.3f),
        new Conn(2,3,.5f)
      )
    )
//    val i1 = net.addInput("i1")
//    val i2 = net.addInput("i2")
//    val n = net.addNeuron()
//    val o = net.addOutput("o")
//    i1 ~> (n,0.1f)
//    i2 ~> (n,0.3f)
//    n ~> (o,0.5f)
    
    val bos1 = new ByteArrayOutputStream()
    NeuralNetwork.serialize(net, bos1)
    val str1 = new String(bos1.toByteArray())
    
    val bis = new ByteArrayInputStream(str1.getBytes)
    val net2 = NeuralNetwork.deserialize(FastNetwork, bis)
    
    val bos2 = new ByteArrayOutputStream()
    NeuralNetwork.serialize(net2, bos2)
    val str2 = new String(bos2.toByteArray())
    
    assert(str1 === str2)
  }
  
  
  
}