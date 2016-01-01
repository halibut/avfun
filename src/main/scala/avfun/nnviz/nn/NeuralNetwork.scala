package avfun.nnviz.nn

import java.io.InputStream
import java.io.BufferedWriter
import java.util.regex.Pattern
import java.io.BufferedReader
import java.io.OutputStreamWriter
import java.io.OutputStream
import java.io.InputStreamReader

trait NeuralNetwork{
  def calc(inputs: Array[Float]):Array[Float];
  
  def numInputs:Int
  def numOutputs:Int
  def numHiddenNeurons:Int
  def numConnections:Int
  
  def ops:NeuralNetworkOps[NeuralNetwork]
}

object NeuralNetwork{
  def serialize(net:NeuralNetwork, os:OutputStream){
    val writer = new BufferedWriter(new OutputStreamWriter(os))
    net.ops.serialize(net, writer)
  }
  
  def deserialize[T <: NeuralNetwork](netOps:NeuralNetworkOps[T], is:InputStream):T = {
    val reader = new BufferedReader(new InputStreamReader(is))
    netOps.deserialize(reader)
  }
}

trait NeuralNetworkOps[T <: NeuralNetwork]{
  
  def serialize(network:T, os:BufferedWriter)
  def deserialize(is:BufferedReader):T
  
  
  protected def serializeLayerStart(inputs:Int, outputs:Int, hidden:Int, connections:Int, writer:BufferedWriter):Unit = {
    writer.write(s"Layer Start => Inputs[${inputs}], Outputs[${outputs}], Hidden[${hidden}], Connections[${connections}]\n")
  }
  val layerStartPattern = Pattern.compile("^Layer Start\\s*=>\\s*Inputs\\[(\\d+)\\][\\s,]+Outputs\\[(\\d+)\\][\\s,]+Hidden\\[(\\d+)\\][\\s,]+Connections\\[(\\d+)\\].*$")
  protected def deserializeLayerStart(line:String):Option[LayerInfo] = {
    val matcher = layerStartPattern.matcher(line)
    if(matcher.matches){
      Some(LayerInfo(matcher.group(1).toInt, matcher.group(2).toInt, matcher.group(2).toInt, matcher.group(3).toInt))
    }
    else {
      None
    }
  }
  
  protected def serializeLayerEnd(writer:BufferedWriter):Unit = {
    writer.write("Layer End\n")
  }
  protected def deSerializeLayerEnd(line:String):Boolean = {
    line.contains("Layer End")
  }
  
  //Code to serialize and deserialize a line that represents a connection
  //The format for a connection line looks like this: ${NeuronType1}[${index1}]->${NeuronType2}[${index2}]=${weight}
  //Where NeuronType1 is the type of the neuron the connection is from and index1 is the index of that type of neuron
  // NeuronType2 is the type of the neuron the connection is to and index2 is the index of that type of neuron
  // and weight is the weight (float/decimal) of the connection
  // Ex: IN[2]->ON[1]=123.5324
  // Ex: HN[23]->ON[3]=0.234
  protected def serializeConnection(fromId:Int, fromType:NeuronType, toId:Int, toType:NeuronType, weight:Float, writer:BufferedWriter):Unit = {
    val fInitial = NeuronType.initial(fromType)
    val tInitial = NeuronType.initial(toType)
    writer.write(f"${fInitial}[${fromId}]->${tInitial}[${toId}]=${weight}%f\n")
  }
  
  val neuronTypes = NeuronType.values.map(t => NeuronType.initial(t)).mkString("(","|",")")
  protected val connectionLinePattern = Pattern.compile("^\\s*"+neuronTypes+"[\\s*(\\d+)\\s*]\\s*->\\s*neuronTypes\\[\\s*(\\d+)\\s*\\]\\s*=\\s*(\\d+\\.\\d+).*$")
  protected def deserializeConnection(line:String):Option[((Int,NeuronType),(Int,NeuronType),Float)] = {
    val matcher = connectionLinePattern.matcher(line)
    if(matcher.matches){
      val fData = (matcher.group(2).toInt,NeuronType.fromInitial(matcher.group(1)).get)
      val tData = (matcher.group(4).toInt,NeuronType.fromInitial(matcher.group(3)).get)
      Some((fData, tData, matcher.group(3).toFloat))
    }
    else {
      None
    }
  }
  
  
  case class LayerInfo(val inputNeurons:Int, val outputNeurons:Int, val hiddenNeurons:Int, val connections:Int)
  
}

trait NeuronType
object NeuronType{
  
  case object InputNeuron extends NeuronType
  case object OutputNeuron extends NeuronType
  case object HiddenNeuron extends NeuronType
  
  val values:Seq[NeuronType] = Seq(InputNeuron, OutputNeuron, HiddenNeuron)
  
  def initial(nType:NeuronType):String = {
    nType match {
      case InputNeuron => "IN"
      case OutputNeuron => "ON"
      case HiddenNeuron => "HN"
    }
  }
  def fromInitial(initial:String):Option[NeuronType] = {
    initial match {
      case "IN" => Some(InputNeuron)
      case "ON" => Some(OutputNeuron)
      case "HN" => Some(HiddenNeuron)
      case _ => None
    }
  }
  
}