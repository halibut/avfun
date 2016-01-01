package avfun.nnviz.nn

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MutMap}
import scala.collection.Map
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.InputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.io.BufferedInputStream
import java.util.regex.Pattern
import java.io.BufferedWriter

class FastNetwork(inputs:Int, outputs:Int, hiddenNeurons:Int, connections:Seq[FastNetwork.Conn], 
    val activationFunc:ActivationFunc = LinearSaturationActivation) extends NeuralNetwork {

  private val _inputNeuronInds:Array[Int] = (0 until inputs).toArray
  private val _outputNeuronInds:Array[Int] = (inputs until (inputs+outputs)).toArray
  private val _neuronCurVals = new Array[Float](inputs + outputs + hiddenNeurons)
  private val _neuronTempVals = new Array[Float](_neuronCurVals.length)
  private val _connections:Array[FastNetwork.Conn] = connections.toArray
  
  private val _neuronCount = _neuronCurVals.length
  private val _connectionCount = _connections.length
  
  private def processNeurons(){
    //initialize all values to zero
    var i = 0
    while(i < _neuronCount){
      _neuronTempVals(i) = 0f
      i+=1
    }
    
    //Sum inputs * weights for each neuron
    i = 0
    while(i < _connectionCount){
      val conn = _connections(i)
      _neuronTempVals(conn.toInd) += _neuronCurVals(conn.fromInd) * conn.weight
      i+=1
    }

    //Run activation function on each neuron and copy to curVals array
    i = 0
    while(i < _neuronCount){
      val activated = activationFunc.activate(_neuronTempVals(i))
      _neuronCurVals(i) = activated
      i+=1
    }
  }
 
  def numInputs = _inputNeuronInds.length
  def numOutputs = _outputNeuronInds.length
  def numHiddenNeurons = _neuronCount - numInputs - numOutputs
  def numConnections = _connectionCount
  
  def calc(inputs: Array[Float]):Array[Float] = {
    //Initialize input neurons
    var i = 0
    var l = inputs.length
    while(i < l){
      _neuronCurVals(_inputNeuronInds(i)) = inputs(i)
      i+=1
    }
    
    //Update network with new input values
    processNeurons()
    
    i = 0
    l = _outputNeuronInds.length
    val outputs = new Array[Float](l)
    while(i < l){
      outputs(i) = _neuronCurVals(_outputNeuronInds(i))
      i+=1
    }
    outputs
  }
  
  def preRun(times:Int)(inputsVal:Float = 0.0f):Unit = {
    val inputs = Array.fill(_inputNeuronInds.length)(inputsVal)
    for(i <- 0 until times) {
      calc(inputs)
    }
  }

  def ops:NeuralNetworkOps[NeuralNetwork] = FastNetwork.asInstanceOf[NeuralNetworkOps[NeuralNetwork]]
  
}

object FastNetwork extends NeuralNetworkOps[FastNetwork] {
  final class Conn(val fromInd:Int, val toInd:Int, val weight:Float)

  def random(inputs:Int,outputs:Int,hidden:Int,connections:Int,weightRange:(Float,Float)=(-1f,1f)):FastNetwork = {
    
    val numNeurons = inputs + outputs + hidden
    val rng = weightRange._2 - weightRange._1
    
    var i = 0
    val connArr = new Array[Conn](connections)
    while(i < connections){
      val fromI = (math.random * numNeurons).toInt
      val toI = (math.random * numNeurons).toInt
      val weight = ((math.random * rng) + weightRange._1).toFloat
      
      //Don't create a connection to an input neuron, flip connection if possible
      if(toI >= inputs){
        connArr(i) = new Conn(fromI, toI, weight)
      }
      else if(fromI >= inputs){
        connArr(i) = new Conn(toI, fromI, weight)
      }
      else{
        //Don't count this connection since we didn't add it
        i-=1
      }
      
      i+=1
    }
    
    val net = new FastNetwork(inputs, outputs, hidden, connArr)
    net
  }
  
  def deserialize(is: BufferedReader): FastNetwork = {
    var layerInfo:LayerInfo = null
    var curConnection = 0
    var connections:Array[Conn] = null
    var line:String = null
    do{
      line = is.readLine()
      if(layerInfo == null){
        val layerStartOpt = deserializeLayerStart(line)
        if(layerStartOpt.isDefined){
          layerInfo = layerStartOpt.get
        }
      }
      else{
        val connOpt = deserializeConnection(line)
        if(connOpt.isDefined){
          val conn = connOpt.get
          val fromIndOffset = getNeuronOffset(layerInfo, conn._1._2)
          val toIndOffset = getNeuronOffset(layerInfo, conn._2._2)
          connections(curConnection) = new Conn(fromIndOffset + conn._1._1, toIndOffset + conn._2._1, conn._3)
          curConnection += 1
        }
        else{
          val layerEnd = deSerializeLayerEnd(line)
          if(layerEnd){
            line = null
          }
        }
      }
      
    }while(line != null);
    
    val net = new FastNetwork(layerInfo.inputNeurons, layerInfo.outputNeurons, layerInfo.hiddenNeurons, connections)
    net
  }
  
  def serialize(network: FastNetwork, writer: BufferedWriter): Unit = {
    serializeLayerStart(network.numInputs, network.numOutputs, network.numHiddenNeurons, network.numConnections, writer)
    var i = 0
    val l = network.numConnections
    while(i < l){
      val conn = network._connections(i)
      val (fType,fInd) = getNeuronTypeAndOffsetInd(network, conn.fromInd)
      val (tType,tInd) = getNeuronTypeAndOffsetInd(network, conn.toInd)
      serializeConnection(fInd, fType, tInd, tType, conn.weight, writer)
      i+=1
    }
    serializeLayerEnd(writer)
    writer.flush
  }
  
  private def getNeuronOffset(layerInfo:LayerInfo, nt:NeuronType):Int = {
    val fromIndOffset = nt match{
      case NeuronType.InputNeuron => 0
      case NeuronType.OutputNeuron => layerInfo.inputNeurons
      case NeuronType.HiddenNeuron => layerInfo.inputNeurons + layerInfo.outputNeurons
    }
    fromIndOffset
  }
  
  private def getNeuronTypeAndOffsetInd(n:FastNetwork, nInd:Int):(NeuronType,Int) = {
    if(nInd < n._inputNeuronInds.length){
      (NeuronType.InputNeuron, nInd)
    }
    else if(nInd < n._inputNeuronInds.length + n._outputNeuronInds.length){
      (NeuronType.OutputNeuron, nInd - n._inputNeuronInds.length)
    }
    else {
      (NeuronType.HiddenNeuron, nInd - n._inputNeuronInds.length - n._outputNeuronInds.length)
    }
  }
  
}

