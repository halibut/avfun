package avfun.pipeline

import java.util.concurrent.ArrayBlockingQueue

class PipelineStage[TI,TO](private val _processFunc:(TI)=>TO, override val maxQueueSize:Int = 10) extends Consumer[TI] {
  
  private var _run = true
  private var _outputStage:Option[Consumer[TO]] = None 
  
  def sendTo[TN](processFunc:(TO)=>TN):PipelineStage[TO,TN] = {
    val newStage = new PipelineStage[TO,TN](processFunc, maxQueueSize)
    this._outputStage = Some(newStage)
    newStage
  }
  
  def end(queueSize:Int = maxQueueSize):PipelineEnd[TO] = {
    val endConsumer = new PipelineEnd[TO](queueSize)
    this._outputStage = Some(endConsumer)
    endConsumer
  }
  
  private val _runnable = new Thread{
    override def run(){
      while(_run){
        val input = get
          val output = _processFunc(input)
          
        _outputStage.foreach{ oStage =>
          oStage.put(output)
        }
      }
    }
  }
  
  def start:Unit = {
    _runnable.start()
    _outputStage.foreach{s =>
      s match {
        case p:PipelineStage[_,_] => p.start
        case _ => 
      }
    } 
  }
  def stop:Unit = {
    _run = false
    _outputStage.foreach{s =>
      s match {
        case p:PipelineStage[_,_] => p.stop
        case _ => 
      }
    } 
  }
  
  override def queueDepths:Seq[Int] = {
    Seq(this.queueDepth) ++ _outputStage.map(_.queueDepths).getOrElse(Seq[Int]()) 
  }
  
  def nextStage:Option[Consumer[TO]] = _outputStage
  
}