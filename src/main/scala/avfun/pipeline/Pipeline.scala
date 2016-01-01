package avfun.pipeline

class Pipeline[I,O](private val _firstStage:PipelineStage[I,_], private val _lastStage:PipelineStage[_,O]){
  
  def this(firstStage:PipelineStage[I,O]) = {
    this(firstStage,firstStage)
  }
  
  private var _resultQueue:PipelineEnd[O] = null
  
  def sendTo[N](processFunc:(O)=>N):Pipeline[I,N] = {
   val nextStage = _lastStage.sendTo(processFunc)
   new Pipeline(_firstStage, nextStage)
  }
  
  def start(endQueueSize:Int = _lastStage.maxQueueSize):PipelineEnd[O] = {
    _resultQueue = _lastStage.end(endQueueSize)
    _firstStage.start
    _resultQueue
  }
  def stop = _firstStage.stop
  
  def put(input:I){
    _firstStage.put(input)
  }
  
  def queueDepths:Seq[Int] = {
    _firstStage.queueDepths
  }
}

object Pipeline{

  def apply[I,O](processFunc:(I)=>O):Pipeline[I,O] = {
    apply(10)(processFunc)
  }
  
  def apply[I,O](queueLength:Int)(processFunc:(I)=>O):Pipeline[I,O] = {
    new Pipeline[I,O](new PipelineStage(processFunc, queueLength))
  }
}