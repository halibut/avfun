package avfun.nnviz.nn

trait ActivationFunc {
  @inline
  def activate(in:Float):Float
}

final object LinearSaturationActivation extends ActivationFunc{
  @inline
  def activate(in:Float):Float = {
    Math.min(1, Math.max(-1, in))
  }
}

final object InverseTangentActivation extends ActivationFunc{
  @inline
  def activate(in:Float):Float = {
    Math.tanh(in).toFloat
  }
}