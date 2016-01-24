package avfun.supercollider.model

sealed trait PatternType

case object RhythmOnlyPatternType extends PatternType
case object MultiVoicePatternType extends PatternType
case object SingleVoicePatternType extends PatternType