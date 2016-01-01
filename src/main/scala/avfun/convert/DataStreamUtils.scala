package avfun.convert

object DataStreamUtils {
  val maxSignedValForBytes = Array[Long](
      0x000000000000007F,
      0x0000000000007FFF,
      0x00000000007FFFFF,
      0x000000007FFFFFFF
  )
  val maxUnsignedValForBytes = maxSignedValForBytes.map(signedMax => signedMax << 1 | 1 )
  
  val divMaxSignedValForBytes = maxSignedValForBytes.map(signedMax => 1.0f / signedMax)
  val divMaxUnsignedValForBytes = maxUnsignedValForBytes.map(unsignedMax => 1.0f / unsignedMax)
}