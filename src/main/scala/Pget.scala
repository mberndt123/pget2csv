import scodec.*
import scodec.bits.*
import scodec.codecs.*
import scala.collection.immutable.ArraySeq

final case class Pget(
  // TODO: add header here
  projections: Vector[Data]
)
final case class Data(
  counts: Vector[(Int, Int, Int, Int)],
  deadtimes: Vector[(Short, Short, Short, Short)],
  countsNeutronTube1: Int,
  countsNeutronTube2: Int,
  deadtimeNeutronTube1: Short,
  deadtimeNeutronTube2: Short,
  motorPosition: Float
)

object Pget:
  private def projectionDecoder(numPixels: Int): Codec[Data] =
    (vectorOfN(provide(numPixels), uint24L :: uint24L :: uint24L :: uint24L) ::
      vectorOfN(provide(numPixels), ushort8 :: ushort8 :: ushort8 :: ushort8) ::
      uint24L ::
      uint24L ::
      ushort8 ::
      ushort8 ::
      floatL).xmap(Data.apply, _ => ???)

  
  val decoder: Decoder[Pget] =
    (for
      _ <- constant(hex"50474554") // "PGET"
      _ <- uint32L.unit(0) // file size
      headerSize <- peek(uint16L)
      _ <- ignore(8 * headerSize) // TODO: check for underflow
      numProjections <- uint16L
      sizePerProjection <- uint16L
      numPixels = (sizePerProjection - 12) / 16
      data <- vectorOfN(provide(numProjections), projectionDecoder(numPixels))
    yield Pget(data)).complete
end Pget
