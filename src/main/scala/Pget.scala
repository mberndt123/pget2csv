import scodec.*
import scodec.bits.*
import scodec.codecs.*
import scala.collection.immutable.ArraySeq

final case class Pget(
    // TODO: add header here
    projections: Vector[Data]
)

case class PixelData(
    counts: (Int, Int, Int, Int),
    deadtimes: (Short, Short, Short, Short)
)

final case class Data(
    pixelData: Vector[PixelData],
    countsNeutronTube1: Int,
    countsNeutronTube2: Int,
    deadtimeNeutronTube1: Short,
    deadtimeNeutronTube2: Short,
    motorPosition: Float
)

object Pget:
  private def projectionDecoder(numPixels: Int): Codec[Data] =
    ((vectorOfN(provide(numPixels), uint24L :: uint24L :: uint24L :: uint24L) ::
      vectorOfN(provide(numPixels), ushort8 :: ushort8 :: ushort8 :: ushort8))
      .xmap(
        { case (counts, deadtimes) =>
          counts.zip(deadtimes).map(PixelData(_, _))
        },
        _.unzip(p => (p.counts, p.deadtimes))
      ) ::
      uint24L ::
      uint24L ::
      ushort8 ::
      ushort8 ::
      floatL).xmap(Data.apply, _ => ???)

  val decoder: Decoder[Pget] =
    (for
      _ <- constant(hex"50474554") // "PGET"
      _ <- uint32L.unit(
        0
      ) // file size (shouldn't be 0, but this doesn't work for writing anyway, so whatever
      headerSize <- peek(uint16L)
      _ <- ignore(8 * headerSize)
      numProjections <- uint16L
      sizePerProjection <- uint16L
      numPixels =
        (sizePerProjection - 12) / 16 // TODO: check for arithmetic underflow
      data <- vectorOfN(provide(numProjections), projectionDecoder(numPixels))
    yield Pget(data)).complete
end Pget
