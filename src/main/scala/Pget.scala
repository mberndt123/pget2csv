import scodec.*
import scodec.bits.*
import scodec.codecs.*
import scala.collection.immutable.ArraySeq

final case class Pget(
    // TODO: add header here
    projections: Vector[ProjectionData]
)

case class PixelData(
    counts: (Int, Int, Int, Int),
    deadtimes: (Short, Short, Short, Short)
)

final case class ProjectionData(
    pixelData: Vector[PixelData],
    countsNeutronTube1: Int,
    countsNeutronTube2: Int,
    deadtimeNeutronTube1: Short,
    deadtimeNeutronTube2: Short,
    motorPosition: Float
)

object Pget:
  private def projectionDecoder(numPixels: Int): Codec[ProjectionData] =
    ((vectorOfN(provide(numPixels), uint24L :: uint24L :: uint24L :: uint24L) ::
      vectorOfN(provide(numPixels), ushort8 :: ushort8 :: ushort8 :: ushort8))
      .xmap(
        { (counts, deadtimes) =>
          counts.zip(deadtimes).map(PixelData(_, _))
        },
        _.unzip(p => (p.counts, p.deadtimes))
      ) ::
      uint24L ::
      uint24L ::
      ushort8 ::
      ushort8 ::
      floatL).xmap(ProjectionData.apply, _ => ???)

  private val BytesPerPixel =
    16 // 4 24-bit numbers for counts and 4 8-bit numbers for deadtimes
  private val ExtraBytesPerProjection =
    12 // number of bytes per projection on top of the pixel data
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
        (sizePerProjection - ExtraBytesPerProjection) / BytesPerPixel // TODO: check for arithmetic underflow
      data <- vectorOfN(provide(numProjections), projectionDecoder(numPixels))
    yield Pget(data)).complete
end Pget
