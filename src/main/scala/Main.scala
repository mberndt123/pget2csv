import java.nio.file.*
import scodec.bits.BitVector
import scodec.Attempt
import scodec.DecodeResult
import java.nio.charset.StandardCharsets.UTF_8
@main def main(in: String, out: String) =
  val path = Paths.get(in)
  val bytes = Files.readAllBytes(path)
  val bitVector = BitVector(bytes)
  Pget.decoder.decode(bitVector) match
    case Attempt.Successful(DecodeResult(value, _)) =>
      val text = value.projections.map { p =>
        p.pixelData.map { case PixelData((c1, c2, c3, c4), (d1, d2, d3, d4)) =>
          s"$c1,$c2,$c3,$c4,$d1,$d2,$d3,$d4\n"
        }.mkString + "\n"
      }.mkString
      Files.writeString(Paths.get(out), text, UTF_8)
    case Attempt.Failure(err) =>
      println(err)
      sys.exit(1)
