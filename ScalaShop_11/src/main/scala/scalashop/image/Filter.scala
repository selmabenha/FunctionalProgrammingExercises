package scalashop.image

import scalashop.common.*

/** Identity filter, does not change pixels of the source image. */
class Identity(src: Image) extends Image(src.height, src.width):
  def apply(x: Int, y: Int): Pixel =
    src(x, y)

/** Grayscale filter, transforms the source image in a grayscale one. */
class Grayscale(src: Image) extends Image(src.height, src.width):
  // we generate a weighted grayscale image
  // to do this, we compute the "Luma" of each pixel
  // these numbers come from a standard called Rec 601
  // and are computed based on how we perceive colour and brightness
  // see: https://en.wikipedia.org/wiki/Luma_(video)
  val lumaR = 0.299f
  val lumaG = 0.587f
  val lumaB = 0.114f
  def grayscale(input: Pixel) =
    val luma = (lumaR * red(input) + lumaG * green(input) + lumaB * blue(input)).toInt
    argb(alpha(input), luma, luma, luma)

  def apply(x: Int, y: Int): Pixel = grayscale(src(x, y))

class RedSplash(src: Image) extends Grayscale(src):
  def isRedEnough(px: Pixel) =
    val r = red(px).toFloat
    val g = green(px).toFloat
    val b = blue(px).toFloat
    (r > 1.7 * g) && (r > 1.7 * b)

  override def apply(x: Int, y: Int): Pixel =
    val pixel = src(x,y)
    if isRedEnough(pixel) then pixel else super.apply(x,y)

/** Performs a simple box-blur of given radius by averaging over a pixel's
  * neighbours
  *
  * @param src
  *   source image
  */
class SimpleBlur(src: Image) extends Image(src.height, src.width):
  val radius: Int = 3

  def apply(x: Int, y: Int): Pixel =

    def getNeighborsWithinRadius: Seq[(Int, Int)] =
      for
        dx <- -radius to radius
        dy <- -radius to radius
        nx = x + dx
        ny = y + dy
        if nx >= 0 && nx < src.width && ny >= 0 && ny < src.height
      yield (nx, ny)

    val (sumA, sumR, sumG, sumB, count) = getNeighborsWithinRadius.foldLeft((0, 0, 0, 0, 0)) {
      case ((accA, accR, accG, accB, num), (nx, ny)) =>
        val pixel = src(nx, ny)
        (
          accA + alpha(pixel),
          accR + red(pixel),
          accG + green(pixel),
          accB + blue(pixel),
          num + 1
        )
    }

    val avgA = (sumA / count).toInt
    val avgR = (sumR / count).toInt
    val avgG = (sumG / count).toInt
    val avgB = (sumB / count).toInt

    argb(avgA, avgR, avgG, avgB)


/** Produce the convolution of an image with a kernel
  *
  * @param src
  *   source image
  * @param kernel
  *   kernel to convolve with
  */
class Convolution(src: Image, kernel: Kernel) extends Matrix[(Float, Float, Float, Float)]:
  val height: Int = src.height
  val width: Int = src.width

  def toImage =
    FloatMatrixImage(this)

  def apply(x: Int, y: Int): (Float, Float, Float, Float) =
    val rx = (kernel.width - 1) / 2
    val ry = (kernel.height - 1) / 2

    val (sumA, sumR, sumG, sumB) = 
      (-rx to rx).foldLeft((0.0f, 0.0f, 0.0f, 0.0f)) { case ((accA, accR, accG, accB), dx) =>
        (-ry to ry).foldLeft((accA, accR, accG, accB)) { case ((a, r, g, b), dy) =>
          val imgX = x + dx
          val imgY = y + dy
          if imgX >= 0 && imgX < width && imgY >= 0 && imgY < height then
            val pixel = src(imgX, imgY)
            val weight = kernel(rx - dx, ry - dy)
            (
              a + alpha(pixel) * weight,
              r + red(pixel) * weight,
              g + green(pixel) * weight,
              b + blue(pixel) * weight
            )
          else
            (a, r, g, b)
        }
      }

    (sumA, sumR, sumG, sumB)

/** Blur filter, computes a convolution between the image and the given blurring
  * kernel.
  */
class Blur(src: Image, kernel: Kernel) extends Image(src.height, src.width):
  private val convolution = Convolution(
    src,
    kernel.map(_ / kernel.sum)
  ).toImage // for blurring, kernels are normalized to have sum = 1
  def apply(x: Int, y: Int): Pixel = convolution(x, y)

/** Box blur filter, blur filter with matrix of size `(radius * 2 + 1) x (radius
  * * 2 + 1)` filled with ones.
  */
class BoxBlur(src: Image, radius: Int) extends Blur(src, Kernel.uniform(radius * 2 + 1))

/** Gaussian blur filter, blurs with a 3x3 Gaussian kernel. */
class GaussianBlur(src: Image) extends Blur(src, Kernel.gaussian3x3)
