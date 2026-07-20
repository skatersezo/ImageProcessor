package com.ivanespinosa.imageprocessor

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Pixel(red: Int, green: Int, blue: Int) {
  assert(isWithinColorRange(red) && isWithinColorRange(green) && isWithinColorRange(blue))

  private def isWithinColorRange(color: Int): Boolean = color >= 0 && color < 256

  // color hex: #12ffcb
  // red = 000000000000000000000000rrrrrrrr
  // green = 000000000000000000000000gggggggg
  // blue = 000000000000000000000000bbbbbbbb
  // n = 00000000rrrrrrrrggggggggbbbbbbbb
  def toInt: Int = (red << 16) | (green << 8) | blue

  infix def +(other: Pixel): Pixel =
    Pixel(
      Pixel.clamp(red + other.red),
      Pixel.clamp(green + other.green),
      Pixel.clamp(blue + other.blue)
    )

  def draw(width: Int, height: Int, path: String) = {
    val color = toInt
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    var pixels = Array.fill(width * height)(color)
    image.setRGB(0, 0, width, height, pixels, 0, width)
    ImageIO.write(image, "JPG", new File(path))
  }
}

object Pixel {
  val BLACK = Pixel(0,0,0)
  val WHITE = Pixel(255,255,255)
  val RED = Pixel(255, 0,0)
  val GREEN = Pixel(0,255,0)
  val BLUE = Pixel(0,0,255)
  val GRAY = Pixel(128,128,128)

  def clamp(v: Int): Int = {
    if v <= 0 then 0
    else if v >= 255 then 255
    else v
  }

  // color    = 00000000rrrrrrrrggggggggbbbbbbbb
  // red mask = 00000000111111110000000000000000
  def fromHex(color: Int): Pixel =
    Pixel(
      (color & 0xFF0000) >> 16,
      (color & 0xFF00) >> 8,
      color & 0xFF
    )

  def main(args: Array[String]): Unit = {
    val red = Pixel(255, 0, 0)
    val green = Pixel(0, 255, 0)
    val yellow = red + green
    val pink = Transparency(0.5).combine(RED, WHITE)
    val darkRed = Multiply.combine(RED, GRAY)
    val lightRed = Screen.combine(RED, GRAY)
    //yellow.draw(40, 40, "src/main/resources/pixels/new-yellow.jpg")
    //pink.draw(40, 40, "src/main/resources/pixels/pink.jpg")
    //darkRed.draw(40, 40, "src/main/resources/pixels/dark-red.jpg")
    lightRed.draw(40, 40, "src/main/resources/pixels/light-red.jpg")
  }
}
