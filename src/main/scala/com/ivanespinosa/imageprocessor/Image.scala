package com.ivanespinosa.imageprocessor

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

class Image private (private val buffImage: BufferedImage) {
  val width = buffImage.getWidth
  val height = buffImage.getHeight

  def getColor(x: Int, y: Int): Pixel =
    Pixel.fromHex(buffImage.getRGB(x, y))

  def setColor(x: Int, y: Int, p: Pixel): Unit =
    buffImage.setRGB(x, y, p.toInt)

  def save(path: String): Unit =
    ImageIO.write(buffImage, "JPG", new File(path))

  def saveResource(path: String): Unit =
    save(s"src/main/resources/$path")

  def crop(startX: Int, startY: Int, width: Int, height: Int): Image = {
    areCropArgsValid(startX, startY, width, height)

    val newPixels = Array.fill(width * height)(0)
    buffImage.getRGB(startX, startY, width, height, newPixels, 0, width)
    val newBuffImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    newBuffImage.setRGB(0,0,width,height,newPixels,0,width)
    new Image(newBuffImage)
  }

  def map(f: Pixel => Pixel): Image = {
    val newPixels = Array.fill(width * height)(0)
    buffImage.getRGB(0,0,width, height, newPixels, 0, width)
    newPixels.mapInPlace { color =>
      val pixel = Pixel.fromHex(color)
      val newPixel = f(pixel)
      newPixel.toInt
    }

    val newBuffImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    newBuffImage.setRGB(0,0,width,height,newPixels,0,width)
    new Image(newBuffImage)
  }

  private def areCropArgsValid(x: Int, y: Int, w: Int, h: Int): Unit = {
    assert(
      x >= 0 &&
        y >=0 &&
        w > 0 && h > 0 &&
        x + w < width && y + h < height
    )
  }
}

object Image {
  
  def black(width: Int, height:Int): Image = {
    val colors = Array.fill(width * height)(0)
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    bufferedImage.setRGB(0,0,width,height,colors,0,width)
    new Image(bufferedImage)
  }
  
  def load(path: String): Image =
    new Image(ImageIO.read(new File(path)))

  def loadResource(path: String): Image =
    load(s"src/main/resources/$path")

  def main(args: Array[String]): Unit = {
    // loadResource("islands.png").crop(1200, 100, 300, 300).saveResource("islands_cropped.jpg")
    // loadResource("storm.png").crop(1200, 100, 600, 600).saveResource("storm_cropped_2.jpg")
    val islands = loadResource("islands.png")
    Grayscale(islands).saveResource("islands_grayscale.jpg")
  }
}
