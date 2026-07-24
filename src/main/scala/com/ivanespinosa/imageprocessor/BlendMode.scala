package com.ivanespinosa.imageprocessor

// source: https://en.wikipedia.org/wiki/Blend_modes

trait BlendMode {
  def combine(foreground: Pixel, background: Pixel): Pixel
}

class Transparency(f: Double) extends BlendMode {
  private val factor =
    if f <= 0 then 0.0
    else if f >= 1.0 then 1.0
    else f

  // result = foreground * f + background * (1 - f)
  override def combine(foreground: Pixel, background: Pixel): Pixel =
    Pixel(
      (foreground.red * factor + background.red * (1 - factor)).toInt,
      (foreground.green * factor + background.green * (1 - factor)).toInt,
      (foreground.blue * factor + background.blue * (1 - factor)).toInt,
    )
}

object Multiply extends BlendMode {

  // result = (foregound/255 * background/255) * 255
  override def combine(foreground: Pixel, background: Pixel): Pixel =
    Pixel(
      (foreground.red * background.red / 255.0).toInt,
      (foreground.green * background.green / 255.0).toInt,
      (foreground.blue * background.blue / 255.0).toInt,
    )
}

object Screen extends BlendMode {

  // result = 255 - ((255 - foregroun)/255 * (255 - background)/255) * 255
  override def combine(foreground: Pixel, background: Pixel): Pixel =
    Pixel(
      (255 - (255 - foreground.red) * (255 - background.red) / 255.0).toInt,
      (255 - (255 - foreground.green) * (255 - background.green) / 255.0).toInt,
      (255 - (255 - foreground.blue) * (255 - background.blue) / 255.0).toInt,
    )
}

object Overlay extends BlendMode {
  private def f(a: Double, b: Double): Double =
    if a < 0.5 then 2 * a * b
    else 1 - 2 * (1-a) * (1-b)

  override def combine(foreground: Pixel, background: Pixel): Pixel =
    Pixel(
      (255 * f(background.red / 255.0, foreground.red / 255.0)).toInt,
      (255 * f(background.green / 255.0, foreground.green / 255.0)).toInt,
      (255 * f(background.blue / 255.0, foreground.blue / 255.0)).toInt,
    )
}
