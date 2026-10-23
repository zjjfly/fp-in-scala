package com.github.zjjfly.fp
package ch4

/**
 * @author <a href="https://github.com/zjjfly"/>zjjfly</a>
 */
sealed trait MyEither[+L, +R] {
}

case class Left[+L](left: L) extends MyEither[L, Nothing]

case class Right[+R](right: R) extends MyEither[Nothing, R]

object MyEither {
  def mean(xs: Seq[Double]): MyEither[String, Double] =
    if (xs.isEmpty) Left("xxx")
    else Right(xs.sum / xs.length)

  def Try[A](a: => A): MyEither[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}


