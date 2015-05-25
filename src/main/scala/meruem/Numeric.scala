package meruem

/**
 * Created by ybamelcash on 5/23/2015.
 */
trait Numeric[A] {
  def value: A
}

case class IntNum(value: Int) extends Numeric[Int]

case class LongNum(value: Long) extends Numeric[Long]

case class DoubleNum(value: Double) extends Numeric[Double]
