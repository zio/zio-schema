package zio.schema.validation

sealed trait NumType[A] {
  def numeric: Numeric[A]
}

object NumType {
  implicit case object IntType extends NumType[Int] {
    def numeric: Numeric[Int] = implicitly[Numeric[Int]]
  }
  implicit case object DoubleType extends NumType[Double] {
    def numeric: Numeric[Double] = implicitly[Numeric[Double]]
  }
  implicit case object FloatType extends NumType[Float] {
    def numeric: Numeric[Float] = implicitly[Numeric[Float]]
  }
  implicit case object LongType extends NumType[Long] {
    def numeric: Numeric[Long] = implicitly[Numeric[Long]]
  }
  implicit case object ShortType extends NumType[Short] {
    def numeric: Numeric[Short] = implicitly[Numeric[Short]]
  }
  implicit case object BigDecimalType extends NumType[BigDecimal] {
    def numeric: Numeric[BigDecimal] = implicitly[Numeric[BigDecimal]]
  }
  implicit case object BigIntType extends NumType[BigInt] {
    def numeric: Numeric[BigInt] = implicitly[Numeric[BigInt]]
  }
}
