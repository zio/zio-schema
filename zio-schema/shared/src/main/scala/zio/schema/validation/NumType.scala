package zio.schema.validation

/**
 * Evidence that `A` is a numeric type accepted by `Validation.greaterThan`,
 * `Validation.lessThan`, `Validation.equalTo` and `Validation.between`. The
 * instances are `Int`, `Long`, `Short`, `Float`, `Double`, `BigDecimal` and
 * `BigInt`.
 */
sealed trait NumType[A] {

  /** Returns the `Numeric` instance used to compare values of `A`. */
  def numeric: Numeric[A]
}

/** The implicit `NumType` instances. */
object NumType {

  /** The `NumType` for `Int`. */
  implicit case object IntType extends NumType[Int] {

    /** Returns the standard `Numeric[Int]`. */
    def numeric: Numeric[Int] = implicitly[Numeric[Int]]
  }

  /** The `NumType` for `Double`. */
  implicit case object DoubleType extends NumType[Double] {

    /** Returns the standard `Numeric[Double]`. */
    def numeric: Numeric[Double] = implicitly[Numeric[Double]]
  }

  /** The `NumType` for `Float`. */
  implicit case object FloatType extends NumType[Float] {

    /** Returns the standard `Numeric[Float]`. */
    def numeric: Numeric[Float] = implicitly[Numeric[Float]]
  }

  /** The `NumType` for `Long`. */
  implicit case object LongType extends NumType[Long] {

    /** Returns the standard `Numeric[Long]`. */
    def numeric: Numeric[Long] = implicitly[Numeric[Long]]
  }

  /** The `NumType` for `Short`. */
  implicit case object ShortType extends NumType[Short] {

    /** Returns the standard `Numeric[Short]`. */
    def numeric: Numeric[Short] = implicitly[Numeric[Short]]
  }

  /** The `NumType` for `BigDecimal`. */
  implicit case object BigDecimalType extends NumType[BigDecimal] {

    /** Returns the standard `Numeric[BigDecimal]`. */
    def numeric: Numeric[BigDecimal] = implicitly[Numeric[BigDecimal]]
  }

  /** The `NumType` for `BigInt`. */
  implicit case object BigIntType extends NumType[BigInt] {

    /** Returns the standard `Numeric[BigInt]`. */
    def numeric: Numeric[BigInt] = implicitly[Numeric[BigInt]]
  }
}
