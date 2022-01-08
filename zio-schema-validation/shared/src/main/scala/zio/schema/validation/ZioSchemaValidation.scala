package zio.schema.validation

object ZioSchemaValidation {
  // Goal: a way to validate zio-schema data that can be serialized
  // - validation cannot embed any scala functions
  // - validations should be able to be stored as generic data
  // - meta-level 
  // Want: A DSL That can encode rules about what valid data is
  //  1) generated in a way that does not care about types
  //  2) we can also try to leverage a type system
  //   validation: A => Boolean


  // validation.fromJsonSchema(schema)

  // Data type:
  sealed trait Bool[A] { self =>
    def &&(that: Bool[A]): Bool[A] = Bool.And(self, that)
    def ||(that: Bool[A]): Bool[A] = Bool.Or(self, that)
    def unary_! : Bool[A] = Bool.Not(self)
  }
  object Bool {
    final case class And[A](left: Bool[A], right: Bool[A]) extends Bool[A]
    final case class Or[A](left: Bool[A], right: Bool[A]) extends Bool[A]
    final case class Leaf[A](value: A) extends Bool[A]
    final case class Not[A](value: Bool[A]) extends Bool[A]
  }

  sealed trait Regex

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

  // Predicates:

  sealed trait Predicate[A]
  object Predicate {
    // String => Boolean
    sealed trait Str[A] extends Predicate[A] //TODO maybe remove type param
    object Str {
      final case class MinLength(n: Int) extends Str[String]
      final case class MaxLength(n: Int) extends Str[String]
      final case class Matches(r: Regex) extends Str[String]
    }

    // A => Boolean
    sealed trait Num[A] extends Predicate[A] {
      def numType: NumType[A]
    }
    object Num {
      final case class GreaterThan[A](numType: NumType[A], value: A) extends Num[A]
      final case class LessThan[A](numType: NumType[A], value: A) extends Num[A]
      final case class EqualTo[A](numType: NumType[A], value: A) extends Num[A]
    }

    final case class True[A]() extends Predicate[A] // A => True
  }

  final case class Validation[A](bool: Bool[Predicate[A]]) { self =>
    def &&(that: Validation[A]): Validation[A] = Validation(self.bool && that.bool)
    def ||(that: Validation[A]): Validation[A] = Validation(self.bool || that.bool)
    def unary_! : Validation[A] = Validation(!self.bool)
  }
  object Validation {
    import Predicate._
    
    // String operations
    def minLength(n: Int): Validation[String] = Validation(Bool.Leaf(Str.MinLength(n)))
    def maxLength(n: Int): Validation[String] = Validation(Bool.Leaf(Str.MaxLength(n)))

    // Numerical operations
    def greaterThan[A](value: A)(implicit numType: NumType[A]): Validation[A] = Validation(Bool.Leaf(Num.GreaterThan(numType, value)))
    def lessThan[A](value: A)(implicit numType: NumType[A]): Validation[A] = Validation(Bool.Leaf(Num.LessThan(numType, value)))
    def equalTo[A](value: A)(implicit numType: NumType[A]): Validation[A] = Validation(Bool.Leaf(Num.EqualTo(numType, value)))

    def any[A]: Validation[A] = Validation(Bool.Leaf(Predicate.True[A]()))
    def none[A]: Validation[A] = !any[A]

    // TODO:
    // all validations apply by glueing them together with && (start with any)
    // have overloads that take an iterable (List, Set, etc)
    def allOf[A](vs: Validation[A]*): Validation[A] = vs.foldLeft(any[A])(_ && _)
    def allOf[A](vl: Iterable[Validation[A]]): Validation[A] = allOf(vl.toSeq:_*)

    // glue with || (start with none)
    def anyOf[A](vs: Validation[A]*): Validation[A] = vs.foldLeft(none[A])(_ || _)
    def anyOf[A](vl: Iterable[Validation[A]]): Validation[A] = anyOf(vl.toSeq:_*)
  }

  object examples {
    import Validation._

    val nameValidation: Validation[String] = minLength(4) && maxLength(50)

    val anyInt: Validation[Int] = anyOf(greaterThan(2), lessThan(4), equalTo(3))
    val anyString: Validation[String] = anyOf(minLength(3), maxLength(10))

    val all: Validation[String] = allOf(minLength(3), maxLength(4))
    val allInt = allOf(greaterThan(2), lessThan(4), equalTo(3))
  }



  // object Validation {
  //   // all the leaves will need to have specialized type parameters
  //   // 1) verify that a field inside a record has a certain type
  //   // 2) verify that an integer falls in a certain range *
  //   // 3) verify that a string length falls inside a certain range *
  //   // 4) verify that a string matches a simplified regular expression *
  //   // 5) verify that a decimal number falls within a certain range. *
  //   // 6) verify that strings represent/contain date-time information.
  //   // 7) cross-field validation
    
  //   final case class Str(stringValidation: Bool[Str]) extends Validation[String] { self =>
  //     def &&(that: Str): Str = {
  //       Str(self.stringValidation && that.stringValidation)
  //     }
  //     def ||(that: Str): Str = {
  //       Str(self.stringValidation || that.stringValidation)
  //     }
  //     def unary_! = Str(!self.stringValidation)
  //   }

  //   final case class Num[A](Num: Bool[Num[A]], numType: NumType[A]) extends Validation[A] { self =>
  //     def &&(that: Num[A]): Num[A] = Num(self.Num && that.Num, that.numType)
  //     def ||(that: Num[A]): Num[A] = Num(self.Num || that.Num, that.numType)
  //     def unary_! : Validation[A] = Num(!self.Num, self.numType)
  //   }
  // }

  // Problems:
  //  - no cross-field validation

  // Need a validation that accepts anything

  // TODO
  // fork zio-schema
  // create new directory/package
  // incorporate this file
  // sketch out numeric validation (use BigDecimal). Needs greather than, equals, less than
}
