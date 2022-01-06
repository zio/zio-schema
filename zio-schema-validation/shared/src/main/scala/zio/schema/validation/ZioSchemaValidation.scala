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

  // String => Boolean
  sealed trait StringP[A]
  object StringP {
    final case class MinLength(n: Int) extends StringP[String]
    final case class MaxLength(n: Int) extends StringP[String]
    final case class Matches(r: Regex) extends StringP[String]
  }

  // 
  sealed trait NumType[A]
  object NumType {
    implicit case object IntType extends NumType[Int]
    implicit case object DoubleType extends NumType[Double]
    implicit case object FloatType extends NumType[Float]
    implicit case object LongType extends NumType[Long]
    implicit case object ShortType extends NumType[Short]
    implicit case object BigDecimalType extends NumType[BigDecimal]
    implicit case object BigIntType extends NumType[BigInt]
  }

  // A => Boolean
  sealed trait NumP[A] {
    def numType: NumType[A]
  }
  object NumP {
    final case class GreaterThan[A](numType: NumType[A], value: A) extends NumP[A]
    final case class LessThan[A](numType: NumType[A], value: A) extends NumP[A]
    final case class EqualTo[A](numType: NumType[A], value: A) extends NumP[A]
  }

  final case class Validation[Pred[_], A](bool: Bool[Pred[A]]) { self =>
    def &&(that: Validation[Pred, A]): Validation[Pred, A] = Validation(self.bool && that.bool)
    def ||(that: Validation[Pred, A]): Validation[Pred, A] = Validation(self.bool || that.bool)
    def unary_! : Validation[Pred, A] = Validation(!self.bool)
  }
  object Validation {
    def minLength(n: Int): Validation[StringP, String] = Validation(Bool.Leaf(StringP.MinLength(n)))

    def maxLength(n: Int): Validation[StringP, String] = Validation(Bool.Leaf(StringP.MaxLength(n)))

    //TODO add num operations
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
    
  //   final case class Str(stringValidation: Bool[StringP]) extends Validation[String] { self =>
  //     def &&(that: Str): Str = {
  //       Str(self.stringValidation && that.stringValidation)
  //     }
  //     def ||(that: Str): Str = {
  //       Str(self.stringValidation || that.stringValidation)
  //     }
  //     def unary_! = Str(!self.stringValidation)
  //   }

  //   final case class Num[A](NumP: Bool[NumP[A]], numType: NumType[A]) extends Validation[A] { self =>
  //     def &&(that: Num[A]): Num[A] = Num(self.NumP && that.NumP, that.numType)
  //     def ||(that: Num[A]): Num[A] = Num(self.NumP || that.NumP, that.numType)
  //     def unary_! : Validation[A] = Num(!self.NumP, self.numType)
  //   }
  // }

  // Problems:
  //  - no cross-field validation

  // TODO
  // fork zio-schema
  // create new directory/package
  // incorporate this file
  // sketch out numeric validation (use BigDecimal). Needs greather than, equals, less than
}
