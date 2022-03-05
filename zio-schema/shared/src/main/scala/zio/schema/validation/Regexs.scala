package zio.schema.validation

trait Regexs {

  val identifier: Validation[String] =
    Validation.regex((Regex.character | Regex.digit).atLeast(1)) // TODO allow underscore _
//   lazy val email: Validation[String] = ??? //TODO write regex here
// val idenfitier: Regex = (Regex.digitOrCharacter).atLeast(1)
}
