package zio.schema

/**
 * A function that rewrites a field or case name into a naming convention, such
 * as `snake_case` or `kebab-case`. `JsonCodec.Configuration` takes one as
 * `fieldNameFormat` for field names and inside `DiscriminatorSetting` for case
 * names.
 *
 * {{{
 * val snake: String  = NameFormat.SnakeCase("firstName")   // "first_name"
 * val kebab: String  = NameFormat.KebabCase("firstName")   // "first-name"
 * val pascal: String = NameFormat.PascalCase("first_name") // "FirstName"
 * }}}
 */
sealed trait NameFormat extends (String => String)

/**
 * The predefined name formats, plus [[NameFormat.Custom]] for any other
 * function.
 */
object NameFormat {
  import java.lang.Character._

  private def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String =
    if (s.indexOf('_') == -1 && s.indexOf('-') == -1) {
      if (s.isEmpty) s
      else {
        val ch      = s.charAt(0)
        val fixedCh =
          if (toPascal) toUpperCase(ch)
          else toLowerCase(ch)
        s"$fixedCh${s.substring(1)}"
      }
    } else {
      val len             = s.length
      val sb              = new StringBuilder(len)
      var i               = 0
      var isPrecedingDash = toPascal
      while (i < len) isPrecedingDash = {
        val ch = s.charAt(i)
        i += 1
        (ch == '_' || ch == '-') || {
          val fixedCh =
            if (isPrecedingDash) toUpperCase(ch)
            else toLowerCase(ch)
          sb.append(fixedCh)
          false
        }
      }
      sb.toString
    }

  private def enforceSnakeOrKebabCase(s: String, separator: Char): String = {
    val len                      = s.length
    val sb                       = new StringBuilder(len << 1)
    var i                        = 0
    var isPrecedingNotUpperCased = false
    while (i < len) isPrecedingNotUpperCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (!isUpperCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingNotUpperCased || i > 1 && i < len && !isUpperCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

  /**
   * A name format that applies `f` to the name.
   */
  case class Custom(f: String => String) extends NameFormat {
    override def apply(memberName: String): String = f(memberName)
  }

  /**
   * Converts a name to `snake_case`: `"firstName"` and `"first-name"` become
   * `"first_name"`, and `"HTTPServer"` becomes `"http_server"`. Each `-` or `_`
   * becomes one `_`, so a `-` followed by an upper-case letter gives two
   * (`"Some-Kebab"` becomes `"some__kebab"`).
   */
  case object SnakeCase extends NameFormat {
    override def apply(memberName: String): String = enforceSnakeOrKebabCase(memberName, '_')
  }

  /**
   * Converts a name to `camelCase`: `"first_name"` and `"first-name"` become
   * `"firstName"`. A name without `_` or `-` only has its first letter
   * lower-cased, so `"HTTPServer"` becomes `"hTTPServer"`.
   */
  case object CamelCase extends NameFormat {
    override def apply(memberName: String): String =
      enforceCamelOrPascalCase(memberName, toPascal = false)
  }

  /**
   * Converts a name to `PascalCase`: `"first_name"` and `"first-name"` become
   * `"FirstName"`. A name without `_` or `-` only has its first letter
   * upper-cased, so `"userID"` becomes `"UserID"`.
   */
  case object PascalCase extends NameFormat {
    override def apply(memberName: String): String =
      enforceCamelOrPascalCase(memberName, toPascal = true)
  }

  /**
   * Converts a name to `kebab-case`: `"firstName"` and `"first_name"` become
   * `"first-name"`, and `"HTTPServer"` becomes `"http-server"`. Each `-` or `_`
   * becomes one `-`, so a `_` followed by an upper-case letter gives two.
   */
  case object KebabCase extends NameFormat {
    override def apply(memberName: String): String = enforceSnakeOrKebabCase(memberName, '-')
  }

  /**
   * Returns the name unchanged. It is the default `fieldNameFormat` of
   * `JsonCodec.Configuration`.
   */
  case object Identity extends NameFormat {
    override def apply(memberName: String): String = memberName
  }
}
