package zio.schema

sealed trait NameFormat extends (String => String)

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

  case class Custom(f: String => String) extends NameFormat {
    override def apply(memberName: String): String = f(memberName)
  }

  case object SnakeCase extends NameFormat {
    override def apply(memberName: String): String = enforceSnakeOrKebabCase(memberName, '_')
  }

  case object CamelCase extends NameFormat {
    override def apply(memberName: String): String =
      enforceCamelOrPascalCase(memberName, toPascal = false)
  }

  case object PascalCase extends NameFormat {
    override def apply(memberName: String): String =
      enforceCamelOrPascalCase(memberName, toPascal = true)
  }

  case object KebabCase extends NameFormat {
    override def apply(memberName: String): String = enforceSnakeOrKebabCase(memberName, '-')
  }

  case object Identity extends NameFormat {
    override def apply(memberName: String): String = memberName
  }
}
