package zio.schema.annotation

import scala.annotation.StaticAnnotation

/*
 * Automatically added in sealed traits with only case objects or case class without parameters.
 * Gives error if it used in other types of enumerations.
 */

final case class simpleEnum(automaticallyAdded: Boolean = false) extends StaticAnnotation
