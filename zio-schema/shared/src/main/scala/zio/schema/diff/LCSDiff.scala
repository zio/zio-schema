package zio.schema.diff

/**
 * One step of an edit script between two sequences: insert an element,
 * delete an element, or keep an element of the original sequence. The
 * longest-common-subsequence differs, such as `Differ.string` and
 * `Differ.chunk`, produce these steps, and `Patch.LCS` holds them.
 */
sealed trait Edit[A]
/** The edit steps. */
object Edit {
  /** Returns the opposite step: `Insert` becomes `Delete` and the reverse; `Keep` stays `Keep`. */
  def invert[A](edit: Edit[A]): Edit[A] = edit match {
    case Insert(value) => Delete(value)
    case Delete(value) => Insert(value)
    case Keep(value)   => Keep(value)
  }
  /** Inserts `value` into the result. */
  final case class Insert[A](value: A) extends Edit[A]
  /** Removes `value`, the next element of the original sequence. */
  final case class Delete[A](value: A) extends Edit[A]
  /** Keeps `value`, the next element of the original sequence. */
  final case class Keep[A](value: A)   extends Edit[A]
}
