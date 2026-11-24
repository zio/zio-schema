package zio.schema

/**
 * Import `zio.schema.syntax._` to call `diffEach`, `runPatch`, `dynamic` and
 * `migrate` directly on any value that has an implicit [[Schema]].
 *
 * {{{
 * import zio.schema.syntax._
 *
 * final case class Point(x: Int, y: Int)
 * object Point {
 *   implicit val schema: Schema[Point] = DeriveSchema.gen[Point]
 * }
 *
 * val patch   = Point(1, 2).diffEach(Point(1, 5))
 * val patched = Point(1, 2).runPatch(patch)
 * // Right(Point(1,5))
 * }}}
 */
object syntax extends SchemaSyntax

/**
 * The extension methods that [[syntax]] brings into scope.
 */
trait SchemaSyntax {

  /**
   * Adds diffing and patching to a value of a type that has an implicit
   * [[Schema]].
   */
  implicit class DiffOps[A: Schema](a: A) {

    /**
     * Returns a [[Patch]] that turns this value into `that`. On collection
     * types, the standard library's own `diff` method takes precedence over
     * this one; use `diffEach` there.
     */
    def diff(that: A): Patch[A] = Schema[A].diff(a, that)

    /**
     * Returns a [[Patch]] that turns this value into `that`. Same as `diff`,
     * under a name that does not clash with the `diff` method of Scala
     * collections.
     */
    def diffEach(that: A): Patch[A] = Schema[A].diff(a, that)

    /**
     * Applies `diff` to this value, returning the patched value or a `Left`
     * with a message when the patch does not apply.
     */
    def runPatch(diff: Patch[A]): Either[String, A] =
      Schema[A].patch(a, diff)
  }

  /**
   * Adds conversion to [[DynamicValue]] to a value of a type that has an
   * implicit [[Schema]].
   */
  implicit class DynamicValueOps[A: Schema](a: A) {

    /**
     * Converts this value to a [[DynamicValue]] with the implicit schema of
     * `A`.
     */
    def dynamic: DynamicValue = Schema[A].toDynamic(a)
  }

  /**
   * Adds migration to another type to a value of a type that has an implicit
   * [[Schema]].
   */
  implicit class MigrationOps[A: Schema](a: A) {

    /**
     * Converts this value to a `B` with `Schema[A].migrate(Schema[B])`, or
     * returns a `Left` when no migration can be derived or the value cannot be
     * converted.
     */
    def migrate[B: Schema]: Either[String, B] =
      Schema[A].migrate(Schema[B]).flatMap(f => f(a))
  }
}
