package zio.schema

object syntax extends SchemaSyntax

trait SchemaSyntax {
  implicit class DiffOps[A: Schema](a: A) {
    def diff(that: A): Diff[A] = Schema[A].diff(a, that)

    /**
     * alias for diff that does not conflict with scala stdlib
     */
    def diffEach(that: A): Diff[A] = Schema[A].diff(a, that)

    def runPatch(diff: Diff[A]): Either[String, A] = Schema[A].patch(a, diff)
  }

  implicit class DynamicValueOps[A: Schema](a: A) {
    def dynamic: DynamicValue = Schema[A].toDynamic(a)
  }

  implicit class MigrationOps[A: Schema](a: A) {

    def migrate[B: Schema]: Either[String, B] =
      Schema[A].migrate(Schema[B]).flatMap(f => f(a))
  }
}
