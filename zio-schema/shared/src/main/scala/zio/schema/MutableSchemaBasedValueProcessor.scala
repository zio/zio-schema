package zio.schema

import scala.collection.immutable.ListMap

import zio.schema.annotation.transientField
import zio.{ Chunk, ChunkBuilder }

/** Base trait for mutable value processors, processing a value with a known schema. An example
 * is protocol encoders.
 *
 * The implementation is stack safe and consists of invocations of a series of processXYZ methods, as well
 * as built-in support for a context value which is handled in a stacked way.
 *
 * Maintaining any global state (per process) such as stream writers etc. is the responsibility of the implementation class.
 *
 * The Target type parameter is the base type for the process function's output value. In case the process is
 * built entirely using side effects (such as calls to a mutable writer interface) this type can be Unit.
 *
 * The Context type parameter is the use-case specific context type which is passed for each process invocation, and
 * can be manipulated before each process call achieving a local state.
 */
trait MutableSchemaBasedValueProcessor[Target, Context] {

  /** Process a primitive value */
  protected def processPrimitive(context: Context, value: Any, typ: StandardType[Any]): Target

  /** Called before processing a record (before calling processXYZ for the record's fields) */
  protected def startProcessingRecord(context: Context, schema: Schema.Record[_]): Unit = {}

  /** Process a record in the given context with the given schema, using the already processed values of its fields. */
  protected def processRecord(context: Context, schema: Schema.Record[_], value: ListMap[String, Target]): Target

  /** Called before processing an enum */
  protected def startProcessingEnum(context: Context, schema: Schema.Enum[_]): Unit = {}

  /** Process an enum in the given context with the given schema using the processed constructor  value and it's name */
  protected def processEnum(context: Context, schema: Schema.Enum[_], tuple: (String, Target)): Target

  /** Called before processing a sequence */
  protected def startProcessingSequence(context: Context, schema: Schema.Sequence[_, _, _], size: Int): Unit = {}

  /** Process a sequence using its already processed elements */
  protected def processSequence(context: Context, schema: Schema.Sequence[_, _, _], value: Chunk[Target]): Target

  /** Called before processing a dictionary */
  protected def startProcessingDictionary(context: Context, schema: Schema.Map[_, _], size: Int): Unit = {}

  /*** Process a dictionary using its already processed key-value pairs */
  protected def processDictionary(context: Context, schema: Schema.Map[_, _], value: Chunk[(Target, Target)]): Target

  /** Called before processing a set */
  protected def startProcessingSet(context: Context, schema: Schema.Set[_], size: Int): Unit = {}

  /** Process a set using its already processed elements */
  protected def processSet(context: Context, schema: Schema.Set[_], value: Set[Target]): Target

  /** Called before processing an either value */
  protected def startProcessingEither(context: Context, schema: Schema.Either[_, _]): Unit = {}

  /** Process an either value using its already processed left or right value */
  protected def processEither(context: Context, schema: Schema.Either[_, _], value: Either[Target, Target]): Target

  /** Called before processing a fallback value */
  protected def startProcessingFallback(context: Context, schema: Schema.Fallback[_, _]): Unit = {}

  /** Process a fallback value using its already processed left or right value */
  protected def processFallback(
    context: Context,
    schema: Schema.Fallback[_, _],
    value: zio.schema.Fallback[Target, Target]
  ): Target

  /** Called before processing an option value */
  protected def startProcessingOption(context: Context, schema: Schema.Optional[_]): Unit = {}

  /** Process an optional value using its already processed inner value, or None */
  protected def processOption(context: Context, schema: Schema.Optional[_], value: Option[Target]): Target

  /** Called before processing a pair of values */
  protected def startProcessingTuple(context: Context, schema: Schema.Tuple2[_, _]): Unit = {}

  /** Process a tuple using its already processed left and right values */
  protected def processTuple(context: Context, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  /** Process a dynamic value. If the result is None it indicates that the processor has no
   * built-in support for dynamic values, and the Dynamic value's schema should be used instead. */
  protected def processDynamic(context: Context, value: DynamicValue): Option[Target]

  /** Fails the processing */
  protected def fail(context: Context, message: String): Target

  /** The initial (top-level) context value */
  protected val initialContext: Context

  /** Gets the context for a record's given field within the parent context */
  protected def contextForRecordField(context: Context, index: Int, field: Schema.Field[_, _]): Context

  /** Gets the context for a tuple's given field within the parent context */
  protected def contextForTuple(context: Context, index: Int): Context

  /** Gets the context for an enum's given constructor within the parent context */
  protected def contextForEnumConstructor(context: Context, index: Int, c: Schema.Case[_, _]): Context

  /** Gets the context for an either's left or right value within the parent context */
  protected def contextForEither(context: Context, e: Either[Unit, Unit]): Context

  /** Gets the context for a fallback's left or right value within the parent context */
  protected def contextForFallback(context: Context, e: zio.schema.Fallback[Unit, Unit]): Context

  /** Gets the context for an option's inner value within the parent context */
  protected def contextForOption(context: Context, o: Option[Unit]): Context

  /** Gets the context for a sequence's given element within the parent context */
  protected def contextForSequence(context: Context, schema: Schema.Sequence[_, _, _], index: Int): Context

  /** Gets the context for a dictionary's given element within the parent context */
  protected def contextForMap(context: Context, schema: Schema.Map[_, _], index: Int): Context

  /** Gets the context for a set's given element within the parent context */
  protected def contextForSet(context: Context, schema: Schema.Set[_], index: Int): Context

  /** Process a value based on it's schema */
  def process[A](schema: Schema[A], value: A): Target = {
    var currentSchema: Schema[_]    = schema
    var currentValue: Any           = value
    var result: Option[Target]      = None
    var stack: List[Target => Unit] = List.empty[Target => Unit]
    var contextStack: List[Context] = List(initialContext)

    def push(f: Target => Unit): Unit =
      stack = f :: stack

    def pushContext(s: Context): Unit =
      contextStack = s :: contextStack

    def finishWith(resultValue: Target): Unit =
      if (stack.nonEmpty) {
        val head = stack.head
        stack = stack.tail
        head(resultValue)
      } else {
        result = Some(resultValue)
      }

    def fields(s: Schema.Record[_], record: Any, fs: Schema.Field[_, _]*): Unit = {
      val nonTransientFields = fs.filterNot(_.annotations.exists(_.isInstanceOf[transientField]))
      val values             = ChunkBuilder.make[Target](nonTransientFields.size)

      def processNext(index: Int, remaining: Seq[Schema.Field[_, _]]): Unit =
        if (remaining.isEmpty) {
          finishWith(
            processRecord(
              contextStack.head,
              s,
              nonTransientFields.map(_.name).zip(values.result()).foldLeft(ListMap.empty[String, Target]) {
                case (lm, pair) =>
                  lm.updated(pair._1, pair._2)
              }
            )
          )
        } else {
          val next = remaining.head
          currentSchema = next.schema
          currentValue = next.asInstanceOf[Schema.Field[Any, Any]].get(record)
          pushContext(contextForRecordField(contextStack.head, index, next))
          push(processField(index, remaining, _))
        }

      def processField(index: Int, currentStructure: Seq[Schema.Field[_, _]], fieldResult: Target): Unit = {
        contextStack = contextStack.tail
        values += fieldResult
        val remaining = currentStructure.tail
        processNext(index + 1, remaining)
      }

      startProcessingRecord(contextStack.head, s)
      processNext(0, nonTransientFields)
    }

    def enumCases(s: Schema.Enum[_], cs: Schema.Case[_, _]*): Unit = {
      startProcessingEnum(contextStack.head, s)

      var found = false
      val it    = cs.iterator
      var index = 0
      while (!found && it.hasNext) {
        val c = it.next().asInstanceOf[Schema.Case[Any, Any]]
        c.deconstructOption(currentValue) match {
          case Some(v) =>
            currentValue = v
            currentSchema = c.schema
            pushContext(contextForEnumConstructor(contextStack.head, index, c))
            push { dv =>
              contextStack = contextStack.tail
              finishWith(processEnum(contextStack.head, s, c.id -> dv))
            }
            found = true
          case None =>
            index = index + 1
        }
      }

      if (!found) {
        //This should never happen unless someone manually builds an Enum and doesn't include all cases
        finishWith(fail(contextStack.head, "Invalid enum constructor"))
      }
    }

    while (result.isEmpty) {
      val currentContext = contextStack.head

      currentSchema match {

        case l @ Schema.Lazy(_) =>
          currentSchema = l.schema

        case Schema.Primitive(p, _) =>
          finishWith(processPrimitive(currentContext, currentValue, p.asInstanceOf[StandardType[Any]]))

        case s @ Schema.GenericRecord(_, structure, _) =>
          val map                = currentValue.asInstanceOf[ListMap[String, _]]
          val nonTransientFields = structure.toChunk.filterNot(_.annotations.exists(_.isInstanceOf[transientField]))
          val values             = ChunkBuilder.make[Target](nonTransientFields.size)

          def processNext(index: Int, remaining: Seq[Schema.Field[ListMap[String, _], _]]): Unit =
            if (remaining.isEmpty) {
              finishWith(
                processRecord(
                  currentContext,
                  s,
                  nonTransientFields.map(_.name).zip(values.result()).foldLeft(ListMap.empty[String, Target]) {
                    case (lm, pair) =>
                      lm.updated(pair._1, pair._2)
                  }
                )
              )
            } else {
              val next = remaining.head
              currentSchema = next.schema
              currentValue = map(next.name)
              pushContext(contextForRecordField(currentContext, index, next))
              push(processField(index, remaining, _))
            }

          def processField(
            index: Int,
            currentStructure: Seq[Schema.Field[ListMap[String, _], _]],
            fieldResult: Target
          ): Unit = {
            contextStack = contextStack.tail
            values += fieldResult
            val remaining = currentStructure.tail
            processNext(index + 1, remaining)
          }

          startProcessingRecord(currentContext, s)
          processNext(0, nonTransientFields)

        case s @ Schema.Enum1(_, case1, _) =>
          enumCases(s, case1)

        case s @ Schema.Enum2(_, case1, case2, _) =>
          enumCases(s, case1, case2)

        case s @ Schema.Enum3(_, case1, case2, case3, _) =>
          enumCases(s, case1, case2, case3)

        case s @ Schema.Enum4(_, case1, case2, case3, case4, _) =>
          enumCases(s, case1, case2, case3, case4)

        case s @ Schema.Enum5(_, case1, case2, case3, case4, case5, _) =>
          enumCases(s, case1, case2, case3, case4, case5)

        case s @ Schema.Enum6(_, case1, case2, case3, case4, case5, case6, _) =>
          enumCases(s, case1, case2, case3, case4, case5, case6)

        case s @ Schema.Enum7(_, case1, case2, case3, case4, case5, case6, case7, _) =>
          enumCases(s, case1, case2, case3, case4, case5, case6, case7)

        case s @ Schema.Enum8(_, case1, case2, case3, case4, case5, case6, case7, case8, _) =>
          enumCases(s, case1, case2, case3, case4, case5, case6, case7, case8)

        case s @ Schema.Enum9(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, _) =>
          enumCases(s, case1, case2, case3, case4, case5, case6, case7, case8, case9)

        case s @ Schema.Enum10(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, _) =>
          enumCases(s, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10)

        case s @ Schema.Enum11(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, _) =>
          enumCases(s, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11)

        case s @ Schema.Enum12(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              _
            ) =>
          enumCases(s, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12)

        case s @ Schema.Enum13(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13
          )

        case s @ Schema.Enum14(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14
          )

        case s @ Schema.Enum15(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15
          )

        case s @ Schema.Enum16(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16
          )

        case s @ Schema.Enum17(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              case17,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16,
            case17
          )

        case s @ Schema.Enum18(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              case17,
              case18,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16,
            case17,
            case18
          )

        case s @ Schema.Enum19(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              case17,
              case18,
              case19,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16,
            case17,
            case18,
            case19
          )

        case s @ Schema.Enum20(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              case17,
              case18,
              case19,
              case20,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16,
            case17,
            case18,
            case19,
            case20
          )

        case s @ Schema.Enum21(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              case17,
              case18,
              case19,
              case20,
              case21,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16,
            case17,
            case18,
            case19,
            case20,
            case21
          )

        case s @ Schema.Enum22(
              _,
              case1,
              case2,
              case3,
              case4,
              case5,
              case6,
              case7,
              case8,
              case9,
              case10,
              case11,
              case12,
              case13,
              case14,
              case15,
              case16,
              case17,
              case18,
              case19,
              case20,
              case21,
              case22,
              _
            ) =>
          enumCases(
            s,
            case1,
            case2,
            case3,
            case4,
            case5,
            case6,
            case7,
            case8,
            case9,
            case10,
            case11,
            case12,
            case13,
            case14,
            case15,
            case16,
            case17,
            case18,
            case19,
            case20,
            case21,
            case22
          )
        //scalafmt: { maxColumn = 120 }

        case s @ Schema.EnumN(_, cases, _) =>
          enumCases(s, cases.toSeq: _*)

        case Schema.Fail(message, _) =>
          finishWith(fail(currentContext, message))

        case s @ Schema.Sequence(schema, _, toChunk, _, _) =>
          val inputChunk  = toChunk.asInstanceOf[Any => Chunk[Any]](currentValue)
          val resultChunk = ChunkBuilder.make[Target](inputChunk.size)

          def processNext(inputIdx: Int): Unit = {
            contextStack = contextStack.tail
            if (inputIdx == inputChunk.size) {
              finishWith(processSequence(currentContext, s, resultChunk.result()))
            } else {
              currentSchema = schema
              currentValue = inputChunk(inputIdx)
              pushContext(contextForSequence(currentContext, s, inputIdx))
              push { dv =>
                resultChunk += dv
                processNext(inputIdx + 1)
              }
            }
          }

          startProcessingSequence(currentContext, s, inputChunk.size)
          pushContext(contextForSequence(currentContext, s, 0))
          processNext(0)

        case s @ Schema.Map(ks: Schema[k], vs: Schema[v], _) =>
          val inputChunk  = Chunk.fromIterable(currentValue.asInstanceOf[Map[k, v]])
          val resultChunk = ChunkBuilder.make[(Target, Target)](inputChunk.size)

          def processNext(inputIdx: Int): Unit =
            if (inputIdx == inputChunk.size) {
              finishWith(processDictionary(currentContext, s, resultChunk.result()))
            } else {
              currentSchema = ks
              val currentTuple = inputChunk(inputIdx)
              currentValue = currentTuple._1

              pushContext(contextForMap(currentContext, s, inputIdx))
              push { (a: Target) =>
                contextStack = contextStack.tail

                currentSchema = vs
                currentValue = currentTuple._2
                pushContext(contextForMap(currentContext, s, inputIdx))
                push { (b: Target) =>
                  contextStack = contextStack.tail
                  val pair = (a, b)
                  resultChunk += pair
                  processNext(inputIdx + 1)
                }
              }
            }

          startProcessingDictionary(currentContext, s, inputChunk.size)
          processNext(0)

        case s @ Schema.Set(as: Schema[a], _) =>
          val inputChunk  = Chunk.fromIterable(currentValue.asInstanceOf[Set[a]])
          val resultChunk = ChunkBuilder.make[Target](inputChunk.size)

          def processNext(inputIdx: Int): Unit = {
            contextStack = contextStack.tail
            if (inputIdx == inputChunk.size) {
              finishWith(processSet(currentContext, s, resultChunk.result().toSet))
            } else {
              currentSchema = as
              currentValue = inputChunk(inputIdx)
              pushContext(contextForSet(currentContext, s, inputIdx))
              push { dv =>
                resultChunk += dv
                processNext(inputIdx + 1)
              }
            }
          }

          startProcessingSet(currentContext, s, inputChunk.size)
          pushContext(contextForSet(currentContext, s, 0))
          processNext(0)

        case s: Schema.Either[l, r] =>
          startProcessingEither(currentContext, s)
          currentValue.asInstanceOf[Either[l, r]] match {
            case Left(value: l) =>
              currentValue = value
              currentSchema = s.left
              pushContext(contextForEither(currentContext, Left(())))
              push { dyn =>
                contextStack = contextStack.tail
                finishWith(processEither(currentContext, s, Left(dyn)))
              }
            case Right(value: r) =>
              currentValue = value
              currentSchema = s.right
              pushContext(contextForEither(currentContext, Right(())))
              push { dyn =>
                contextStack = contextStack.tail
                finishWith(processEither(currentContext, s, Right(dyn)))
              }
          }

        case s: Schema.Fallback[l, r] =>
          startProcessingFallback(currentContext, s)
          currentValue.asInstanceOf[zio.schema.Fallback[l, r]] match {
            case zio.schema.Fallback.Left(value: l) =>
              currentValue = value
              currentSchema = s.left
              pushContext(contextForFallback(currentContext, zio.schema.Fallback.Left(())))
              push { dyn =>
                contextStack = contextStack.tail
                finishWith(processFallback(currentContext, s, zio.schema.Fallback.Left(dyn)))
              }
            case zio.schema.Fallback.Right(value: r) =>
              currentValue = value
              currentSchema = s.right
              pushContext(contextForFallback(currentContext, zio.schema.Fallback.Right(())))
              push { dyn =>
                contextStack = contextStack.tail
                finishWith(processFallback(currentContext, s, zio.schema.Fallback.Right(dyn)))
              }
            case zio.schema.Fallback.Both(left, right) =>
              currentValue = left
              currentSchema = s.left
              pushContext(contextForFallback(currentContext, zio.schema.Fallback.Both((), ())))
              push { dynLeft =>
                contextStack = contextStack.tail
                currentValue = right
                currentSchema = s.right
                pushContext(contextForFallback(currentContext, zio.schema.Fallback.Right(())))
                push { dynRight =>
                  contextStack = contextStack.tail
                  finishWith(processFallback(currentContext, s, zio.schema.Fallback.Both(dynLeft, dynRight)))
                }
              }
          }

        case s: Schema.Tuple2[a, b] =>
          startProcessingTuple(currentContext, s)
          val (a: a, b: b) = currentValue.asInstanceOf[(a, b)]
          currentValue = a
          currentSchema = s.left
          pushContext(contextForTuple(currentContext, 1))
          push { dynA =>
            currentValue = b
            currentSchema = s.right
            contextStack = contextStack.tail
            pushContext(contextForTuple(currentContext, 2))
            push { dynB =>
              contextStack = contextStack.tail
              finishWith(processTuple(currentContext, s, dynA, dynB))
            }
          }

        case s: Schema.Optional[a] =>
          startProcessingOption(currentContext, s)
          currentValue.asInstanceOf[Option[a]] match {
            case Some(value: a) =>
              currentValue = value
              currentSchema = s.schema
              pushContext(contextForOption(currentContext, Some(())))
              push { dyn =>
                contextStack = contextStack.tail
                finishWith(processOption(currentContext, s, Some(dyn)))
              }
            case None =>
              finishWith(processOption(currentContext, s, None))
          }

        case Schema.Transform(schema, _, g, _, _) =>
          g.asInstanceOf[Any => Either[String, Any]](currentValue) match {
            case Left(message) =>
              finishWith(fail(currentContext, message))
            case Right(a) =>
              currentValue = a
              currentSchema = schema
          }

        case s @ Schema.CaseClass0(_, _, _) =>
          fields(s, currentValue)

        case s @ Schema.CaseClass1(_, f, _, _) =>
          fields(s, currentValue, f)

        case s @ Schema.CaseClass2(_, f1, f2, _, _) =>
          fields(s, currentValue, f1, f2)
        case s @ Schema.CaseClass3(_, f1, f2, f3, _, _) =>
          fields(s, currentValue, f1, f2, f3)
        case s @ Schema.CaseClass4(_, f1, f2, f3, f4, _, _) =>
          fields(s, currentValue, f1, f2, f3, f4)
        case s @ Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, _) =>
          fields(s, currentValue, f1, f2, f3, f4, f5)
        case s @ Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, _) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6)
        case s @ Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, _) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7)
        case s @ Schema.CaseClass8(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8)
        case s @ Schema.CaseClass9(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9)
        case s @ Schema.CaseClass10(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
        case s @ Schema.CaseClass11(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
        case s @ Schema.CaseClass12(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
        case s @ Schema.CaseClass13(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
        case s @ Schema.CaseClass14(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
        case s @ Schema.CaseClass15(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
        case s @ Schema.CaseClass16(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        case s @ Schema.CaseClass17(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              f17,
              _,
              _
            ) =>
          fields(s, currentValue, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
        case s @ Schema.CaseClass18(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              f17,
              f18,
              _,
              _
            ) =>
          fields(
            s,
            currentValue,
            f1,
            f2,
            f3,
            f4,
            f5,
            f6,
            f7,
            f8,
            f9,
            f10,
            f11,
            f12,
            f13,
            f14,
            f15,
            f16,
            f17,
            f18
          )
        case s @ Schema.CaseClass19(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              f17,
              f18,
              f19,
              _,
              _
            ) =>
          fields(
            s,
            currentValue,
            f1,
            f2,
            f3,
            f4,
            f5,
            f6,
            f7,
            f8,
            f9,
            f10,
            f11,
            f12,
            f13,
            f14,
            f15,
            f16,
            f17,
            f18,
            f19
          )
        case s @ Schema.CaseClass20(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              f17,
              f18,
              f19,
              f20,
              _
            ) =>
          fields(
            s,
            currentValue,
            f1,
            f2,
            f3,
            f4,
            f5,
            f6,
            f7,
            f8,
            f9,
            f10,
            f11,
            f12,
            f13,
            f14,
            f15,
            f16,
            f17,
            f18,
            f19,
            f20
          )
        case s @ Schema.CaseClass21(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              f17,
              f18,
              f19,
              f20,
              tail
            ) =>
          fields(
            s,
            currentValue,
            f1,
            f2,
            f3,
            f4,
            f5,
            f6,
            f7,
            f8,
            f9,
            f10,
            f11,
            f12,
            f13,
            f14,
            f15,
            f16,
            f17,
            f18,
            f19,
            f20,
            tail._1
          )
        case s @ Schema.CaseClass22(
              _,
              f1,
              f2,
              f3,
              f4,
              f5,
              f6,
              f7,
              f8,
              f9,
              f10,
              f11,
              f12,
              f13,
              f14,
              f15,
              f16,
              f17,
              f18,
              f19,
              f20,
              tail
            ) =>
          fields(
            s,
            currentValue,
            f1,
            f2,
            f3,
            f4,
            f5,
            f6,
            f7,
            f8,
            f9,
            f10,
            f11,
            f12,
            f13,
            f14,
            f15,
            f16,
            f17,
            f18,
            f19,
            f20,
            tail._1,
            tail._2
          )
        case Schema.Dynamic(_) =>
          processDynamic(currentContext, currentValue.asInstanceOf[DynamicValue]) match {
            case Some(target) => finishWith(target)
            case None =>
              currentSchema = DynamicValue.schema
          }

        case _ => throw new Exception(s"Missing a handler for schema ${currentSchema.toString()}.")
      }
    }
    result.get
  }
}

/** A simpler version of MutableSchemaBasedValueProcessor without using any Context  */
trait SimpleMutableSchemaBasedValueProcessor[Target] extends MutableSchemaBasedValueProcessor[Target, Unit] {

  protected def processPrimitive(value: Any, typ: StandardType[Any]): Target

  protected def processRecord(schema: Schema.Record[_], value: ListMap[String, Target]): Target

  protected def processEnum(schema: Schema.Enum[_], tuple: (String, Target)): Target

  protected def processSequence(schema: Schema.Sequence[_, _, _], value: Chunk[Target]): Target

  protected def processDictionary(schema: Schema.Map[_, _], value: Chunk[(Target, Target)]): Target

  protected def processSet(schema: Schema.Set[_], value: Set[Target]): Target

  protected def processEither(schema: Schema.Either[_, _], value: Either[Target, Target]): Target

  protected def processFallback(schema: Schema.Fallback[_, _], value: zio.schema.Fallback[Target, Target]): Target

  protected def processOption(schema: Schema.Optional[_], value: Option[Target]): Target

  protected def processTuple(schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  protected def processDynamic(value: DynamicValue): Option[Target]

  protected def fail(message: String): Target

  override protected def processPrimitive(context: Unit, value: Any, typ: StandardType[Any]): Target =
    processPrimitive(value, typ)

  override protected def processRecord(
    context: Unit,
    schema: Schema.Record[_],
    value: ListMap[String, Target]
  ): Target =
    processRecord(schema, value)

  override protected def processEnum(context: Unit, schema: Schema.Enum[_], tuple: (String, Target)): Target =
    processEnum(schema, tuple)

  override protected def processSequence(
    context: Unit,
    schema: Schema.Sequence[_, _, _],
    value: Chunk[Target]
  ): Target =
    processSequence(schema, value)

  override protected def processDictionary(
    context: Unit,
    schema: Schema.Map[_, _],
    value: Chunk[(Target, Target)]
  ): Target =
    processDictionary(schema, value)

  override protected def processSet(context: Unit, schema: Schema.Set[_], value: Set[Target]): Target =
    processSet(schema, value)

  override protected def processEither(
    context: Unit,
    schema: Schema.Either[_, _],
    value: Either[Target, Target]
  ): Target =
    processEither(schema, value)

  override protected def processFallback(
    context: Unit,
    schema: Schema.Fallback[_, _],
    value: zio.schema.Fallback[Target, Target]
  ): Target =
    processFallback(schema, value)

  override protected def processOption(context: Unit, schema: Schema.Optional[_], value: Option[Target]): Target =
    processOption(schema, value)

  override protected def processTuple(context: Unit, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target =
    processTuple(schema, left, right)

  override protected def fail(context: Unit, message: String): Target =
    fail(message)

  override protected def processDynamic(context: Unit, value: DynamicValue): Option[Target] =
    processDynamic(value)

  override protected val initialContext: Unit = ()

  override protected def contextForRecordField(context: Unit, index: Int, field: Schema.Field[_, _]): Unit =
    ()

  override protected def contextForEnumConstructor(context: Unit, index: Int, c: Schema.Case[_, _]): Unit =
    ()

  override protected def contextForEither(context: Unit, e: Either[Unit, Unit]): Unit =
    ()

  override protected def contextForFallback(context: Unit, e: zio.schema.Fallback[Unit, Unit]): Unit =
    ()

  override protected def contextForOption(context: Unit, o: Option[Unit]): Unit =
    ()

  override protected def contextForTuple(context: Unit, index: Int): Unit =
    ()

  override protected def contextForSequence(context: Unit, schema: Schema.Sequence[_, _, _], index: Int): Unit =
    ()

  override protected def contextForMap(context: Unit, schema: Schema.Map[_, _], index: Int): Unit =
    ()

  override protected def contextForSet(context: Unit, schema: Schema.Set[_], index: Int): Unit =
    ()
}
