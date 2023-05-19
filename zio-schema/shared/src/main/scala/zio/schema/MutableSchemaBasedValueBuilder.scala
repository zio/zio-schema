package zio.schema

import scala.util.control.NonFatal

import zio.prelude.Validation
import zio.schema.MutableSchemaBasedValueBuilder.CreateValueFromSchemaError
import zio.{ Chunk, ChunkBuilder }

/**
 * Base trait for mutable builders producing a value based on a schema, such as codec decoders.
 *
 * The implementation is stack safe and consists of a series of invocations of the protected methods
 * the trait defines. Maintaining the state of the builder, such as stream position etc. is the responsibility
 * of the implementation class via mutable state.
 *
 * The Target type parameter is the base type for the generated values - this in many cases can be Any but
 * potentially could be used to track errors in value level as well - although failure in the context handler
 * manipulation methods cannot be expressed this way.
 *
 * The Context type parameter is a use-case dependent type that is managed in a stack during the execution of the builder.
 * The implementation can generate new context values for the value's subtrees and it can be used to track local state
 * required for gathering all information for the value to be created. The current context value is also propagated to
 * any exception thrown so it can be used to provide detailed location information for decoder errors.
 */
trait MutableSchemaBasedValueBuilder[Target, Context] {

  /** Creates a primitive value of the given standard type */
  protected def createPrimitive(context: Context, typ: StandardType[_]): Target

  /** The next value to build is a record with the given schema */
  protected def startCreatingRecord(context: Context, record: Schema.Record[_]): Context

  /** Called for each field of a record. The resulting tuple is either None indicating there are no more fields to read,
   * or it contains an updated context belonging to the field and the next field's index in the schema. This allows
   * the implementation to instantiate fields in a different order than what the schema defines.
   *
   * The index parameter is a 0-based index, incremented by one for each field read within a record.
   */
  protected def startReadingField(context: Context, record: Schema.Record[_], index: Int): Option[(Context, Int)]

  /** Creates a record value from the gathered field values */
  protected def createRecord(context: Context, record: Schema.Record[_], values: Chunk[(Int, Target)]): Target

  /** The next value to build is an enum with the given schema */
  protected def startCreatingEnum(context: Context, cases: Chunk[Schema.Case[_, _]]): (Context, Int)

  /** Creates an enum value from the read constructor value */
  protected def createEnum(context: Context, cases: Chunk[Schema.Case[_, _]], index: Int, value: Target): Target

  /** The next value to build is a sequence. If the returned value is None, the builder creates an empty sequence,
   * otherwise it calls startCreatingOneSequenceElement and finishedCreatingOneSequenceElement for
   * each element with the returned context.
   */
  protected def startCreatingSequence(context: Context, schema: Schema.Sequence[_, _, _]): Option[Context]

  /** Called before constructing a next sequence element. The returned context is used for constructing
   * that single element.*/
  protected def startCreatingOneSequenceElement(context: Context, schema: Schema.Sequence[_, _, _]): Context

  /** Called after constructing a single sequence element. The context is the context of the whole sequence.
   * If the returned value is true, a next element will be read otherwise the sequence is completed and createSequence
   * is called. */
  protected def finishedCreatingOneSequenceElement(
    context: Context,
    schema: Schema.Sequence[_, _, _],
    index: Int
  ): Boolean

  /** Creates the sequence value from the chunk of element values */
  protected def createSequence(context: Context, schema: Schema.Sequence[_, _, _], values: Chunk[Target]): Target

  /** The next value to build is a dictionary. If the returned value is None, the builder creates an empty dictionary,
   * otherwise it calls startCreatingOneDictionaryElement, startCreatingOneDictionaryValue and
   * finishedCreatingOneDictionaryElement for each element with the returned context.
   */
  protected def startCreatingDictionary(context: Context, schema: Schema.Map[_, _]): Option[Context]

  /** Called before constructing a next dictionary element. The returned context is used for constructing
   * that single element's key. */
  protected def startCreatingOneDictionaryElement(context: Context, schema: Schema.Map[_, _]): Context

  /** Called after the key of a single element was created, before the value gets created.
   * The returned context is for constructing the element's value part. */
  protected def startCreatingOneDictionaryValue(context: Context, schema: Schema.Map[_, _]): Context

  /** Called after constructing a single dictionary element. The context is the context of the whole dictionary.
   * If the returned value is true, a next element will be read otherwise the dictionary is completed and createDictionary
   * is called. */
  protected def finishedCreatingOneDictionaryElement(
    context: Context,
    schema: Schema.Map[_, _],
    index: Int
  ): Boolean

  /** Creates the dictionary value from the chunk of key-value pairs */
  protected def createDictionary(context: Context, schema: Schema.Map[_, _], values: Chunk[(Target, Target)]): Target

  /** The next value to build is a set. If the returned value is None, the builder creates an empty set,
   * otherwise it calls startCreatingOneSetElement and finishedCreatingOneSetElement for
   * each element with the returned context.
   */
  protected def startCreatingSet(context: Context, schema: Schema.Set[_]): Option[Context]

  /** Called before constructing a next set element. The returned context is used for constructing
   * that single element. */
  protected def startCreatingOneSetElement(context: Context, schema: Schema.Set[_]): Context

  /** Called after constructing a single set element. The context is the context of the whole set.
   * If the returned value is true, a next element will be read otherwise the set is completed and createSet
   * is called. */
  protected def finishedCreatingOneSetElement(context: Context, schema: Schema.Set[_], index: Int): Boolean

  /** Creates the set value from the chunk of element values */
  protected def createSet(context: Context, schema: Schema.Set[_], values: Chunk[Target]): Target

  /** The next value to be created is an optional value. If the result is None, the optional value created
   * will be None. If it is a context, that context will be used to create the optional value. */
  protected def startCreatingOptional(context: Context, schema: Schema.Optional[_]): Option[Context]

  /** Creates the optional value from the inner value */
  protected def createOptional(context: Context, schema: Schema.Optional[_], value: Option[Target]): Target

  /** The next value to be created is an either value with the given schema. Similarly to optional values,
   * this method is responsible for gathering enough information to decide whether the created value will
   * be a Left or a Right. The result value represents this, and for each case allows specifying a context
   * that will be used to create the inner value. */
  protected def startCreatingEither(
    context: Context,
    schema: Schema.Either[_, _]
  ): Either[Context, Context]

  /** Create the either value from an inner value */
  protected def createEither(
    context: Context,
    schema: Schema.Either[_, _],
    value: Either[Target, Target]
  ): Target

  /** The next value to be created is a tuple with the given schema. The returned context is used to
   * construct the first element of the tuple. */
  protected def startCreatingTuple(context: Context, schema: Schema.Tuple2[_, _]): Context

  /** Called after finished constructing the first element, before constructing the second. The returned
   * context is used to construct the second value.
   */
  protected def startReadingSecondTupleElement(context: Context, schema: Schema.Tuple2[_, _]): Context

  /** Creates the tuple from the constructed first and second values */
  protected def createTuple(context: Context, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  /** Creates a Dynamic value. If the returned value is None, it indicates that the builder does
   * not have any built-in support for Dynamic values, and it will be built using Dynamic's schema. */
  protected def createDynamic(context: Context): Option[Target]

  /** Transforms a value with the given function that can fail. Making this customizable allows encoding the failure
   * in Target.
   */
  protected def transform(
    context: Context,
    value: Target,
    f: Any => Validation[String, Any],
    schema: Schema[_]
  ): Target

  /** Fail the builder with the given message */
  protected def fail(context: Context, message: String): Target

  /** The initial (top-level) context value */
  protected val initialContext: Context

  /** Create a value of type A with the provided schema using this builder */
  def create[A](schema: Schema[A]): Target = {
    var currentSchema: Schema[_]    = schema
    var result: Option[Target]      = None
    var stack: List[Target => Unit] = List.empty[Target => Unit]
    var contextStack: List[Context] = List(initialContext)

    def finishWith(resultValue: Target): Unit =
      if (stack.nonEmpty) {
        val head = stack.head
        stack = stack.tail
        head(resultValue)
      } else {
        result = Some(resultValue)
      }

    def push(f: Target => Unit): Unit =
      stack = f :: stack

    def pushContext(s: Context): Unit =
      contextStack = s :: contextStack

    def record(record: Schema.Record[_]): Unit = {
      val values = ChunkBuilder.make[(Int, Target)](record.fields.size)

      def readField(index: Int): Unit = {
        contextStack = contextStack.tail
        startReadingField(contextStack.head, record, index) match {
          case Some((updatedState, idx)) =>
            pushContext(updatedState)
            currentSchema = record.fields(idx).schema
            push { field =>
              val elem = (idx, field)
              values += elem
              readField(index + 1)
            }
          case None =>
            finishWith(createRecord(contextStack.head, record, values.result()))
        }
      }

      pushContext(startCreatingRecord(contextStack.head, record))
      readField(0)
    }

    def enumCases(casesChunk: Chunk[Schema.Case[_, _]]): Unit = {
      val (newState, index) = startCreatingEnum(contextStack.head, casesChunk)
      currentSchema = casesChunk(index).schema
      pushContext(newState)
      push { value =>
        contextStack = contextStack.tail
        finishWith(createEnum(contextStack.head, casesChunk, index, value))
      }
    }

    try {
      while (result.isEmpty) {
        val currentContext = contextStack.head
        currentSchema match {

          case l @ Schema.Lazy(_) =>
            currentSchema = l.schema

          case Schema.Primitive(p, _) =>
            finishWith(createPrimitive(currentContext, p.asInstanceOf[StandardType[Any]]))

          case s @ Schema.GenericRecord(_, _, _) =>
            record(s)

          case s @ Schema.Enum1(_, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum2(_, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum3(_, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum4(_, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum5(_, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum6(_, _, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum7(_, _, _, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum8(_, _, _, _, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum9(_, _, _, _, _, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum10(_, _, _, _, _, _, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum11(_, _, _, _, _, _, _, _, _, _, _, _, _) =>
            enumCases(s.cases)

          case s @ Schema.Enum12(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum13(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum14(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum15(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum16(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum17(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum18(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum19(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum20(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum21(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)

          case s @ Schema.Enum22(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            enumCases(s.cases)
          //scalafmt: { maxColumn = 120 }

          case s @ Schema.EnumN(_, _, _) =>
            enumCases(s.cases)

          case Schema.Fail(message, _) =>
            finishWith(fail(currentContext, message))

          case s @ Schema.Sequence(elementSchema, _, _, _, _) =>
            val elems = ChunkBuilder.make[Target]()

            def readOne(index: Int): Unit =
              push { elem =>
                elems += elem

                contextStack = contextStack.tail
                val continue = finishedCreatingOneSequenceElement(contextStack.head, s, index)

                if (continue) {
                  currentSchema = elementSchema
                  pushContext(startCreatingOneSequenceElement(contextStack.head, s))
                  readOne(index + 1)
                } else {
                  contextStack = contextStack.tail
                  finishWith(createSequence(contextStack.head, s, elems.result()))
                }
              }

            currentSchema = elementSchema
            startCreatingSequence(currentContext, s) match {
              case Some(startingState) =>
                pushContext(startingState)
                pushContext(startCreatingOneSequenceElement(startingState, s))
                readOne(0)
              case None =>
                finishWith(createSequence(currentContext, s, Chunk.empty))
            }

          case s @ Schema.Map(ks: Schema[k], vs: Schema[v], _) =>
            val elems = ChunkBuilder.make[(Target, Target)]()

            def readOne(index: Int): Unit =
              push { key =>
                currentSchema = vs
                pushContext(startCreatingOneDictionaryValue(currentContext, s))

                push { value =>
                  val elem = (key, value)
                  elems += elem

                  contextStack = contextStack.tail.tail
                  val continue = finishedCreatingOneDictionaryElement(contextStack.head, s, index)

                  if (continue) {
                    currentSchema = ks
                    pushContext(startCreatingOneDictionaryElement(contextStack.head, s))
                    readOne(index + 1)
                  } else {
                    val state = contextStack.head
                    contextStack = contextStack.tail
                    finishWith(createDictionary(state, s, elems.result()))
                  }
                }
              }

            startCreatingDictionary(currentContext, s) match {
              case Some(startingState) =>
                currentSchema = ks
                pushContext(startingState)
                pushContext(startCreatingOneDictionaryElement(startingState, s))
                readOne(0)
              case None =>
                finishWith(createDictionary(contextStack.head, s, Chunk.empty))
            }

          case s @ Schema.Set(as: Schema[a], _) =>
            val elems = ChunkBuilder.make[Target]()

            def readOne(index: Int): Unit =
              push { elem =>
                elems += elem

                contextStack = contextStack.tail
                val continue = finishedCreatingOneSetElement(contextStack.head, s, index)

                if (continue) {
                  currentSchema = as
                  pushContext(startCreatingOneSetElement(contextStack.head, s))
                  readOne(index + 1)
                } else {
                  val state = contextStack.head
                  contextStack = contextStack.tail
                  finishWith(createSet(state, s, elems.result()))
                }
              }

            startCreatingSet(currentContext, s) match {
              case Some(startingState) =>
                currentSchema = as
                pushContext(startingState)
                pushContext(startCreatingOneSetElement(startingState, s))
                readOne(0)
              case None =>
                finishWith(createSet(contextStack.head, s, Chunk.empty))
            }

          case s: Schema.Either[l, r] =>
            startCreatingEither(currentContext, s) match {
              case Left(newState) =>
                currentSchema = s.left
                pushContext(newState)
                push { value =>
                  contextStack = contextStack.tail
                  finishWith(createEither(contextStack.head, s, Left(value)))
                }
              case Right(newState) =>
                currentSchema = s.right
                pushContext(newState)
                push { value =>
                  contextStack = contextStack.tail
                  finishWith(createEither(contextStack.head, s, Right(value)))
                }
            }

          case s: Schema.Tuple2[a, b] =>
            currentSchema = s.left
            pushContext(startCreatingTuple(currentContext, s))
            push { left =>
              contextStack = contextStack.tail
              val newState = startReadingSecondTupleElement(contextStack.head, s)
              currentSchema = s.right
              pushContext(newState)
              push { right =>
                contextStack = contextStack.tail
                finishWith(createTuple(contextStack.head, s, left, right))
              }
            }

          case s: Schema.Optional[a] =>
            startCreatingOptional(currentContext, s) match {
              case Some(newState) =>
                currentSchema = s.schema
                pushContext(newState)
                push { value =>
                  contextStack = contextStack.tail
                  finishWith(createOptional(contextStack.head, s, Some(value)))
                }
              case None =>
                finishWith(createOptional(contextStack.head, s, None))
            }

          case s @ Schema.Transform(schema, f, _, _, _) =>
            currentSchema = schema
            push { result =>
              finishWith(
                transform(currentContext, result, f.asInstanceOf[Any => Validation[String, Any]], s)
              )
            }

          case s @ Schema.CaseClass0(_, _, _) =>
            record(s)

          case s @ Schema.CaseClass1(_, _, _, _) =>
            record(s)

          case s @ Schema.CaseClass2(_, _, _, _, _) =>
            record(s)
          case s @ Schema.CaseClass3(_, _, _, _, _, _) =>
            record(s)
          case s @ Schema.CaseClass4(_, _, _, _, _, _, _) =>
            record(s)
          case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _) =>
            record(s)
          case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _) =>
            record(s)
          case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _) =>
            record(s)
          case s @ Schema.CaseClass8(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass9(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass10(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass11(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass12(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass13(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass14(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass15(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass16(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass17(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass18(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass19(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass20(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass21(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case s @ Schema.CaseClass22(
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _,
                _
              ) =>
            record(s)
          case Schema.Dynamic(_) =>
            createDynamic(currentContext) match {
              case Some(value) =>
                finishWith(value)
              case None =>
                currentSchema = DynamicValue.schema
            }
          case _ => throw new Exception(s"Missing a handler for schema ${currentSchema.toString()}.")
        }
      }
    } catch {
      case NonFatal(reason) =>
        throw CreateValueFromSchemaError(contextStack.head, reason)
    }
    result.get
  }
}

object MutableSchemaBasedValueBuilder {
  case class CreateValueFromSchemaError[Context](context: Context, cause: Throwable) extends RuntimeException
}

/** A simpler version of SimpleMutableSchemaBasedValueBuilder without using any Context */
trait SimpleMutableSchemaBasedValueBuilder[Target] extends MutableSchemaBasedValueBuilder[Target, Unit] {
  override protected def createPrimitive(context: Unit, typ: StandardType[_]): Target =
    createPrimitive(typ)
  protected def createPrimitive(typ: StandardType[_]): Target

  override protected def startCreatingRecord(context: Unit, record: Schema.Record[_]): Unit =
    startCreatingRecord(record)
  protected def startCreatingRecord(record: Schema.Record[_]): Unit

  override protected def startReadingField(context: Unit, record: Schema.Record[_], index: Int): Option[(Unit, Int)] =
    startReadingField(record, index).map(((), _))
  protected def startReadingField(record: Schema.Record[_], index: Int): Option[Int]

  override protected def createRecord(context: Unit, record: Schema.Record[_], values: Chunk[(Int, Target)]): Target =
    createRecord(record, values)
  protected def createRecord(record: Schema.Record[_], values: Chunk[(Int, Target)]): Target

  override protected def startCreatingEnum(context: Unit, cases: Chunk[Schema.Case[_, _]]): (Unit, Int) =
    ((), startCreatingEnum(cases))

  protected def startCreatingEnum(cases: Chunk[Schema.Case[_, _]]): Int

  override protected def createEnum(context: Unit, cases: Chunk[Schema.Case[_, _]], index: Int, value: Target): Target =
    createEnum(cases, index, value)

  protected def createEnum(cases: Chunk[Schema.Case[_, _]], index: Int, value: Target): Target

  override protected def startCreatingSequence(context: Unit, schema: Schema.Sequence[_, _, _]): Option[Unit] =
    startCreatingSequence(schema)

  protected def startCreatingSequence(schema: Schema.Sequence[_, _, _]): Option[Unit]

  override protected def startCreatingOneSequenceElement(state: Unit, schema: Schema.Sequence[_, _, _]): Unit =
    startReadingOneSequenceElement(schema)

  protected def startReadingOneSequenceElement(schema: Schema.Sequence[_, _, _]): Unit

  override protected def createSequence(
    context: Unit,
    schema: Schema.Sequence[_, _, _],
    values: Chunk[Target]
  ): Target =
    createSequence(schema, values)

  protected def createSequence(schema: Schema.Sequence[_, _, _], values: Chunk[Target]): Target

  override protected def startCreatingDictionary(context: Unit, schema: Schema.Map[_, _]): Option[Unit] =
    startCreatingDictionary(schema)

  protected def startCreatingDictionary(schema: Schema.Map[_, _]): Option[Unit]

  override protected def startCreatingOneDictionaryElement(state: Unit, schema: Schema.Map[_, _]): Unit =
    startReadingOneDictionaryKey(schema)

  protected def startReadingOneDictionaryKey(schema: Schema.Map[_, _]): Unit

  override protected def startCreatingOneDictionaryValue(state: Unit, schema: Schema.Map[_, _]): Unit =
    startReadingOneDictionaryValue(schema)

  protected def startReadingOneDictionaryValue(schema: Schema.Map[_, _]): Unit

  override protected def createDictionary(
    context: Unit,
    schema: Schema.Map[_, _],
    values: Chunk[(Target, Target)]
  ): Target =
    createDictionary(schema, values)

  protected def createDictionary(schema: Schema.Map[_, _], values: Chunk[(Target, Target)]): Target

  override protected def startCreatingSet(context: Unit, schema: Schema.Set[_]): Option[Unit] =
    startCreatingSet(schema)

  protected def startCreatingSet(schema: Schema.Set[_]): Option[Unit]

  override protected def startCreatingOneSetElement(state: Unit, schema: Schema.Set[_]): Unit =
    startReadingOneSetElement(schema)

  protected def startReadingOneSetElement(schema: Schema.Set[_]): Unit

  override protected def createSet(context: Unit, schema: Schema.Set[_], values: Chunk[Target]): Target =
    createSet(schema, values)

  protected def createSet(schema: Schema.Set[_], values: Chunk[Target]): Target

  override protected def startCreatingOptional(context: Unit, schema: Schema.Optional[_]): Option[Unit] =
    startCreatingOptional(schema)

  protected def startCreatingOptional(schema: Schema.Optional[_]): Option[Unit]

  override protected def createOptional(context: Unit, schema: Schema.Optional[_], value: Option[Target]): Target =
    createOptional(schema, value)

  protected def createOptional(schema: Schema.Optional[_], value: Option[Target]): Target

  override protected def startCreatingEither(
    context: Unit,
    schema: Schema.Either[_, _]
  ): Either[Unit, Unit] =
    startCreatingEither(schema)
  protected def startCreatingEither(schema: Schema.Either[_, _]): Either[Unit, Unit]

  override protected def createEither(
    context: Unit,
    schema: Schema.Either[_, _],
    value: Either[Target, Target]
  ): Target =
    createEither(schema, value)

  protected def createEither(schema: Schema.Either[_, _], value: Either[Target, Target]): Target

  override protected def startCreatingTuple(context: Unit, schema: Schema.Tuple2[_, _]): Unit =
    startCreatingTuple(schema)

  protected def startCreatingTuple(schema: Schema.Tuple2[_, _]): Unit

  override protected def startReadingSecondTupleElement(context: Unit, schema: Schema.Tuple2[_, _]): Unit =
    startReadingSecondTupleElement(schema)

  protected def startReadingSecondTupleElement(schema: Schema.Tuple2[_, _]): Unit

  override protected def createTuple(context: Unit, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target =
    createTuple(schema, left, right)

  protected def createTuple(schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  override protected def createDynamic(context: Unit): Option[Target] =
    createDynamic()

  protected def createDynamic(): Option[Target]

  override protected def transform(
    context: Unit,
    value: Target,
    f: Any => Validation[String, Any],
    schema: Schema[_]
  ): Target =
    transform(value, f, schema)

  protected def transform(value: Target, f: Any => Validation[String, Any], schema: Schema[_]): Target

  override protected def fail(context: Unit, message: String): Target =
    fail(message)

  protected def fail(message: String): Target

  override protected val initialContext: Unit = ()
}
