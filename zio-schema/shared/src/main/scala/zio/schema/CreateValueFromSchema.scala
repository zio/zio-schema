package zio.schema

import zio.{ Chunk, ChunkBuilder }

trait CreateValueFromSchema[Target, State] {

  protected def createPrimitive(state: State, typ: StandardType[_]): Target

  protected def startCreatingRecord(state: State, record: Schema.Record[_]): State
  protected def startReadingField(state: State, record: Schema.Record[_], index: Int): (State, Option[Int])
  protected def createRecord(state: State, record: Schema.Record[_], values: Chunk[(Int, Target)]): Target

  protected def startCreatingEnum(state: State, cases: Chunk[Schema.Case[_, _]]): (State, Int)

  protected def createEnum(state: State, cases: Chunk[Schema.Case[_, _]], index: Int, value: Target): Target

  protected def startCreatingSequence(state: State, schema: Schema.Sequence[_, _, _]): Option[State]
  protected def readOneSequenceElement(state: State, schema: Schema.Sequence[_, _, _]): (State, Boolean)
  protected def createSequence(state: State, schema: Schema.Sequence[_, _, _], values: Chunk[Target]): Target

  protected def startCreatingDictionary(state: State, schema: Schema.Map[_, _]): Option[State]
  protected def readOneDictionaryElement(state: State, schema: Schema.Map[_, _]): (State, Boolean)
  protected def createDictionary(state: State, schema: Schema.Map[_, _], values: Chunk[(Target, Target)]): Target

  protected def startCreatingSet(state: State, schema: Schema.Set[_]): Option[State]
  protected def readOneSetElement(state: State, schema: Schema.Set[_]): (State, Boolean)
  protected def createSet(state: State, schema: Schema.Set[_], values: Chunk[Target]): Target

  protected def startCreatingOptional(state: State, schema: Schema.Optional[_]): Option[State]
  protected def createOptional(state: State, schema: Schema.Optional[_], value: Option[Target]): Target

  protected def startCreatingEither(state: State, schema: Schema.Either[_, _]): Either[State, State]
  protected def createEither(state: State, schema: Schema.Either[_, _], value: Either[Target, Target]): Target

  protected def startCreatingTuple(state: State, schema: Schema.Tuple2[_, _]): State
  protected def startReadingSecondTupleElement(state: State, schema: Schema.Tuple2[_, _]): State
  protected def createTuple(state: State, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  protected def createDynamic(state: State): Option[Target]

  protected def transform(state: State, value: Target, f: Any => Either[String, Any]): Target
  protected def fail(state: State, message: String): Target

  protected val initialState: State

  def create[A](schema: Schema[A]): Target = {
    var currentSchema: Schema[_]    = schema
    var result: Option[Target]      = None
    var stack: List[Target => Unit] = List.empty[Target => Unit]
    var stateStack: List[State]     = List(initialState)

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

    def pushState(s: State): Unit =
      stateStack = s :: stateStack

    def record(record: Schema.Record[_]): Unit = {
      val values = ChunkBuilder.make[(Int, Target)](record.fields.size)

      def readField(index: Int): Unit = {
        val (updatedState, fieldIndex) = startReadingField(stateStack.head, record, index)
        stateStack = stateStack.tail
        fieldIndex match {
          case Some(idx) =>
            currentSchema = record.fields(idx).schema
            pushState(updatedState)
            push { field =>
              val elem = (idx, field)
              values += elem
              readField(index + 1)
            }
          case None =>
            finishWith(createRecord(stateStack.head, record, values.result()))
        }
      }

      pushState(startCreatingRecord(stateStack.head, record))
      readField(0)
    }

    def enumCases(casesChunk: Chunk[Schema.Case[_, _]]): Unit = {
      val (newState, index) = startCreatingEnum(stateStack.head, casesChunk)
      currentSchema = casesChunk(index).schema
      pushState(newState)
      push { value =>
        stateStack = stateStack.tail
        finishWith(createEnum(stateStack.head, casesChunk, index, value))
      }
    }

    while (result.isEmpty) {
      val state = stateStack.head
      currentSchema match {

        case l @ Schema.Lazy(_) =>
          currentSchema = l.schema

        case Schema.Primitive(p, _) =>
          finishWith(createPrimitive(state, p.asInstanceOf[StandardType[Any]]))

        case s @ Schema.GenericRecord(_, structure, _) =>
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
          finishWith(fail(state, message))

        case s @ Schema.Sequence(elementSchema, _, _, _, _) =>
          val elems = ChunkBuilder.make[Target]()

          def readOne(): Unit =
            push { elem =>
              elems += elem

              val (newState, continue) = readOneSequenceElement(stateStack.head, s)
              stateStack = stateStack.tail

              if (continue) {
                currentSchema = elementSchema
                pushState(newState)
                readOne()
              } else {
                finishWith(createSequence(stateStack.head, s, elems.result()))
              }
            }

          currentSchema = elementSchema
          startCreatingSequence(state, s) match {
            case Some(startingState) =>
              pushState(startingState)
              readOne()
            case None =>
              finishWith(createSequence(stateStack.head, s, Chunk.empty))
          }

        case s @ Schema.Map(ks: Schema[k], vs: Schema[v], _) =>
          val elems = ChunkBuilder.make[(Target, Target)]()

          def readOne(): Unit =
            push { key =>
              currentSchema = vs

              push { value =>
                val elem = (key, value)
                elems += elem

                val (newState, continue) = readOneDictionaryElement(stateStack.head, s)
                stateStack = stateStack.tail

                if (continue) {
                  currentSchema = ks
                  pushState(newState)
                  readOne()
                } else {
                  finishWith(createDictionary(stateStack.head, s, elems.result()))
                }
              }
            }

          startCreatingDictionary(state, s) match {
            case Some(startingState) =>
              currentSchema = ks
              pushState(startingState)
              readOne()
            case None =>
              finishWith(createDictionary(stateStack.head, s, Chunk.empty))
          }

        case s @ Schema.Set(as: Schema[a], _) =>
          val elems = ChunkBuilder.make[Target]()

          def readOne(): Unit =
            push { elem =>
              elems += elem

              val (newState, continue) = readOneSetElement(stateStack.head, s)
              stateStack = stateStack.tail

              if (continue) {
                currentSchema = as
                pushState(newState)
                readOne()
              } else {
                finishWith(createSet(stateStack.head, s, elems.result()))
              }
            }

          startCreatingSet(state, s) match {
            case Some(startingState) =>
              currentSchema = as
              pushState(startingState)
              readOne()
            case None =>
              finishWith(createSet(stateStack.head, s, Chunk.empty))
          }

        case s: Schema.Either[l, r] =>
          startCreatingEither(state, s) match {
            case Left(newState) =>
              currentSchema = s.left
              pushState(newState)
              push { value =>
                stateStack = stateStack.tail
                finishWith(createEither(stateStack.head, s, Left(value)))
              }
            case Right(newState) =>
              currentSchema = s.right
              pushState(newState)
              push { value =>
                stateStack = stateStack.tail
                finishWith(createEither(stateStack.head, s, Right(value)))
              }
          }

        case s: Schema.Tuple2[a, b] =>
          currentSchema = s.left
          pushState(startCreatingTuple(state, s))
          push { left =>
            val newState = startReadingSecondTupleElement(stateStack.head, s)
            stateStack = stateStack.tail
            currentSchema = s.right
            pushState(newState)
            push { right =>
              stateStack = stateStack.tail
              finishWith(createTuple(stateStack.head, s, left, right))
            }
          }

        case s: Schema.Optional[a] =>
          startCreatingOptional(state, s) match {
            case Some(newState) =>
              currentSchema = s.schema
              pushState(newState)
              push { value =>
                stateStack = stateStack.tail
                finishWith(createOptional(stateStack.head, s, Some(value)))
              }
            case None =>
              finishWith(createOptional(stateStack.head, s, None))
          }

        case Schema.Transform(schema, f, _, _, _) =>
          currentSchema = schema
          push { result =>
            finishWith(transform(state, result, f.asInstanceOf[Any => Either[String, Any]]))
          }

        case s @ Schema.CaseClass0(_, _, _) =>
          finishWith(createRecord(state, s, Chunk.empty))

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
              _,
              _,
              _,
              _
            ) =>
          record(s)
        case Schema.Dynamic(_) =>
          createDynamic(state) match {
            case Some(value) =>
              finishWith(value)
            case None =>
              currentSchema = Schema.dynamicValue
          }
      }
    }
    result.get
  }
}
