package zio.schema

import scala.collection.immutable.ListMap
import zio.{ Chunk, ChunkBuilder }

import scala.annotation.nowarn

trait ProcessValueWithSchema[Target, State] {
  protected def processPrimitive(state: State, value: Any, typ: StandardType[Any]): Target

  @nowarn protected def startProcessingRecord(state: State, schema: Schema.Record[_]): Unit = {}

  protected def processRecord(state: State, schema: Schema.Record[_], value: ListMap[String, Target]): Target

  @nowarn protected def startProcessingEnum(state: State, schema: Schema.Enum[_]): Unit = {}

  protected def processEnum(state: State, schema: Schema.Enum[_], tuple: (String, Target)): Target

  @nowarn protected def startProcessingSequence(state: State, schema: Schema.Sequence[_, _, _], size: Int): Unit = {}

  protected def processSequence(state: State, schema: Schema.Sequence[_, _, _], value: Chunk[Target]): Target

  @nowarn protected def startProcessingDictionary(state: State, schema: Schema.Map[_, _], size: Int): Unit = {}

  protected def processDictionary(state: State, schema: Schema.Map[_, _], value: Chunk[(Target, Target)]): Target

  @nowarn protected def startProcessingSet(state: State, schema: Schema.Set[_], size: Int): Unit = {}

  protected def processSet(state: State, schema: Schema.Set[_], value: Set[Target]): Target

  @nowarn protected def startProcessingEither(state: State, schema: Schema.Either[_, _]): Unit = {}

  protected def processEither(state: State, schema: Schema.Either[_, _], value: Either[Target, Target]): Target

  @nowarn protected def startProcessingOption(state: State, schema: Schema.Optional[_]): Unit = {}

  protected def processOption(state: State, schema: Schema.Optional[_], value: Option[Target]): Target

  @nowarn protected def startProcessingTuple(state: State, schema: Schema.Tuple2[_, _]): Unit = {}

  protected def processTuple(state: State, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  protected def processDynamic(state: State, value: DynamicValue): Option[Target]

  protected def fail(state: State, message: String): Target

  protected val initialState: State

  protected def stateForRecordField(state: State, index: Int, field: Schema.Field[_, _]): State
  protected def stateForTuple(state: State, index: Int): State
  protected def stateForEnumConstructor(state: State, index: Int, c: Schema.Case[_, _]): State
  protected def stateForEither(state: State, e: Either[Unit, Unit]): State
  protected def stateForOption(state: State, o: Option[Unit]): State
  protected def stateForSequence(state: State, schema: Schema.Sequence[_, _, _], index: Int): State
  protected def stateForMap(state: State, schema: Schema.Map[_, _], index: Int): State
  protected def stateForSet(state: State, schema: Schema.Set[_], index: Int): State

  def process[A](schema: Schema[A], value: A): Target = {
    var currentSchema: Schema[_]    = schema
    var currentValue: Any           = value
    var result: Option[Target]      = None
    var stack: List[Target => Unit] = List.empty[Target => Unit]
    var stateStack: List[State]     = List(initialState)

    def push(f: Target => Unit): Unit =
      stack = f :: stack

    def pushState(s: State): Unit =
      stateStack = s :: stateStack

    def finishWith(resultValue: Target): Unit =
      if (stack.nonEmpty) {
        val head = stack.head
        stack = stack.tail
        head(resultValue)
      } else {
        result = Some(resultValue)
      }

    def fields(s: Schema.Record[_], record: Any, fs: Schema.Field[_, _]*): Unit = {
      val values = ChunkBuilder.make[Target](fs.size)

      def processNext(index: Int, remaining: List[Schema.Field[_, _]]): Unit =
        remaining match {
          case next :: _ =>
            currentSchema = next.schema
            currentValue = next.asInstanceOf[Schema.Field[Any, Any]].get(record)
            pushState(stateForRecordField(stateStack.head, index, next))
            push(processField(index, remaining, _))
          case Nil =>
            finishWith(
              processRecord(
                stateStack.head,
                s,
                fs.map(_.name).zip(values.result()).foldLeft(ListMap.empty[String, Target]) {
                  case (lm, pair) =>
                    lm.updated(pair._1, pair._2)
                }
              )
            )
        }

      def processField(index: Int, currentStructure: List[Schema.Field[_, _]], fieldResult: Target): Unit = {
        stateStack = stateStack.tail
        values += fieldResult
        val remaining = currentStructure.tail
        processNext(index + 1, remaining)
      }

      startProcessingRecord(stateStack.head, s)
      processNext(0, fs.toList)
    }

    def enumCases(s: Schema.Enum[_], cs: Schema.Case[_, _]*): Unit = {
      startProcessingEnum(stateStack.head, s)

      var found = false
      val it    = cs.iterator
      var index = 0
      while (!found && it.hasNext) {
        val c = it.next().asInstanceOf[Schema.Case[Any, Any]]
        c.deconstructOption(currentValue) match {
          case Some(v) =>
            currentValue = v
            currentSchema = c.schema
            pushState(stateForEnumConstructor(stateStack.head, index, c))
            push { dv =>
              stateStack = stateStack.tail
              finishWith(processEnum(stateStack.head, s, c.id -> dv))
            }
            found = true
          case None =>
            index = index + 1
        }
      }

      if (!found) {
        //This should never happen unless someone manually builds an Enum and doesn't include all cases
        finishWith(fail(stateStack.head, "Invalid enum constructor"))
      }
    }

    while (result.isEmpty) {
      val state = stateStack.head

      currentSchema match {

        case l @ Schema.Lazy(_) =>
          currentSchema = l.schema

        case Schema.Primitive(p, _) =>
          finishWith(processPrimitive(state, currentValue, p.asInstanceOf[StandardType[Any]]))

        case s @ Schema.GenericRecord(_, structure, _) =>
          val map            = currentValue.asInstanceOf[ListMap[String, _]]
          val structureChunk = structure.toChunk
          val values         = ChunkBuilder.make[Target](structureChunk.size)

          def processNext(index: Int, remaining: List[Schema.Field[ListMap[String, _], _]]): Unit =
            remaining match {
              case next :: _ =>
                currentSchema = next.schema
                currentValue = map(next.name)
                pushState(stateForRecordField(state, index, next))
                push(processField(index, remaining, _))
              case Nil =>
                finishWith(
                  processRecord(
                    state,
                    s,
                    structureChunk.map(_.name).zip(values.result()).foldLeft(ListMap.empty[String, Target]) {
                      case (lm, pair) =>
                        lm.updated(pair._1, pair._2)
                    }
                  )
                )
            }

          def processField(
            index: Int,
            currentStructure: List[Schema.Field[ListMap[String, _], _]],
            fieldResult: Target
          ): Unit = {
            stateStack = stateStack.tail
            values += fieldResult
            val remaining = currentStructure.tail
            processNext(index + 1, remaining)
          }

          startProcessingRecord(state, s)
          processNext(0, structureChunk.toList)

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
          finishWith(fail(state, message))

        case s @ Schema.Sequence(schema, _, toChunk, _, _) =>
          val inputChunk  = toChunk.asInstanceOf[Any => Chunk[Any]](currentValue)
          val resultChunk = ChunkBuilder.make[Target](inputChunk.size)

          def processNext(inputIdx: Int): Unit = {
            stateStack = stateStack.tail
            if (inputIdx == inputChunk.size) {
              finishWith(processSequence(state, s, resultChunk.result()))
            } else {
              currentSchema = schema
              currentValue = inputChunk(inputIdx)
              pushState(stateForSequence(state, s, inputIdx))
              push { dv =>
                resultChunk += dv
                processNext(inputIdx + 1)
              }
            }
          }

          startProcessingSequence(state, s, inputChunk.size)
          pushState(stateForSequence(state, s, 0))
          processNext(0)

        case s @ Schema.Map(ks: Schema[k], vs: Schema[v], _) =>
          val inputChunk  = Chunk.fromIterable(currentValue.asInstanceOf[Map[k, v]])
          val resultChunk = ChunkBuilder.make[(Target, Target)](inputChunk.size)

          def processNext(inputIdx: Int): Unit =
            if (inputIdx == inputChunk.size) {
              finishWith(processDictionary(state, s, resultChunk.result()))
            } else {
              currentSchema = ks
              val currentTuple = inputChunk(inputIdx)
              currentValue = currentTuple._1

              pushState(stateForMap(state, s, inputIdx))
              push { (a: Target) =>
                stateStack = stateStack.tail

                currentSchema = vs
                currentValue = currentTuple._2
                pushState(stateForMap(state, s, inputIdx))
                push { (b: Target) =>
                  stateStack = stateStack.tail
                  val pair = (a, b)
                  resultChunk += pair
                  processNext(inputIdx + 1)
                }
              }
            }

          startProcessingDictionary(state, s, inputChunk.size)
          processNext(0)

        case s @ Schema.Set(as: Schema[a], _) =>
          val inputChunk  = Chunk.fromIterable(currentValue.asInstanceOf[Set[a]])
          val resultChunk = ChunkBuilder.make[Target](inputChunk.size)

          def processNext(inputIdx: Int): Unit = {
            stateStack = stateStack.tail
            if (inputIdx == inputChunk.size) {
              finishWith(processSet(state, s, resultChunk.result().toSet))
            } else {
              currentSchema = as
              currentValue = inputChunk(inputIdx)
              pushState(stateForSet(state, s, inputIdx))
              push { dv =>
                resultChunk += dv
                processNext(inputIdx + 1)
              }
            }
          }

          startProcessingSet(state, s, inputChunk.size)
          pushState(stateForSet(state, s, 0))
          processNext(0)

        case s: Schema.Either[l, r] =>
          startProcessingEither(state, s)
          currentValue.asInstanceOf[Either[l, r]] match {
            case Left(value: l) =>
              currentValue = value
              currentSchema = s.left
              pushState(stateForEither(state, Left(())))
              push { dyn =>
                stateStack = stateStack.tail
                finishWith(processEither(state, s, Left(dyn)))
              }
            case Right(value: r) =>
              currentValue = value
              currentSchema = s.right
              pushState(stateForEither(state, Right(())))
              push { dyn =>
                stateStack = stateStack.tail
                finishWith(processEither(state, s, Right(dyn)))
              }
          }

        case s: Schema.Tuple2[a, b] =>
          startProcessingTuple(state, s)
          val (a: a, b: b) = currentValue.asInstanceOf[(a, b)]
          currentValue = a
          currentSchema = s.left
          pushState(stateForTuple(state, 1))
          push { dynA =>
            currentValue = b
            currentSchema = s.right
            stateStack = stateStack.tail
            pushState(stateForTuple(state, 2))
            push { dynB =>
              stateStack = stateStack.tail
              finishWith(processTuple(state, s, dynA, dynB))
            }
          }

        case s: Schema.Optional[a] =>
          startProcessingOption(state, s)
          currentValue.asInstanceOf[Option[a]] match {
            case Some(value: a) =>
              currentValue = value
              currentSchema = s.schema
              pushState(stateForOption(state, Some(())))
              push { dyn =>
                stateStack = stateStack.tail
                finishWith(processOption(state, s, Some(dyn)))
              }
            case None =>
              finishWith(processOption(state, s, None))
          }

        case Schema.Transform(schema, _, g, _, _) =>
          g.asInstanceOf[Any => Either[String, Any]](currentValue) match {
            case Left(message) =>
              finishWith(fail(state, message))
            case Right(a) =>
              currentValue = a
              currentSchema = schema
          }

        case s @ Schema.CaseClass0(_, _, _) =>
          finishWith(processRecord(state, s, ListMap()))

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
              f21,
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
            f19,
            f20,
            f21
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
              f21,
              f22,
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
            f19,
            f20,
            f21,
            f22
          )
        case Schema.Dynamic(_) =>
          processDynamic(state, currentValue.asInstanceOf[DynamicValue]) match {
            case Some(target) => finishWith(target)
            case None =>
              currentSchema = Schema.dynamicValue
          }
      }
    }
    result.get
  }
}

trait ProcessSchemaAndValueWithoutState[Target] extends ProcessValueWithSchema[Target, Unit] {

  protected def processPrimitive(value: Any, typ: StandardType[Any]): Target

  protected def processRecord(schema: Schema.Record[_], value: ListMap[String, Target]): Target

  protected def processEnum(schema: Schema.Enum[_], tuple: (String, Target)): Target

  protected def processSequence(schema: Schema.Sequence[_, _, _], value: Chunk[Target]): Target

  protected def processDictionary(schema: Schema.Map[_, _], value: Chunk[(Target, Target)]): Target

  protected def processSet(schema: Schema.Set[_], value: Set[Target]): Target

  protected def processEither(schema: Schema.Either[_, _], value: Either[Target, Target]): Target

  protected def processOption(schema: Schema.Optional[_], value: Option[Target]): Target

  protected def processTuple(schema: Schema.Tuple2[_, _], left: Target, right: Target): Target

  protected def processDynamic(value: DynamicValue): Option[Target]

  protected def fail(message: String): Target

  override protected def processPrimitive(state: Unit, value: Any, typ: StandardType[Any]): Target =
    processPrimitive(value, typ)

  override protected def processRecord(state: Unit, schema: Schema.Record[_], value: ListMap[String, Target]): Target =
    processRecord(schema, value)

  override protected def processEnum(state: Unit, schema: Schema.Enum[_], tuple: (String, Target)): Target =
    processEnum(schema, tuple)

  override protected def processSequence(state: Unit, schema: Schema.Sequence[_, _, _], value: Chunk[Target]): Target =
    processSequence(schema, value)

  override protected def processDictionary(
    state: Unit,
    schema: Schema.Map[_, _],
    value: Chunk[(Target, Target)]
  ): Target =
    processDictionary(schema, value)

  override protected def processSet(state: Unit, schema: Schema.Set[_], value: Set[Target]): Target =
    processSet(schema, value)

  override protected def processEither(
    state: Unit,
    schema: Schema.Either[_, _],
    value: Either[Target, Target]
  ): Target =
    processEither(schema, value)

  override protected def processOption(state: Unit, schema: Schema.Optional[_], value: Option[Target]): Target =
    processOption(schema, value)

  override protected def processTuple(state: Unit, schema: Schema.Tuple2[_, _], left: Target, right: Target): Target =
    processTuple(schema, left, right)

  override protected def fail(state: Unit, message: String): Target =
    fail(message)

  override protected def processDynamic(state: Unit, value: DynamicValue): Option[Target] =
    processDynamic(value)

  override protected val initialState: Unit = ()

  override protected def stateForRecordField(state: Unit, index: Int, field: Schema.Field[_, _]): Unit =
    ()

  override protected def stateForEnumConstructor(state: Unit, index: Int, c: Schema.Case[_, _]): Unit =
    ()

  override protected def stateForEither(state: Unit, e: Either[Unit, Unit]): Unit =
    ()

  override protected def stateForOption(state: Unit, o: Option[Unit]): Unit =
    ()

  override protected def stateForTuple(state: Unit, index: Int): Unit =
    ()

  override protected def stateForSequence(state: Unit, schema: Schema.Sequence[_, _, _], index: Int): Unit =
    ()

  override protected def stateForMap(state: Unit, schema: Schema.Map[_, _], index: Int): Unit =
    ()

  override protected def stateForSet(state: Unit, schema: Schema.Set[_], index: Int): Unit =
    ()
}
