package zio.schema

import zio.{Chunk, URIO}
import zio.random._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen.{SchemaTest, anySchema, anySchemaAndValue, schemasAndGens}
import zio.test.Assertion._
import zio.test._

object OrderingSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Any] =
    suite("schemas should generate correct orderings")(

      suite("primitives")(primitiveOrderingTests:_*),

      suite("structures")(structureTestCases.map(structureOrderingTest):_*),

      suite("laws")(
        testM("reflexivity"){
          check(anySchemaAndValue)( _ match {
            case (schema, a) => assert(schema.ordering.compare(a,a))(equalTo(0))
          })
        },
        testM("antisymmetry"){
          check(genAnyOrderedPair)( _ match {
            case (schema, x, y) => assert(schema.ordering.compare(y,x))(isGreaterThan(0))
          })
        }
        // todo transitivity
      ),

    )

  val primitiveOrderingTests: List[ZSpec[Sized with Random with TestConfig, Nothing]] = schemasAndGens.map {
    case SchemaTest(name, schema, gen) =>
      testM(s"$name") {
        primitiveOrderingLaw(gen, schema)
      }
  }

  private def primitiveOrderingLaw[R,A](gen:Gen[R,A], standardType:StandardType[A]): URIO[R with Random with TestConfig, TestResult] = {
    check(Gen.listOfN(2)(gen)) { lst =>
      val schema = Primitive(standardType)
      val schemaOrdering = schema.ordering
      val l = lst(0)
      val r = lst(1)
      assert(schemaOrdering.compare(l,r))(equalTo(standardType.compare(l,r)))
    }
  }


  case class StructureTestCase(name:String,increasingPairGen:Gen[Random with Sized,SchemaAndPair[_]])

  private val structureTestCases =
    Seq(
      StructureTestCase("either",genAnyOrderedPairEither),
      StructureTestCase("tuple", genAnyOrderedPairTuple),
      StructureTestCase("sequence", genOrderedPairOfChunks),
    )

  def structureOrderingTest(t:StructureTestCase) = {
    testM(t.name)(check(t.increasingPairGen)( _ match {
      case (schema, l, r) => assert(schema.ordering.compare(l,r))(isLessThan(0))
    }))
  }

  def genOrderedPairOfChunks:Gen[Random with Sized,SchemaAndPair[_]] =
    anySchema.flatMap(genOrderedPairChunk(_))


  def genOrderedPairChunk[A](schema:Schema[A]):Gen[Random with Sized,SchemaAndPair[Chunk[A]]] = {
    for {
      init  <- Gen.chunkOf(genFromSchema(schema))
      rems <- Gen.oneOf(
      genChunkPairWithOrderedFirstElement(schema),
      genChunkPairWithOnlyFirstEmpty(schema)
      )
    } yield (Schema.chunk(schema),init++rems._1,init++rems._2)
  }

  def genChunkPairWithOrderedFirstElement[A](schema:Schema[A]):Gen[Random with Sized,(Chunk[A],Chunk[A]) ] = {
    for {
      inits <- genOrderedPair(schema)
      remL <- Gen.chunkOf(genFromSchema(schema))
      remR <- Gen.chunkOf(genFromSchema(schema))
    } yield (remL.prepended(inits._1),remR.prepended(inits._2))
  }


  def genChunkPairWithOnlyFirstEmpty[A](schema:Schema[A]):Gen[Random with Sized,(Chunk[A],Chunk[A]) ] = {
    for {
      init <- genFromSchema(schema)
      rem <- Gen.chunkOf(genFromSchema(schema))
    } yield (Chunk(), init +: rem )
  }


  def genAnyOrderedPairEither:Gen[Random with Sized, SchemaAndPair[Either[_,_]]] = {
    for{
      leftSchema <- anySchema
      rightSchema <- anySchema
      (l,r) <- genOrderedPairEither(leftSchema,rightSchema)
    } yield (Schema.EitherSchema(leftSchema,rightSchema),l,r).asInstanceOf[SchemaAndPair[Either[_,_]]]
  }

  def genOrderedPairEither[A,B](lSchema:Schema[A], rSchema:Schema[B]):Gen[Random with Sized,(Either[A,B],Either[A,B]) ] = Gen.oneOf(
    for{
      (small,large) <- genOrderedPair(lSchema)
    } yield (Left(small),Left(large)),
    for{
      (small,large) <- genOrderedPair(rSchema)
    } yield (Right(small),Right(large)),
    for{
      l <- genFromSchema(lSchema)
      r <- genFromSchema(rSchema)
    } yield (Left(l),Right(r))
  )

  def genAnyOrderedPairTuple:Gen[Random with Sized, SchemaAndPair[_]] = {
    for{
      xSchema <- anySchema
      ySchema <- anySchema
      (l,r) <- genOrderedPairTuple(xSchema,ySchema)
    } yield (Schema.Tuple(xSchema,ySchema),l,r).asInstanceOf[SchemaAndPair[Either[_,_]]]
  }

  def genOrderedPairTuple[A,B](xSchema:Schema[A], ySchema:Schema[B]):Gen[Random with Sized,((A,B),(A,B)) ] =
    Gen.oneOf(
      for{
        (smallX,largeX) <- genOrderedPair(xSchema)
        leftY <- genFromSchema(ySchema)
        rightY <- genFromSchema(ySchema)
      } yield ((smallX,leftY),(largeX,rightY)),
      for{
        x <- genFromSchema(xSchema)
        (smallY, largeY) <- genOrderedPair(ySchema)
      } yield ((x,smallY),(x,largeY))
  )


  type SchemaAndPair[A] = (Schema[A], A, A)

  def genFromSchema[A](schema:Schema[A]):Gen[Random with Sized, A] = {
    DynamicValueGen.anyDynamicValueOfSchema(schema)
      .map(d => schema.fromDynamic(d).toOption.get)
  }


  def genAnyOrderedPair:Gen[Random with Sized, SchemaAndPair[_]] = {
    for{
      schema <- anySchema
      pair <- genOrderedPair(schema)
    } yield ( schema, pair._1, pair._2 ).asInstanceOf[(Schema[Any],Any,Any)]
  }

  def genOrderedPair[A](schema:Schema[A]):Gen[Random with Sized, (A,A)] = {
    (genFromSchema(schema) <*> genFromSchema(schema))
      .withFilter{ case (l,r) => {
        schema.ordering.compare(l,r) < 0
      }}
  }

}