package zio.schema

import zio.{Chunk, URIO}
import zio.random._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen.{SchemaTest, anySchema, anySchemaAndValue, anyStructure, anyTree, schemasAndGens}
import zio.test.Assertion._
import zio.test._

import scala.collection.immutable.ListMap

object OrderingSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Any] =
    suite("schemas should generate correct orderings")(

      suite("primitives")(primitiveOrderingTests:_*),

      suite("structures")(structureTestCases.map(structureOrderingTest):_*),

      suite("laws")(
        testM("reflexivity"){
          check(anySchemaAndValue){
            case (schema, a) => assert(schema.ordering.compare(a,a))(equalTo(0))
          }
        },
        testM("antisymmetry"){
          check(genAnyOrderedPair) {
            case (schema, x, y) => assert(schema.ordering.compare(y, x))(isGreaterThan(0))
          }
        },
        testM("transitivity"){
          check(genAnyOrderedTriplet) {
            case (schema, x, _, z) => assert(schema.ordering.compare(x, z))(isLessThan(0))
          }
        }
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
      StructureTestCase("sequence", genAnyOrderedPairChunks),
      StructureTestCase("option", genAnyOrderedPairOption),
      StructureTestCase("transform", genAnyOrderedPairTransform),
      StructureTestCase("record",genAnyOrderedPairRecord),
      //TODO enums
    )

  def structureOrderingTest(t:StructureTestCase) = {
    testM(t.name)(check(t.increasingPairGen)( _ match {
      case (schema, l, r) => assert(schema.ordering.compare(l,r))(isLessThan(0))
    }))
  }

  def genAnyOrderedPairChunks:Gen[Random with Sized,SchemaAndPair[_]] =
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

  def genAnyOrderedPairOption:Gen[Random with Sized, SchemaAndPair[Option[_]]] = {
    for{
      schema <- anySchema
      (l,r) <- genOrderedPairOption(schema)
    } yield (Schema.Optional(schema),l,r).asInstanceOf[SchemaAndPair[Option[_]]]
  }

  def genOrderedPairOption[A](schema:Schema[A]):Gen[Random with Sized, (Option[A],Option[A])] = {
    Gen.oneOf(
      for{
        (smallA,largeA) <- genOrderedPair(schema)
      } yield(Some(smallA), Some(largeA)),
      for{
        a <- genFromSchema(schema)
      } yield(None,Some(a)),
    )
  }

  def genAnyOrderedPairTransform:Gen[Random with Sized, SchemaAndPair[_]] = {
    for {
      schema <- anySchema
      pair <- genOrderedPairTransform(schema)
    } yield pair
  }

  //TODO maybe do this with json parsing transformation?
  def genOrderedPairTransform[A](schema:Schema[A]):Gen[Random with Sized, SchemaAndPair[A]] = {
    for {
      (small, large) <- genOrderedPair(schema)
    } yield (Schema.Transform(schema,{a:A=>Right(a)},{a:A=>Right(a)}),small,large)
  }

  def genAnyOrderedPairRecord:Gen[Random with Sized, SchemaAndPair[_]] = {
    for {
      schema <- anyStructure(anyTree(1)).map(fields => Schema.GenericRecord(Chunk.fromIterable(fields)))
      pair <- genOrderedPairRecord(schema)
    } yield pair
  }

  def genOrderedPairRecord[A](schema:Schema.Record[A]):Gen[Random with Sized, SchemaAndPair[A]] = {
    val fields:Chunk[Schema.Field[_]] = schema.structure
    for {
      diffInd <- Gen.int(0,fields.size-1)
      equalFields <- genEqualFields(fields,0,diffInd)
      orderedFields <- genOrderedField(fields(diffInd))
      randomFields <- genRandomFields(fields,diffInd+1)
    } yield {
      val allFields = equalFields.appended(orderedFields) ++ randomFields
      val xFields = allFields.map{ case (label, value, _) => (label,value)}
      val yFields = allFields.map{ case (label, _, value) => (label,value)}
      val x = DynamicValue.Record(ListMap(xFields:_*)).toTypedValue(schema).toOption.get
      val y = DynamicValue.Record(ListMap(yFields:_*)).toTypedValue(schema).toOption.get
      (schema, x, y)
    }
  }

  def genEqualFields(fields:Chunk[Schema.Field[_]],currentInd:Int, diffInd:Int):Gen[Random with Sized, Chunk[(String,DynamicValue,DynamicValue)]] = {
    if(currentInd>=diffInd) Gen.unit.map(_ => Chunk())
    else {
      val field = fields(currentInd)
      for{
        x <- genFromSchema(field.schema)
        d = DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]],x)
        rem <- genEqualFields(fields,currentInd+1,diffInd)
      } yield (field.label, d, d) +: rem
    }
  }

  def genOrderedField(field:Schema.Field[_]):Gen[Random with Sized, (String,DynamicValue,DynamicValue)] = {
    genOrderedPair(field.schema).map{ case (a,b) =>
      (
        field.label,
        DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]],a),
        DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]],b)
      )}
  }

  def genRandomFields(fields:Chunk[Schema.Field[_]], currentInd:Int):Gen[Random with Sized, Chunk[(String,DynamicValue,DynamicValue)]] = {
    if(currentInd>=fields.size) Gen.unit.map(_ => Chunk())
    else {
      val field = fields(currentInd)
      for{
        x <- genFromSchema(field.schema)
        y <- genFromSchema(field.schema)
        dx = DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]],x)
        dy =  DynamicValue.fromSchemaAndValue(field.schema.asInstanceOf[Schema[Any]],y)
        rem <- genRandomFields(fields,currentInd+1)
      } yield (field.label, dx, dy) +: rem
    }
  }

  type SchemaAndPair[A] = (Schema[A], A, A)
  type SchemaAndTriplet[A] = (Schema[A], A, A, A)

  def genFromSchema[A](schema:Schema[A]):Gen[Random with Sized, A] = {
    DynamicValueGen.anyDynamicValueOfSchema(schema)
      .map(d => schema.fromDynamic(d).toOption.get)
  }

  def genAnyOrderedPair:Gen[Random with Sized, SchemaAndPair[_]] = {
    for{
      schema <- anySchema
      (x, y) <- genOrderedPair(schema)
    } yield ( schema, x, y ).asInstanceOf[(Schema[Any],Any,Any)]
  }

  def genOrderedPair[A](schema:Schema[A]):Gen[Random with Sized, (A,A)] = {
    (genFromSchema(schema) <*> genFromSchema(schema))
      .withFilter{ case (l,r) => {
        schema.ordering.compare(l,r) < 0
      }}
  }

  def genAnyOrderedTriplet:Gen[Random with Sized, SchemaAndTriplet[_]] = {
    for{
      schema <- anySchema
      (x, y, z) <- genOrderedTriplet(schema)
    } yield ( schema, x, y, z ).asInstanceOf[(Schema[Any],Any,Any,Any)]
  }

  def genOrderedTriplet[A](schema:Schema[A]):Gen[Random with Sized, (A,A,A)] = {
    for{
      (x,y) <- genOrderedPair(schema)
      z <- genFromSchema(schema)
        .withFilter{ cur => schema.ordering.compare(y,cur) < 0 }
    } yield (x,y,z)
  }

}