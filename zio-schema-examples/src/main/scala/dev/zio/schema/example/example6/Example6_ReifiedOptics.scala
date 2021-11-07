package dev.zio.schema.example.example6

import zio._
import zio.schema._
import zio.schema.Schema._
import zio.schema.optics.ZioOpticsBuilder
import zio.schema.syntax._

private[example6] object Domain {
  final case class User(name: String, age: Int)
  final case class Address(street: String, city: String, state: String)
  final case class UserAddress(user: User, address: Address)
  final case class Company(boss: User, employees: List[UserAddress])

  implicit val userSchema: Schema.CaseClass2[String, Int, User] = Schema.CaseClass2[String, Int, User](
    field1 = Field("name", Schema.primitive[String]),
    field2 = Field("age", Schema.primitive[Int]),
    construct = (name, years) => User(name, years),
    extractField1 = _.name,
    extractField2 = _.age,
  )

  implicit val addressSchema = Schema.CaseClass3[String, String, String, Address](
    field1 = Field("street", Schema.primitive[String]),
    field2 = Field("city", Schema.primitive[String]),
    field3 = Field("state", Schema.primitive[String]),
    construct = (street, city, state) => Address(street, city, state),
    extractField1 = _.street,
    extractField2 = _.city,
    extractField3 = _.state,
  )

  implicit val userAddressSchema = Schema.CaseClass2[User, Address, UserAddress](
    field1 = Field("user", userSchema),
    field2 = Field("address", addressSchema),
    construct = (user, address) => UserAddress(user, address),
    extractField1 = _.user,
    extractField2 = _.address,
  )

  implicit val companySchema = Schema.CaseClass2[User, List[UserAddress], Company](
    field1 = Field("boss", userSchema),
    field2 = Field("employees", Schema.list(userAddressSchema)),
    construct = (boss, employees) => Company(boss, employees),
    extractField1 = _.boss,
    extractField2 = _.employees,
  )


}

object Example6_ReifiedOptics extends zio.App {
  import Domain._

  val lensTest1 = for {
    _ <- ZIO.debug("lens test 1")
    user = User("Dominik", 35)
    userAccessors = userSchema.makeAccessors(ZioOpticsBuilder)
    lensName = userAccessors._1
    lensAge = userAccessors._2
    changedUser = lensName.zip(lensAge).setOptic(("Mike", 32))(user)
    _ <- ZIO.debug(user)
    _ <- ZIO.debug(changedUser)
  } yield ()

  val lensTest2 = for {
    _ <- ZIO.debug("lens test 2")
    user = User("Dominik", 35)
    address = Address("Street", "City", "State")
    userAddress = UserAddress(user, address)
    userAddressAccessors = userAddressSchema.makeAccessors(ZioOpticsBuilder)
    userAccessors = userSchema.makeAccessors(ZioOpticsBuilder)
    addressAccessors = addressSchema.makeAccessors(ZioOpticsBuilder)

    changedUserAddress = (userAddressAccessors._2 >>> addressAccessors._3).setOptic("New State")(userAddress)
    _ <- ZIO.debug(userAddress)
    _ <- ZIO.debug(changedUserAddress)
  } yield ()


  val traversalTest1 = for {
    _ <- ZIO.debug("traversal test 1.. trying to add a employee to a company")
    boss = User("Dominik", 36)
    company = Company(boss, Nil)
    companyAccessors = companySchema.makeAccessors(ZioOpticsBuilder)
    employeeTraversal = companySchema.field2.schema.makeAccessors(ZioOpticsBuilder)


//    userAccessors = userSchema.makeAccessors(ZioOpticsBuilder)
//    userAddressAccessors = userAddressSchema.makeAccessors(ZioOpticsBuilder)
//    x: _root_.zio.optics.Optic[List[UserAddress], List[UserAddress], Chunk[UserAddress], _root_.zio.optics.OpticFailure, _root_.zio.optics.OpticFailure, Chunk[UserAddress], List[UserAddress]] = ZioOpticsBuilder.makeTraversal[List[UserAddress], UserAddress](companySchema.field2.schema.asInstanceOf[Sequence[List[UserAddress], UserAddress]], companySchema.field2.schema)
//
//    _ = (companyAccessors._2 >>> x).

//    _ = (companyAccessors_2 >>>

//    _ = (companyAccessors._2 >>> employeeTraversal) ++ (UserAddress(User("joe", 22), Address("s1", "s2", "s3")())


  } yield ()


  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (lensTest1 *> lensTest2 *> traversalTest1).exitCode
}
