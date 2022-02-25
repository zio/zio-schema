package dev.zio.schema.example.example6

import zio._
import zio.schema.Schema._
import zio.schema._
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
    extractField2 = _.age
  )

  implicit val addressSchema: CaseClass3[String, String, String, Address] =
    Schema.CaseClass3[String, String, String, Address](
      field1 = Field("street", Schema.primitive[String]),
      field2 = Field("city", Schema.primitive[String]),
      field3 = Field("state", Schema.primitive[String]),
      construct = (street, city, state) => Address(street, city, state),
      extractField1 = _.street,
      extractField2 = _.city,
      extractField3 = _.state
    )

  implicit val userAddressSchema: CaseClass2[User, Address, UserAddress] =
    Schema.CaseClass2[User, Address, UserAddress](
      field1 = Field("user", userSchema),
      field2 = Field("address", addressSchema),
      construct = (user, address) => UserAddress(user, address),
      extractField1 = _.user,
      extractField2 = _.address
    )

  implicit val companySchema: CaseClass2[User, List[UserAddress], Company] =
    Schema.CaseClass2[User, List[UserAddress], Company](
      field1 = Field("boss", userSchema),
      field2 = Field("employees", Schema.list(userAddressSchema)),
      construct = (boss, employees) => Company(boss, employees),
      extractField1 = _.boss,
      extractField2 = _.employees
    )

}

object Example6_ReifiedOptics extends zio.App {
  import Domain._

  val lensTest1: ZIO[Any, Nothing, Unit] = for {
    _             <- ZIO.debug("lens test 1")
    user          = User("Dominik", 35)
    userAccessors = userSchema.makeAccessors(ZioOpticsBuilder)
    lensName      = userAccessors._1
    lensAge       = userAccessors._2
    changedUser   = lensName.zip(lensAge).setOptic(("Mike", 32))(user)
    _             <- ZIO.debug(user)
    _             <- ZIO.debug(changedUser)
  } yield ()

  val lensTest2: ZIO[Any, Nothing, Unit] = for {
    _                    <- ZIO.debug("lens test 2")
    user                 = User("Dominik", 35)
    address              = Address("Street", "City", "State")
    userAddress          = UserAddress(user, address)
    userAddressAccessors = userAddressSchema.makeAccessors(ZioOpticsBuilder)
    userAccessors        = userSchema.makeAccessors(ZioOpticsBuilder)
    addressAccessors     = addressSchema.makeAccessors(ZioOpticsBuilder)

    changedUserAddress = (userAddressAccessors._2 >>> addressAccessors._3).setOptic("New State")(userAddress)
    _                  <- ZIO.debug(userAddress)
    _                  <- ZIO.debug(changedUserAddress)
  } yield ()

  val traversalTest1: ZIO[Any, Nothing, Unit] = for {
    _                         <- ZIO.debug("\n\n\n\n")
    _                         <- ZIO.debug("traversal test 1.. trying to add a employee to a company")
    company                   = Company(boss = User("Dominik", 36), List.empty[UserAddress])
    _                         <- ZIO.debug("old company     :       " + company)
    (bossLens, employeesLens) = companySchema.makeAccessors(ZioOpticsBuilder)

    employeeSchema = companySchema.field2.schema.asInstanceOf[Sequence[List[UserAddress], UserAddress, _]]
    employeesTraversal = ZioOpticsBuilder
      .makeTraversal[List[UserAddress], UserAddress](employeeSchema, userAddressSchema)

    // not working approach
    updatedCompany = (employeesLens >>> employeesTraversal).update(company)(
      emps => emps ++ Chunk(UserAddress(User("joe", 22), Address("s1", "s2", "s3")))
    )
    _ <- ZIO.debug("updated company : " + updatedCompany)

    // working approach
    updatedCompany2 = employeesLens.update(company)(
      emps => emps ++ List(UserAddress(User("joe", 22), Address("s1", "s2", "s3")))
    )
    _ <- ZIO.debug("updated company2: " + updatedCompany2)
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = (lensTest1 *> lensTest2 *> traversalTest1).exitCode
}

/**
 * Example by adam that shows pure usage of optics without any schema or derivation
 */
object Example6_PureOptics extends scala.App {
  import zio.optics._

  final case class User(name: String)
  final case class Employee(name: String)

  final case class Company(boss: User, employees: List[Employee])

  val company: Company = Company(User("boss"), List(Employee("employee1")))
  println("company with 1 employee :       " + company)

  val employees: Lens[Company, List[Employee]] =
    Lens(
      company => Right(company.employees),
      employees => company => Right(company.copy(employees = employees))
    )

  val employee: Employee = Employee("employee2")

  println("company with 2 employees: " + employees.update(company)(employees => employee :: employees))
}
