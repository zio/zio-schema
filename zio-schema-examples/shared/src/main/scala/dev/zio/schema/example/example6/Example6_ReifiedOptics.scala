package dev.zio.schema.example.example6

import zio._
import zio.schema.Schema._
import zio.schema._
import zio.schema.optics.ZioOpticsBuilder

private[example6] object Domain {
  final case class User(name: String, age: Int)
  final case class Address(street: String, city: String, state: String)
  final case class UserAddress(user: User, address: Address)
  final case class Company(boss: User, employees: List[UserAddress])

  implicit val userSchema: Schema.CaseClass2[String, Int, User] = Schema.CaseClass2[String, Int, User](
    TypeId.parse("dev.zio.schema.example.example6.Domain.User"),
    field1 = Field("name", Schema.primitive[String], get0 = _.name, set0 = (p, v) => p.copy(name = v)),
    field2 = Field("age", Schema.primitive[Int], get0 = _.age, set0 = (p, v) => p.copy(age = v)),
    construct = (name, years) => User(name, years)
  )

  implicit val addressSchema: CaseClass3[String, String, String, Address] =
    Schema.CaseClass3[String, String, String, Address](
      TypeId.parse("dev.zio.schema.example.example6.Domain.Address"),
      field1 = Field("street", Schema.primitive[String], get0 = _.street, set0 = (p, v) => p.copy(street = v)),
      field2 = Field("city", Schema.primitive[String], get0 = _.city, set0 = (p, v) => p.copy(city = v)),
      field3 = Field("state", Schema.primitive[String], get0 = _.state, set0 = (p, v) => p.copy(state = v)),
      construct = (street, city, state) => Address(street, city, state)
    )

  implicit val userAddressSchema: CaseClass2[User, Address, UserAddress] =
    Schema.CaseClass2[User, Address, UserAddress](
      TypeId.parse("dev.zio.schema.example.example6.Domain.UserAddress"),
      field1 = Field("user", userSchema, get0 = _.user, set0 = (p, v) => p.copy(user = v)),
      field2 = Field("address", addressSchema, get0 = _.address, set0 = (p, v) => p.copy(address = v)),
      construct = (user, address) => UserAddress(user, address)
    )

  implicit val companySchema: CaseClass2[User, List[UserAddress], Company] =
    Schema.CaseClass2[User, List[UserAddress], Company](
      TypeId.parse("dev.zio.schema.example.example6.Domain.Company"),
      field1 = Field("boss", userSchema, get0 = _.boss, set0 = (p, v) => p.copy(boss = v)),
      field2 =
        Field("employees", Schema.list(userAddressSchema), get0 = _.employees, set0 = (p, v) => p.copy(employees = v)),
      construct = (boss, employees) => Company(boss, employees)
    )

}

object Example6_ReifiedOptics extends ZIOAppDefault {
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
    //userAccessors        = userSchema.makeAccessors(ZioOpticsBuilder)
    addressAccessors = addressSchema.makeAccessors(ZioOpticsBuilder)

    changedUserAddress = (userAddressAccessors._2 >>> addressAccessors._3).setOptic("New State")(userAddress)
    _                  <- ZIO.debug(userAddress)
    _                  <- ZIO.debug(changedUserAddress)
  } yield ()

  val traversalTest1: ZIO[Any, Nothing, Unit] = for {
    _                  <- ZIO.debug("\n\n\n\n")
    _                  <- ZIO.debug("traversal test 1.. trying to add a employee to a company")
    company            = Company(boss = User("Dominik", 36), List.empty[UserAddress])
    _                  <- ZIO.debug("old company     :       " + company)
    (_, employeesLens) = companySchema.makeAccessors(ZioOpticsBuilder)

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

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] = (lensTest1 *> lensTest2 *> traversalTest1)
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
