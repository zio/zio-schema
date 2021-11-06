package dev.zio.schema.example.example6

import zio._
import zio.schema._
import zio.schema.Schema._
import zio.schema.syntax._

private[example6] object Domain {
  final case class User(name: String, age: Int)
  final case class Address(street: String, city: String, state: String)
  final case class UserAddress(user: User, address: Address)

  implicit val userSchema: Schema[User] = DeriveSchema.gen[User]
  implicit val addressSchema: Schema[Address] = DeriveSchema.gen[Address]
  implicit val userAddressSchema: Schema[UserAddress] = DeriveSchema.gen[UserAddress]
}

object Example6_ReifiedOptics extends zio.App {
  import Domain._


  val effect = for {
    _ <- ZIO.unit
// not yet possible with 0.1.1
    //    userAccessors = userSchema.makeAccessors(ZioOpticsBuilder)

  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = effect.exitCode
}
