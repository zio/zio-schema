package zio.schema.validation

object PhoneNumberValidation extends PhoneNumberValidation {}

trait PhoneNumberValidation {

  val optionalSpace: Regex       = Regex.literal(" ").atMost(1)
  val optionalSeparator: Regex   = Regex.oneOf('-', ' ').atMost(1)
  val twoDigits: Regex           = Regex.digit.exactly(2)
  val threeDigits: Regex         = Regex.digit.exactly(3)
  val plus: Regex                = Regex.literal("+")
  val doubleZero: Regex          = Regex.literal("00")
  val internationalPrefix: Regex = plus | doubleZero
  val nationalPrefixZero: Regex  = Regex.literal("0")
  val nationalPrefixOne: Regex   = Regex.literal("1")
  val nationalPrefixEight: Regex = Regex.literal("8")
  val digitsWithSeparator: Regex = Regex.digit.exactly(1) ~ optionalSeparator

  /** Phone number validation for Ascension Island */
  lazy val phoneNumberAC: Validation[String] = {
    val countryCode = Regex.literal("247")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Andorra */
  lazy val phoneNumberAD: Validation[String] = {
    val countryCode = Regex.literal("376")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for United Arab Emirates */
  lazy val phoneNumberAE: Validation[String] = {
    val countryCode = Regex.literal("971")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Afghanistan */
  lazy val phoneNumberAF: Validation[String] = {
    val countryCode = Regex.literal("93")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Antigua and Barbuda */
  lazy val phoneNumberAG: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("268")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Anguilla */
  lazy val phoneNumberAI: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("264")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Albania */
  lazy val phoneNumberAL: Validation[String] = {
    val countryCode = Regex.literal("355")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Armenia */
  lazy val phoneNumberAM: Validation[String] = {
    val countryCode = Regex.literal("374")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Netherlands Antilles */
  lazy val phoneNumberAN: Validation[String] = {
    val countryCode = Regex.literal("599")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Angola */
  lazy val phoneNumberAO: Validation[String] = {
    val countryCode = Regex.literal("244")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Antarctica */
  lazy val phoneNumberAQ: Validation[String] = {
    val countryCode = Regex.literal("672")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Argentina */
  lazy val phoneNumberAR: Validation[String] = {
    val countryCode = Regex.literal("54")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for American Samoa */
  lazy val phoneNumberAS: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("684")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Austria */
  lazy val phoneNumberAT: Validation[String] = {
    val countryCode = Regex.literal("43")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Australia */
  lazy val phoneNumberAU: Validation[String] = {
    val countryCode         = Regex.literal("61")
    val internationalPrefix = plus | Regex.literal("0011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Aruba */
  lazy val phoneNumberAW: Validation[String] = {
    val countryCode = Regex.literal("297")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Aland Islands */
  lazy val phoneNumberAX: Validation[String] = {
    val countryCode   = Regex.literal("358")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val leadingDigits = Regex.literal("18")
    val phoneNumber   = digitsWithSeparator.between(6, 8)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Azerbaijan */
  lazy val phoneNumberAZ: Validation[String] = {
    val countryCode = Regex.literal("994")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bosnia and Herzegovina */
  lazy val phoneNumberBA: Validation[String] = {
    val countryCode = Regex.literal("387")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Barbados */
  lazy val phoneNumberBB: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("246")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bangladesh */
  lazy val phoneNumberBD: Validation[String] = {
    val countryCode = Regex.literal("880")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Belgium */
  lazy val phoneNumberBE: Validation[String] = {
    val countryCode = Regex.literal("32")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Burkina Faso */
  lazy val phoneNumberBF: Validation[String] = {
    val countryCode = Regex.literal("226")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bulgaria */
  lazy val phoneNumberBG: Validation[String] = {
    val countryCode = Regex.literal("359")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bahrain */
  lazy val phoneNumberBH: Validation[String] = {
    val countryCode = Regex.literal("973")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Burundi */
  lazy val phoneNumberBI: Validation[String] = {
    val countryCode = Regex.literal("257")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Benin */
  lazy val phoneNumberBJ: Validation[String] = {
    val countryCode = Regex.literal("229")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint-Barthelemy */
  lazy val phoneNumberBL: Validation[String] = {
    val countryCode = Regex.literal("590")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bermuda */
  lazy val phoneNumberBM: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("441")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Brunei Darussalam */
  lazy val phoneNumberBN: Validation[String] = {
    val countryCode = Regex.literal("673")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bolivia */
  lazy val phoneNumberBO: Validation[String] = {
    val countryCode = Regex.literal("591")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Caribbean Netherlands */
  lazy val phoneNumberBQ: Validation[String] = {
    val countryCode   = Regex.literal("599")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("347")
    val phoneNumber   = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Brazil */
  lazy val phoneNumberBR: Validation[String] = {
    val countryCode = Regex.literal("55")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bahamas */
  lazy val phoneNumberBS: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("242")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bhutan */
  lazy val phoneNumberBT: Validation[String] = {
    val countryCode = Regex.literal("975")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Bouvet Island */
  lazy val phoneNumberBV: Validation[String] = {
    val countryCode = Regex.literal("47")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Botswana */
  lazy val phoneNumberBW: Validation[String] = {
    val countryCode = Regex.literal("267")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Belarus */
  lazy val phoneNumberBY: Validation[String] = {
    val countryCode         = Regex.literal("375")
    val internationalPrefix = plus | Regex.literal("00810")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixEight
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Belize */
  lazy val phoneNumberBZ: Validation[String] = {
    val countryCode = Regex.literal("501")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Canada */
  lazy val phoneNumberCA: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cocos (Keeling) Islands */
  lazy val phoneNumberCC: Validation[String] = {
    val countryCode         = Regex.literal("61")
    val internationalPrefix = plus | Regex.literal("0011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Central African Republic */
  lazy val phoneNumberCF: Validation[String] = {
    val countryCode = Regex.literal("236")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Congo (Brazzaville) */
  lazy val phoneNumberCG: Validation[String] = {
    val countryCode = Regex.literal("242")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Congo, (Kinshasa) */
  lazy val phoneNumberCD: Validation[String] = {
    val countryCode = Regex.literal("243")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Switzerland */
  lazy val phoneNumberCH: Validation[String] = {
    val countryCode = Regex.literal("41")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(8, 9)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cote d'Ivoire */
  lazy val phoneNumberCI: Validation[String] = {
    val countryCode = Regex.literal("225")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cook Islands */
  lazy val phoneNumberCK: Validation[String] = {
    val countryCode = Regex.literal("682")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Chile */
  lazy val phoneNumberCL: Validation[String] = {
    val countryCode = Regex.literal("56")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cameroon */
  lazy val phoneNumberCM: Validation[String] = {
    val countryCode = Regex.literal("237")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for China */
  lazy val phoneNumberCN: Validation[String] = {
    val countryCode = Regex.literal("86")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Colombia */
  lazy val phoneNumberCO: Validation[String] = {
    val countryCode = Regex.literal("57")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Costa Rica */
  lazy val phoneNumberCR: Validation[String] = {
    val countryCode = Regex.literal("506")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cuba */
  lazy val phoneNumberCU: Validation[String] = {
    val countryCode         = Regex.literal("53")
    val internationalPrefix = plus | Regex.literal("119")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cape Verde */
  lazy val phoneNumberCV: Validation[String] = {
    val countryCode = Regex.literal("238")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Curacao */
  lazy val phoneNumberCW: Validation[String] = {
    val countryCode   = Regex.literal("599")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("69")
    val phoneNumber   = digitsWithSeparator.between(6, 8)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Christmas Island */
  lazy val phoneNumberCX: Validation[String] = {
    val countryCode         = Regex.literal("61")
    val internationalPrefix = plus | Regex.literal("0011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cyprus */
  lazy val phoneNumberCY: Validation[String] = {
    val countryCode = Regex.literal("357")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Czech Republic */
  lazy val phoneNumberCZ: Validation[String] = {
    val countryCode = Regex.literal("420")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Germany */
  lazy val phoneNumberDE: Validation[String] = {
    val countryCode = Regex.literal("49")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(8, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Djibouti */
  lazy val phoneNumberDJ: Validation[String] = {
    val countryCode = Regex.literal("253")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Denmark */
  lazy val phoneNumberDK: Validation[String] = {
    val countryCode = Regex.literal("45")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Dominica */
  lazy val phoneNumberDM: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("767")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Dominican Republic */
  lazy val phoneNumberDO: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("8001")
    val phoneNumber         = digitsWithSeparator.exactly(6)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Algeria */
  lazy val phoneNumberDZ: Validation[String] = {
    val countryCode = Regex.literal("213")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Ecuador */
  lazy val phoneNumberEC: Validation[String] = {
    val countryCode = Regex.literal("593")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Estonia */
  lazy val phoneNumberEE: Validation[String] = {
    val countryCode = Regex.literal("372")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Egypt */
  lazy val phoneNumberEG: Validation[String] = {
    val countryCode = Regex.literal("20")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Western Sahara */
  lazy val phoneNumberEH: Validation[String] = {
    val countryCode   = Regex.literal("212")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val leadingDigits = Regex.literal("528")
    val phoneNumber   = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Eritrea */
  lazy val phoneNumberER: Validation[String] = {
    val countryCode = Regex.literal("291")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Spain */
  lazy val phoneNumberES: Validation[String] = {
    val countryCode = Regex.literal("34")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Ethiopia */
  lazy val phoneNumberET: Validation[String] = {
    val countryCode = Regex.literal("251")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Finland */
  lazy val phoneNumberFI: Validation[String] = {
    val countryCode   = Regex.literal("358")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val leadingDigits = Regex.literal("1")
    val phoneNumber   = digitsWithSeparator.between(6, 9)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Fiji */
  lazy val phoneNumberFJ: Validation[String] = {
    val countryCode = Regex.literal("679")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Falkland Islands (Malvinas) */
  lazy val phoneNumberFK: Validation[String] = {
    val countryCode = Regex.literal("500")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Micronesia */
  lazy val phoneNumberFM: Validation[String] = {
    val countryCode = Regex.literal("691")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Faroe Islands */
  lazy val phoneNumberFO: Validation[String] = {
    val countryCode = Regex.literal("298")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for France */
  lazy val phoneNumberFR: Validation[String] = {
    val countryCode = Regex.literal("33")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Gabon */
  lazy val phoneNumberGA: Validation[String] = {
    val countryCode = Regex.literal("241")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for United Kingdom */
  lazy val phoneNumberGB: Validation[String] = {
    val countryCode = Regex.literal("44")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Grenada */
  lazy val phoneNumberGD: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("473")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Georgia */
  lazy val phoneNumberGE: Validation[String] = {
    val countryCode = Regex.literal("995")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for French Guiana */
  lazy val phoneNumberGF: Validation[String] = {
    val countryCode = Regex.literal("594")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guernsey */
  lazy val phoneNumberGG: Validation[String] = {
    val countryCode = Regex.literal("44")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Ghana */
  lazy val phoneNumberGH: Validation[String] = {
    val countryCode = Regex.literal("233")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Gibraltar */
  lazy val phoneNumberGI: Validation[String] = {
    val countryCode = Regex.literal("350")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Greenland */
  lazy val phoneNumberGL: Validation[String] = {
    val countryCode = Regex.literal("299")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Gambia */
  lazy val phoneNumberGM: Validation[String] = {
    val countryCode = Regex.literal("220")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guinea */
  lazy val phoneNumberGN: Validation[String] = {
    val countryCode = Regex.literal("224")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guadeloupe */
  lazy val phoneNumberGP: Validation[String] = {
    val countryCode = Regex.literal("590")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Equatorial Guinea */
  lazy val phoneNumberGQ: Validation[String] = {
    val countryCode = Regex.literal("240")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Greece */
  lazy val phoneNumberGR: Validation[String] = {
    val countryCode = Regex.literal("30")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for South Sandwich Islands */
  lazy val phoneNumberGS: Validation[String] = {
    val countryCode = Regex.literal("500")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guatemala */
  lazy val phoneNumberGT: Validation[String] = {
    val countryCode = Regex.literal("502")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guam */
  lazy val phoneNumberGU: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("671")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guinea-Bissau */
  lazy val phoneNumberGW: Validation[String] = {
    val countryCode         = Regex.literal("245")
    val internationalPrefix = plus | Regex.literal("00001")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Guyana */
  lazy val phoneNumberGY: Validation[String] = {
    val countryCode         = Regex.literal("592")
    val internationalPrefix = plus | Regex.literal("001")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Hong Kong */
  lazy val phoneNumberHK: Validation[String] = {
    val countryCode = Regex.literal("852")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Heard and Mcdonald Islands */
  lazy val phoneNumberHM: Validation[String] = {
    val countryCode = Regex.literal("672")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Honduras */
  lazy val phoneNumberHN: Validation[String] = {
    val countryCode = Regex.literal("504")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Croatia */
  lazy val phoneNumberHR: Validation[String] = {
    val countryCode = Regex.literal("385")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Haiti */
  lazy val phoneNumberHT: Validation[String] = {
    val countryCode = Regex.literal("509")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Hungary */
  lazy val phoneNumberHU: Validation[String] = {
    val countryCode    = Regex.literal("36")
    val nationalPrefix = Regex.literal("06")
    val prefix         = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefix
    val phoneNumber    = digitsWithSeparator.between(8, 9)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Indonesia */
  lazy val phoneNumberID: Validation[String] = {
    val countryCode = Regex.literal("62")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Ireland */
  lazy val phoneNumberIE: Validation[String] = {
    val countryCode = Regex.literal("353")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Israel */
  lazy val phoneNumberIL: Validation[String] = {
    val countryCode         = Regex.literal("972")
    val internationalPrefix = plus | Regex.literal("00") | Regex.literal("01")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Isle of Man */
  lazy val phoneNumberIM: Validation[String] = {
    val countryCode   = Regex.literal("44")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val leadingDigits = Regex.literal("74576")
    val phoneNumber   = digitsWithSeparator.between(4, 5)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for India */
  lazy val phoneNumberIN: Validation[String] = {
    val countryCode = Regex.literal("91")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for British Indian Ocean Territory */
  lazy val phoneNumberIO: Validation[String] = {
    val countryCode = Regex.literal("246")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Iraq */
  lazy val phoneNumberIQ: Validation[String] = {
    val countryCode = Regex.literal("964")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Iran */
  lazy val phoneNumberIR: Validation[String] = {
    val countryCode = Regex.literal("98")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Iceland */
  lazy val phoneNumberIS: Validation[String] = {
    val countryCode = Regex.literal("354")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Italy */
  lazy val phoneNumberIT: Validation[String] = {
    val countryCode = Regex.literal("39")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Jersey */
  lazy val phoneNumberJE: Validation[String] = {
    val countryCode = Regex.literal("44")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Jamaica */
  lazy val phoneNumberJM: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("658") | Regex.literal("876")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Jordan */
  lazy val phoneNumberJO: Validation[String] = {
    val countryCode = Regex.literal("962")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Japan */
  lazy val phoneNumberJP: Validation[String] = {
    val countryCode         = Regex.literal("81")
    val internationalPrefix = plus | Regex.literal("010")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Kenya */
  lazy val phoneNumberKE: Validation[String] = {
    val countryCode         = Regex.literal("254")
    val internationalPrefix = plus | Regex.literal("000")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Kyrgyzstan */
  lazy val phoneNumberKG: Validation[String] = {
    val countryCode = Regex.literal("996")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cambodia */
  lazy val phoneNumberKH: Validation[String] = {
    val countryCode = Regex.literal("855")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Kiribati */
  lazy val phoneNumberKI: Validation[String] = {
    val countryCode = Regex.literal("686")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Comoros */
  lazy val phoneNumberKM: Validation[String] = {
    val countryCode = Regex.literal("269")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint Kitts and Nevis */
  lazy val phoneNumberKN: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("869")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for North Korea */
  lazy val phoneNumberKP: Validation[String] = {
    val countryCode         = Regex.literal("850")
    val internationalPrefix = plus | Regex.literal("00") | Regex.literal("99")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for South Korea */
  lazy val phoneNumberKR: Validation[String] = {
    val countryCode = Regex.literal("82")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Kuwait */
  lazy val phoneNumberKW: Validation[String] = {
    val countryCode = Regex.literal("965")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Cayman Islands */
  lazy val phoneNumberKY: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("345")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Kazakhstan */
  lazy val phoneNumberKZ: Validation[String] = {
    val countryCode         = Regex.literal("7")
    val internationalPrefix = plus | Regex.literal("00810")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val leadingDigits       = Regex.literal("33") | Regex.literal("7")
    val phoneNumber         = digitsWithSeparator.between(6, 8)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Lao PDR */
  lazy val phoneNumberLA: Validation[String] = {
    val countryCode = Regex.literal("856")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Lebanon */
  lazy val phoneNumberLB: Validation[String] = {
    val countryCode = Regex.literal("961")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint Lucia */
  lazy val phoneNumberLC: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("758")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Liechtenstein */
  lazy val phoneNumberLI: Validation[String] = {
    val countryCode = Regex.literal("423")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Sri Lanka */
  lazy val phoneNumberLK: Validation[String] = {
    val countryCode = Regex.literal("94")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Liberia */
  lazy val phoneNumberLR: Validation[String] = {
    val countryCode = Regex.literal("231")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Lesotho */
  lazy val phoneNumberLS: Validation[String] = {
    val countryCode = Regex.literal("266")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Lithuania */
  lazy val phoneNumberLT: Validation[String] = {
    val countryCode = Regex.literal("370")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixEight
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Luxembourg */
  lazy val phoneNumberLU: Validation[String] = {
    val countryCode = Regex.literal("352")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Latvia */
  lazy val phoneNumberLV: Validation[String] = {
    val countryCode = Regex.literal("371")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Libya */
  lazy val phoneNumberLY: Validation[String] = {
    val countryCode = Regex.literal("218")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Morocco */
  lazy val phoneNumberMA: Validation[String] = {
    val countryCode = Regex.literal("212")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Monaco */
  lazy val phoneNumberMC: Validation[String] = {
    val countryCode = Regex.literal("377")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Moldova */
  lazy val phoneNumberMD: Validation[String] = {
    val countryCode = Regex.literal("373")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Montenegro */
  lazy val phoneNumberME: Validation[String] = {
    val countryCode = Regex.literal("382")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint-Martin (French) */
  lazy val phoneNumberMF: Validation[String] = {
    val countryCode = Regex.literal("590")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Madagascar */
  lazy val phoneNumberMG: Validation[String] = {
    val countryCode = Regex.literal("261")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Marshall Islands */
  lazy val phoneNumberMH: Validation[String] = {
    val countryCode         = Regex.literal("692")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Macedonia, Republic of */
  lazy val phoneNumberMK: Validation[String] = {
    val countryCode = Regex.literal("389")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mali */
  lazy val phoneNumberML: Validation[String] = {
    val countryCode = Regex.literal("223")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Myanmar */
  lazy val phoneNumberMM: Validation[String] = {
    val countryCode = Regex.literal("95")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mongolia */
  lazy val phoneNumberMN: Validation[String] = {
    val countryCode         = Regex.literal("976")
    val internationalPrefix = plus | Regex.literal("001")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Macao, SAR China */
  lazy val phoneNumberMO: Validation[String] = {
    val countryCode = Regex.literal("853")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Northern Mariana Islands */
  lazy val phoneNumberMP: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits       = Regex.literal("670")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Martinique */
  lazy val phoneNumberMQ: Validation[String] = {
    val countryCode = Regex.literal("596")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mauritania */
  lazy val phoneNumberMR: Validation[String] = {
    val countryCode = Regex.literal("222")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Montserrat */
  lazy val phoneNumberMS: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("664")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Malta */
  lazy val phoneNumberMT: Validation[String] = {
    val countryCode = Regex.literal("356")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mauritius */
  lazy val phoneNumberMU: Validation[String] = {
    val countryCode         = Regex.literal("230")
    val internationalPrefix = plus | Regex.literal("0020")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Maldives */
  lazy val phoneNumberMV: Validation[String] = {
    val countryCode = Regex.literal("960")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Malawi */
  lazy val phoneNumberMW: Validation[String] = {
    val countryCode = Regex.literal("265")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mexico */
  lazy val phoneNumberMX: Validation[String] = {
    val countryCode    = Regex.literal("52")
    val nationalPrefix = Regex.literal("01")
    val prefix         = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefix
    val phoneNumber    = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Malaysia */
  lazy val phoneNumberMY: Validation[String] = {
    val countryCode = Regex.literal("60")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mozambique */
  lazy val phoneNumberMZ: Validation[String] = {
    val countryCode = Regex.literal("258")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Namibia */
  lazy val phoneNumberNA: Validation[String] = {
    val countryCode = Regex.literal("264")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for New Caledonia */
  lazy val phoneNumberNC: Validation[String] = {
    val countryCode = Regex.literal("687")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Niger */
  lazy val phoneNumberNE: Validation[String] = {
    val countryCode = Regex.literal("227")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Norfolk Island */
  lazy val phoneNumberNF: Validation[String] = {
    val countryCode = Regex.literal("672")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Nigeria */
  lazy val phoneNumberNG: Validation[String] = {
    val countryCode         = Regex.literal("234")
    val internationalPrefix = plus | Regex.literal("009")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)
    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Nicaragua */
  lazy val phoneNumberNI: Validation[String] = {
    val countryCode = Regex.literal("505")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Netherlands */
  lazy val phoneNumberNL: Validation[String] = {
    val countryCode = Regex.literal("31")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Norway */
  lazy val phoneNumberNO: Validation[String] = {
    val countryCode   = Regex.literal("47")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("02")
    val phoneNumber   = digitsWithSeparator.between(6, 8)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Nepal */
  lazy val phoneNumberNP: Validation[String] = {
    val countryCode = Regex.literal("977")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Nauru */
  lazy val phoneNumberNR: Validation[String] = {
    val countryCode = Regex.literal("674")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Niue */
  lazy val phoneNumberNU: Validation[String] = {
    val countryCode = Regex.literal("683")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for New Zealand */
  lazy val phoneNumberNZ: Validation[String] = {
    val countryCode = Regex.literal("64")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Oman */
  lazy val phoneNumberOM: Validation[String] = {
    val countryCode = Regex.literal("968")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Panama */
  lazy val phoneNumberPA: Validation[String] = {
    val countryCode = Regex.literal("507")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Peru */
  lazy val phoneNumberPE: Validation[String] = {
    val countryCode         = Regex.literal("51")
    val internationalPrefix = plus | Regex.literal("0019")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for French Polynesia */
  lazy val phoneNumberPF: Validation[String] = {
    val countryCode = Regex.literal("689")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Papua New Guinea */
  lazy val phoneNumberPG: Validation[String] = {
    val countryCode = Regex.literal("675")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Philippines */
  lazy val phoneNumberPH: Validation[String] = {
    val countryCode = Regex.literal("63")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Pakistan */
  lazy val phoneNumberPK: Validation[String] = {
    val countryCode = Regex.literal("92")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Poland */
  lazy val phoneNumberPL: Validation[String] = {
    val countryCode = Regex.literal("48")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint Pierre and Miquelon */
  lazy val phoneNumberPM: Validation[String] = {
    val countryCode = Regex.literal("508")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Pitcairn Islands */
  lazy val phoneNumberPN: Validation[String] = {
    val countryCode = Regex.literal("870")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Puerto Rico */
  lazy val phoneNumberPR: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("787") | Regex.literal("939")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Palestinian Territory */
  lazy val phoneNumberPS: Validation[String] = {
    val countryCode = Regex.literal("970")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Portugal */
  lazy val phoneNumberPT: Validation[String] = {
    val countryCode = Regex.literal("351")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(8, 9)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Palau */
  lazy val phoneNumberPW: Validation[String] = {
    val countryCode         = Regex.literal("680")
    val internationalPrefix = plus | Regex.literal("01")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Paraguay */
  lazy val phoneNumberPY: Validation[String] = {
    val countryCode = Regex.literal("595")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Qatar */
  lazy val phoneNumberQA: Validation[String] = {
    val countryCode = Regex.literal("974")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Reunion */
  lazy val phoneNumberRE: Validation[String] = {
    val countryCode = Regex.literal("262")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Romania */
  lazy val phoneNumberRO: Validation[String] = {
    val countryCode = Regex.literal("40")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Serbia */
  lazy val phoneNumberRS: Validation[String] = {
    val countryCode = Regex.literal("381")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Russia */
  lazy val phoneNumberRU: Validation[String] = {
    val countryCode         = Regex.literal("7")
    val internationalPrefix = plus | Regex.literal("810")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixEight
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Rwanda */
  lazy val phoneNumberRW: Validation[String] = {
    val countryCode = Regex.literal("250")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saudi Arabia */
  lazy val phoneNumberSA: Validation[String] = {
    val countryCode = Regex.literal("966")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Solomon Islands */
  lazy val phoneNumberSB: Validation[String] = {
    val countryCode = Regex.literal("677")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Seychelles */
  lazy val phoneNumberSC: Validation[String] = {
    val countryCode = Regex.literal("248")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Sudan */
  lazy val phoneNumberSD: Validation[String] = {
    val countryCode = Regex.literal("249")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Sweden */
  lazy val phoneNumberSE: Validation[String] = {
    val countryCode = Regex.literal("46")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Singapore */
  lazy val phoneNumberSG: Validation[String] = {
    val countryCode         = Regex.literal("65")
    val internationalPrefix = plus | Regex.literal("0")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint Helena */
  lazy val phoneNumberSH: Validation[String] = {
    val countryCode   = Regex.literal("290")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("256")
    val phoneNumber   = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Slovenia */
  lazy val phoneNumberSI: Validation[String] = {
    val countryCode = Regex.literal("386")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Svalbard and Jan Mayen Islands */
  lazy val phoneNumberSJ: Validation[String] = {
    val countryCode   = Regex.literal("47")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("79")
    val phoneNumber   = digitsWithSeparator.between(6, 8)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Slovakia */
  lazy val phoneNumberSK: Validation[String] = {
    val countryCode = Regex.literal("421")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Sierra Leone */
  lazy val phoneNumberSL: Validation[String] = {
    val countryCode = Regex.literal("232")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for San Marino */
  lazy val phoneNumberSM: Validation[String] = {
    val countryCode = Regex.literal("378")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Senegal */
  lazy val phoneNumberSN: Validation[String] = {
    val countryCode = Regex.literal("221")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Somalia */
  lazy val phoneNumberSO: Validation[String] = {
    val countryCode = Regex.literal("252")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Suriname */
  lazy val phoneNumberSR: Validation[String] = {
    val countryCode = Regex.literal("597")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for South Sudan */
  lazy val phoneNumberSS: Validation[String] = {
    val countryCode = Regex.literal("211")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Sao Tome and Principe */
  lazy val phoneNumberST: Validation[String] = {
    val countryCode = Regex.literal("239")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for El Salvador */
  lazy val phoneNumberSV: Validation[String] = {
    val countryCode = Regex.literal("503")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint Marten */
  lazy val phoneNumberSX: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("721")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Syria */
  lazy val phoneNumberSY: Validation[String] = {
    val countryCode = Regex.literal("963")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Swaziland */
  lazy val phoneNumberSZ: Validation[String] = {
    val countryCode = Regex.literal("268")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tristan da Cunha */
  lazy val phoneNumberTA: Validation[String] = {
    val countryCode   = Regex.literal("290")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("8")
    val phoneNumber   = digitsWithSeparator.between(6, 9)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Turks and Caicos Islands */
  lazy val phoneNumberTC: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("649")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Chad */
  lazy val phoneNumberTD: Validation[String] = {
    val countryCode = Regex.literal("235")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for French Southern Territories */
  lazy val phoneNumberTF: Validation[String] = {
    val countryCode = Regex.literal("262")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Togo */
  lazy val phoneNumberTG: Validation[String] = {
    val countryCode = Regex.literal("228")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Thailand */
  lazy val phoneNumberTH: Validation[String] = {
    val countryCode = Regex.literal("66")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tajikistan */
  lazy val phoneNumberTJ: Validation[String] = {
    val countryCode         = Regex.literal("992")
    val internationalPrefix = plus | Regex.literal("00810")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tokelau */
  lazy val phoneNumberTK: Validation[String] = {
    val countryCode = Regex.literal("690")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Timor-Leste */
  lazy val phoneNumberTL: Validation[String] = {
    val countryCode = Regex.literal("670")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Turkmenistan */
  lazy val phoneNumberTM: Validation[String] = {
    val countryCode         = Regex.literal("993")
    val internationalPrefix = plus | Regex.literal("00810")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixEight
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tunisia */
  lazy val phoneNumberTN: Validation[String] = {
    val countryCode = Regex.literal("216")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tonga */
  lazy val phoneNumberTO: Validation[String] = {
    val countryCode = Regex.literal("676")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Turkey */
  lazy val phoneNumberTR: Validation[String] = {
    val countryCode = Regex.literal("90")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Trinidad and Tobago */
  lazy val phoneNumberTT: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("868")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tuvalu */
  lazy val phoneNumberTV: Validation[String] = {
    val countryCode = Regex.literal("688")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Taiwan */
  lazy val phoneNumberTW: Validation[String] = {
    val countryCode = Regex.literal("886")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Tanzania */
  lazy val phoneNumberTZ: Validation[String] = {
    val countryCode         = Regex.literal("255")
    val internationalPrefix = plus | Regex.literal("00[056]")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Ukraine */
  lazy val phoneNumberUA: Validation[String] = {
    val countryCode = Regex.literal("380")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Uganda */
  lazy val phoneNumberUG: Validation[String] = {
    val countryCode         = Regex.literal("256")
    val internationalPrefix = plus | Regex.literal("00[057]")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for US Minor Outlying Islands */
  lazy val phoneNumberUM: Validation[String] = {
    val countryCode = Regex.literal("1")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for United States of America */
  lazy val phoneNumberUS: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Uruguay */
  lazy val phoneNumberUY: Validation[String] = {
    val countryCode = Regex.literal("598")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Uzbekistan */
  lazy val phoneNumberUZ: Validation[String] = {
    val countryCode         = Regex.literal("998")
    val internationalPrefix = plus | Regex.literal("00810")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixEight
    val phoneNumber         = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Holy See (Vatican City State) */
  lazy val phoneNumberVA: Validation[String] = {
    val countryCode   = Regex.literal("39")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode
    val leadingDigits = Regex.literal("06698")
    val phoneNumber   = digitsWithSeparator.between(4, 6)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Saint Vincent and Grenadines */
  lazy val phoneNumberVC: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("784")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Venezuela */
  lazy val phoneNumberVE: Validation[String] = {
    val countryCode = Regex.literal("58")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for British Virgin Islands */
  lazy val phoneNumberVG: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("284")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Virgin Islands, US */
  lazy val phoneNumberVI: Validation[String] = {
    val countryCode         = Regex.literal("1")
    val internationalPrefix = plus | Regex.literal("011")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixOne
    val leadingDigits       = Regex.literal("340")
    val phoneNumber         = digitsWithSeparator.between(6, 7)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Vietnam */
  lazy val phoneNumberVN: Validation[String] = {
    val countryCode = Regex.literal("84")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Vanuatu */
  lazy val phoneNumberVU: Validation[String] = {
    val countryCode = Regex.literal("678")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Wallis and Futuna Islands */
  lazy val phoneNumberWF: Validation[String] = {
    val countryCode = Regex.literal("681")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Samoa */
  lazy val phoneNumberWS: Validation[String] = {
    val countryCode         = Regex.literal("685")
    val internationalPrefix = plus | Regex.literal("0")
    val prefix              = internationalPrefix ~ optionalSeparator ~ countryCode
    val phoneNumber         = digitsWithSeparator.between(9, 10)
    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Kosovo */
  lazy val phoneNumberXK: Validation[String] = {
    val countryCode = Regex.literal("383")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Yemen */
  lazy val phoneNumberYE: Validation[String] = {
    val countryCode = Regex.literal("967")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Mayotte */
  lazy val phoneNumberYT: Validation[String] = {
    val countryCode   = Regex.literal("262")
    val prefix        = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val leadingDigits = Regex.literal("269") | Regex.literal("63")
    val phoneNumber   = digitsWithSeparator.between(6, 8)

    Validation.regex(prefix ~ optionalSeparator ~ leadingDigits ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for South Africa */
  lazy val phoneNumberZA: Validation[String] = {
    val countryCode = Regex.literal("27")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Zambia */
  lazy val phoneNumberZM: Validation[String] = {
    val countryCode = Regex.literal("260")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

  /** Phone number validation for Zimbabwe */
  lazy val phoneNumberZW: Validation[String] = {
    val countryCode = Regex.literal("263")
    val prefix      = internationalPrefix ~ optionalSeparator ~ countryCode | nationalPrefixZero
    val phoneNumber = digitsWithSeparator.between(9, 10)

    Validation.regex(prefix ~ optionalSeparator ~ phoneNumber)
  }

}
