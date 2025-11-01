package zio.schema.validation

import zio.Scope
import zio.test._

object PhoneNumberValidationSpec extends ZIOSpecDefault {

  def spec: Spec[Environment with TestEnvironment with Scope, Any] =
    suite("PhoneNumberValidationSpec")(
      test("Regex phone number validation for Ascension Island") {
        val validation = PhoneNumberValidation.phoneNumberAC

        assertTrue(validation.validate("+247 007 897 156").isRight)
      },
      test("Regex phone number validation for Andorra") {
        val validation = PhoneNumberValidation.phoneNumberAD

        assertTrue(validation.validate("+376 9587541843").isRight)
      },
      test("Regex phone number validation for United Arab Emirates") {
        val validation = PhoneNumberValidation.phoneNumberAE

        assertTrue(validation.validate("00 971 0 362744241").isRight)
      },
      test("Regex phone number validation for Afghanistan") {
        val validation = PhoneNumberValidation.phoneNumberAF

        assertTrue(validation.validate("00 93 0 157218486").isRight)
      },
      test("Regex phone number validation for Antigua and Barbuda") {
        val validation = PhoneNumberValidation.phoneNumberAG

        assertTrue(validation.validate("011 1 268 612 6402").isRight)
      },
      test("Regex phone number validation for Anguilla") {
        val validation = PhoneNumberValidation.phoneNumberAI

        assertTrue(validation.validate("011 1 264 957 0153").isRight)
      },
      test("Regex phone number validation for Albania") {
        val validation = PhoneNumberValidation.phoneNumberAL

        assertTrue(validation.validate("+ 355 0747518589").isRight)
      },
      test("Regex phone number validation for Armenia") {
        val validation = PhoneNumberValidation.phoneNumberAM

        assertTrue(validation.validate("00 374 0756680505").isRight)
      },
      test("Regex phone number validation for Netherlands Antilles") {
        val validation = PhoneNumberValidation.phoneNumberAN

        assertTrue(validation.validate("00 5996053610108").isRight)
      },
      test("Regex phone number validation for Angola") {
        val validation = PhoneNumberValidation.phoneNumberAO

        assertTrue(validation.validate("00 2442385294120").isRight)
      },
      test("Regex phone number validation for Antarctica") {
        val validation = PhoneNumberValidation.phoneNumberAQ

        assertTrue(validation.validate("00 6720645145061").isRight)
      },
      test("Regex phone number validation for Argentina") {
        val validation = PhoneNumberValidation.phoneNumberAR

        assertTrue(validation.validate("+54 90 2883 6466").isRight)
      },
      test("Regex phone number validation for American Samoa") {
        val validation = PhoneNumberValidation.phoneNumberAS

        assertTrue(validation.validate("011 1 684 262 8882").isRight)
      },
      test("Regex phone number validation for Austria") {
        val validation = PhoneNumberValidation.phoneNumberAT

        assertTrue(validation.validate("+43 75 7429 2998").isRight)
      },
      test("Regex phone number validation for Australia") {
        val validation = PhoneNumberValidation.phoneNumberAU

        assertTrue(validation.validate("0011 61 2 8238 5964").isRight)
      },
      test("Regex phone number validation for Aruba") {
        val validation = PhoneNumberValidation.phoneNumberAW

        assertTrue(validation.validate("00 2979763865107").isRight)
      },
      test("Regex phone number validation for Aland Islands") {
        val validation = PhoneNumberValidation.phoneNumberAX

        assertTrue(validation.validate("+358 18 6919660").isRight)
      },
      test("Regex phone number validation for Azerbaijan") {
        val validation = PhoneNumberValidation.phoneNumberAZ

        assertTrue(validation.validate("+994 6152139308").isRight)
      },
      test("Regex phone number validation for Bosnia and Herzegovina") {
        val validation = PhoneNumberValidation.phoneNumberBA

        assertTrue(validation.validate("+387 4089348656").isRight)
      },
      test("Regex phone number validation for Barbados") {
        val validation = PhoneNumberValidation.phoneNumberBB

        assertTrue(validation.validate("011 1 246 990 0727").isRight)
      },
      test("Regex phone number validation for Bangladesh") {
        val validation = PhoneNumberValidation.phoneNumberBD

        assertTrue(validation.validate("+880 7320198614").isRight)
      },
      test("Regex phone number validation for Belgium") {
        val validation = PhoneNumberValidation.phoneNumberBE

        assertTrue(validation.validate("+32 8965598550").isRight)
      },
      test("Regex phone number validation for Burkina Faso") {
        val validation = PhoneNumberValidation.phoneNumberBF

        assertTrue(validation.validate("00 2267355948208").isRight)
      },
      test("Regex phone number validation for Bulgaria") {
        val validation = PhoneNumberValidation.phoneNumberBG

        assertTrue(validation.validate("+359 3442692667").isRight)
      },
      test("Regex phone number validation for Bahrain") {
        val validation = PhoneNumberValidation.phoneNumberBH

        assertTrue(validation.validate("00 9739488339568").isRight)
      },
      test("Regex phone number validation for Burundi") {
        val validation = PhoneNumberValidation.phoneNumberBI

        assertTrue(validation.validate("00 2574280643674").isRight)
      },
      test("Regex phone number validation for Benin") {
        val validation = PhoneNumberValidation.phoneNumberBJ

        assertTrue(validation.validate("00 2298643095311").isRight)
      },
      test("Regex phone number validation for Saint-Barthelemy") {
        val validation = PhoneNumberValidation.phoneNumberBL

        assertTrue(validation.validate("+590 6941370797").isRight)
      },
      test("Regex phone number validation for Bermuda") {
        val validation = PhoneNumberValidation.phoneNumberBM

        assertTrue(validation.validate("011 1 441 5849940").isRight)
      },
      test("Regex phone number validation for Brunei Darussalam") {
        val validation = PhoneNumberValidation.phoneNumberBN

        assertTrue(validation.validate("00 6736806811588").isRight)
      },
      test("Regex phone number validation for Bolivia") {
        val validation = PhoneNumberValidation.phoneNumberBO

        assertTrue(validation.validate("+591 7962476910").isRight)
      },
      test("Regex phone number validation for Caribbean Netherlands") {
        val validation = PhoneNumberValidation.phoneNumberBQ

        assertTrue(validation.validate("00 599 347 0831770").isRight)
      },
      test("Regex phone number validation for Brazil") {
        val validation = PhoneNumberValidation.phoneNumberBR

        assertTrue(validation.validate("+55 2309750213").isRight)
      },
      test("Regex phone number validation for Bahamas") {
        val validation = PhoneNumberValidation.phoneNumberBS

        assertTrue(validation.validate("011 1 242 6083082").isRight)
      },
      test("Regex phone number validation for Bhutan") {
        val validation = PhoneNumberValidation.phoneNumberBT

        assertTrue(validation.validate("00 9756295499608").isRight)
      },
      test("Regex phone number validation for Bouvet Island") {
        val validation = PhoneNumberValidation.phoneNumberBV

        assertTrue(validation.validate("00 474251138559").isRight)
      },
      test("Regex phone number validation for Botswana") {
        val validation = PhoneNumberValidation.phoneNumberBW

        assertTrue(validation.validate("00 2671639040488").isRight)
      },
      test("Regex phone number validation for Belarus") {
        val validation = PhoneNumberValidation.phoneNumberBY

        assertTrue(validation.validate("00810 375 376602746").isRight)
      },
      test("Regex phone number validation for Belize") {
        val validation = PhoneNumberValidation.phoneNumberBZ

        assertTrue(validation.validate("00 5010929861925").isRight)
      },
      test("Regex phone number validation for Canada") {
        val validation = PhoneNumberValidation.phoneNumberCA

        assertTrue(validation.validate("011 1 3665464257").isRight)
      },
      test("Regex phone number validation for Cocos (Keeling) Islands") {
        val validation = PhoneNumberValidation.phoneNumberCC

        assertTrue(validation.validate("0011 61 963775620").isRight)
      },
      test("Regex phone number validation for Congo, (Kinshasa)") {
        val validation = PhoneNumberValidation.phoneNumberCD

        assertTrue(validation.validate("00 243 9494344055").isRight)
      },
      test("Regex phone number validation for Central African Republic") {
        val validation = PhoneNumberValidation.phoneNumberCF

        assertTrue(validation.validate("+236 59 6075 5901").isRight)
      },
      test("Regex phone number validation for Congo (Brazzaville)") {
        val validation = PhoneNumberValidation.phoneNumberCG

        assertTrue(validation.validate("00 24283 8219 9231").isRight)
      },
      test("Regex phone number validation for Switzerland") {
        val validation = PhoneNumberValidation.phoneNumberCH

        assertTrue(validation.validate("+41 62 9504 5752").isRight)
        assertTrue(validation.validate("0041791234567").isRight) &&
        assertTrue(validation.validate("0041 79 123 45 67").isRight) &&
        assertTrue(validation.validate("+41791234567").isRight) &&
        assertTrue(validation.validate("+41 79 123 45 67").isRight) &&
        assertTrue(validation.validate("0791234567").isRight) &&
        assertTrue(validation.validate("-41 79 123 45 67").isLeft) &&
        assertTrue(validation.validate("+41 79 123 45 678").isLeft) &&
        assertTrue(validation.validate("79 123 45 678").isLeft)
      },
      test("Regex phone number validation for Côte d'Ivoire") {
        val validation = PhoneNumberValidation.phoneNumberCI

        assertTrue(validation.validate("00 2256102971096").isRight)
      },
      test("Regex phone number validation for Cook Islands") {
        val validation = PhoneNumberValidation.phoneNumberCK

        assertTrue(validation.validate("00 6829326903969").isRight)
      },
      test("Regex phone number validation for Chile") {
        val validation = PhoneNumberValidation.phoneNumberCL

        assertTrue(validation.validate("00 565768733529").isRight)
      },
      test("Regex phone number validation for Cameroon") {
        val validation = PhoneNumberValidation.phoneNumberCM

        assertTrue(validation.validate("00 2379601725044").isRight)
      },
      test("Regex phone number validation for China") {
        val validation = PhoneNumberValidation.phoneNumberCN

        assertTrue(validation.validate("00 86 6313884064").isRight)
      },
      test("Regex phone number validation for Colombia") {
        val validation = PhoneNumberValidation.phoneNumberCO

        assertTrue(validation.validate("00 57 5035525009").isRight)
      },
      test("Regex phone number validation for Costa Rica") {
        val validation = PhoneNumberValidation.phoneNumberCR

        assertTrue(validation.validate("+506 2608425852").isRight)
      },
      test("Regex phone number validation for Cuba") {
        val validation = PhoneNumberValidation.phoneNumberCU

        assertTrue(validation.validate("119 53 8684721023").isRight)
      },
      test("Regex phone number validation for Cape Verde") {
        val validation = PhoneNumberValidation.phoneNumberCV

        assertTrue(validation.validate("+238 54 1914 6255").isRight)
      },
      test("Regex phone number validation for Curacao") {
        val validation = PhoneNumberValidation.phoneNumberCW

        assertTrue(validation.validate("+599 69 9206 6789").isRight)
      },
      test("Regex phone number validation for Christmas Island") {
        val validation = PhoneNumberValidation.phoneNumberCX

        assertTrue(validation.validate("0011 61 8606709622").isRight)
      },
      test("Regex phone number validation for Cyprus") {
        val validation = PhoneNumberValidation.phoneNumberCY

        assertTrue(validation.validate("00 3570126511197").isRight)
      },
      test("Regex phone number validation for Czech Republic") {
        val validation = PhoneNumberValidation.phoneNumberCZ

        assertTrue(validation.validate("00 4209207508015").isRight)
      },
      test("Regex phone number validation for Germany") {
        val validation = PhoneNumberValidation.phoneNumberDE

        assertTrue(validation.validate("00 49 8591548021").isRight)
        assertTrue(validation.validate("+49 30 901820").isRight) &&
        assertTrue(validation.validate("004930901820").isRight) &&
        assertTrue(validation.validate("030901820").isRight) &&
        assertTrue(validation.validate("030 901820").isRight) &&
        assertTrue(validation.validate("+49 1522 343333").isRight) &&
        assertTrue(validation.validate("+49 152 901820").isRight) &&
        assertTrue(validation.validate("0049 152 901820").isRight) &&
        assertTrue(validation.validate("0041 30 901820").isLeft) &&
        assertTrue(validation.validate("49 152 901820").isLeft) &&
        assertTrue(validation.validate("049 152 901820").isLeft)
      },
      test("Regex phone number validation for Djibouti") {
        val validation = PhoneNumberValidation.phoneNumberDJ

        assertTrue(validation.validate("00 2539228818387").isRight)
      },
      test("Regex phone number validation for Denmark") {
        val validation = PhoneNumberValidation.phoneNumberDK

        assertTrue(validation.validate("00 450829342925").isRight)
      },
      test("Regex phone number validation for Dominica") {
        val validation = PhoneNumberValidation.phoneNumberDM

        assertTrue(validation.validate("011 1 767 117 934").isRight)
      },
      test("Regex phone number validation for Dominican Republic") {
        val validation = PhoneNumberValidation.phoneNumberDO

        assertTrue(validation.validate("011 1 8001 026250").isRight)
      },
      test("Regex phone number validation for Algeria") {
        val validation = PhoneNumberValidation.phoneNumberDZ

        assertTrue(validation.validate("00 213 1519404947").isRight)
      },
      test("Regex phone number validation for Ecuador") {
        val validation = PhoneNumberValidation.phoneNumberEC

        assertTrue(validation.validate("00 593 7661327173").isRight)
      },
      test("Regex phone number validation for Estonia") {
        val validation = PhoneNumberValidation.phoneNumberEE

        assertTrue(validation.validate("00 3727289832181").isRight)
      },
      test("Regex phone number validation for Egypt") {
        val validation = PhoneNumberValidation.phoneNumberEG

        assertTrue(validation.validate("00 20 3371313171").isRight)
      },
      test("Regex phone number validation for Western Sahara") {
        val validation = PhoneNumberValidation.phoneNumberEH

        assertTrue(validation.validate("00 212 528 7355097").isRight)
      },
      test("Regex phone number validation for Eritrea") {
        val validation = PhoneNumberValidation.phoneNumberER

        assertTrue(validation.validate("00 291 1389368590").isRight)
      },
      test("Regex phone number validation for Spain") {
        val validation = PhoneNumberValidation.phoneNumberES

        assertTrue(validation.validate("00 345499372300").isRight)
      },
      test("Regex phone number validation for Ethiopia") {
        val validation = PhoneNumberValidation.phoneNumberET

        assertTrue(validation.validate("00 251 9034175157").isRight)
      },
      test("Regex phone number validation for Finland") {
        val validation = PhoneNumberValidation.phoneNumberFI

        assertTrue(validation.validate("+358 1 20 504 1325").isRight)
      },
      test("Regex phone number validation for Fiji") {
        val validation = PhoneNumberValidation.phoneNumberFJ

        assertTrue(validation.validate("00 6792349653587").isRight)
      },
      test("Regex phone number validation for Falkland Islands (Malvinas)") {
        val validation = PhoneNumberValidation.phoneNumberFK

        assertTrue(validation.validate("00 5004003005535").isRight)
      },
      test("Regex phone number validation for Micronesia, Federated States of") {
        val validation = PhoneNumberValidation.phoneNumberFM

        assertTrue(validation.validate("00 6918410093532").isRight)
      },
      test("Regex phone number validation for Faroe Islands") {
        val validation = PhoneNumberValidation.phoneNumberFO

        assertTrue(validation.validate("00 298 5560667813").isRight)
      },
      test("Regex phone number validation for France") {
        val validation = PhoneNumberValidation.phoneNumberFR

        assertTrue(validation.validate("00 33 13 3901 4277").isRight)
      },
      test("Regex phone number validation for Gabon") {
        val validation = PhoneNumberValidation.phoneNumberGA

        assertTrue(validation.validate("00 241 97 2596 5544").isRight)
      },
      test("Regex phone number validation for United Kingdom") {
        val validation = PhoneNumberValidation.phoneNumberGB

        assertTrue(validation.validate("+44 94 2730 0927").isRight)
      },
      test("Regex phone number validation for Grenada") {
        val validation = PhoneNumberValidation.phoneNumberGD

        assertTrue(validation.validate("011 1 473 0027293").isRight)
      },
      test("Regex phone number validation for Georgia") {
        val validation = PhoneNumberValidation.phoneNumberGE

        assertTrue(validation.validate("+995 40 9347 1630").isRight)
      },
      test("Regex phone number validation for French Guiana") {
        val validation = PhoneNumberValidation.phoneNumberGF

        assertTrue(validation.validate("+594 18 821 2131").isRight)
      },
      test("Regex phone number validation for Guernsey") {
        val validation = PhoneNumberValidation.phoneNumberGG

        assertTrue(validation.validate("00 44 37 9295 8099").isRight)
      },
      test("Regex phone number validation for Ghana") {
        val validation = PhoneNumberValidation.phoneNumberGH

        assertTrue(validation.validate("+233 18 4682 1216").isRight)
      },
      test("Regex phone number validation for Gibraltar") {
        val validation = PhoneNumberValidation.phoneNumberGI

        assertTrue(validation.validate("00 3508527907652").isRight)
      },
      test("Regex phone number validation for Greenland") {
        val validation = PhoneNumberValidation.phoneNumberGL

        assertTrue(validation.validate("00 2996975320055").isRight)
      },
      test("Regex phone number validation for Gambia") {
        val validation = PhoneNumberValidation.phoneNumberGM

        assertTrue(validation.validate("00 2208600678245").isRight)
      },
      test("Regex phone number validation for Guinea") {
        val validation = PhoneNumberValidation.phoneNumberGN

        assertTrue(validation.validate("00 2246345663760").isRight)
      },
      test("Regex phone number validation for Guadeloupe") {
        val validation = PhoneNumberValidation.phoneNumberGP

        assertTrue(validation.validate("00 590 15 1203 0997").isRight)
      },
      test("Regex phone number validation for Equatorial Guinea") {
        val validation = PhoneNumberValidation.phoneNumberGQ

        assertTrue(validation.validate("00 2408656400449").isRight)
      },
      test("Regex phone number validation for Greece") {
        val validation = PhoneNumberValidation.phoneNumberGR

        assertTrue(validation.validate("00 305729313542").isRight)
      },
      test("Regex phone number validation for South Sandwich Islands") {
        val validation = PhoneNumberValidation.phoneNumberGS

        assertTrue(validation.validate("00 5003096544376").isRight)
      },
      test("Regex phone number validation for Guatemala") {
        val validation = PhoneNumberValidation.phoneNumberGT

        assertTrue(validation.validate("00 5029817333325").isRight)
      },
      test("Regex phone number validation for Guam") {
        val validation = PhoneNumberValidation.phoneNumberGU

        assertTrue(validation.validate("011 1 671 5507500").isRight)
      },
      test("Regex phone number validation for Guinea-Bissau") {
        val validation = PhoneNumberValidation.phoneNumberGW

        assertTrue(validation.validate("00001 245 72 3061 6510").isRight)
      },
      test("Regex phone number validation for Guyana") {
        val validation = PhoneNumberValidation.phoneNumberGY

        assertTrue(validation.validate("001 5923161184154").isRight)
      },
      test("Regex phone number validation for Hong Kong") {
        val validation = PhoneNumberValidation.phoneNumberHK

        assertTrue(validation.validate("00 8521317558295").isRight)
      },
      test("Regex phone number validation for Heard and Mcdonald Islands") {
        val validation = PhoneNumberValidation.phoneNumberHM

        assertTrue(validation.validate("00 6723191710825").isRight)
      },
      test("Regex phone number validation for Honduras") {
        val validation = PhoneNumberValidation.phoneNumberHN

        assertTrue(validation.validate("00 5047371910554").isRight)
      },
      test("Regex phone number validation for Croatia") {
        val validation = PhoneNumberValidation.phoneNumberHR

        assertTrue(validation.validate("00 385 52 1194 4850").isRight)
      },
      test("Regex phone number validation for Haiti") {
        val validation = PhoneNumberValidation.phoneNumberHT

        assertTrue(validation.validate("00 5092551395589").isRight)
      },
      test("Regex phone number validation for Hungary") {
        val validation = PhoneNumberValidation.phoneNumberHU

        assertTrue(validation.validate("+36 18 925 2015").isRight)
        assertTrue(validation.validate("003612318855").isRight) &&
        assertTrue(validation.validate("0036 1 231 88 55").isRight) &&
        assertTrue(validation.validate("0036 1 231 8855").isRight) &&
        assertTrue(validation.validate("+3611234567").isRight) &&
        assertTrue(validation.validate("+36 1 123 45 67").isRight) &&
        assertTrue(validation.validate("+36 1 123 4567").isRight) &&
        assertTrue(validation.validate("0611234567").isRight) &&
        assertTrue(validation.validate("0036-30-231-88-55").isRight) &&
        assertTrue(validation.validate("0036-30-231-8855").isRight) &&
        assertTrue(validation.validate("+36301234567").isRight) &&
        assertTrue(validation.validate("+36-30-123-45-67").isRight) &&
        assertTrue(validation.validate("+36-30-123-4567").isRight) &&
        assertTrue(validation.validate("06301234567").isRight) &&
        assertTrue(validation.validate("+36 11 123 45 67").isRight) &&
        assertTrue(validation.validate("+36 5 123 45 67").isRight) &&
        assertTrue(validation.validate("+36 1 123 45 678").isRight) &&
        assertTrue(validation.validate("-36 1 123 45 67").isLeft) &&
        assertTrue(validation.validate("1 123 45 678").isLeft) &&
        assertTrue(validation.validate("-36-30-123-45-67").isLeft) &&
        assertTrue(validation.validate("+36-30-123-45-678").isLeft) &&
        assertTrue(validation.validate("30-123-45-678").isLeft)
      },
      test("Regex phone number validation for Indonesia") {
        val validation = PhoneNumberValidation.phoneNumberID

        assertTrue(validation.validate("+62 69 5257 3582").isRight)
      },
      test("Regex phone number validation for Ireland") {
        val validation = PhoneNumberValidation.phoneNumberIE

        assertTrue(validation.validate("+353 82 9286 4067").isRight)
      },
      test("Regex phone number validation for Israel") {
        val validation = PhoneNumberValidation.phoneNumberIL

        assertTrue(validation.validate("01 972 60 8248 1948").isRight)
      },
      test("Regex phone number validation for Isle of Man") {
        val validation = PhoneNumberValidation.phoneNumberIM

        assertTrue(validation.validate("+44 74576 49 709").isRight)
      },
      test("Regex phone number validation for India") {
        val validation = PhoneNumberValidation.phoneNumberIN

        assertTrue(validation.validate("00 91 90 3412 7804").isRight)
      },
      test("Regex phone number validation for British Indian Ocean Territory") {
        val validation = PhoneNumberValidation.phoneNumberIO

        assertTrue(validation.validate("00 2462992060814").isRight)
      },
      test("Regex phone number validation for Iraq") {
        val validation = PhoneNumberValidation.phoneNumberIQ

        assertTrue(validation.validate("+964 257 4565 059").isRight)
      },
      test("Regex phone number validation for Iran") {
        val validation = PhoneNumberValidation.phoneNumberIR

        assertTrue(validation.validate("00 98 19 3019 1031").isRight)
      },
      test("Regex phone number validation for Iceland") {
        val validation = PhoneNumberValidation.phoneNumberIS

        assertTrue(validation.validate("00 3541289658929").isRight)
      },
      test("Regex phone number validation for Italy") {
        val validation = PhoneNumberValidation.phoneNumberIT

        assertTrue(validation.validate("00 393878141457").isRight)
      },
      test("Regex phone number validation for Jersey") {
        val validation = PhoneNumberValidation.phoneNumberJE

        assertTrue(validation.validate("+44 57 9972 9605").isRight)
      },
      test("Regex phone number validation for Jamaica") {
        val validation = PhoneNumberValidation.phoneNumberJM

        assertTrue(validation.validate("011 1 658 1150558").isRight)
      },
      test("Regex phone number validation for Jordan") {
        val validation = PhoneNumberValidation.phoneNumberJO

        assertTrue(validation.validate("00 962 84 4532 6108").isRight)
      },
      test("Regex phone number validation for Japan") {
        val validation = PhoneNumberValidation.phoneNumberJP

        assertTrue(validation.validate("010 81 20 4493 0924").isRight)
      },
      test("Regex phone number validation for Kenya") {
        val validation = PhoneNumberValidation.phoneNumberKE

        assertTrue(validation.validate("000 254 48 5947 4534").isRight)
      },
      test("Regex phone number validation for Kyrgyzstan") {
        val validation = PhoneNumberValidation.phoneNumberKG

        assertTrue(validation.validate("+996 16 6735 9115").isRight)
      },
      test("Regex phone number validation for Cambodia") {
        val validation = PhoneNumberValidation.phoneNumberKH

        assertTrue(validation.validate("+855 43 4519 9326").isRight)
      },
      test("Regex phone number validation for Kiribati") {
        val validation = PhoneNumberValidation.phoneNumberKI

        assertTrue(validation.validate("+686 77 5928 5264").isRight)
      },
      test("Regex phone number validation for Comoros") {
        val validation = PhoneNumberValidation.phoneNumberKM

        assertTrue(validation.validate("00 2698571387031").isRight)
      },
      test("Regex phone number validation for Saint Kitts and Nevis") {
        val validation = PhoneNumberValidation.phoneNumberKN

        assertTrue(validation.validate("011 1 869 6539479").isRight)
      },
      test("Regex phone number validation for North Korea") {
        val validation = PhoneNumberValidation.phoneNumberKP

        assertTrue(validation.validate("99 850 17 0277 0657").isRight)
      },
      test("Regex phone number validation for South Korea") {
        val validation = PhoneNumberValidation.phoneNumberKR

        assertTrue(validation.validate("+82 9649592789").isRight)
      },
      test("Regex phone number validation for Kuwait") {
        val validation = PhoneNumberValidation.phoneNumberKW

        assertTrue(validation.validate("+965 28 5107 9046").isRight)
      },
      test("Regex phone number validation for Cayman Islands") {
        val validation = PhoneNumberValidation.phoneNumberKY

        assertTrue(validation.validate("011 1 345 0106523").isRight)
      },
      test("Regex phone number validation for Kazakhstan") {
        val validation = PhoneNumberValidation.phoneNumberKZ

        assertTrue(validation.validate("00810 7 33 1450 8219").isRight)
      },
      test("Regex phone number validation for Lao PDR") {
        val validation = PhoneNumberValidation.phoneNumberLA

        assertTrue(validation.validate("00 856 5368127779").isRight)
      },
      test("Regex phone number validation for Lebanon") {
        val validation = PhoneNumberValidation.phoneNumberLB

        assertTrue(validation.validate("+961 3652044279").isRight)
      },
      test("Regex phone number validation for Saint Lucia") {
        val validation = PhoneNumberValidation.phoneNumberLC

        assertTrue(validation.validate("011 1 758 0471732").isRight)
      },
      test("Regex phone number validation for Liechtenstein") {
        val validation = PhoneNumberValidation.phoneNumberLI

        assertTrue(validation.validate("00 423 6420715797").isRight)
      },
      test("Regex phone number validation for Sri Lanka") {
        val validation = PhoneNumberValidation.phoneNumberLK

        assertTrue(validation.validate("00 94 1935386465").isRight)
      },
      test("Regex phone number validation for Liberia") {
        val validation = PhoneNumberValidation.phoneNumberLR

        assertTrue(validation.validate("00 231 6596099981").isRight)
      },
      test("Regex phone number validation for Lesotho") {
        val validation = PhoneNumberValidation.phoneNumberLS

        assertTrue(validation.validate("00 2662854967720").isRight)
      },
      test("Regex phone number validation for Lithuania") {
        val validation = PhoneNumberValidation.phoneNumberLT

        assertTrue(validation.validate("00 370 1974024335").isRight)
      },
      test("Regex phone number validation for Luxembourg") {
        val validation = PhoneNumberValidation.phoneNumberLU

        assertTrue(validation.validate("00 3523 505709778").isRight)
      },
      test("Regex phone number validation for Latvia") {
        val validation = PhoneNumberValidation.phoneNumberLV

        assertTrue(validation.validate("00 3712161145531").isRight)
      },
      test("Regex phone number validation for Libya") {
        val validation = PhoneNumberValidation.phoneNumberLY

        assertTrue(validation.validate("00 218 8264419925").isRight)
      },
      test("Regex phone number validation for Morocco") {
        val validation = PhoneNumberValidation.phoneNumberMA

        assertTrue(validation.validate("00 212 3517364769").isRight)
      },
      test("Regex phone number validation for Monaco") {
        val validation = PhoneNumberValidation.phoneNumberMC

        assertTrue(validation.validate("00 377 2021117087").isRight)
      },
      test("Regex phone number validation for Moldova") {
        val validation = PhoneNumberValidation.phoneNumberMD

        assertTrue(validation.validate("00 373 4446201390").isRight)
      },
      test("Regex phone number validation for Montenegro") {
        val validation = PhoneNumberValidation.phoneNumberME

        assertTrue(validation.validate("+382 19 7927 6970").isRight)
      },
      test("Regex phone number validation for Saint-Martin (French)") {
        val validation = PhoneNumberValidation.phoneNumberMF

        assertTrue(validation.validate("+590 01 0516 3845").isRight)
      },
      test("Regex phone number validation for Madagascar") {
        val validation = PhoneNumberValidation.phoneNumberMG

        assertTrue(validation.validate("+261 353 5945 041").isRight)
      },
      test("Regex phone number validation for Marshall Islands") {
        val validation = PhoneNumberValidation.phoneNumberMH

        assertTrue(validation.validate("011 692 5127396972").isRight)
      },
      test("Regex phone number validation for Macedonia") {
        val validation = PhoneNumberValidation.phoneNumberMK

        assertTrue(validation.validate("+389 408 929 2478").isRight)
      },
      test("Regex phone number validation for Mali") {
        val validation = PhoneNumberValidation.phoneNumberML

        assertTrue(validation.validate("00 2233170146268").isRight)
      },
      test("Regex phone number validation for Myanmar") {
        val validation = PhoneNumberValidation.phoneNumberMM

        assertTrue(validation.validate("+95 05 8180 4483").isRight)
      },
      test("Regex phone number validation for Mongolia") {
        val validation = PhoneNumberValidation.phoneNumberMN

        assertTrue(validation.validate("001 976 15 3640 9604").isRight)
      },
      test("Regex phone number validation for Macao") {
        val validation = PhoneNumberValidation.phoneNumberMO

        assertTrue(validation.validate("00 8531510792742").isRight)
      },
      test("Regex phone number validation for Northern Mariana Islands") {
        val validation = PhoneNumberValidation.phoneNumberMP

        assertTrue(validation.validate("011 1 670 893 643").isRight)
      },
      test("Regex phone number validation for Martinique") {
        val validation = PhoneNumberValidation.phoneNumberMQ

        assertTrue(validation.validate("+596 1932085020").isRight)
      },
      test("Regex phone number validation for Mauritania") {
        val validation = PhoneNumberValidation.phoneNumberMR

        assertTrue(validation.validate("+222 54 6048 0691").isRight)
      },
      test("Regex phone number validation for Montserrat") {
        val validation = PhoneNumberValidation.phoneNumberMS

        assertTrue(validation.validate("011 1 664 393 4545").isRight)
      },
      test("Regex phone number validation for Malta") {
        val validation = PhoneNumberValidation.phoneNumberMT

        assertTrue(validation.validate("00 3565701318096").isRight)
      },
      test("Regex phone number validation for Mauritius") {
        val validation = PhoneNumberValidation.phoneNumberMU

        assertTrue(validation.validate("0020 230 12 9500 3440").isRight)
      },
      test("Regex phone number validation for Maldives") {
        val validation = PhoneNumberValidation.phoneNumberMV

        assertTrue(validation.validate("00 9601707178237").isRight)
      },
      test("Regex phone number validation for Malawi") {
        val validation = PhoneNumberValidation.phoneNumberMW

        assertTrue(validation.validate("00 265 4648760757").isRight)
      },
      test("Regex phone number validation for Mexico") {
        val validation = PhoneNumberValidation.phoneNumberMX

        assertTrue(validation.validate("+52 55 1831 3440").isRight)
      },
      test("Regex phone number validation for Malaysia") {
        val validation = PhoneNumberValidation.phoneNumberMY

        assertTrue(validation.validate("+60 8189523546").isRight)
      },
      test("Regex phone number validation for Mozambique") {
        val validation = PhoneNumberValidation.phoneNumberMZ

        assertTrue(validation.validate("+2589622830508").isRight)
      },
      test("Regex phone number validation for Namibia") {
        val validation = PhoneNumberValidation.phoneNumberNA

        assertTrue(validation.validate("+264 4876840196").isRight)
      },
      test("Regex phone number validation for New Caledonia") {
        val validation = PhoneNumberValidation.phoneNumberNC

        assertTrue(validation.validate("+6872884247049").isRight)
      },
      test("Regex phone number validation for Niger") {
        val validation = PhoneNumberValidation.phoneNumberNE

        assertTrue(validation.validate("+2271480678971").isRight)
      },
      test("Regex phone number validation for Norfolk Island") {
        val validation = PhoneNumberValidation.phoneNumberNF

        assertTrue(validation.validate("+6725311013853").isRight)
      },
      test("Regex phone number validation for Nigeria") {
        val validation = PhoneNumberValidation.phoneNumberNG

        assertTrue(validation.validate("009 234 78 6668 2580").isRight)
      },
      test("Regex phone number validation for Nicaragua") {
        val validation = PhoneNumberValidation.phoneNumberNI

        assertTrue(validation.validate("+5057480416270").isRight)
      },
      test("Regex phone number validation for Netherlands") {
        val validation = PhoneNumberValidation.phoneNumberNL

        assertTrue(validation.validate("+31 3437953416").isRight)
      },
      test("Regex phone number validation for Norway") {
        val validation = PhoneNumberValidation.phoneNumberNO

        assertTrue(validation.validate("+47 02 6239 2816").isRight)
      },
      test("Regex phone number validation for Nepal") {
        val validation = PhoneNumberValidation.phoneNumberNP

        assertTrue(validation.validate("00 977 8947592034").isRight)
      },
      test("Regex phone number validation for Nauru") {
        val validation = PhoneNumberValidation.phoneNumberNR

        assertTrue(validation.validate("+674 14 0184 1819").isRight)
      },
      test("Regex phone number validation for Niue") {
        val validation = PhoneNumberValidation.phoneNumberNU

        assertTrue(validation.validate("00 683 81 5796 1422").isRight)
      },
      test("Regex phone number validation for New Zealand") {
        val validation = PhoneNumberValidation.phoneNumberNZ

        assertTrue(validation.validate("00 64 4309656720").isRight)
      },
      test("Regex phone number validation for Oman") {
        val validation = PhoneNumberValidation.phoneNumberOM

        assertTrue(validation.validate("+968 17 4687 2788").isRight)
      },
      test("Regex phone number validation for Panama") {
        val validation = PhoneNumberValidation.phoneNumberPA

        assertTrue(validation.validate("+507 10 8679 9468").isRight)
      },
      test("Regex phone number validation for Peru") {
        val validation = PhoneNumberValidation.phoneNumberPE

        assertTrue(validation.validate("0019 51 51 6407 3711").isRight)
      },
      test("Regex phone number validation for French Polynesia") {
        val validation = PhoneNumberValidation.phoneNumberPF

        assertTrue(validation.validate("00 689 31 4664 6885").isRight)
      },
      test("Regex phone number validation for Papua New Guinea") {
        val validation = PhoneNumberValidation.phoneNumberPG

        assertTrue(validation.validate("+675 10 2567 0318").isRight)
      },
      test("Regex phone number validation for Philippines") {
        val validation = PhoneNumberValidation.phoneNumberPH

        assertTrue(validation.validate("00 63 2851764972").isRight)
      },
      test("Regex phone number validation for Pakistan") {
        val validation = PhoneNumberValidation.phoneNumberPK

        assertTrue(validation.validate("+92 7553664097").isRight)
      },
      test("Regex phone number validation for Poland") {
        val validation = PhoneNumberValidation.phoneNumberPL

        assertTrue(validation.validate("00 48168250790").isRight)
      },
      test("Regex phone number validation for Saint Pierre and Miquelon") {
        val validation = PhoneNumberValidation.phoneNumberPM

        assertTrue(validation.validate("00 508 3214890710").isRight)
      },
      test("Regex phone number validation for Pitcairn Islands") {
        val validation = PhoneNumberValidation.phoneNumberPN

        assertTrue(validation.validate("+870 92 6226 9171").isRight)
      },
      test("Regex phone number validation for Puerto Rico") {
        val validation = PhoneNumberValidation.phoneNumberPR

        assertTrue(validation.validate("011 1 787 475 939").isRight)
      },
      test("Regex phone number validation for Palestinian Territory") {
        val validation = PhoneNumberValidation.phoneNumberPS

        assertTrue(validation.validate("00 970 5758777122").isRight)
      },
      test("Regex phone number validation for Portugal") {
        val validation = PhoneNumberValidation.phoneNumberPT

        assertTrue(validation.validate("+3516910095620").isRight)
        assertTrue(validation.validate("00351211140200").isRight) &&
        assertTrue(validation.validate("00351 211 140 200").isRight) &&
        assertTrue(validation.validate("00351 21 114 02 00").isRight) &&
        assertTrue(validation.validate("+351211140200").isRight) &&
        assertTrue(validation.validate("+351 21 114 02 00").isRight) &&
        assertTrue(validation.validate("+351 21 114 0200").isRight) &&
        assertTrue(validation.validate("-351 21 114 02 00").isLeft) &&
        assertTrue(validation.validate("+351 21 114 02 006").isLeft) &&
        assertTrue(validation.validate("21 114 02 006").isLeft)
      },
      test("Regex phone number validation for Palau") {
        val validation = PhoneNumberValidation.phoneNumberPW

        assertTrue(validation.validate("01 680 54 5298 6941").isRight)
      },
      test("Regex phone number validation for Paraguay") {
        val validation = PhoneNumberValidation.phoneNumberPY

        assertTrue(validation.validate("+595 7488642578").isRight)
      },
      test("Regex phone number validation for Qatar") {
        val validation = PhoneNumberValidation.phoneNumberQA

        assertTrue(validation.validate("00 974094360402").isRight)
      },
      test("Regex phone number validation for Réunion") {
        val validation = PhoneNumberValidation.phoneNumberRE

        assertTrue(validation.validate("00 262 8485511353").isRight)
      },
      test("Regex phone number validation for Romania") {
        val validation = PhoneNumberValidation.phoneNumberRO

        assertTrue(validation.validate("+40 4126654789").isRight)
      },
      test("Regex phone number validation for Serbia") {
        val validation = PhoneNumberValidation.phoneNumberRS

        assertTrue(validation.validate("00 381 5718415376").isRight)
        assertTrue(validation.validate("+381111234567").isRight) &&
        assertTrue(validation.validate("+381 11 123 45 67").isRight) &&
        assertTrue(validation.validate("00381111234567").isRight) &&
        assertTrue(validation.validate("00381 11 123 45 67").isRight) &&
        assertTrue(validation.validate("00381 230 123 45 67").isRight) &&
        assertTrue(validation.validate("0111234567").isRight) &&
        assertTrue(validation.validate("-381 11 123 45 67").isLeft) &&
        assertTrue(validation.validate("+381 11 123 45 6789").isLeft) &&
        assertTrue(validation.validate("11 123 45 678").isLeft)
      },
      test("Regex phone number validation for Russian Federation") {
        val validation = PhoneNumberValidation.phoneNumberRU

        assertTrue(validation.validate("810 7 8023940837").isRight)
      },
      test("Regex phone number validation for Rwanda") {
        val validation = PhoneNumberValidation.phoneNumberRW

        assertTrue(validation.validate("00 250 1071423830").isRight)
      },
      test("Regex phone number validation for Saudi Arabia") {
        val validation = PhoneNumberValidation.phoneNumberSA

        assertTrue(validation.validate("+966 2314438338").isRight)
      },
      test("Regex phone number validation for Solomon Islands") {
        val validation = PhoneNumberValidation.phoneNumberSB

        assertTrue(validation.validate("+677 68 0771 5592").isRight)
      },
      test("Regex phone number validation for Seychelles") {
        val validation = PhoneNumberValidation.phoneNumberSC

        assertTrue(validation.validate("+2483511003232").isRight)
      },
      test("Regex phone number validation for Sudan") {
        val validation = PhoneNumberValidation.phoneNumberSD

        assertTrue(validation.validate("+249 8299453804").isRight)
      },
      test("Regex phone number validation for Sweden") {
        val validation = PhoneNumberValidation.phoneNumberSE

        assertTrue(validation.validate("+46 3782292031").isRight)
      },
      test("Regex phone number validation for Singapore") {
        val validation = PhoneNumberValidation.phoneNumberSG

        assertTrue(validation.validate("0 65 894769144").isRight)
      },
      test("Regex phone number validation for Saint Helena") {
        val validation = PhoneNumberValidation.phoneNumberSH

        assertTrue(validation.validate("+290 256 649 891").isRight)
      },
      test("Regex phone number validation for Slovenia") {
        val validation = PhoneNumberValidation.phoneNumberSI

        assertTrue(validation.validate("+386 9216604503").isRight)
      },
      test("Regex phone number validation for Svalbard and Jan Mayen Islands") {
        val validation = PhoneNumberValidation.phoneNumberSJ

        assertTrue(validation.validate("+47 79 13828050").isRight)
      },
      test("Regex phone number validation for Slovakia") {
        val validation = PhoneNumberValidation.phoneNumberSK

        assertTrue(validation.validate("00 421 9837820736").isRight)
      },
      test("Regex phone number validation for Sierra Leone") {
        val validation = PhoneNumberValidation.phoneNumberSL

        assertTrue(validation.validate("00 232 4479684349").isRight)
      },
      test("Regex phone number validation for San Marino") {
        val validation = PhoneNumberValidation.phoneNumberSM

        assertTrue(validation.validate("00 378038935173").isRight)
      },
      test("Regex phone number validation for Senegal") {
        val validation = PhoneNumberValidation.phoneNumberSN

        assertTrue(validation.validate("00 2218570840759").isRight)
      },
      test("Regex phone number validation for Somalia") {
        val validation = PhoneNumberValidation.phoneNumberSO

        assertTrue(validation.validate("00 252 2908185816").isRight)
      },
      test("Regex phone number validation for Suriname") {
        val validation = PhoneNumberValidation.phoneNumberSR

        assertTrue(validation.validate("00 5975953326179").isRight)
      },
      test("Regex phone number validation for South Sudan") {
        val validation = PhoneNumberValidation.phoneNumberSS

        assertTrue(validation.validate("00 211 6613246029").isRight)
      },
      test("Regex phone number validation for Sao Tome and Principe") {
        val validation = PhoneNumberValidation.phoneNumberST

        assertTrue(validation.validate("00 239431200296").isRight)
      },
      test("Regex phone number validation for El Salvador") {
        val validation = PhoneNumberValidation.phoneNumberSV

        assertTrue(validation.validate("00 503389285910").isRight)
      },
      test("Regex phone number validation for Saint Marten") {
        val validation = PhoneNumberValidation.phoneNumberSX

        assertTrue(validation.validate("011 1 721 528183").isRight)
      },
      test("Regex phone number validation for Syria") {
        val validation = PhoneNumberValidation.phoneNumberSY

        assertTrue(validation.validate("00 963 3253918464").isRight)
      },
      test("Regex phone number validation for Swaziland") {
        val validation = PhoneNumberValidation.phoneNumberSZ

        assertTrue(validation.validate("00 268184154720").isRight)
      },
      test("Regex phone number validation for Tristan da Cunha") {
        val validation = PhoneNumberValidation.phoneNumberTA

        assertTrue(validation.validate("+290 8 63587169").isRight)
      },
      test("Regex phone number validation for Turks and Caicos Islands") {
        val validation = PhoneNumberValidation.phoneNumberTC

        assertTrue(validation.validate("011 1 649 102537").isRight)
      },
      test("Regex phone number validation for Chad") {
        val validation = PhoneNumberValidation.phoneNumberTD

        assertTrue(validation.validate("00 2359209745639").isRight)
      },
      test("Regex phone number validation for French Southern Territories") {
        val validation = PhoneNumberValidation.phoneNumberTF

        assertTrue(validation.validate("00 262 4215656462").isRight)
      },
      test("Regex phone number validation for Togo") {
        val validation = PhoneNumberValidation.phoneNumberTG

        assertTrue(validation.validate("00 2281952004914").isRight)
      },
      test("Regex phone number validation for Thailand") {
        val validation = PhoneNumberValidation.phoneNumberTH

        assertTrue(validation.validate("00 66 7946617112").isRight)
      },
      test("Regex phone number validation for Tajikistan") {
        val validation = PhoneNumberValidation.phoneNumberTJ

        assertTrue(validation.validate("00810 992 57 5953 7962").isRight)
      },
      test("Regex phone number validation for Tokelau") {
        val validation = PhoneNumberValidation.phoneNumberTK

        assertTrue(validation.validate("00 6908354403605").isRight)
      },
      test("Regex phone number validation for Timor-Leste") {
        val validation = PhoneNumberValidation.phoneNumberTL

        assertTrue(validation.validate("00 6704835783535").isRight)
      },
      test("Regex phone number validation for Turkmenistan") {
        val validation = PhoneNumberValidation.phoneNumberTM

        assertTrue(validation.validate("00810 993 299 05 5741").isRight)
      },
      test("Regex phone number validation for Tunisia") {
        val validation = PhoneNumberValidation.phoneNumberTN

        assertTrue(validation.validate("00 2162273895609").isRight)
      },
      test("Regex phone number validation for Tonga") {
        val validation = PhoneNumberValidation.phoneNumberTO

        assertTrue(validation.validate("00 6769802885047").isRight)
      },
      test("Regex phone number validation for Turkey") {
        val validation = PhoneNumberValidation.phoneNumberTR

        assertTrue(validation.validate("00 90 8195268757").isRight)
      },
      test("Regex phone number validation for Trinidad and Tobago") {
        val validation = PhoneNumberValidation.phoneNumberTT

        assertTrue(validation.validate("011 1 868 432 464").isRight)
      },
      test("Regex phone number validation for Tuvalu") {
        val validation = PhoneNumberValidation.phoneNumberTV

        assertTrue(validation.validate("+688 75 8858 5314").isRight)
      },
      test("Regex phone number validation for Taiwan") {
        val validation = PhoneNumberValidation.phoneNumberTW

        assertTrue(validation.validate("+886 35 6499 1065").isRight)
      },
      test("Regex phone number validation for Tanzania") {
        val validation = PhoneNumberValidation.phoneNumberTZ

        assertTrue(validation.validate("+255 0143374246").isRight)
      },
      test("Regex phone number validation for Ukraine") {
        val validation = PhoneNumberValidation.phoneNumberUA

        assertTrue(validation.validate("00 380 8406262037").isRight)
      },
      test("Regex phone number validation for Uganda") {
        val validation = PhoneNumberValidation.phoneNumberUG

        assertTrue(validation.validate("+256 3856449691").isRight)
      },
      test("Regex phone number validation for US Minor Outlying Islands") {
        val validation = PhoneNumberValidation.phoneNumberUM

        assertTrue(validation.validate("+1 929 131 901").isRight)
      },
      test("Regex phone number validation for United States of America") {
        val validation = PhoneNumberValidation.phoneNumberUS

        assertTrue(validation.validate("011 1 447142964").isRight)
      },
      test("Regex phone number validation for Uruguay") {
        val validation = PhoneNumberValidation.phoneNumberUY

        assertTrue(validation.validate("00 598 0928779109").isRight)
      },
      test("Regex phone number validation for Uzbekistan") {
        val validation = PhoneNumberValidation.phoneNumberUZ

        assertTrue(validation.validate("00810 998 251 702 0097").isRight)
      },
      test("Regex phone number validation for Holy See (Vatican City State)") {
        val validation = PhoneNumberValidation.phoneNumberVA

        assertTrue(validation.validate("+39 06698 10418").isRight)
      },
      test("Regex phone number validation for Saint Vincent and Grenadines") {
        val validation = PhoneNumberValidation.phoneNumberVC

        assertTrue(validation.validate("011 1 784 3314547").isRight)
      },
      test("Regex phone number validation for Venezuela") {
        val validation = PhoneNumberValidation.phoneNumberVE

        assertTrue(validation.validate("+58 0161257166").isRight)
      },
      test("Regex phone number validation for British Virgin Islands") {
        val validation = PhoneNumberValidation.phoneNumberVG

        assertTrue(validation.validate("011 1 284 966822").isRight)
      },
      test("Regex phone number validation for Virgin Islands, US") {
        val validation = PhoneNumberValidation.phoneNumberVI

        assertTrue(validation.validate("011 1 340 249394").isRight)
      },
      test("Regex phone number validation for Viet Nam") {
        val validation = PhoneNumberValidation.phoneNumberVN

        assertTrue(validation.validate("00 84 5582883606").isRight)
      },
      test("Regex phone number validation for Vanuatu") {
        val validation = PhoneNumberValidation.phoneNumberVU

        assertTrue(validation.validate("00 6786895604772").isRight)
      },
      test("Regex phone number validation for Wallis and Futuna Islands") {
        val validation = PhoneNumberValidation.phoneNumberWF

        assertTrue(validation.validate("00 6816712124320").isRight)
      },
      test("Regex phone number validation for Samoa") {
        val validation = PhoneNumberValidation.phoneNumberWS

        assertTrue(validation.validate("0 6856225946106").isRight)
      },
      test("Regex phone number validation for Kosovo") {
        val validation = PhoneNumberValidation.phoneNumberXK

        assertTrue(validation.validate("00 383 4979043799").isRight)
      },
      test("Regex phone number validation for Yemen") {
        val validation = PhoneNumberValidation.phoneNumberYE

        assertTrue(validation.validate("00 967 8589630609").isRight)
      },
      test("Regex phone number validation for Mayotte") {
        val validation = PhoneNumberValidation.phoneNumberYT

        assertTrue(validation.validate("00 262 269 44 43 21").isRight)
      },
      test("Regex phone number validation for South Africa") {
        val validation = PhoneNumberValidation.phoneNumberZA

        assertTrue(validation.validate("00 27 0081060414").isRight)
      },
      test("Regex phone number validation for Zambia") {
        val validation = PhoneNumberValidation.phoneNumberZM

        assertTrue(validation.validate("00 260 3563071452").isRight)
      },
      test("Regex phone number validation for Zimbabwe") {
        val validation = PhoneNumberValidation.phoneNumberZW

        assertTrue(validation.validate("+263 1141482994").isRight)
      }
    )
}
