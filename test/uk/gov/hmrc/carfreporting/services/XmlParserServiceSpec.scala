/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.carfreporting.services

import uk.gov.hmrc.carfreporting.base.NoGuiceSpecBase
import uk.gov.hmrc.carfreporting.dispatchers.{MainDispatcherName, XmlDispatcher}
import uk.gov.hmrc.carfreporting.models.ExtractedFileDetails
import uk.gov.hmrc.carfreporting.models.errors.{InternalServerError, XmlErrors}

class XmlParserServiceSpec extends NoGuiceSpecBase {

  val mainDispatcherName = new MainDispatcherName()
  val xmlDispatcher      = new XmlDispatcher(actorSystem, mainDispatcherName)
  val service            = new XmlParserService(testEnv)(xmlDispatcher)

  "XmlParserService" - {

    "must return file_not_found error when the XML file does not exist" in {
      val result = service.validateAndExtract("invalid/path/nonexistent.xml").value.futureValue

      result match {
        case Left(e: XmlErrors) => e.errors.head.errorCode mustBe "file_not_found"
        case _                  => fail()
      }
    }

    "must successfully validate and extract a well-formed XML that matches the schema" - {
      "given a standard valid XML file" in {
        val path = "data/examples/valid-carf.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-2024-0001",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Acme Crypto Exchange Ltd"),
            messageTypeIndic = "CARF701",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD1"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing test data" in {
        val path = "data/examples/test-data-oecd10-rcasp.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-TESTDATA-RCASP",
            sendingEntityIn = "RCASP001",
            rcaspName = Some("Test-Only Exchange Ltd"),
            messageTypeIndic = "CARF701",
            hasOtherNexus = false,
            hasCryptoUsers = false,
            docTypeIndic = Some("OECD10"),
            isTestData = true,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file for a nil report" in {
        val path = "data/examples/nil-report.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-NO-USERS",
            sendingEntityIn = "SENDER-001",
            rcaspName = None,
            messageTypeIndic = "CARF703",
            hasOtherNexus = false,
            hasCryptoUsers = false,
            docTypeIndic = None,
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing a notification of reporting outside the UK" in {
        val path = "data/examples/reporting-outside-uk.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-OTHER-NEXUS",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Cross-Border Exchange Ltd"),
            messageTypeIndic = "CARF701",
            hasOtherNexus = true,
            hasCryptoUsers = false,
            docTypeIndic = Some("OECD1"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing new information" in {
        val path = "data/examples/new-info.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-NEW-INFO",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Production Typical Exchange Ltd"),
            messageTypeIndic = "CARF701",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD1"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing additional information for an existing report" in {
        val path = "data/examples/additional-info.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-ADDITIONAL-INFO",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Production Typical Exchange Ltd"),
            messageTypeIndic = "CARF701",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD0"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file for deletion of an existing report" in {
        val path = "data/examples/deleted-report.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-DELETE-REPORT",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Deletions Exchange Ltd"),
            messageTypeIndic = "CARF702",
            hasOtherNexus = false,
            hasCryptoUsers = false,
            docTypeIndic = Some("OECD3"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing corrected information for an existing report" in {
        val path = "data/examples/corrected-info.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-ALL-CORRECTIONS",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Corrections Exchange Ltd"),
            messageTypeIndic = "CARF702",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD2"),
            isTestData = false,
            allCryptoUsersAreCorrections = true,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing deleted information for an existing report" in {
        val path = "data/examples/deleted-info.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-ALL-DELETIONS",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("Deletions Exchange Ltd"),
            messageTypeIndic = "CARF702",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD0"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = true
          )
        )
      }

      "given an XML file containing corrected and deleted information for an existing report" in {
        val path = "data/examples/corrected-and-deleted-info.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-CORRECTIONS-AND-DELETIONS",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("John Smith"),
            messageTypeIndic = "CARF702",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD2"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = false
          )
        )
      }

      "given an XML file containing an unexpected docTypeIndic for messageTypeIndic CARF702 (reportable information fallback)" in {
        val path = "data/examples/fallback.xml"

        val result = service.validateAndExtract(path).value.futureValue

        result mustBe Right(
          ExtractedFileDetails(
            messageRefId = "MSG-FALLBACK",
            sendingEntityIn = "SENDER-001",
            rcaspName = Some("John Smith"),
            messageTypeIndic = "CARF702",
            hasOtherNexus = false,
            hasCryptoUsers = true,
            docTypeIndic = Some("OECD1"),
            isTestData = false,
            allCryptoUsersAreCorrections = false,
            allCryptoUsersAreDeletions = true
          )
        )
      }
    }

    "must return XmlErrors when the XML is well-formed but has invalid root" in {
      val path = "data/examples/invalid-xml.xml"

      val result = service.validateAndExtract(path).value.futureValue

      result match {
        case Left(e: XmlErrors) =>
          e.errors.length          mustBe 3
          e.errors.head.errorMessage must include("InvalidRoot")
        case _                  => fail()
      }
    }

    "must return XmlErrors when the XML is well-formed but fails schema validation (under 101 errors)" in {
      val path = "data/examples/invalid-carf.xml"

      val result = service.validateAndExtract(path).value.futureValue

      result match {
        case Left(e: XmlErrors) =>
          val errorMessages = e.errors.map(_.errorMessage)
          println(errorMessages.mkString(",\n"))
          e.errors.length  mustBe 4
          errorMessages.head must include("\"MessageTypeIndic\" is not allowed.")
          errorMessages(1)   must include("\"ReportingPeriod\" is not allowed.")
          errorMessages(2)   must include("\"Timestamp\" is not allowed.")
          errorMessages(3)   must include("uncompleted content model.")
        case _                  => fail()
      }
    }

    "must truncate errors and exit cleanly when schema errors exceed max errors of (101) and xml contains 150 errors" in {
      val path = "data/examples/too-many-schema-errors.xml"

      val result = service.validateAndExtract(path).value.futureValue

      result match {
        case Left(e: XmlErrors) =>
          e.errors.length            mustBe 101
          e.errors.map(_.errorMessage) must contain only "the value is not a member of the enumeration."
        case _                  => fail()
      }
    }

    "must return an InternalServerError when the XML is completely malformed (Fatal XML Stream Error)" in {
      val path   = "data/examples/malformed-xml.xml"
      val result = service.validateAndExtract(path).value.futureValue

      result match {
        case Left(e: InternalServerError) =>
          println(e.message)
          e.message must include("Unexpected EOF; was expecting a close tag for element <Root>")
        case _                            => fail()
      }
    }
  }
}
