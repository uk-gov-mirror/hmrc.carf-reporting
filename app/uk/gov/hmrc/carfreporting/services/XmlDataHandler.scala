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

import cats.data.NonEmptyChain
import com.ctc.wstx.stax.WstxInputFactory
import org.codehaus.stax2.XMLStreamReader2
import org.codehaus.stax2.validation.{XMLValidationProblem, XMLValidationSchema}
import play.api.Logging
import uk.gov.hmrc.carfreporting.config.Constants.*
import uk.gov.hmrc.carfreporting.models.ExtractedFileDetails
import uk.gov.hmrc.carfreporting.models.errors.*

import java.io.InputStream
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class XmlDataHandler extends Logging {

  inline private val maxErrors = 101

  private val errors = ListBuffer.empty[XmlError]

  private val path = ListBuffer.empty[String]

  private case class CarfBodyRcaspName(
      var firstName: String = "",
      var lastName: String = "",
      var entityName: String = ""
  ) {
    def individualName: String = s"$firstName $lastName"
  }

  private val carfBodyRcaspName = CarfBodyRcaspName()

  private var sendingEntityIn: String  = ""
  private var messageType: String      = ""
  private var messageRefId: String     = ""
  private var messageTypeIndic: String = ""

  private var rcaspName: Option[String]         = None
  private var rcaspDocTypeIndic: Option[String] = None

  private val cryptoUserDocTypeIndics = ListBuffer.empty[String]

  private var hasOtherNexus: Boolean  = false
  private var hasCryptoUsers: Boolean = false

  private var carfBodyCompleted: Boolean = false

  private def currentPath: List[String] = path.reverse.toList

  private def pathEndsWith(expected: String*): Boolean = currentPath.startsWith(expected.toList)

  def validationAndExtraction(
      schema: XMLValidationSchema,
      inputStream: InputStream
  ): Either[CarfError, ExtractedFileDetails] = {
    val factory = new WstxInputFactory()

    // Security hardening: block XML External Entity (XXE) attacks.
    factory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)

    val reader: XMLStreamReader2 = factory.createXMLStreamReader(inputStream).asInstanceOf[XMLStreamReader2]

    reader.validateAgainst(schema)

    reader.setValidationProblemHandler { (problem: XMLValidationProblem) =>
      val loc  = problem.getLocation
      val line = if (loc != null) loc.getLineNumber else 0
      if (errors.size < maxErrors) {
        errors += XmlError(line, problem.getType, problem.getMessage)
      } else {
        logger.warn(s"Truncated: more than $maxErrors schema errors in this file; further errors dropped.")
        throw new XmlStreamFailSafeException
      }
    }

    def readElement(): String = {
      val value = reader.getElementText.trim
      if (path.nonEmpty) path.remove(path.size - 1)
      value
    }

    Try {
      while (reader.hasNext)
        reader.next() match {
          case XMLStreamConstants.START_ELEMENT if !carfBodyCompleted =>
            val localName = reader.getLocalName
            path += localName

            localName match {
              // MessageSpec data
              case "SendingEntityIN" if pathEndsWith("SendingEntityIN", "MessageSpec")   =>
                sendingEntityIn = readElement()
              case "MessageType" if pathEndsWith("MessageType", "MessageSpec")           =>
                messageType = readElement()
              case "MessageRefId" if pathEndsWith("MessageRefId", "MessageSpec")         =>
                messageRefId = readElement()
              case "MessageTypeIndic" if pathEndsWith("MessageTypeIndic", "MessageSpec") =>
                messageTypeIndic = readElement()

              case "DocTypeIndic" if pathEndsWith("DocTypeIndic", "DocSpec", "RCASP", "CARFBody")                  =>
                rcaspDocTypeIndic = Some(readElement())
              case "DocTypeIndic" if pathEndsWith("DocTypeIndic", "DocSpec", "CryptoUsers", "CARFBody")            =>
                cryptoUserDocTypeIndics += readElement()

              // RCASP name
              case "FirstName" if pathEndsWith("FirstName", "Name", "Individual", "RCASP_ID", "RCASP", "CARFBody") =>
                carfBodyRcaspName.firstName = readElement()
              case "LastName" if pathEndsWith("LastName", "Name", "Individual", "RCASP_ID", "RCASP", "CARFBody")   =>
                carfBodyRcaspName.lastName = readElement()
              case "Name" if pathEndsWith("Name", "Entity", "RCASP_ID", "RCASP", "CARFBody")                       =>
                carfBodyRcaspName.entityName = readElement()

              case "CryptoUsers" if pathEndsWith("CryptoUsers", "CARFBody")        =>
                hasCryptoUsers = true
              case "OtherNexus" if pathEndsWith("OtherNexus", "RCASP", "CARFBody") =>
                hasOtherNexus = true

              case _ => // Nothing
            }

          case XMLStreamConstants.END_ELEMENT if !carfBodyCompleted =>
            val localName = reader.getLocalName
            if (localName == "CARFBody") {
              val name =
                if (carfBodyRcaspName.entityName.nonEmpty) carfBodyRcaspName.entityName
                else carfBodyRcaspName.individualName
              rcaspName = Some(name)
              carfBodyCompleted = true
            }
            if (path.nonEmpty) path.remove(path.size - 1)

          case _ => // Nothing
        }
    } match {
      case Success(value)                         =>
        reader.close()
        resolveErrors(errors)
      case Failure(e: XmlStreamFailSafeException) =>
        reader.close()
        resolveErrors(errors)
      case Failure(e)                             =>
        reader.close()
        Left(InternalServerError(e.getMessage))
    }
  }

  private def resolveErrors(errors: ListBuffer[XmlError]): Either[CarfError, ExtractedFileDetails] =
    NonEmptyChain.fromSeq(errors.toSeq) match {
      case Some(nec) => Left(XmlErrors(nec.toNonEmptyVector.toVector))
      case None      =>
        Right(
          ExtractedFileDetails(
            messageRefId = messageRefId,
            sendingEntityIn = if sendingEntityIn.isEmpty then "missing" else sendingEntityIn,
            rcaspName = if messageTypeIndic == nilReportMessageTypeIndic then None else rcaspName,
            messageTypeIndic = messageTypeIndic,
            hasOtherNexus = hasOtherNexus,
            hasCryptoUsers = hasCryptoUsers,
            docTypeIndic = rcaspDocTypeIndic,
            isTestData = {
              val docTypeIndics = rcaspDocTypeIndic.fold(List.empty)(List(_)) ++ cryptoUserDocTypeIndics
              docTypeIndics.exists(docTypeIndic => testDataDocTypeIndics.contains(docTypeIndic))
            },
            allCryptoUsersAreCorrections =
              cryptoUserDocTypeIndics.nonEmpty && cryptoUserDocTypeIndics.forall(_ == correctionDocTypeIndic),
            allCryptoUsersAreDeletions =
              cryptoUserDocTypeIndics.nonEmpty && cryptoUserDocTypeIndics.forall(_ == deletionDocTypeIndic)
          )
        )
    }
}
