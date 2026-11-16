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

package uk.gov.hmrc.carfreporting.services.submission

import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{never, reset, times, verify, verifyNoInteractions, when}
import uk.gov.hmrc.carfreporting.base.SpecBase
import uk.gov.hmrc.carfreporting.config.Constants.*
import uk.gov.hmrc.carfreporting.connectors.{EmailConnector, EmailNotSent, EmailSent}
import uk.gov.hmrc.carfreporting.models.{ExtractedCarfFileDetails, ReportType}
import uk.gov.hmrc.carfreporting.models.submission.*

import java.time.Instant
import scala.concurrent.Future

class EmailServiceSpec extends SpecBase {

  private val mockEmailConnector = mock[EmailConnector]
  private val service            = new EmailService(mockEmailConnector)

  private val statusTime       = Instant.parse("2027-03-01T11:14:00Z")
  private val expectedDateTime = "11:14am on 1 March 2027"

  private val userEmail          = "jane.smith@example.com"
  private val rcaspEmail         = "test@example.com"
  private val secondUserEmail    = "second.user@example.com"
  private val secondRcaspEmail   = "second.rcasp@example.com"
  private val secondUserOrgName  = "Second Org Ltd"
  private val secondRcaspContact = "secondRcaspContact"

  private val rcaspUser = OrganisationRcaspDetailsRcaspUser("ZMCAR0123456788", true, "Crypto Co")

  private val orgSubscription: DisplaySubscriptionDetails = displaySubscriptionDetailsOrg

  private val orgNoTradingName: DisplaySubscriptionDetails = displaySubscriptionDetailsOrg.copy(tradingName = None)

  private val indSubscription: DisplaySubscriptionDetails = displaySubscriptionDetailsInd

  private def submission(
      subscription: DisplaySubscriptionDetails = orgSubscription,
      rcasp: RcaspDetails = individualRcaspDetails,
      details: ExtractedCarfFileDetails = extractedFileDetailsCarf
  ): SubmissionDetailsCache =
    testSubmissionDetailsCache.copy(
      subscriptionDetails = subscription,
      rcaspDetails = rcasp,
      extractedFileDetails = details,
      lastStatusUpdateTime = statusTime
    )

  private val nilReportDetails = extractedFileDetailsCarf.copy(
    messageTypeIndic = nilReportMessageTypeIndic,
    rcaspName = None,
    docTypeIndic = None,
    reportType = None
  )

  private val passedCommon = Map(
    "dateTime"   -> expectedDateTime,
    "fileId"     -> "MSG-2024-0001",
    "rcaspId"    -> "SENDER-001",
    "rcaspName"  -> "Acme Crypto Exchange Ltd",
    "reportType" -> ReportType.NewInformation.description
  )

  private val failedCommon = Map(
    "dateTime"   -> expectedDateTime,
    "fileId"     -> "MSG-2024-0001",
    "rcaspId"    -> "SENDER-001",
    "rcaspLabel" -> "RCASP (RCASP Name)",
    "rcaspName"  -> "Acme Crypto Exchange Ltd"
  )

  private val secondUserContact =
    DisplaySubscriptionContact(None, Some(DisplaySubscriptionOrganisation(secondUserOrgName)), secondUserEmail)

  private def withBusinessName(params: Map[String, String], businessName: Option[String]): Map[String, String] =
    businessName.fold(params)(name => params + ("userBusinessName" -> name))

  private def passedUserParams(contactName: String): Map[String, String] =
    passedCommon + ("contactName" -> contactName)

  private def passedRcaspParams(
      contactName: String,
      businessName: Option[String] = Some(testTradingName)
  ): Map[String, String] =
    withBusinessName(passedCommon + ("contactName" -> contactName), businessName)

  private def failedParams(
      contactName: String,
      businessName: Option[String] = Some(testTradingName)
  ): Map[String, String] =
    withBusinessName(failedCommon + ("contactName" -> contactName), businessName)

  override def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockEmailConnector)
    when(mockEmailConnector.sendEmail(any(), any(), any())(any(), any()))
      .thenReturn(Future.successful(EmailSent))
  }

  private def verifySent(address: String, template: String, params: Map[String, String]): Unit =
    verify(mockEmailConnector, times(1)).sendEmail(eqTo(address), eqTo(template), eqTo(params))(any(), any())

  private def verifyTotalSent(count: Int): Unit =
    verify(mockEmailConnector, times(count)).sendEmail(any(), any(), any())(any(), any())

  "EmailService" - {

    "sendFilePassedEmails" - {

      "must email the user and the RCASP contact when the user is not the RCASP" in {
        service.sendFilePassedEmails(submission()).futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams(testOrgName))
        verifySent(rcaspEmail, carfFilePassedRcaspTemplateId, passedRcaspParams("testContactName"))
        verifyTotalSent(2)
      }

      "must email every user contact and every RCASP contact" in {
        val subscription = orgSubscription.copy(secondaryContact = Some(secondUserContact))
        val rcasp        = organisationRcaspDetails.copy(
          SecondaryContactDetails = Some(RcaspContactDetails(secondRcaspContact, secondRcaspEmail))
        )

        service.sendFilePassedEmails(submission(subscription = subscription, rcasp = rcasp)).futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams(testOrgName))
        verifySent(secondUserEmail, carfFilePassedUserTemplateId, passedUserParams(secondUserOrgName))
        verifySent(rcaspEmail, carfFilePassedRcaspTemplateId, passedRcaspParams("testContactName"))
        verifySent(secondRcaspEmail, carfFilePassedRcaspTemplateId, passedRcaspParams(secondRcaspContact))
        verifyTotalSent(4)
      }

      "must email only the user when the user is the RCASP" in {
        service.sendFilePassedEmails(submission(rcasp = rcaspUser)).futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams(testOrgName))
        verify(mockEmailConnector, never())
          .sendEmail(any(), eqTo(carfFilePassedRcaspTemplateId), any())(any(), any())
        verifyTotalSent(1)
      }

      "must email only the user when an individual RCASP is also the user" in {
        service
          .sendFilePassedEmails(submission(rcasp = individualRcaspDetails.copy(IsRCASPUser = true)))
          .futureValue

        verifyTotalSent(1)
        verify(mockEmailConnector, never()).sendEmail(eqTo(rcaspEmail), any(), any())(any(), any())
      }

      "must greet an individual user by their full name" in {
        service
          .sendFilePassedEmails(submission(subscription = indSubscription, rcasp = rcaspUser))
          .futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams("Jane Smith"))
      }

      "must use the trading name as the user business name in the RCASP email" in {
        service.sendFilePassedEmails(submission()).futureValue

        verifySent(
          rcaspEmail,
          carfFilePassedRcaspTemplateId,
          passedRcaspParams("testContactName", Some(testTradingName))
        )
      }

      "must not use the primary contact's organisation name as the user business name" in {
        service.sendFilePassedEmails(submission()).futureValue

        verify(mockEmailConnector, never()).sendEmail(
          any(),
          eqTo(carfFilePassedRcaspTemplateId),
          eqTo(passedRcaspParams("testContactName", Some(testOrgName)))
        )(any(), any())
      }

      "must use the individual's full name as the user business name when there is no trading name" in {
        service.sendFilePassedEmails(submission(subscription = indSubscription)).futureValue

        verifySent(
          rcaspEmail,
          carfFilePassedRcaspTemplateId,
          passedRcaspParams("testContactName", Some("Jane Smith"))
        )
      }

      "must use the trading name over the individual's full name when both are present" in {
        val subscription = indSubscription.copy(tradingName = Some(testTradingName))

        service.sendFilePassedEmails(submission(subscription = subscription)).futureValue

        verifySent(
          rcaspEmail,
          carfFilePassedRcaspTemplateId,
          passedRcaspParams("testContactName", Some(testTradingName))
        )
      }

      "must treat a blank trading name as missing and use the individual's full name" in {
        val subscription = indSubscription.copy(tradingName = Some("   "))

        service.sendFilePassedEmails(submission(subscription = subscription)).futureValue

        verifySent(
          rcaspEmail,
          carfFilePassedRcaspTemplateId,
          passedRcaspParams("testContactName", Some("Jane Smith"))
        )
      }

      "must still email the RCASP, without a user business name, for an organisation with no trading name" in {
        service.sendFilePassedEmails(submission(subscription = orgNoTradingName)).futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams(testOrgName))
        verifySent(rcaspEmail, carfFilePassedRcaspTemplateId, passedRcaspParams("testContactName", None))
        verifyTotalSent(2)
      }

      "must still email the RCASP, without a user business name, for an organisation with a blank trading name" in {
        val subscription = orgNoTradingName.copy(tradingName = Some(""))

        service.sendFilePassedEmails(submission(subscription = subscription)).futureValue

        verifySent(rcaspEmail, carfFilePassedRcaspTemplateId, passedRcaspParams("testContactName", None))
      }

      "must use the reportType already stored on the file details when there is one" in {
        val details = extractedFileDetailsCarf.copy(reportType = Some(ReportType.DeletionOfExistingReport))

        service.sendFilePassedEmails(submission(rcasp = rcaspUser, details = details)).futureValue

        verifySent(
          userEmail,
          carfFilePassedUserTemplateId,
          passedUserParams(testOrgName) + ("reportType" -> "Deletion of the whole of an existing report")
        )
      }

      "must work out the reportType from the file details when none is stored" in {
        val details = extractedFileDetailsCarf.copy(reportType = None)

        service.sendFilePassedEmails(submission(rcasp = rcaspUser, details = details)).futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams(testOrgName))
      }

      "must use the RCASP name from management data for a nil report (individual RCASP)" in {
        service.sendFilePassedEmails(submission(details = nilReportDetails)).futureValue

        verifySent(
          userEmail,
          carfFilePassedUserTemplateId,
          passedUserParams(testOrgName) ++ Map(
            "rcaspName"  -> "testFirstName testLastName",
            "reportType" -> ReportType.NilReport.description
          )
        )
      }

      "must use the RCASP name from management data for a nil report (organisation RCASP)" in {
        service
          .sendFilePassedEmails(submission(rcasp = organisationRcaspDetails, details = nilReportDetails))
          .futureValue

        verifySent(
          userEmail,
          carfFilePassedUserTemplateId,
          passedUserParams(testOrgName) ++ Map(
            "rcaspName"  -> "testFirstName",
            "reportType" -> ReportType.NilReport.description
          )
        )
      }

      "must send no email when no user contact has a name and the user is the RCASP" in {
        val subscription =
          orgSubscription.copy(primaryContact = DisplaySubscriptionContact(None, None, userEmail))

        service.sendFilePassedEmails(submission(subscription = subscription, rcasp = rcaspUser)).futureValue

        verifyNoInteractions(mockEmailConnector)
      }

      "must skip a user contact with no name but still email the other contact" in {
        val noNameContact = DisplaySubscriptionContact(None, None, "noname@example.com")
        val subscription  = orgSubscription.copy(secondaryContact = Some(noNameContact))

        service.sendFilePassedEmails(submission(subscription = subscription, rcasp = rcaspUser)).futureValue

        verifySent(userEmail, carfFilePassedUserTemplateId, passedUserParams(testOrgName))
        verify(mockEmailConnector, never()).sendEmail(eqTo("noname@example.com"), any(), any())(any(), any())
        verifyTotalSent(1)
      }

      "must still send the other emails when one fails to send" in {
        when(mockEmailConnector.sendEmail(eqTo(userEmail), any(), any())(any(), any()))
          .thenReturn(Future.successful(EmailNotSent))

        service.sendFilePassedEmails(submission()).futureValue

        verifySent(rcaspEmail, carfFilePassedRcaspTemplateId, passedRcaspParams("testContactName"))
        verifyTotalSent(2)
      }

      "must complete successfully when every email fails to send" in {
        when(mockEmailConnector.sendEmail(any(), any(), any())(any(), any()))
          .thenReturn(Future.successful(EmailNotSent))

        service.sendFilePassedEmails(submission()).futureValue mustBe ((): Unit)
      }
    }

    "sendFileFailedEmails" - {

      "must email the user with the failed template, using the trading name as the user business name" in {
        service.sendFileFailedEmails(submission()).futureValue

        verifySent(userEmail, carfFileFailedUserTemplateId, failedParams(testOrgName, Some(testTradingName)))
        verifyTotalSent(1)
      }

      "must email every user contact" in {
        val subscription = orgSubscription.copy(secondaryContact = Some(secondUserContact))

        service.sendFileFailedEmails(submission(subscription = subscription)).futureValue

        verifySent(userEmail, carfFileFailedUserTemplateId, failedParams(testOrgName))
        verifySent(secondUserEmail, carfFileFailedUserTemplateId, failedParams(secondUserOrgName))
        verifyTotalSent(2)
      }

      "must not email the RCASP, even when the user is not the RCASP" in {
        service.sendFileFailedEmails(submission()).futureValue

        verify(mockEmailConnector, never()).sendEmail(eqTo(rcaspEmail), any(), any())(any(), any())
        verify(mockEmailConnector, never())
          .sendEmail(any(), eqTo(carfFilePassedRcaspTemplateId), any())(any(), any())
      }

      "must not use the primary contact's organisation name as the user business name" in {
        service.sendFileFailedEmails(submission()).futureValue

        verify(mockEmailConnector, never()).sendEmail(
          any(),
          eqTo(carfFileFailedUserTemplateId),
          eqTo(failedParams(testOrgName, Some(testOrgName)))
        )(any(), any())
      }

      "must use the individual's full name as the user business name when there is no trading name" in {
        service.sendFileFailedEmails(submission(subscription = indSubscription)).futureValue

        verifySent(userEmail, carfFileFailedUserTemplateId, failedParams("Jane Smith", Some("Jane Smith")))
      }

      "must use the trading name over the individual's full name when both are present" in {
        val subscription = indSubscription.copy(tradingName = Some(testTradingName))

        service.sendFileFailedEmails(submission(subscription = subscription)).futureValue

        verifySent(userEmail, carfFileFailedUserTemplateId, failedParams("Jane Smith", Some(testTradingName)))
      }

      "must still email the user, without a user business name, for an organisation with no trading name" in {
        service.sendFileFailedEmails(submission(subscription = orgNoTradingName)).futureValue

        verifySent(userEmail, carfFileFailedUserTemplateId, failedParams(testOrgName, None))
        verifyTotalSent(1)
      }

      "must treat a blank trading name as missing for an organisation" in {
        val subscription = orgNoTradingName.copy(tradingName = Some("  "))

        service.sendFileFailedEmails(submission(subscription = subscription)).futureValue

        verifySent(userEmail, carfFileFailedUserTemplateId, failedParams(testOrgName, None))
      }

      "must use the label RCASP and the management data name for a nil report" in {
        service.sendFileFailedEmails(submission(details = nilReportDetails)).futureValue

        verifySent(
          userEmail,
          carfFileFailedUserTemplateId,
          failedParams(testOrgName) ++ Map(
            "rcaspLabel" -> "RCASP",
            "rcaspName"  -> "testFirstName testLastName"
          )
        )
      }

      "must send no email when no user contact has a name" in {
        val subscription =
          orgSubscription.copy(primaryContact = DisplaySubscriptionContact(None, None, userEmail))

        service.sendFileFailedEmails(submission(subscription = subscription)).futureValue

        verifyNoInteractions(mockEmailConnector)
      }

      "must complete successfully when the email fails to send" in {
        when(mockEmailConnector.sendEmail(any(), any(), any())(any(), any()))
          .thenReturn(Future.successful(EmailNotSent))

        service.sendFileFailedEmails(submission()).futureValue mustBe ((): Unit)
      }
    }
  }
}
