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

import uk.gov.hmrc.carfreporting.config.Constants.*
import uk.gov.hmrc.carfreporting.connectors.{EmailConnector, EmailSent, EmailStatus}
import uk.gov.hmrc.carfreporting.models.{getReportType, ExtractedCarfFileDetails}
import uk.gov.hmrc.carfreporting.models.submission.*
import uk.gov.hmrc.carfreporting.utils.EmailDateTimeFormatter
import uk.gov.hmrc.carfreporting.utils.LoggerUtil.*
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EmailService @Inject() (
    emailConnector: EmailConnector
)(implicit ec: ExecutionContext) {

  private case class Recipient(name: String, email: String)

  def sendFilePassedEmails(submission: SubmissionDetailsCache)(implicit hc: HeaderCarrier): Future[Unit] = {
    val details = submission.extractedFileDetails
    val common  = Map(
      "dateTime"   -> EmailDateTimeFormatter.format(submission.lastStatusUpdateTime),
      "fileId"     -> details.messageRefId,
      "rcaspId"    -> details.sendingEntityIn,
      "rcaspName"  -> rcaspName(submission),
      "reportType" -> details.reportType.getOrElse(details.getReportType).description
    )

    val userSends = userRecipients(submission).map { r =>
      emailConnector.sendEmail(r.email, carfFilePassedUserTemplateId, common + ("contactName" -> r.name))
    }

    val rcaspSends =
      if (submission.rcaspDetails.IsRCASPUser) {
        logInfo("[EmailService][sendFilePassedEmails] User is the RCASP, skipping RCASP email")
        List.empty[Future[EmailStatus]]
      } else {
        val rcaspParams = withUserBusinessName("sendFilePassedEmails", submission, common)
        rcaspRecipients(submission).map { r =>
          emailConnector.sendEmail(r.email, carfFilePassedRcaspTemplateId, rcaspParams + ("contactName" -> r.name))
        }
      }

    Future.sequence(userSends ++ rcaspSends).map(logOutcome("sendFilePassedEmails"))
  }

  def sendFileFailedEmails(submission: SubmissionDetailsCache)(implicit hc: HeaderCarrier): Future[Unit] = {
    val details = submission.extractedFileDetails
    val base    = Map(
      "dateTime"   -> EmailDateTimeFormatter.format(submission.lastStatusUpdateTime),
      "fileId"     -> details.messageRefId,
      "rcaspId"    -> details.sendingEntityIn,
      "rcaspLabel" -> rcaspLabel(details),
      "rcaspName"  -> rcaspName(submission)
    )
    val common  = withUserBusinessName("sendFileFailedEmails", submission, base)

    val sends = userRecipients(submission).map { r =>
      emailConnector.sendEmail(r.email, carfFileFailedUserTemplateId, common + ("contactName" -> r.name))
    }

    Future.sequence(sends).map(logOutcome("sendFileFailedEmails"))
  }

  private def userRecipients(submission: SubmissionDetailsCache): List[Recipient] =
    (submission.subscriptionDetails.primaryContact :: submission.subscriptionDetails.secondaryContact.toList)
      .flatMap { c =>
        subscriptionContactName(c) match {
          case Some(name) => Some(Recipient(name, c.email))
          case None       =>
            logWarn("[EmailService][userRecipients] Contact has no name, skipping email")
            None
        }
      }

  private def rcaspRecipients(submission: SubmissionDetailsCache): List[Recipient] =
    submission.rcaspDetails match {
      case i: IndividualRcaspDetails            => List(toRecipient(i.PrimaryContactDetails))
      case _: OrganisationRcaspDetailsRcaspUser => List.empty
      case s: OrganisationRcaspDetailsStandard  =>
        (s.PrimaryContactDetails :: s.SecondaryContactDetails.toList).map(toRecipient)
    }

  private def toRecipient(c: RcaspContactDetails): Recipient = Recipient(c.ContactName, c.EmailAddress)

  private def subscriptionContactName(c: DisplaySubscriptionContact): Option[String] =
    (c.individual, c.organisation) match {
      case (Some(individual), None)   => Some(individual.fullName)
      case (None, Some(organisation)) => Some(organisation.name)
      case _                          => None
    }

  private def userBusinessName(submission: SubmissionDetailsCache): Option[String] = {
    val details = submission.subscriptionDetails
    details.tradingName
      .map(_.trim)
      .filter(_.nonEmpty)
      .orElse(details.primaryContact.individual.map(_.fullName))
  }

  private def withUserBusinessName(
      context: String,
      submission: SubmissionDetailsCache,
      params: Map[String, String]
  ): Map[String, String] =
    userBusinessName(submission) match {
      case Some(name) => params + ("userBusinessName" -> name)
      case None       =>
        logInfo(s"[EmailService][$context] User business name not known, using fallback content")
        params
    }

  private def rcaspName(submission: SubmissionDetailsCache): String = {
    val details = submission.extractedFileDetails
    if (isNilReport(details)) managementRcaspName(submission)
    else details.rcaspName.getOrElse(managementRcaspName(submission))
  }

  private def managementRcaspName(submission: SubmissionDetailsCache): String =
    submission.rcaspDetails match {
      case i: IndividualRcaspDetails   => s"${i.FirstName} ${i.LastName}"
      case o: OrganisationRcaspDetails => o.RCASPName
    }

  private def isNilReport(details: ExtractedCarfFileDetails): Boolean =
    details.messageTypeIndic == nilReportMessageTypeIndic

  private def rcaspLabel(details: ExtractedCarfFileDetails): String =
    if (isNilReport(details)) "RCASP" else "RCASP (RCASP Name)"

  private def logOutcome(context: String)(statuses: List[EmailStatus]): Unit = {
    val sent   = statuses.count(_ == EmailSent)
    val failed = statuses.length - sent
    if (failed > 0) logWarn(s"[EmailService][$context] Failed to send $failed of ${statuses.length} email(s)")
    logInfo(s"[EmailService][$context] Sent $sent of ${statuses.length} email(s)")
  }
}
