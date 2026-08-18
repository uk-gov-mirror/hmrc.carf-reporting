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

import org.codehaus.stax2.validation.*
import play.api.{Environment, Logging}
import uk.gov.hmrc.carfreporting.dispatchers.XmlDispatcher
import uk.gov.hmrc.carfreporting.models.ExtractedFileDetails
import uk.gov.hmrc.carfreporting.models.errors.*
import uk.gov.hmrc.carfreporting.types.ResultT

import java.io.{BufferedInputStream, InputStream}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

@Singleton
class XmlParserService @Inject() (env: Environment)(implicit xmlDispatcher: XmlDispatcher) extends Logging {

  def validateAndExtract(path: String): ResultT[ExtractedFileDetails] =
    for {
      schema      <- loadSchema
      inputStream <- openInputStream(path)
      result      <- initiate(schema, inputStream)
    } yield result

  private def initiate(schema: XMLValidationSchema, inputStream: InputStream): ResultT[ExtractedFileDetails] =
    ResultT.fromFuture {
      Future {
        val handler = new XmlDataHandler
        handler.validationAndExtraction(schema, inputStream)
      } andThen { _ =>
        inputStream.close()
      } recover { e =>
        logger.error(s"Fatal Error XML Stream Failed with message: ${e.getMessage}")
        Left(InternalServerError(e.getMessage))
      }
    }

  private def openInputStream(path: String): ResultT[InputStream] =
    Try {
      env.resource(path).map { url =>
        new java.io.BufferedInputStream(url.openStream())
      }
    } match {
      case Success(Some(in)) => ResultT.fromValue(in)
      case Success(None)     =>
        ResultT.fromError(
          XmlErrors(
            Vector(
              XmlError(
                0,
                "file_not_found",
                "File cannot be found with path provided"
              )
            )
          )
        )
      case Failure(e)        =>
        ResultT.fromError(
          XmlErrors(
            Vector(
              XmlError(
                0,
                "unexpected_error",
                s"Unexpected error when creating Buffered Input Stream with message: ${e.getMessage}"
              )
            )
          )
        )
    }

  private def loadSchema: ResultT[XMLValidationSchema] = {
    val defaultSchemaPath = "data/schemas/CARFXML_v1.5.xsd"

    Try {
      val schemaFactory = XMLValidationSchemaFactory
        .newInstance(XMLValidationSchema.SCHEMA_ID_W3C_SCHEMA)

      env.resource(defaultSchemaPath).map { url =>
        schemaFactory.createSchema(url)
      }
    } match {
      case Success(Some(validationSchema)) => ResultT.fromValue(validationSchema)
      case Success(None)                   =>
        resolveError(
          "file_not_found",
          s"Schema file cannot be found"
        ) // Would have to delete schema for test coverage but would rely on test to be guaranteed last, not recommended
      case Failure(e)                      =>
        resolveError(
          "unexpected_error",
          s"Unexpected error when creating Buffered Input Stream with message: ${e.getMessage}"
        )
    }
  }

  private def resolveError(code: String, message: String): ResultT[XMLValidationSchema] =
    ResultT.fromError[XMLValidationSchema](XmlErrors(Vector(XmlError(0, code, message))))
}
