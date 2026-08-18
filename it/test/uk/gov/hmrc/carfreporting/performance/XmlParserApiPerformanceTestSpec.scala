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

package uk.gov.hmrc.carfreporting.performance

import play.api.libs.json.Json
import uk.gov.hmrc.carfreporting.base.NoGuiceSpecBase
import uk.gov.hmrc.carfreporting.controllers.XmlValidationAndExtractionController
import uk.gov.hmrc.carfreporting.dispatchers.{DispatcherName, XmlDispatcher}
import uk.gov.hmrc.carfreporting.itutil.Reporter.*
import uk.gov.hmrc.carfreporting.services.XmlParserService

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class XmlParserApiPerformanceTestSpec extends NoGuiceSpecBase {

  override def beforeEach(): Unit = {
    println("Cleaning")
    System.gc()
  }

  inline val validCarfXmlSizeOnDisk   = 4000L // 4kb (size on disk)
  inline val invalidCarfXmlSizeOnDisk = 4000L
  val twoFiftyMbInBytes               = 274726912L // 262mb (size on disk)
  inline val bigInvalidXmlSizeOnDisk  = 12000L // 12kb (size on disk)

  private val timeout = 30.seconds

  override def beforeAll(): Unit = {
    println("Warming up")
    val smallDispatcher = new DispatcherName {
      val name: String = "small-xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    val testController: XmlValidationAndExtractionController = new XmlValidationAndExtractionController(cc, service)

    val validCarfXmlSizeOnDisk = 4000L

    lazy val calls = Vector( // keep lazy Futures are eager
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, testController),
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, testController)
    )

    val _ = Await.result(Future.sequence(calls), timeout)
    println("Warm up complete")
  }

  "XmlParser Api (small thread pool)" - {
    val smallDispatcher = new DispatcherName {
      val name: String = "small-xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    val testController: XmlValidationAndExtractionController = new XmlValidationAndExtractionController(cc, service)

    "must handle a small batch of small valid and invalid XML files (4kb)" in {
      smallBatchOfSmall(testController)
    }

    "must handle a large batch of small valid and invalid XML files(4kb)" in {
      largeBatchOfSmall(testController)
    }

    "must handle a small batch of large valid and invalid XML files (262MB) & (12kb)" in {
      smallBatchOfLarge(testController)
    }

    "must handle a large batch of large valid and invalid XML files (250mb) & (12kb)" in {
      largeBatchOfLarge(testController)
    }
  }

  "XmlParser Api (medium thread pool)" - {
    val smallDispatcher = new DispatcherName {
      val name: String = "xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    val testController: XmlValidationAndExtractionController = new XmlValidationAndExtractionController(cc, service)

    "must handle a small batch of small valid and invalid XML files (4kb)" in {
      smallBatchOfSmall(testController)
    }

    "must handle a large batch of small valid and invalid XML files(4kb)" in {
      largeBatchOfSmall(testController)
    }

    "must handle a small batch of large valid and invalid XML files (262MB) & (12kb)" in {
      smallBatchOfLarge(testController)
    }

    "must handle a large batch of large valid and invalid XML files (250mb) & (12kb)" in {
      largeBatchOfLarge(testController)
    }
  }

  "XmlParser Api (large thread pool)" - {
    val smallDispatcher = new DispatcherName {
      val name: String = "large-xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    val testController: XmlValidationAndExtractionController = new XmlValidationAndExtractionController(cc, service)

    "must handle a small batch of small valid and invalid XML files (4kb)" in {
      smallBatchOfSmall(testController)
    }

    "must handle a large batch of small valid and invalid XML files(4kb)" in {
      largeBatchOfSmall(testController)
    }

    "must handle a small batch of large valid and invalid XML files (262MB) & (12kb)" in {
      smallBatchOfLarge(testController)
    }

    "must handle a large batch of large valid and invalid XML files (250mb) & (12kb)" in {
      largeBatchOfLarge(testController)
    }
  }

  private def usedMemory(rt: Runtime): Long = rt.totalMemory() - rt.freeMemory()

  private def smallBatchOfSmall(controller: XmlValidationAndExtractionController) = {
    lazy val calls = Vector( // keep lazy Futures are eager
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, controller),
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, controller),
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, controller),
      createAndMeasureExecution("data/examples/invalid-carf.xml", invalidCarfXmlSizeOnDisk, controller),
      createAndMeasureExecution("data/examples/invalid-carf.xml", invalidCarfXmlSizeOnDisk, controller)
    )

    val runtime = Runtime.getRuntime

    val memBefore = usedMemory(runtime)
    val started   = System.nanoTime()

    val results = Await.result(Future.sequence(calls), timeout)

    val elapsedNs = System.nanoTime() - started
    val memAfter  = usedMemory(runtime)

    val totalBytes = results.map(_.sizeInBytes).sum

    reportResult(results, elapsedNs, totalBytes, memBefore, memAfter)
    results.size mustBe 5
  }

  private def largeBatchOfSmall(controller: XmlValidationAndExtractionController) = {
    lazy val validFiles =
      (1 to 25).map(_ => createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, controller))

    lazy val invalidFiles = (1 to 25).map(_ =>
      createAndMeasureExecution("data/examples/invalid-carf.xml", invalidCarfXmlSizeOnDisk, controller)
    )

    lazy val calls = (validFiles ++ invalidFiles).toVector

    val runtime = Runtime.getRuntime

    val memBefore = usedMemory(runtime)
    val started   = System.nanoTime()

    val results = Await.result(Future.sequence(calls), timeout)

    val elapsedNs = System.nanoTime() - started
    val memAfter  = usedMemory(runtime)

    val totalBytes = results.map(_.sizeInBytes).sum

    reportResult(results, elapsedNs, totalBytes, memBefore, memAfter)
    results.size mustBe 50
  }

  private def smallBatchOfLarge(controller: XmlValidationAndExtractionController) = {
    lazy val calls = Vector(
      createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, controller),
      createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, controller),
      createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, controller),
      createAndMeasureExecution("data/examples/too-many-schema-errors.xml", bigInvalidXmlSizeOnDisk, controller),
      createAndMeasureExecution("data/examples/too-many-schema-errors.xml", bigInvalidXmlSizeOnDisk, controller)
    )

    val runtime = Runtime.getRuntime

    val memBefore = usedMemory(runtime)
    val started   = System.nanoTime()

    val results = Await.result(Future.sequence(calls), timeout)

    val elapsedNs = System.nanoTime() - started
    val memAfter  = usedMemory(runtime)

    val totalBytes = results.map(_.sizeInBytes).sum

    reportResult(results, elapsedNs, totalBytes, memBefore, memAfter)
    results.size mustBe 5
  }

  private def largeBatchOfLarge(controller: XmlValidationAndExtractionController) = {
    lazy val validFiles =
      (1 to 25).map(_ => createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, controller))

    lazy val invalidFiles = (1 to 25).map(_ =>
      createAndMeasureExecution("data/examples/too-many-schema-errors.xml", bigInvalidXmlSizeOnDisk, controller)
    )

    lazy val calls = (validFiles ++ invalidFiles).toVector

    val runtime = Runtime.getRuntime

    val memBefore = usedMemory(runtime)
    val started   = System.nanoTime()

    val results = Await.result(Future.sequence(calls), timeout)

    val elapsedNs = System.nanoTime() - started
    val memAfter  = usedMemory(runtime)

    val totalBytes = results.map(_.sizeInBytes).sum

    reportResult(results, elapsedNs, totalBytes, memBefore, memAfter)
    results.size mustBe 50
  }

  private def createAndMeasureExecution(
      path: String,
      fileSizeInBytes: Long,
      controller: => XmlValidationAndExtractionController
  ) =
    val requestBody = Json.parse(
      s"""
         |{
         |  "path": "$path"
         |}
         |""".stripMargin
    )

    Future {
      val startTime = System.nanoTime()
      controller.processXml(fakeRequestWithJsonBody(requestBody)) map { _ =>
        FileInfo(fileSizeInBytes, System.nanoTime() - startTime)
      }
    }.flatten
}
