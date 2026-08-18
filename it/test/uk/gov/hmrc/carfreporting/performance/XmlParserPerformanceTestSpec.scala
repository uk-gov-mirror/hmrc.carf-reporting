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

import uk.gov.hmrc.carfreporting.base.NoGuiceSpecBase
import uk.gov.hmrc.carfreporting.dispatchers.{DispatcherName, XmlDispatcher}
import uk.gov.hmrc.carfreporting.services.XmlParserService

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class XmlParserPerformanceTestSpec extends NoGuiceSpecBase {

  import uk.gov.hmrc.carfreporting.itutil.Reporter.*

  override def beforeEach(): Unit = {
    println("Cleaning")
    System.gc()
  }

  override def beforeAll(): Unit = {
    println("Warming up")
    val smallDispatcher = new DispatcherName {
      val name: String = "small-xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    val validCarfXmlSizeOnDisk = 4000L

    lazy val calls = Vector(
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, service),
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, service)
    )
    val _          = Await.result(Future.sequence(calls), timeout)
    println("Warm up complete")
  }

  inline val validCarfXmlSizeOnDisk   = 4000L // 4kb (size on disk)
  inline val invalidCarfXmlSizeOnDisk = 4000L
  val twoFiftyMbInBytes               = 274726912L // 262mb (size on disk)
  inline val bigInvalidXmlSizeOnDisk  = 12000L // 12kb (size on disk)

  private val timeout = 30.seconds

  "XmlParserService (small thread pool)" - {
    val smallDispatcher = new DispatcherName {
      val name: String = "small-xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    "must handle a small batch of small valid and invalid XML files (4kb)" in {
      smallBatchOfSmall(service)
    }

    "must handle a large batch of small valid and invalid XML files(4kb)" in {
      largeBatchOfSmall(service)
    }

    "must handle a small batch of large valid and invalid XML files (262MB) & (12kb)" in {
      smallBatchOfLarge(service)
    }

    "must handle a large batch of large valid and invalid XML files (250mb) & (12kb)" in {
      largeBatchOfLarge(service)
    }
  }

  "XmlParserService (medium thread pool)" - { // fixed-pool-size = 8, optimal pool size for m5.xlarge
    val smallDispatcher = new DispatcherName {
      val name: String = "xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    "must handle a small batch of small valid and invalid XML files (4kb)" in {
      smallBatchOfSmall(service)
    }

    "must handle a large batch of small valid and invalid XML files(4kb)" in {
      largeBatchOfSmall(service)
    }

    "must handle a small batch of large valid and invalid XML files (262MB) & (12kb)" in {
      smallBatchOfLarge(service)
    }

    "must handle a large batch of large valid and invalid XML files (250mb) & (12kb)" in {
      largeBatchOfLarge(service)
    }
  }

  "XmlParserService (large thread pool)" - { // fixed-pool-size = 16, optimal pool size for Mac M5 Chip
    val smallDispatcher = new DispatcherName {
      val name: String = "large-xml-dispatcher"
    }

    val xmlDispatcher = new XmlDispatcher(actorSystem, smallDispatcher)
    val service       = new XmlParserService(testEnv)(xmlDispatcher)

    "must handle a small batch of small valid and invalid XML files (4kb)" in {
      smallBatchOfSmall(service)
    }

    "must handle a large batch of small valid and invalid XML files(4kb)" in {
      largeBatchOfSmall(service)
    }

    "must handle a small batch of large valid and invalid XML files (262MB) & (12kb)" in {
      smallBatchOfLarge(service)
    }

    "must handle a large batch of large valid and invalid XML files (250mb) & (12kb)" in {
      largeBatchOfLarge(service)
    }
  }

  private def createAndMeasureExecution(path: String, fileSizeInBytes: Long, service: => XmlParserService) =
    Future {
      val startTime = System.nanoTime()
      service.validateAndExtract(path).value map { _ =>
        FileInfo(fileSizeInBytes, System.nanoTime() - startTime)
      }
    }.flatten

  private def usedMemory(rt: Runtime): Long = rt.totalMemory() - rt.freeMemory()

  private def smallBatchOfSmall(service: XmlParserService) = {
    lazy val calls = Vector( // keep lazy Futures are eager
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, service),
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, service),
      createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, service),
      createAndMeasureExecution("data/examples/invalid-carf.xml", invalidCarfXmlSizeOnDisk, service),
      createAndMeasureExecution("data/examples/invalid-carf.xml", invalidCarfXmlSizeOnDisk, service)
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

  private def largeBatchOfSmall(service: XmlParserService) = {
    lazy val validFiles =
      (1 to 25).map(_ => createAndMeasureExecution("data/examples/valid-carf.xml", validCarfXmlSizeOnDisk, service))

    lazy val invalidFiles =
      (1 to 25).map(_ => createAndMeasureExecution("data/examples/invalid-carf.xml", invalidCarfXmlSizeOnDisk, service))

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

  private def smallBatchOfLarge(service: XmlParserService) = {
    lazy val calls = Vector(
      createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, service),
      createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, service),
      createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, service),
      createAndMeasureExecution("data/examples/too-many-schema-errors.xml", bigInvalidXmlSizeOnDisk, service),
      createAndMeasureExecution("data/examples/too-many-schema-errors.xml", bigInvalidXmlSizeOnDisk, service)
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

  private def largeBatchOfLarge(service: XmlParserService) = {
    lazy val validFiles =
      (1 to 25).map(_ => createAndMeasureExecution("data/sized/carf-262mb.xml", twoFiftyMbInBytes, service))

    lazy val invalidFiles = (1 to 25).map(_ =>
      createAndMeasureExecution("data/examples/too-many-schema-errors.xml", bigInvalidXmlSizeOnDisk, service)
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
}

case class FileInfo(sizeInBytes: Long, nano: Long)
