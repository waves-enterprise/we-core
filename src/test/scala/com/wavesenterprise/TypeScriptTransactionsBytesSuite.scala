package com.wavesenterprise

import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.async.ResultCallback
import com.github.dockerjava.api.model.{ExposedPort, Frame, HostConfig, PortBinding}
import com.wavesenterprise.DockerClientBuilder.createDefaultApacheDockerClient
import com.wavesenterprise.TypeScriptTransactionsBytesSuite.waitFor
import com.wavesenterprise.account.PrivateKeyAccount
import com.wavesenterprise.transaction.Transaction
import com.wavesenterprise.transaction.docker.ContractTransactionGen
import com.wavesenterprise.utils.ScorexLogging
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import org.asynchttpclient.Dsl.{asyncHttpClient, get, post, config => clientConfig}
import org.asynchttpclient.{AsyncHttpClient, Response}
import org.scalacheck.Gen
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Ignore, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import play.api.libs.json.{JsPath, Json, Reads}

import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.compat.java8.FutureConverters.CompletionStageOps
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

class TypeScriptTransactionsBytesSuite extends FreeSpec with Matchers with ContractTransactionGen with NodeHost with BeforeAndAfterAll
    with ScorexLogging {

  val tsServerPort: String               = "3000"
  val dockerClient: DockerClient         = createDefaultApacheDockerClient()
  val containerLogTask: SerialCancelable = SerialCancelable()
  var httpClient: AsyncHttpClient        = _
  var image: String                      = _
  var containerId: String                = _
  var apiUrl: String                     = _

  private def startTsServerInDocker(): Unit = {
    val imageName = "ts-tx-test-server"
    dockerClient.buildImageCmd(Paths.get(sys.props("user.dir")).resolve(
      "transactions-factory").toFile).withTags(Set(s"$imageName").asJava).start().awaitImageId()

    val hostConfigBuilder = new HostConfig()
      .withPortBindings(PortBinding.parse(tsServerPort))

    val container = dockerClient.createContainerCmd(s"$imageName:latest")
      .withName("ts-tx-test-server-container")
      .withExposedPorts(ExposedPort.parse(tsServerPort))
      .withHostConfig(hostConfigBuilder)
      .exec()

    containerId = container.getId
    dockerClient.startContainerCmd(containerId).exec()

    val callback = new ResultCallback.Adapter[Frame] {
      override def onNext(frame: Frame): Unit = {
        if (frame != null) {
          log.info(s"[ts-container]: ${new String(frame.getPayload)}")
        }
      }
    }
    containerLogTask := Task
      .eval {
        dockerClient.logContainerCmd(containerId)
          .withStdOut(true)
          .withFollowStream(true)
          .exec(callback)
      }
      .executeAsync
      .runToFuture(monix.execution.Scheduler.global)

    val boundPort = dockerClient.inspectContainerCmd(containerId).exec.getNetworkSettings.getPorts.getBindings.get(
      new ExposedPort(tsServerPort.toInt)).head.getHostPortSpec

    apiUrl = s"http://$nodeHost:$boundPort/"
    waitForTsServerStartup()
  }

  private def setTypeScriptServerChainId(): Unit = {
    val setChainIdTask = httpClient
      .executeRequest {
        post(apiUrl + "networkByte")
          .setHeader("Content-type", "application/json;charset=utf-8")
          .setBody(s"""{ "networkByte": $currentChainId }""")
      }
      .toCompletableFuture
      .toScala
      .flatMap { response =>
        if (200 != response.getStatusCode)
          Future.failed(new RuntimeException(s"Unexpected response status code form TypeScript server: '${response.getStatusCode}'"))
        else Future.unit
      }(monix.execution.Scheduler.global)

    Await.result(setChainIdTask, 5.seconds)
  }

  override protected def beforeAll(): Unit = {
    httpClient = asyncHttpClient {
      clientConfig()
        .setKeepAlive(false)
        .setNettyTimer(GlobalTimer.instance)
    }
    startTsServerInDocker()
    setTypeScriptServerChainId()
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    if (httpClient != null) {
      Try(httpClient.close())
    }
    if (containerId != null) {
      Try {
        dockerClient.stopContainerCmd(containerId).withTimeout(10).exec
        dockerClient.removeContainerCmd(containerId).withForce(true).exec
      }
    }
    if (!containerLogTask.isCanceled) {
      containerLogTask.cancel()
    }
    if (image != null) {
      Try(dockerClient.removeImageCmd(image).exec)
    }
    dockerClient.close()
    super.afterAll()
  }

  lazy val sampleAccount: PrivateKeyAccount       = PrivateKeyAccount(crypto.generateKeyPair())
  override def accountGen: Gen[PrivateKeyAccount] = Gen.const(sampleAccount)

  "should return the same bytes for" - {
    "RegisterNodeTransaction" in {
      forAll(registerNodeTransactionGen())(assertSameBytes)
    }

    "CreateAliasTransactionV2" in {
      forAll(createAliasV2Gen)(assertSameBytes)
    }

    "CreateAliasTransactionV3" in {
      forAll(createAliasV3Gen)(assertSameBytes)
    }

    "IssueTransactionV2" in {
      forAll(issueGen)(assertSameBytes)
    }

    "ReissueTransactionV2" in {
      forAll(reissueGen)(assertSameBytes)
    }

    "BurnTransactionV2" in {
      forAll(burnGen)(assertSameBytes)
    }

    "LeaseTransactionV2" in {
      forAll(leaseV2Gen)(assertSameBytes)
    }

    "LeaseCancelTransactionV2" in {
      forAll(leaseCancelGen)(assertSameBytes)
    }

    "SponsorFeeTransaction" in {
      forAll(sponsorFeeGen)(assertSameBytes)
    }

    "SetAssetScriptTransaction" in {
      forAll(setAssetScriptTransactionGen) { case (_, tx) => assertSameBytes(tx) }
    }

    "DataTransaction" in {
      forAll(dataTransactionV1Gen)(assertSameBytes)
    }

    "DataTransactionV2" in {
      forAll(dataTransactionV2Gen)(assertSameBytes)
    }

    "TransferTransactionV2" in {
      forAll(transferV2Gen)(assertSameBytes)
    }

    "TransferTransactionV3" in {
      forAll(transferV3Gen())(assertSameBytes)
    }

    "MassTransferTransaction" in {
      forAll(massTransferV1Gen(minTransfersCount = 2))(assertSameBytes)
    }

    "MassTransferTransactionV2" in {
      forAll(massTransferV2Gen(minTransfersCount = 2))(assertSameBytes)
    }

    "PermitTransactionV1" in {
      forAll(permitTransactionV1Gen())(assertSameBytes)
    }

    "PermitTransactionV2" in {
      forAll(permitTransactionV2Gen())(assertSameBytes)
    }

    "CreatePolicyTransaction" in {
      forAll(createPolicyTransactionV1Gen())(createPolicyWrap => assertSameBytes(createPolicyWrap.tx))
    }

    "CreatePolicyTransactionV2" in {
      forAll(createPolicyTransactionV2Gen())(createPolicyWrap => assertSameBytes(createPolicyWrap.tx))
    }

    "CreatePolicyTransactionV3" in {
      forAll(createPolicyTransactionV3Gen())(createPolicyWrap => assertSameBytes(createPolicyWrap.tx))
    }

    "UpdatePolicyTransaction" in {
      forAll(updatePolicyTransactionV1Gen())(assertSameBytes)
    }

    "UpdatePolicyTransactionV2" in {
      forAll(updatePolicyTransactionV2Gen())(assertSameBytes)
    }

    "UpdatePolicyTransactionV3" in {
      forAll(updatePolicyTransactionV3Gen())(assertSameBytes)
    }

    "PolicyDataHashTransactionV3" in {
      forAll(policyDataHashTransactionV3Gen())(pdhWrap => assertSameBytes(pdhWrap.tx))
    }

    "CreateContractTransaction" in {
      forAll(createContractV1ParamGen)(assertSameBytes)
    }

    "CreateContractTransactionV2" in {
      forAll(createContractV2ParamGen)(assertSameBytes)
    }

    "CreateContractTransactionV3" in {
      forAll(createContractV3ParamGen())(assertSameBytes)
    }

    "CreateContractTransactionV4" in {
      forAll(createContractV4ParamGen())(assertSameBytes)
    }

    "CallContractTransaction" in {
      forAll(callContractV1ParamGen)(assertSameBytes)
    }

    "CallContractTransactionV2" in {
      forAll(callContractV2ParamGen)(assertSameBytes)
    }

    "CallContractTransactionV3" in {
      forAll(callContractV3ParamGen)(assertSameBytes)
    }

    "CallContractTransactionV4" in {
      forAll(callContractV4ParamGen())(assertSameBytes)
    }

    "DisableContractTransaction" in {
      forAll(disableContractV1ParamGen)(assertSameBytes)
    }

    "DisableContractTransactionV2" in {
      forAll(disableContractV2ParamGen)(assertSameBytes)
    }

    "DisableContractTransactionV3" in {
      forAll(disableContractV3ParamGen())(assertSameBytes)
    }

    "UpdateContractTransaction" in {
      forAll(updateContractV1ParamGen)(assertSameBytes)
    }

    "UpdateContractTransactionV2" in {
      forAll(updateContractV2ParamGen)(assertSameBytes)
    }

    "UpdateContractTransactionV3" in {
      forAll(updateContractV3ParamGen())(assertSameBytes)
    }

    "UpdateContractTransactionV4" in {
      forAll(updateContractV4ParamGen())(assertSameBytes)
    }

    "SetScriptTransaction" in {
      forAll(setScriptTransactionGen)(assertSameBytes)
    }

    "AtomicTransaction" in {
      forAll(atomicTxV1Gen)(assertSameBytes)
    }
  }

  private def waitForTsServerStartup(): Unit = {
    implicit val scheduler: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
    waitFor("TypeScript server startup") {
      val pingResponse = httpClient
        .executeRequest(get(apiUrl).build())
        .toCompletableFuture
        .toScala
        .map(_ => true)
        .recover { case _ => false }

      Await.result(pingResponse, 10 seconds)
    }
  }

  private def assertSameBytes(tx: Transaction) = {
    val expectedBytes: Array[Byte] = tx.proofSourceBytes
    val json: String               = tx.json().toString()
    val actualBytes: Array[Byte]   = Json.fromJson[BytesResponse](Json.parse(executeRequest(json).getResponseBody)).get.bytes

    expectedBytes should contain theSameElementsInOrderAs actualBytes
  }

  private def executeRequest(body: String): Response = {
    val futureResponse = httpClient
      .executeRequest {
        post(apiUrl)
          .setHeader("Content-type", "application/json;charset=utf-8")
          .setBody(body)
          .build()
      }
      .toCompletableFuture
      .toScala

    Await.result(futureResponse, 10 seconds)
  }
}

object TypeScriptTransactionsBytesSuite {
  def waitFor(waitingFor: String, retriesCount: Int = 10, sleepMs: Int = 5000)(test: => Boolean): Unit = {

    @tailrec
    def go(n: Int) {
      if (n > 0) {
        if (!test) {
          Thread.sleep(sleepMs)
          go(n - 1)
        }
      } else {
        throw new RuntimeException(s"$waitingFor has been failed after '${retriesCount * sleepMs / 1000}' seconds")
      }
    }

    go(retriesCount)
  }
}

case class BytesResponse(bytes: Array[Byte])

object BytesResponse {
  implicit val bytesResponseReads: Reads[BytesResponse] = {
    (JsPath \ "bytes").read[Array[Byte]].map(BytesResponse.apply)
  }
}
