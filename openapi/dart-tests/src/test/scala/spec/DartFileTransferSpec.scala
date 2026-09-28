package spec

import fabric.*
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spice.http.durable.*
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.net.*
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

import java.nio.file.{Files, Path}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

case class XferEvent(message: String) derives RW
case class XferMeta(title: String, kind: String) derives RW
case class XferInfo(userId: String) derives RW

/**
 * The generated Dart client's file facet against a real server: an upload, a download, an upload that resumes after
 * the server drops the connection part-way, and a download over the client's size limit refused. Needs `dart` on the
 * PATH and pub.dev reachable for the client's packages; cancels otherwise.
 */
class DartFileTransferSpec extends AnyWordSpec with Matchers {
  private val fileConfig = FileTransferConfig(frameSize = 16 * 1024, windowChunks = 4, ackEvery = 2)
  private val durableServer = new DurableSocketServer[String, XferEvent, XferInfo](
    config = DurableSocketConfig(ackBatchDelay = 50.millis, ackBatchCount = 3),
    eventLog = new InMemoryEventLog[String, XferEvent],
    resolveChannel = (_, info) => Task.pure(info.userId),
    fileTransfer = fileConfig
  )
  object server extends MutableHttpServer

  private def randomBytes(size: Int, seed: Long): Array[Byte] = {
    val bytes = new Array[Byte](size)
    new scala.util.Random(seed).nextBytes(bytes)
    bytes
  }

  private def run(dir: Path, command: String*): (Int, String) = {
    val log = Files.createTempFile("dart-", ".log")
    val process = new ProcessBuilder(command*).directory(dir.toFile).redirectErrorStream(true).redirectOutput(log.toFile).start()
    val exited = process.waitFor(4, TimeUnit.MINUTES)
    if (!exited) process.destroyForcibly()
    (if (exited) process.exitValue() else -1, Files.readString(log))
  }

  private def dartOnPath: Boolean =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).exists(d => Files.isExecutable(Path.of(d, "dart")))

  "A generated Dart client" should {
    "send and receive files, resume an interrupted send, and refuse a file over its limit" in {
      if (!dartOnPath) cancel("dart is not on the PATH")
      val project = Files.createTempDirectory("dart-xfer-")
      val generator = DurableSocketDartGenerator(DurableSocketDartConfig(
        serviceName = "Xfer",
        infoFields = List("userId" -> "String"),
        wireType = "XferEvent" -> summon[RW[XferEvent]].definition,
        fileValueType = Some("XferMeta" -> summon[RW[XferMeta]].definition)
      ))
      generator.write(generator.generate(), project)
      Files.writeString(project.resolve("pubspec.yaml"),
        """name: xfer_probe
          |environment:
          |  sdk: ">=3.0.0 <4.0.0"
          |dependencies:
          |  web_socket_channel: ^3.0.0
          |  http: ^1.2.0
          |  copy_with_extension: ^6.0.0
          |dev_dependencies:
          |  build_runner: ^2.4.0
          |  copy_with_extension_gen: ^6.0.0
          |""".stripMargin)
      Files.createDirectories(project.resolve("bin"))
      Files.writeString(project.resolve("bin/main.dart"),
        """import 'dart:async';
          |import 'dart:io';
          |import 'dart:typed_data';
          |
          |import 'package:xfer_probe/ws/durable/xfer_durable_client.dart';
          |import 'package:xfer_probe/ws/durable/xfer_types.dart';
          |
          |Future<void> main(List<String> args) async {
          |  final dir = args[1];
          |  final client = XferDurableClient(
          |    wsUri: Uri.parse(args[0]),
          |    clientId: 'dart-xfer',
          |    info: XferConnectionInfo(userId: 'dart'),
          |    fileTransfer: const FileTransferConfig(frameSize: 16384, windowChunks: 4, ackEvery: 2, maxInboundBytes: 1000000),
          |  );
          |  final files = client.files;
          |  final downloads = StreamIterator(files.onFile);
          |  final bye = client.onEphemeral.firstWhere((m) => m['type'] == 'bye');
          |  await client.connect();
          |
          |  final upload = File('$dir/upload.bin').readAsBytesSync();
          |  await files.send('upload.bin', 'application/octet-stream', upload, XferMeta(title: 'upload.bin', kind: 'blob'));
          |  print('uploaded');
          |
          |  if (!await downloads.moveNext().timeout(const Duration(seconds: 60))) throw StateError('no download');
          |  final received = downloads.current;
          |  File('$dir/downloaded.bin').writeAsBytesSync(received.bytes);
          |  print('downloaded ${received.value.title} ${received.value.kind} ${received.name} ${received.contentType}');
          |
          |  final big = File('$dir/resume.bin').readAsBytesSync();
          |  await files.sendFrom('resume.bin', 'application/octet-stream', big.length,
          |      (offset, length) async => Uint8List.sublistView(big, offset, offset + length),
          |      XferMeta(title: 'resume.bin', kind: 'blob'));
          |  print('resumed');
          |
          |  await bye.timeout(const Duration(seconds: 60));
          |  client.close();
          |  print('bye');
          |  exit(0);
          |}
          |""".stripMargin)
      val (pubCode, pubOut) = run(project, "dart", "pub", "get")
      if (pubCode != 0) cancel(s"dart pub get failed (pub.dev unreachable?):\n$pubOut")
      val (buildCode, buildOut) = run(project, "dart", "run", "build_runner", "build", "--delete-conflicting-outputs")
      withClue(buildOut) { buildCode shouldBe 0 }
      val (analyzeCode, analyzeOut) = run(project, "dart", "analyze", "--no-fatal-warnings", "lib", "bin")
      withClue(analyzeOut) { analyzeCode shouldBe 0 }

      val upload = randomBytes(300_000, 1L)
      val download = randomBytes(500_000, 2L)
      val resume = randomBytes(2_000_000, 3L)
      val tooBig = randomBytes(1_500_000, 4L)
      Files.write(project.resolve("upload.bin"), upload)
      Files.write(project.resolve("resume.bin"), resume)

      val received = new ConcurrentLinkedQueue[(String, Array[Byte])]()
      val dropped = new AtomicBoolean(false)
      @volatile var refusal: Option[String] = None
      // A resumed session is announced again; its file channel is wired once.
      val wired = ConcurrentHashMap.newKeySet[AnyRef]()
      def wire(session: DurableSession[String, XferEvent, XferInfo]): Unit = {
        val files = session.protocol.files[XferMeta]
        files.onProgress.attach { p =>
          if (p.totalBytes == resume.length && p.bytesSoFar > resume.length / 3 && dropped.compareAndSet(false, true))
            Task(session.protocol.disconnect()).start()
        }
        files.onFile.attach { rf =>
          received.add(rf.value.title -> Files.readAllBytes(rf.path))
          rf.value.title match {
            case "upload.bin" =>
              files.send("download.bin", "image/png", Stream.emits(download.toIndexedSeq), XferMeta("download.bin", "image")).start()
            case "resume.bin" =>
              files.send("too-big.bin", "application/octet-stream", Stream.emits(tooBig.toIndexedSeq), XferMeta("too-big.bin", "blob"))
                .attempt
                .map { outcome =>
                  refusal = outcome.failed.toOption.map(_.getMessage)
                  session.protocol.sendEphemeral(obj("type" -> str("bye")))
                }
                .start()
            case _ =>
          }
        }
      }
      durableServer.onSession.attach { session =>
        if (wired.add(session.protocol)) wire(session)
      }

      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(path"/xfer" / durableServer))
      server.start().sync()
      try {
        val port = server.config.listeners().head.port.getOrElse(0)
        val (code, out) = run(project, "dart", "run", "bin/main.dart", s"ws://localhost:$port/xfer", project.toString)
        withClue(out) {
          code shouldBe 0
          out should include("downloaded download.bin image download.bin image/png")
          out should include("bye")
        }
        val byTitle = received.asScala.toMap
        java.util.Arrays.equals(byTitle("upload.bin"), upload) shouldBe true
        java.util.Arrays.equals(byTitle("resume.bin"), resume) shouldBe true
        java.util.Arrays.equals(Files.readAllBytes(project.resolve("downloaded.bin")), download) shouldBe true
        dropped.get() shouldBe true
        refusal.getOrElse(fail("the over-limit send succeeded")) should include("over the 1000000-byte limit")
      } finally server.stop().sync()
    }
  }
}
