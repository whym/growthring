/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import scala.jdk.CollectionConverters._
import java.io.{ Writer, PrintWriter, StringWriter, BufferedWriter, OutputStreamWriter }
import java.net.{ Socket, ServerSocket, InetSocketAddress }
import scala.io
import org.scalatest.funsuite.AnyFunSuite
import sttp.client3._

/**
  *  @author Yusuke Matsubara <whym@whym.org>
  */
class TestSimpleHttpServer extends AnyFunSuite {
  import util.SimpleHttpServer
  test("simple http server 0") {
    // launch server
    val a = SimpleHttpServer.findFreeAddress()
    val s = SimpleHttpServer.create("localhost", a.getPort, Map(("/abc", "here be dragons")))
    s.start
    val r = TestSimpleHttpServer.waitUntilPrepared(a, 10000L)
    assert(r)

    // send requests
    val backend = HttpURLConnectionBackend()
    def retrieve(path: String): sttp.client3.Response[Either[String, String]] = {
      basicRequest.get(uri"http://${a.getHostString()}:${a.getPort()}/$path").send(backend)
    }
    assert(retrieve("not-found").code.toString.contains("404"))
    assert(retrieve("abc").body == Right("here be dragons"))
  }
}

object TestSimpleHttpServer {
  def waitUntilPrepared(address: InetSocketAddress, limit: Long): Boolean = {
    val start = System.currentTimeMillis
    while (true) {
      val sock = new Socket(address.getAddress, address.getPort)
      if (sock.isConnected) {
        sock.close
        return true
      } else {
        if (System.currentTimeMillis - start > limit) {
          return false
        }
        Thread.sleep(limit / 20)
      }
    }
    return false
  }
}
