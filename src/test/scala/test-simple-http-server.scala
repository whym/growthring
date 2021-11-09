/**
  *  @author Yusuke Matsubara <whym@whym.org>
  *
  */

import scala.collection.JavaConverters._
import java.io.{ Writer, PrintWriter, StringWriter, BufferedWriter, OutputStreamWriter }
import java.net.{ Socket, ServerSocket, InetSocketAddress }
import scala.io
import org.scalatest.funsuite.AnyFunSuite

/**
  *  @author Yusuke Matsubara <whym@whym.org>
  */
class TestSimpleHttpServer extends AnyFunSuite {
  import util.SimpleHttpServer

  def retrieve(address: InetSocketAddress, path: String): List[String] = {
    val sock = new Socket(address.getAddress, address.getPort)
    val writer = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream))
    writer.write(f"GET ${path} HTTP/1.0\r\n")
    writer.flush
    sock.shutdownOutput
    val ret = io.Source.fromInputStream(sock.getInputStream).getLines().toList
    writer.close
    sock.close
    ret
  }

  test("simple http server 0") {
    assertResult((true, "def")) {
      val a = SimpleHttpServer.findFreeAddress()
      val s = SimpleHttpServer.create("localhost", a.getPort, Map(("/abc", "def")))
      s.start
      val r = TestSimpleHttpServer.waitUntilPrepared(a, 10000L)
      (r, retrieve(a, "/abc").last)
    }
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
