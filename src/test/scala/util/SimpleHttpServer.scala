package util

import java.net.{ HttpURLConnection, InetSocketAddress, ServerSocket }
import java.io.IOException
import com.sun.net.httpserver.{ HttpServer, HttpHandler, HttpExchange }

object SimpleHttpServer {
  class MyHandler(path: String, body: String, ctype: String) extends HttpHandler {
    @throws[IOException] override def handle(exchange: HttpExchange): Unit = {
      val response = body.getBytes()
      exchange.sendResponseHeaders(
        HttpURLConnection.HTTP_OK,
        response.length)
      exchange.getResponseHeaders().set("Content-type", ctype)
      exchange.getResponseBody().write(response)
      exchange.close()
    }
  }
  def create(address: String, port: Int, map: Map[String, String]): HttpServer = {
    val address = new InetSocketAddress(port);
    val httpServer = HttpServer.create(address, 0);

    for ((path, body) <- map) {
      val ctype = if (body.size > 0 && body(0) == '<') { "text/xml" }
      else if (body.size > 0 && body(0) == '{') { "application/json" }
      else { "text/plain" }
      httpServer.createContext(if (path(0) == '/') { path } else { "/" + path }, new MyHandler(path, body, ctype))
    }
    httpServer
  }

  def findFreeAddress(): InetSocketAddress = {
    val sock = new ServerSocket(0)
    val addr = new InetSocketAddress(sock.getLocalPort)
    sock.close
    addr
  }

  def main(args: Array[String]): Unit = {
    import scala.util.Properties.{ propOrElse => p }
    val addr = findFreeAddress()
    val s = create(p("host", "localhost"), p("port", addr.getPort.toString).toInt,
      Range(1, 10).map(x => (p("path" + x, x.toString), p("body" + x, "OK"))).toMap)
    System.err.println(f"host: ${s.getAddress}")
    s.start
  }
}
