package org.whym.growthring

import java.net.{HttpURLConnection,InetSocketAddress}
import java.io.IOException
import com.sun.net.httpserver.{HttpServer,HttpHandler,HttpExchange}

object SimpleHttpServer {
  class MyHandler(path: String, body: String, ctype: String) extends HttpHandler {
    @throws[IOException] override def handle(exchange: HttpExchange) {
      val response = body.getBytes();
      exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK,
                                   response.length)
      exchange.getResponseHeaders().set("Content-type", ctype)
      exchange.getResponseBody().write(response)
      exchange.close()
    }
  }
  def create(address: String, port: Int, map: Map[String,String]): HttpServer = {
    val address = new InetSocketAddress(port);
    val httpServer = HttpServer.create(address, 0);

    for ( (path,body) <- map ) {
      val ctype = if (body.size > 0 && body(0) == '<') { "text/xml" }
                  else if (body.size > 0 && body(0) == '{') { "application/json" }
                  else { "text/plain" }
      httpServer.createContext(path, new MyHandler(path, body, ctype))
    }
    httpServer
  }

  def main(args: Array[String]) {
    import scala.util.Properties.{propOrElse => p}
    val s = create(p("host", "localhost"), p("port", "8111").toInt,
                   Range(1, 10).map(x => (p("path"+x, ""), p("body"+x, ""))).toMap)
    s.start
  }
}
