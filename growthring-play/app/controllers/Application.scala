package controllers

import org.whym.growthring.{WikiBlameServlet => wbs}

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {
  
  def index = Action {
    Ok("index ok")
  }

  val wbForm = Form(
    tuple("title" -> nonEmptyText,
          "base"  -> default(nonEmptyText, "http://en.wikipedia.org/w"),
          "n"     -> default(number(min=1, max=200), 100))
  )

  def wikiblameFormReq = Action { implicit request =>
    wbForm.bindFromRequest.fold(
      errors => BadRequest(views.html.wikiblame("error!", errors.toString)),
      value => value match {
        case (title, base, n) => {
          val revs = wbs.getRevs(title, base, n)
          val html = wbs.getHtml(revs, n)
          Ok(views.html.wikiblame(title, html))
          //Redirect(routes.Application.wikiblameLandingPage)
        }
      }
    )
  }
  
}
