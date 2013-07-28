package controllers

import org.whym.growthring.{WikiBlameServlet => wbs}

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._

object Application extends Controller {
  
  def index = Action {
    Ok("index ok")
  }

  val wbForm = Form(
    mapping("title" -> nonEmptyText,
            "base"  -> default(nonEmptyText, "http://en.wikipedia.org/w"),
            "n"     -> default(number(min=1, max=200), 100))
    (WikiBlameParams.apply)(WikiBlameParams.unapply)
  )

  def wikiblameFormReq = Action { implicit request =>
    wbForm.bindFromRequest.fold(
      errors => BadRequest(views.html.wikiblame(wbForm, "error!", Nil, errors.toString)),
      v => {
        val revs = wbs.getRevs(v.title, v.base, v.n)
        val spans = wbs.getSpans(revs, v.n)
        Ok(views.html.wikiblame(wbForm.fill(v), v.title, spans, ""))
        //Redirect(routes.Application.wikiblameLandingPage)
      }
    )
  }

  def wikiblameForm = Action {
    Ok(views.html.wikiblame(wbForm, "", Nil, ""))
  }
  
}
