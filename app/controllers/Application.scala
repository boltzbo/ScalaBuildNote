package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json

object Application extends Controller {

	def index = Action {
		// Ok(views.html.index("Your new application is ready."))
		Ok
	}

	// case class Stock(code: String, value: Double)

	def getStock(id: String) = Action {
		// val stock = Stock("GOOG", 650.0)
		Ok(Json.toJson(
				Map("status"->"OK", "message"->("Hello " + id))))
		// Ok
	}
}
