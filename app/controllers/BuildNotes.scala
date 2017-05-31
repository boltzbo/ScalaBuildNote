package controllers

import com.dstoutput.buildnotes.BuildNotesUtil
import com.dstoutput.buildnotes.PerforceConnectionInfo
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.BodyParsers
import play.api.libs.json.JsPath
import play.api.libs.json.JsArray

object BuildNotes extends Controller {

	def edit(label: String) = Action {
		Ok(views.html.edit(label))
	}

	def buildnote = Action {
		Ok(views.html.buildnote())
	}

	/***/
	def login = Action {
		Ok(views.html.login())
	}

	def list(user: String, pass: String, repo: String) = Action {
		Ok(views.html.list(user, pass, repo))
	}
	/***/

	def getAllBuilds = Action {
		Ok("Returns all builds")
	}

	def getBuildsInYear(year: Int) = Action {
		Ok("Returns builds in year " + year)

	}

	def getBuildById(id: String) = Action {
		val con = PerforceConnectionInfo("p4java://pserver:2693", "usrnm", "passwd", "//depot/proj/...")
		val result = BuildNotesUtil.retrieve(id, con)

		//Ok("Returns specific build " + id + "\n" + result)
		Ok(result)

	}

	def addBuild(id: String) = Action {
		Ok("Build " + id + " has been added.")
	}

	def updateBuild(id: String) = Action {
		Ok("Build " + id + " has been updated.")
	}

	/*
	def findAddedBuilds = Action {
	val con = PerforceConnectionInfo("p4java://pserver:2693", "usrnm", "passwd", "//depot/proj/...")
	val result = BuildNotesUtil.findAddedBuilds(con)

	//Ok("Returns specific build " + id + "\n" + result)
	Ok(result)
	}
	*/

	def findAddedBuilds(user: String, pass: String, repo: String) = Action {
		val con = PerforceConnectionInfo("p4java://pserver:2693", user, pass, repo)
		val result = BuildNotesUtil.findAddedBuilds(con)
		Ok(result)
	}

	def saveChanges = Action(BodyParsers.parse.json) { request =>
		val con = PerforceConnectionInfo("p4java://pserver:2693", "usrnm", "passwd", "//depot/proj/...")
		BuildNotesUtil.saveChanges(request.body, con)
		Ok(request.body)
	}
}
