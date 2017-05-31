package com.dstoutput.buildnotes

import com.perforce.p4java.server._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import java.text.SimpleDateFormat
import com.perforce.p4java.core.IUserSummary
import play.api.libs.json._
import com.perforce.p4java.core.file.FileSpecBuilder
import scala.io.Source

case class PerforceConnectionInfo(url: String, username: String, password: String, repo: String)

//case class BuildNote(label: String, buildDate: String, uaDate: String, prodDate: String, defects: List[Defect], files: List[BuildFile])
//case class Defect(id: String, owner: String, title: String, status: String, href: String)
//case class BuildFile(path: String, revision: Int)

object BuildNotesUtil {

	def writeFile(path: String, content: String) = {
		val file = new java.io.PrintWriter(new java.io.File(path))
		file.write(content)
		file.close
	}

	def connectPerforce(con: PerforceConnectionInfo) = {
		val server = ServerFactory.getServer(con.url, null)
		server.setUserName(con.username)
		//server.login(con.password)
		server.connect
		server
	}

	def release(server: IServer) = {
		server.disconnect
	}

	def retrieve(id: String, con: PerforceConnectionInfo) = {
		def process(server: IServer, con: PerforceConnectionInfo, id: String) = {
			val label1 = con.repo + "@ebill." + (id.toInt - 1)
			val label2 = con.repo + "@ebill." + id.toInt

			val result = server.execMapCmd("diff2", Array(label1, label2), null)

			// filter for only revision changed items
			val diffMap = result.filter(p => p.get("rev") != p.get("rev2")).toList

			def getDiffFileWithRef(fmap: List[java.util.Map[String, Object]]) = {
				fmap.map(f => {
					(f.get("rev"), f.get("rev2")) match {
						case (x, null) =>
						// remove
						f.get("depotFile") + "#" + (x.toString.toInt + 1) :: Nil

						case (null, y) =>
						// add
						f.get("depotFile2") + "#" + y :: Nil

						case (x, y) =>
						// update
						((x.toString.toInt + 1) to y.toString.toInt).map(t => f.get("depotFile") + "#" + t + '\n')
					}
				}).flatten
			}

			// get diff file names with all changed revisions
			val diffFilesAllRevs = getDiffFileWithRef(diffMap)

			// get each file info
			val result2 = diffFilesAllRevs.map(p => {
				server.execMapCmd("filelog", Array("-l", "-m", "1", p.trim), null)
			}).map(p => p.head)


			def buildDefectName(p: java.util.Map[String, Object]) = {
				val str = p.get("desc0").toString
				val usr = p.get("user0").toString
				val client = p.get("client0").toString
				val pattern = """^[D|d]efect.?(\d+)\s*-?\s*(.*)$""".r

				val defectName = str.split("\n").map(x => pattern.findAllIn(x).matchData.map(f => "Defect " + f.group(1) + "  " + usr + '@' + client + " : " + f.group(2)).mkString).mkString("\n")
				defectName.replaceAll("\n", "")
			}

			val rawlist = scala.util.Sorting.stableSort(result2.toList, (x: java.util.Map[String, Object], y: java.util.Map[String, Object]) => x.toMap.get("desc0").mkString < y.toMap.get("desc0").mkString)

			val refinedlist = rawlist.filter(p => p.get("user0") != "buildEBill").distinct




			/* Ready for Email */
			println
			//println("Email")
			val emailList = refinedlist.map(p => buildDefectName(p)).distinct.sortBy(p => p)
			emailList.foreach(println);
	
			def buildDataJSON = {
				val users = server.getUsers(null, 0)
				def findUser(loginName: String) = {
					val userSum: IUserSummary = users.find(p => p.getLoginName() == loginName).get
					userSum.getFullName()
				}
	
				def buildEachDefectJson(p: java.util.Map[String, Object]) = {
					val str = p.get("desc0").toString.split("\n").head.trim
					val usr = p.get("user0").toString
					val client = p.get("client0").toString
					val pattern = """^[D|d]efect.?(\d+)\s*-?\s*(.*)$""".r
		
					// defect : [...]
					str.split("\n").map(x => pattern.findAllIn(x).matchData.map(f => {
						val id = f.group(1)
						val owner = findUser(usr)
						val title = f.group(2).replaceAllLiterally("\"", "\\\"")
	
						s"""{ "id": "$id", "owner": "$owner", "title": "$title", "status": "NO RESPONSE" }"""
					}).mkString).mkString(",\n")
				}
		
				/* Defects */
				def buildDefectListJSON(list: Array[java.util.Map[String, Object]]): String = {
					val defectsJSON = list.map(p => buildEachDefectJson(p)).distinct
					defectsJSON.mkString("[", ",", "]")
				}
	
				/* Files */
				def buildFileListJSON(list: List[java.util.Map[String, Object]]) = {
					val remainfiles = list.filter(p => p.get("status") != "left only")
					val filesList = remainfiles.map(f => {
						val name = f.get("depotFile2")
						val revision = f.get("rev2")
						s"""{ "path": "$name", "rev": "$revision" }"""
					}).distinct
					filesList.mkString("[", ",", "]")
				}
	
				/* build info */
				val autoItem = rawlist.find(p => p.get("user0").toString == "buildEBill").get
				val changelist = server.getChangelist(autoItem.get("change0").toString.toInt)
				val builddate = new SimpleDateFormat("MMM dd, yyyy").format(changelist.getDate())
				val defects = buildDefectListJSON(refinedlist)
				val files = buildFileListJSON(diffMap)

				val result = s"""{ "label": "$id", "builddate": "$builddate", "uadate": "", "proddate": "NA", "defects": $defects, "files": $files }"""

				//println(result)

				result
			}

			val ttt = "{ \"builds\": [" + buildDataJSON + "] }"

			Json.parse(ttt)
		}



		val server = connectPerforce(con)
		if (!server.isConnected) {
			server.login(con.password)
			server.connect
		}
		val result = process(server, con, id)

		//println("released")
		release(server)

		result
	}

	//def update(id: String, json: String)) = ???




	def findAddedBuilds(con: PerforceConnectionInfo) = {

		def process(server: IServer, con: PerforceConnectionInfo) = {

			def getHtmlBuilds = {
				val result = server.execMapCmd("grep", Array("-e", """<b>build""", "//tserver/TIP_build_notes.html"), null)
				val rawlines = result.map(p => p.get("matchedLine"))

				val pattern = """<b>build (\S+)</b>""".r
				val refinedResult = pattern.findAllIn(rawlines.mkString).matchData.map(f => f.group(1).mkString("\"", "", "\"")).mkString("[", ",", "]")

				//s""" { "htmlBuilds": $refinedResult }"""
				refinedResult
			}

			def getAutoLogBuilds = {
				val result = server.execMapCmd("grep", Array("-a", "-e", """BUILD.LABEL.NUMBER=""", "//tserver/base_int_label.properties"), null)
				val rawlines = result.map(p => p.get("matchedLine").toString)

				val refinedResult = rawlines.map(f => f.split("=").tail.mkString("\"", "", "\"")).mkString("[", ", ", "]")

				//s""" { "autoBuilds": $refinedResult }"""
				refinedResult
			}

			val result = s"""{ "autoBuilds": $getAutoLogBuilds, "htmlBuilds": $getHtmlBuilds }"""
			//getAutoLogBuilds getHtmlBuilds

			result
		}

		val server = connectPerforce(con)
		if (!server.isConnected) {
			server.login(con.password)
			server.connect
		}
		val result = process(server, con)

		//println("released")
		release(server)

		result
	}






	def saveChanges(body: JsValue, con: PerforceConnectionInfo) = {

		val builds = (body \\ "builds").map(_.as[JsArray])
		val item = (body \\ "builds").map(_.as[JsArray]).get(0).value

		def getValSingle(key: String) = {
			item.map(f => (f \ key).as[String]).get(0)
		}

		def getValMulti(key: String) = {
			item.map(f => (f \ key).as[JsArray]).get(0).value
		}

		val label = getValSingle("label")
		val builddate = getValSingle("builddate")
		val uadate = getValSingle("uadate")
		val proddate = getValSingle("proddate")
		val defects = getValMulti("defects")
		val files = getValMulti("files")

		// val goDefects = defects.filter(d => (d \ "status").as[String].toUpperCase.trim == "GO")
		// val noGoDefects = defects.filter(d => (d \ "status").as[String].toUpperCase.trim != "GO")



		def writeDefectUrl(id: String) = "http://dserver/defect/" + id + ".html"
		def writeDefectStatus(status: String) = status.toUpperCase.trim match {
			case "GO" => ""
			case other => " - " + other
		}
		def loopDefects(id: String, status: String, owner: String, title: String) = <li><a href={ writeDefectUrl(id) }>{ id }</a>{ writeDefectStatus(status) } - { owner } - { title }</li>




		def buildExistingDefectTuples = {
			// read existing from perforce
			def readExistingBuildnotesHtml(con: PerforceConnectionInfo) = {
				def process(server: IServer, con: PerforceConnectionInfo) = {
					val path = List("//tserver/TIP_build_notes.html")
					val filespec = FileSpecBuilder.makeFileSpecList(path)
					val is = server.getFileContents(filespec, false, true)

					Source.fromInputStream(is).mkString
				}

				val server = connectPerforce(con)
				if (!server.isConnected) {
					server.login(con.password)
					server.connect
				}
				val result = process(server, con)
				release(server)

				result
			}
			val content = readExistingBuildnotesHtml(con)

			// match no go defects to a tuple
			val openIndex = content.indexOf("""<font face="arial" size="-1">""")
			val closeIndex = content.indexOf("</font>")
			val buildContentBlock = content.slice(openIndex, closeIndex)

			def matchingDefectListItems(content: String) = {
				val pattern = """(.+html.?>(\d+)<\/a>.+)""".r
				pattern.findAllIn(content).matchData.map(f => (f.group(2), f.group(1))).toList
			}

			def buildDefectTuples(contentBlock: String) = {
				val nogoBlockOpener = contentBlock.indexOf("""<ul>""") + """<ul>""".length
				val nogoBlockCloser = contentBlock.indexOf("""</ul>""")
				val recentNoGoContent = contentBlock.slice(nogoBlockOpener, nogoBlockCloser)

				matchingDefectListItems(recentNoGoContent)
			}

			buildDefectTuples(buildContentBlock)
		}


		val ttt = buildExistingDefectTuples.map(_._1).mkString("\n")
		writeFile("C:/ebill/buildnotes/resultNewHtml.txt", ttt)






		def buildHtml = {
			// val builds = (body \\ "builds").map(_.as[JsArray])
			// val item = (body \\ "builds").map(_.as[JsArray]).get(0).value
			// 
			// def getValSingle(key: String) = {
			//   item.map(f => (f \ key).as[String]).get(0)
			// }
			// 
			// def getValMulti(key: String) = {
			//   item.map(f => (f \ key).as[JsArray]).get(0).value
			// }
			// 
			// val label = getValSingle("label")
			// val builddate = getValSingle("builddate")
			// val uadate = getValSingle("uadate")
			// val proddate = getValSingle("proddate")
			// val defects = getValMulti("defects")
			// val files = getValMulti("files")

			val goDefects = defects.filter(d => (d \ "status").as[String].toUpperCase.trim == "GO")
			val noGoDefects = defects.filter(d => (d \ "status").as[String].toUpperCase.trim != "GO")






			def writeUaDate(value: String) = value.isEmpty match {
				case true => ""
				case _ => s" [UA - $value]"
			}

			// def writeDefectUrl(id: String) = "http://dserver/defect/" + id + ".html"
			// def writeDefectStatus(status: String) = status.toUpperCase.trim match {
			//   case "GO" => ""
			//   case other => " - " + other
			// }
			// def loopDefects(id: String, status: String, owner: String, title: String) = <li><a href={ writeDefectUrl(id) }>{ id }</a>{ writeDefectStatus(status) } - { owner } - { title }</li>
			def loopFiles(path: String, rev: String) = <li>{ path }#{ rev }</li>

			val result =
				<font face="arial" size="-1">
				<p><b>build { label }</b>&nbsp;[{ builddate }] - <i>{ writeUaDate(uadate) } [Production - { proddate }]</i></p>
				<h5>label notes</h5>
				<ul>
					{ noGoDefects.map(f => loopDefects((f \ "id").as[String], (f \ "status").as[String], (f \ "owner").as[String], (f \ "title").as[String])) }
				</ul>
				<h5>bug fixes</h5>
				<ul>
					{ goDefects.map(f => loopDefects((f \ "id").as[String], (f \ "status").as[String], (f \ "owner").as[String], (f \ "title").as[String])) }
				</ul>
				<h5>Patch file names</h5>
				<ul>
					{
					files.map(f => loopFiles((f \ "path").as[String], (f \ "rev").as[String]))
					}
				</ul>
				</font>

			result
		}

		val newHtml = buildHtml



	//    def readExistingBuildnotesHtml(con: PerforceConnectionInfo) = {
	//      def process(server: IServer, con: PerforceConnectionInfo) = {
	//        val path = List("//tserver/TIP_build_notes.html")
	//        val filespec = FileSpecBuilder.makeFileSpecList(path)
	//         val is = server.getFileContents(filespec , false, true)
	//         val content = Source.fromInputStream(is)
	//         
	//         content.mkString
	//      }
	//
	//      val server = connectPerforce(con)
	//      if (!server.isConnected) {
	//        server.login(con.password)
	//        server.connect
	//      }
	//      val result = process(server, con)
	//
	//      //println("released")
	//      release(server)
	//
	//      result
	//    }
	//    
	//    val content = readExistingBuildnotesHtml(con)
	//
	//    /*
	//    val file = new java.io.PrintWriter(new java.io.File("C:/ebill/buildnotes/result.txt"))
	//    file.write(content)
	//    file.close
	//    * 
	//    */
	
	//    val openIndex = content.indexOf("""<font face="arial" size="-1">""")
	//    val closeIndex = content.indexOf("</font>")
	//    val buildContentBlock = content.slice(openIndex, closeIndex)
	//
	//    def matchingDefectListItems(content: String) = {
	//      val pattern = """(.+html.?>(\d+)<\/a>.+)""".r
	//      pattern.findAllIn(content).matchData.map(f => (f.group(2), f.group(1))).toList
	//    }
	
	//    def buildDefectTuples(contentBlock: String) = {
	//      val nogoBlockOpener = contentBlock.indexOf("""<ul>""") + """<ul>""".length
	//      val nogoBlockCloser = contentBlock.indexOf("""</ul>""")
	//      val recentNoGoContent = contentBlock.slice(nogoBlockOpener, nogoBlockCloser)
	//
	//      matchingDefectListItems(recentNoGoContent)
	//    }
	
	//    val existingDefectTuples = buildDefectTuples(buildContentBlock)
	//    val newDefectTuples = matchingDefectListItems(newHtml.mkString("\n"))
	//
	//    val finalNoGoDefectTuples = existingDefectTuples.filterNot(p => newDefectTuples.exists(q => q._1 == p._1)) ++ newDefectTuples
	//    val dd = "new\n" + newDefectTuples.map(_._2).mkString("\n") +
	//      "\n\n\n\n\n" +
	//      "existing\n" + existingDefectTuples.map(_._2).mkString("\n") +
	//      "\n\n\n\n\n" +
	//      "new method\n" + finalNoGoDefectTuples.map(_._2).mkString("\n") +
	//      "\n\n\n\n\n" +
	//      "source\n" + newHtml.mkString("\n")
	//
	//    writeFile("C:/ebill/buildnotes/resultSpecial.txt", dd)
	//
	//    writeFile("C:/ebill/buildnotes/resultNewHtml.txt", newHtml.mkString("\n"))
	}
}