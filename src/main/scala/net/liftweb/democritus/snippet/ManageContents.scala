package net.liftweb.democritus.snippet

import java.util.Date

import _root_.scala.xml._
import net.liftweb._
import common._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._
import model._
import _root_.net.liftweb.util._
import _root_.javax.servlet._
import _root_.javax.servlet.http._
import utils.Utilities._
import _root_.net.liftweb.mapper._
import _root_.java.io._
import _root_.java.text.SimpleDateFormat

class ManageContents {
  
   val slashDate = new SimpleDateFormat("MM/dd/yyyy")
   
  def addNew(xhtml:NodeSeq):NodeSeq =  User.currentUser match{
    case Full(user) => <h3><a href="/editContent">Add new Content</a></h3>
    case _ => NodeSeq.Empty
    }
    
  def manage (xhtml : NodeSeq) : NodeSeq = {
    def deleteContent (c : Content) {
      // No cascade???
      Content.delTags(c)
      c.admins.foreach(_.delete_!)
      c.tags.foreach(_.delete_!)
      c.delete_!
    }
    
    User.currentUser.map({user =>
        Content.findAll.flatMap({c =>
            bind("content", chooseTemplate("content", "entry", xhtml),
                 "title" -> Text(c.title.is),
                 "type" -> Text(c.contentType.fromInt(c.contentType.toInt).toString),  
                 "lastupdated" -> Text(c.lastUpdated.is.toString),
                 "tags" -> c.showTags,
                 "actions" -> { link("/listContents", () => deleteContent(c), Text("Delete")) ++ Text(" ") ++
                               link("/editContent", () => currentContentVar(c), Text("Edit")) })
          })
			}) openOr Text("You're not logged in")
    }
    
  
  object currentContentVar extends RequestVar[Content]({
      Content.create.author(User.currentUser.open_!).creationDate(new Date)
    })
    
  def currentContent = currentContentVar.is
  
  object page extends RequestVar[String]("")
  
  def edit (xhtml : NodeSeq) : NodeSeq = {
    var tags = currentContent.tags.map(_.itemName.is).mkString(", ")
    
    def doTagsAndSubmit(t:String){
       tags = t
       if (tags.trim.length == 0) error("We're going to need at least one tag.")
       else {
         currentContent.tags(tags)         
       }
    }

    
    def doSave () = { 
      isValidElem(page.is) match {
        case true => {
	        currentContent.validate match {
		        case Nil =>
		          currentContent.author(User.currentUser.open_!)
		          Content.delTags(currentContent)
		          currentContent.tags(tags)
		          currentContent.lastUpdated(new Date)
		          currentContent.save
		          Content.addTags(currentContent)
		          writeHtml(currentContent.link.is, page.is)
		          redirectTo("/listContents")
		        case x => error(x)
	        }
         }
         case false => error("Not valid xhtml")
       }
    } 

    val content = currentContent
    
    val header = currentContent.id.is match{
      case -1 => "Add new Content"
      case _ => "Edit Content"
    } 
    
    
    bind("content", xhtml,
         "header" -> Text(header),    
         "id" -> hidden(() => currentContentVar(content)),
         "title" -> text(currentContent.title.is, currentContent.title(_), ("size","60")),
         "link" -> text(currentContent.link.is, currentContent.link(_)),
         "description" -> textarea(currentContent.description.is, currentContent.description(_)),
         "type" -> currentContent.contentType.toForm,
         "detail" -> textarea(loadHtml(currentContent.link.is).toString, page(_), ("class", "wymeditor")),
         "tags" -> text(tags, doTagsAndSubmit),
         "save" -> submit("Save", doSave, ("class", "wymupdate")),
    	 "cancel" -> submit("Cancel", () => redirectTo("/listContents")))
  }
  
  def loadLink(page:String):Elem = page match {
      case null => null
      case x => new File(dataDir + x + ".html").exists match {
        case true => 
          try{
        	  XML.loadFile(dataDir + x + ".html")
           }catch {
      
           		case e => {
                    
           			<span></span>
           		}
           }
        case false => <span></span>
      }
  }
  
  def showContents(xhtml:NodeSeq):NodeSeq ={
    val links = S.param("tags") openOr ""
    println("Detail page = " +links)
    links.roboSplit(",").last match {
      case null => NodeSeq.Empty
      case x => bind("contents", xhtml, "page"->loadLink(x))
    }
  }
  
  def content(xhtml:NodeSeq):NodeSeq = {
    val c = S.param("tags") openOr ""
    
    def _tags = c.roboSplit(",").map(S.?(_))
    
    val id:String= S.param("id") openOr {
      ContentTag.findTagContents(c.roboSplit(",").last) match {
        case Nil => "0"
        case x => x.last.id.is.toString
      }
    }
    
    val contentList = Content.findAll(By(Content.id, id.toLong))
     
    val title = contentList match{
      case Nil => ""
      case x => x.head.title.is
    }
    
    val summary = contentList match{
      case Nil => ""
      case x => x.head.description.is
    }
    
    val page = contentList match{
      case Nil => ""
      case x => x.head.link.is
    }
    
    bind("c", xhtml,
    	"heading" -> _tags.mkString(" -> "),
    	"title" -> title,    	
        "summary" -> summary,
        "page" -> loadLink(page)
        
    )
       
  }
  
  def feed(xhtml:NodeSeq):NodeSeq = {
    S.attr("url") match {
      case Full(url) => RSSFeedDemo(url)    
      case Empty => RSSFeedDemo.localFeed(S.attr("tags") openOr "", S.attr("showDetail") match {
        case Full(_) => true
        case Empty => false
        case Failure(_, _, _) => false                             
      	})
      case Failure(_,_,__) => NodeSeq.Empty
    }     
   
  }
}
