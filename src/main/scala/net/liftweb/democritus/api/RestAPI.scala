package net.liftweb.democritus.api

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import js._
import JE._
import rest._
import _root_.net.liftweb.util._
import StringHelpers._
import _root_.scala.xml._
import _root_.net.liftweb.mapper._
import java.util.Date
import net.liftweb.democritus.utils.Utilities._
import net.liftweb.democritus.model._
import _root_.scala.collection.mutable._
import _root_.net.liftweb.json._


object RestAPI extends XMLApiHelper {
	def dispatch:LiftRules.DispatchPF = {
	  case Req("api" :: "content" :: eid ::atom :: Nil, "", GetRequest) => () => showContentAtom(eid:String)
	  case Req("api" :: "content" :: all :: tags :: atom :: Nil, "", GetRequest) => () => listContentByTagsAtom(tags:String)  
      case Req("api" :: "content" :: atom :: Nil, "", GetRequest)=> () => listContentsAtom
      case Req("api" :: "content" :: Nil, "", PutRequest)=> () => saveContent      
      
      
	  //Invalid API request - route to our error handler
      case Req("api" :: x :: Nil, "", _) => failure _
	}
 
   def createDate(dt:String) = dt match {
        	case null => new Date(dt)
        	case _ => new Date()
      	}
      
   
   def feedWrapper(eList:List[NodeSeq]) = {
     val host = "http://" + S.hostName  
     val link = host + ":8080" + S.contextPath + "/api/content/atom"     
     val updated = Content.mostRecentContent match {
         case Full(c) => formatDate(c.lastUpdated.is, rfcDate.format) open_!
         case _ => formatDate(new Date(), rfcDate.format) open_!
         }
    
     
     <feed xmlns="http://www.w3.org/2005/Atom"
     	xmlns:ex="http//democritus.com/ex/elements/1.0/" xml:lang="en-us">
          <id>{host + "/"}</id>
          <title>Democritus News</title>
          <link href={host + "/"}/>
          <link rel="self" href={link} /> 
          <updated>{updated}</updated>
          {eList}
          </feed>
    }
 
  // final wrap of responses
   def createTag(in: NodeSeq) = {
    println("[CreateTag] " + in)
    <apbul_api>{in}</apbul_api>

  }
    
    def wrapXmlBody(in:NodeSeq) = {
      println("[WrapXMLBody]: " + in)
      <apbul_api>{in}</apbul_api>     
    }
 
 	def failure():LiftResponse = {
 	  val ret:Box[NodeSeq] = Full(<op id="FAILURE"></op>)
 	  NotFoundResponse()
 	}
  
    //Reacts to GET request
    def showContentAtom(eid:String):AtomResponse = {
 	  val e: Box[Node] = for(e <- Content.find(By(Content.id, eid.toLong))) yield {
 	    e.toAtom
 	  }
 	  AtomResponse(e.open_!)
 	}
    
    //Reacts to GET request with tags
    def listContentByTagsAtom(tags:String):AtomResponse = {
        val _tags = tags.roboSplit(",")
        def contents(tag:String) =
	      	  ContentTag.findAll(
	    			  In(ContentTag.tag, Tag.id,
	    					 Like(Tag.itemName, tag))).map(_.content.obj.open_!).removeDuplicates
        
        def allContents = for(t <- _tags)  yield{contents(t).map(_.toAtom)} 
        
        def results = allContents.map(e => <all>{e}</all>)
        
        println("Local feed = " + feedWrapper(results))
        AtomResponse(feedWrapper(results))        
	    			  
    }
           
  
    
    def addContent(req:Req):LiftResponse = {
      var tempEmail = ""
      var tempPass = ""     
     
      
      var content = new Content
      println("You requested to save xml: " + req.xml)
            
      req.xml match {
       case Full(<entry>{parameters @ _*}</entry>) => {
         for(parameter <- parameters){parameter match{
	           	case <email>{email}</email> => tempEmail = email.text
	           	case <password>{password}</password> => tempPass = password.text
	            case <title>{title}</title> => content.title(title.text)
	            case <description>{description}</description> => content.description(description.text)
	            //case <detail>{detail}</detail> => content.detail(detail.text)
                case <creationDate>{creationDate}</creationDate> => content.creationDate(createDate(creationDate.text))
                case <lastUpdated>{lastUpdated}</lastUpdated> => content.lastUpdated(createDate(lastUpdated.text))
                //case <tags>{tags}</tags> => content.tags(tags.get)
             case _ =>
            }
         }
         
         try {
	           val u:User = User.find(By(User.email, tempEmail)) match {
	             case Full(user) if user.validated && user.password.match_?(tempPass) => User.logUserIn(user)
	             user
	             case _ => new User
	           }
            
               content.validate match {
                 case Nil => 
                   content.save
                   println(content.title)
                   CreatedResponse(wrapXmlBody(<operation id="add_content" success="true"></operation>), "text/xml")
                 case _ =>
                   CreatedResponse(wrapXmlBody(<operation id="add_content" success="false"></operation>), "text/xml")
               }
         }catch {
           case e => Log.error("Could not add content", e); BadResponse()
         }
         
     }
       case _ => Log.error("Request was malformed"); BadResponse() 
       
     }
     }
    
    //Reacts to PUT request
   def saveContent:LiftResponse = 
     addContent(S.request open_!) 
   
   
   //Reacts to Get all reguest
   def listContentsAtom:AtomResponse = {
     val eList = for(e <- Content.findAll) yield {
 	    e.toAtom
 	  }
      
     Log.info(feedWrapper(eList))
 	 AtomResponse(feedWrapper(eList))
    	 
   }
     
}
