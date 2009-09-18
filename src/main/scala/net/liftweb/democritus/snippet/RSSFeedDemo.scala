package net.liftweb.democritus.snippet

import _root_.scala.xml._
import _root_.java.net.{URLConnection, URL}
import _root_.scala.collection.mutable._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import net.liftweb.democritus.api._
import net.liftweb._
import http._
import SHtml._
import S._

object RSSFeedDemo {
 
  def apply(feedUrl: String) = new RSSFeedDemo().render(feedUrl)
  
  def localFeed(tags:String, showDetail:Boolean):NodeSeq = {
    //val feed = RestAPI.listContentsAtom.out
    val feed = RestAPI.listContentByTagsAtom(tags).out
             
    var src = new Queue[Node]()
          
    for (c <- findElems(feed){_.label == "entry"}) {
        val id = {(c \\ "id")}
        val links = for(t <-(c \\ "tag")) yield(t.text)
        val link = 
          "http://" + S.hostName + ":8080" + S.contextPath + "/contents/?tags=" + links.mkString(",") + "&id=" + id.text
        
        val title =  showDetail match {
            case true => {(c \\ "entry" \\ "title").text}
            case false => <a href={link}>{(c \\ "entry" \\ "title").text}</a>
        }
        
        src+= <li class="rsswidgetitem">{title}<h5>{(c \\ "summary").text}</h5></li>
       
        
    }
    
    
    <div class="rsswidget"><ul class="xoxo">{src}</ul></div>
  }  
  
}
 
class RSSFeedDemo {
  
  object page extends RequestVar[String]("")
  
  /**
   * Renders an RSS feed using a list
   */
  def render(feedUrl: String): NodeSeq = {
    val feed = getFeed(feedUrl)
 
    var src = new Queue[Node]()
    
 
    //src += <li class="rsswidgettitle"><b><a href={ (feed \ "channel" \ "link").text }>{ ( feed \ "channel" \ "title" ).text }</a></b></li>
 
    for (c <- findElems(feed){_.label == "item"}) {
      val img = {(c \\ "thumbnail")}
      val url = {img  \ "@url"}
      val imgElem = <img src={url}/>
     
      src += <li class="rsswidgetitem"><a href={(c \\ "link").text}>{(c \\ "title").text}</a><br/>{(c \\ "summary").text}</li>
      
    }    
    
    <div class="rsswidget"><ul>{src}</ul></div>
  }
 
  /**
   * Returns the feed as a plain XML
   */
  def getFeed(feedUrl: String): Elem = {
    val u = new URL(feedUrl)
    val con = u.openConnection
    val is = con.getInputStream
    doClose(is) {
      XML.load(is)
    }
  }
    
  
  def addAtomLinks(xhtml:NodeSeq):NodeSeq = {
	    val linkURL = "http://" + S.hostName +":8080" + S.contextPath + "/api/content/atom"
        
	    <head>
	    <link rel="alternate" type="application/atom+xml" 
	    	title="Democritus CMS in English" hreflang="en-US"
	    	href={linkURL}/>     
	    </head>
        <a href={linkURL}><img src="/images/feed-icon.png"/></a> 
     }
}
