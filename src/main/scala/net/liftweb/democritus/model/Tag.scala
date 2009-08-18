package net.liftweb.democritus.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import java.util.Date
import _root_.scala.xml._
import Helpers._

abstract class TagContent
case object NullTagContent extends TagContent
case class FullTagContent(content: Content) extends TagContent

object Tag extends Tag with LongKeyedMetaMapper[Tag] {
  def byName (name : String) = 
    findAll(By(Tag.name, name)) match {
      case tag :: rest => tag
      // create a tag for the given name if it doesn't exist... 
      case Nil => Tag.create.name(name).saveMe
    }
}
class Tag extends LongKeyedMapper[Tag] with IdPK {
	
  def getSingleton = Tag
  
  object name extends MappedPoliteString(this, 64) {
    override def setFilter = notNull _ :: trim _ :: super.setFilter 
  }
   
  
  def toAtom = {
    
    val id = "http://www.apgarbullard.com/model/tag/" + this.id
    
    <entry xmlns="http://www.w3.org/2005/Atom">
      <tag>
      	<id>{id}</id>
        <name>{name.is}</name>
      </tag>
    </entry>
   }
        
}
