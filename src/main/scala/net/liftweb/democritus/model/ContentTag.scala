package net.liftweb.democritus.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import _root_.java.util.Date
import _root_.scala.xml._
import Helpers._

object ContentTag extends ContentTag with LongKeyedMetaMapper[ContentTag] {
  override def fieldOrder = Nil
  
  def join (tag:Tag, e:Content) = this.create.tag(tag).content(e).save
  
  def findTagContents (search : String) : List[Content] = 
    findAll(In(ContentTag.tag,
	       Tag.id,
	       Like(Tag.name, search))).map(_.content.obj.open_!).removeDuplicates
  
  def findAll(e:Content):List[ContentTag] = findAll(By(ContentTag.content, e))
}
class ContentTag extends LongKeyedMapper[ContentTag] with IdPK {
  def getSingleton = ContentTag
  
  object tag extends MappedLongForeignKey(this, Tag){
    override def dbIndexed_? = true
  }
  
  object content extends MappedLongForeignKey(this,Content) {
    override def dbIndexed_? = true
  } 
}

