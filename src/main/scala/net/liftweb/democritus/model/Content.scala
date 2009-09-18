package net.liftweb.democritus.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import auth.{AuthRole}
import java.util.Date
import _root_.scala.xml._
import Helpers._
import utils.Utilities._

 object Content extends Content with LongKeyedMetaMapper[Content] {  
  override def fieldOrder = List(title, link, description)
  
  def findByName (author : User, name : String) : List[Content] = 
    Content.findAll(By(Content.author, author.id.is), By(Content.title, title))
  
  
  override def afterSave = addTags _ :: Nil
    
  def addTags(entry: Content) {
    if(entry._tags ne null){
       entry._tags.foreach(ContentTag.join(_, entry))
    }
  }
  
  def delTags(entry:Content) =
    ContentTag.findAll(By(ContentTag.content, entry)).foreach(_.delete_!) 
 
  
  def listMenu = Menu(Loc("listContents", List("listContents"), "List Contents",
          LocGroup("admin"), If(() => User.isa_?("admin"), S.?("not_authorized"))
  		   /*HttpAuthProtected(() => Full(AuthRole("admin"))*/))
  
  
  def createMenu = Menu(Loc("createContents", List("editContent"), "Create Contents", Hidden, LocGroup("admin1")))
  
  //Content Menu - must include in siteMap for RewriteRequest to work
  val contentMenu = Menu(Loc("content", List("content") -> true, "Internal Content", Hidden))   
    
    //Contents Menu - link where all the sites content pages are stored
  val contentsMenu = Menu(Loc("contents", ("contents" :: Nil) -> true, "Internal Contents", Hidden))
                        
  def menus = listMenu :: createMenu :: contentMenu :: contentsMenu :: Nil
  
  def mostRecentContent:Box[Content] = 
    Content.findAll.sort((s,t) => s.lastUpdated.is.after(t.lastUpdated.is)) match {
      case Nil => Empty
      case x => Full(x.head)
    }

 }

class Content extends LongKeyedMapper[Content] with IdPK {

  def getSingleton = Content
  
  object author extends MappedLongForeignKey(this, User){
    //override def validSelectValues: Box[List[(Long, String)]] = 
    //	Full(User.findAll.map(u => (u.id.is, u.lastName.is)))
    override def dbIndexed_? = true
  }
  
  def admins = ContentAdmin.findAll(By(ContentAdmin.content, this.id))
  
  def addAdmin (user : User) = ContentAdmin.create.content(this).administrator(user).save
  
  object creationDate extends MappedDateTime(this)
  
  object lastUpdated extends MappedDateTime(this)
 
  object title extends MappedString(this, 100){
    override def displayName = "Title"   
   
    override def validations =  valMinLen(1, S.??("Title must not be empty")) _ :: super.validations  
    
  } 
 
  object description extends MappedTextarea(this, 255) {
      override def displayName = "Description" 
      override def _toForm = super._toForm match {
        case Full(e) => addElemClass(e,"style","height: 65px; width:360px")
        case Empty => Empty
        case Failure(_, _, _) => Empty 
        }
      
      override def validations =  valMinLen(1, S.??("Description must not be empty")) _ :: super.validations  
  }
  
  object link extends MappedString(this, 100){
    override def displayName = "Page link"
    override def validations =  valMinLen(1, S.??("Page must not be empty")) _ :: super.validations  
  }
  
  object types extends Enumeration {
    val Xhtml = Value("Xhtml")
    val File = Value("File")
    val Form = Value("Form")
  }
  
  object contentType extends MappedEnum(this, types){
    override def displayName = "Type"
  }
  
  private object _dbTags extends HasManyThrough(this, Tag, ContentTag, ContentTag.content, ContentTag.tag)
  
  private[model] var _tags : List[Tag] = _
  
  private val locker = new Object
  
  def tags : List[Tag] = locker.synchronized {
    if(_tags eq null){
      _tags = _dbTags()
    }
    _tags
  }
  
  def tags(newTags:String) = locker.synchronized {
    _tags = newTags.roboSplit(",").map(Tag.byName(_))
    this
  }
  
  def tags(newTags:List[Tag]) = locker.synchronized {
    _tags = newTags
    this
  }
  
  def tagsToo:List[Tag] = ContentTag.findAll(By(ContentTag.content, this.id)).map(_.tag.obj.open_!)
  
  def showTags = Text(tags.map(_.name.is).mkString(", "))
  
   // get a list of tags of the form <tag>tagname1</tag><tag>tagname2</tag>
   def showXMLTags: NodeSeq = tags.map(t => <ex:tag>{t.name.is}</ex:tag>)
  
  def toAtom = { 
   
    val id = this.id
    val linkURL =
       "http://" + S.hostName +":8080" + S.contextPath + "/contents/?tags=" + tags.map(_.name.is).mkString(",") + "&id=" + id
     
    val _author = User.findAll(By(User.id, author.is)).head
    
    <entry>
        <id>{id}</id>
        <title>{title.is}</title>
        <link href={linkURL}/>
        <author><name>{_author.firstName.is + " " + _author.lastName.is}</name></author> 
        <updated>{formatDate(lastUpdated.is, rfcDate.format) open_!}</updated>
        <summary>{description.is}</summary>  
        <content type="xhtml"><div xmlns="http://www.w3.org/1999/xhtml">{link.is}</div></content>
        <ex:tags>{showXMLTags}</ex:tags>  
    </entry>
    
  } 
    
  override def equals (other : Any) = other match {
    case e:Content if e.id.is == this.id.is => true
    case _ => false
  }
  
  override def hashCode = this.id.is.hashCode  
}

// Rights classes
class ContentAdmin extends LongKeyedMapper[ContentAdmin] with IdPK {
  def getSingleton = ContentAdmin

  object content extends MappedLongForeignKey(this, Content) {
    override def dbIndexed_? = true
  }

  object administrator extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }
}

object ContentAdmin extends ContentAdmin with LongKeyedMetaMapper[ContentAdmin] {
}