package net.liftweb.democritus.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http._
import SHtml._
import java.util.Date
import _root_.scala.xml._
import Helpers._
import _root_.scala.collection.mutable._
import S._
import js.JsCmds._

trait MetaMegaTreeItem[ModelType <: MegaTreeItem[ModelType]] extends KeyedMetaMapper[Long, ModelType] {
  self: ModelType =>   
  
  lazy val Prefix = calcPrefix  
  def calcPrefix = List(dbTableName)  
  
  def isAuthorized():Boolean
    
  def listTreeItemsMenu = 
    Menu(Loc("List" + Prefix, "list" + Prefix :: Nil,  "List " + dbTableName + "s",                  
                  listItems, listSnippets, If(/*() => User.isa_?("admin")*/ isAuthorized, S.?("not_authorized")))) 
  
  def addTreeItemMenu = 
	 Menu(Loc("Add" + Prefix, "add" + Prefix :: Nil, "Add  " + Prefix, listItems, Hidden))
  
  def menus:List[Menu] = listTreeItemsMenu :: Nil
  
  lazy val listSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "items.addNew" => addNew(create, S.??("Created"))
      case "items.list" => list
    }
  }
    
  def addNew(item: ModelType, noticeMsg: String)(xhtml:NodeSeq):NodeSeq =  
	    bind("item", xhtml, 
	       	"addNew" -> {SHtml.a({ ()=> 
	       	  SetHtml("item-save", edit(item))},
              Text("Create") 
	       	  )}
	         ) 
      
  def edit(item: ModelType) =        
      <form>
  		{item.toForm(Full("Save"), { _.save })}
      </form>
 
  def delete(item:ModelType) = {
         item.delete_!
       }
  
  def list(ns: NodeSeq): NodeSeq =  User.currentUser.map({user => 
       findAll.flatMap({e => 
            bind("item", chooseTemplate("item", "entry", ns),
                 "name" -> Text(e.itemName.is),
                 "parent" -> Text(e.parentName),
                 "actions" ->{ link("", () => delete(e), Text("Delete")) ++ Text(" ") ++
                               SHtml.a( {() =>                                                                       
                                 SetHtml("item-save", edit(e))}, Text("Edit")) }
                )
          })
			}) openOr Text("You're not logged in")
  
  
  def listItems = Template({ () =>
  <lift:surround with="default" at="content">
   <h3>{dbTableName + "s"}</h3>
    <div id="entryform">      
      <table>
        <tr>
	        <th>{dbTableName + " name"}</th>
            <th>Parent</th>
	        <th>
                <lift:items.addNew>        
	        	   <item:addNew/>
	        	</lift:items.addNew>	        	
            </th>	        	        
        </tr>        
         <lift:items.list>
         	<item:entry>
		      <tr>
		        <td style="vertical-align: top"><item:name /></td>
	            <td style="vertical-align: top"><item:parent /></td>
                <td style="vertical-align: top"><item:actions/></td>	
		      </tr>
           </item:entry>           
          </lift:items.list>
	  </table>
      </div>
      <hr />      
      <div id="item-save"/>      
      
</lift:surround>
  })

}

trait MegaTreeItem[T <: MegaTreeItem[T]] extends KeyedMapper[Long, T]{
  self: T =>
  def owner: T with MetaMegaTreeItem[T]
  
  
  override def primaryKeyField = id
  
  // the primary key for the database
  object id extends MappedLongIndex(this)                                        
  
  // First Name
  object itemName extends MappedString(this, 50) {
    override def displayName = fieldOwner.itemNameDisplayName
    override def setFilter = notNull _ :: trim _ :: super.setFilter 
    override val fieldId = Some(Text("txtItemName"))
  }

  def itemNameDisplayName = ??("Name") 
  
  object parent extends MappedLongForeignKey(this, owner){
    override def dbIndexed_? = true   
    override def validSelectValues: Box[List[(Long, String)]] = 
      Full(
        List[(Long, String)]((-1, "NA")) ::: owner.findAll.map( x => (x.id.is, x.itemName.is) ))
    
    override def displayName = "Parent:"
  }
  
  def parentName =
	  parent.obj.map(_.itemName.is) openOr "NA"
  
  def parentId =
    parent.obj.map(_.id.is) openOr -1
  
  def parents = owner.findAll(By(owner.parent, owner.id))
  
  def byItemName (name : String) = 
    owner.findAll(By(owner.itemName, name)) match {
      case o :: rest => o
      // create a treeItem for the given name if it doesn't exist... 
      case Nil => owner.create.itemName(name).saveMe
    } 
  
  
}


