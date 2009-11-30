package net.liftweb.democritus.model

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 * 
 */

//import _root_.scala.collection.immutable._
import _root_.net.liftweb.common._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import js._
import js.JsCmds._
import js.jquery._
import JE._
import JqJsCmds._
import JqJE._
import SHtml._
import java.util.Date
import _root_.scala.xml._
import Helpers._
import _root_.scala.collection.mutable._
import S._
import net.liftweb.democritus.snippet._
import _root_.net.liftweb.widgets.tree._


trait MetaMegaTreeItem[ModelType <: MegaTreeItem[ModelType]] extends KeyedMetaMapper[Long, ModelType] {
  self: ModelType =>   
  
  lazy val Prefix = calcPrefix
  lazy val MenuName_List = "List" + Prefix
  lazy val MenuName_Add = "Add" + Prefix
  lazy val MenuName_ListView = "Listview" + Prefix
  lazy val MenuName_TreeView = "Treeview" + Prefix
  lazy val MenuTitle_Add = "Add " + Prefix.head
  lazy val MenuName_Edit = "Edit " + Prefix
  
  def calcPrefix = List(dbTableName)  
  
  def isAuthorized():Boolean
  
  def hasKids(n:ModelType) = findAll.filter(e => e.parentId == n.id.is) match {
    case Nil => false
    case _  => true
  }
    
  def listTreeItemsMenu = 
    Menu(Loc(MenuName_List, "list" + Prefix :: Nil,  "List " + dbTableName + "s",                  
                  listItems, listSnippets, If(/*() => User.isa_?("admin")*/ isAuthorized, S.?("not_authorized")))) 
  
  def buildListViewMenu = 
    Menu(Loc(MenuName_ListView, "list" + Prefix :: Nil,  "List View",                  
                  listItems, listSnippets, If(/*() => User.isa_?("admin")*/ isAuthorized, S.?("not_authorized")),
    	 Hidden)) 
  
  def buildTreeViewMenu = 
	 Menu(Loc(MenuName_TreeView, "tree" + Prefix :: Nil, "Tree View",
			      treeItems, treeSnippets, If(/*() => User.isa_?("admin")*/ isAuthorized, S.?("not_authorized")),
         Hidden)) 
  
  def menus:List[Menu] = listTreeItemsMenu :: buildListViewMenu :: buildTreeViewMenu :: Nil
  
  lazy val listSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "items.addNew" => addNew(create, S.??("Created"))
      case "items.list" => list      
    }
  }
  
  lazy val treeSnippets = new DispatchLocSnippets {
    val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "items.buildTree" => buildTree
      case "items.addNew" => addNew(create, S.??("Created"))      
    }
  } 
    
  def buildTree(in:NodeSeq):NodeSeq = {    
   
       val func = AnonFunc( SHtml.ajaxCall(JsRaw("this.id"), (id: String) =>
        SetHtml("item_edit", owner.find(id).map(edit) openOr
        		NodeSeq.Empty))._2 ) 		
       
       TreeView("tree", JsObj(("persist", "location"), ("toggle",  func)), loadTree, loadNode)
    }
  
     
  def node(n:ModelType) = {
    def anchor = n.itemName.is
    if(hasKids(n))
       Tree(anchor, n.id.is.toString, true) 
    else
       Tree(anchor, n.id.is.toString, false)
 }
  
  def loadTree() = 
    findAll.filter(e => e.parentId == -1).map(e => node(e))
  
  
  def loadNode(ids: String): List[Tree] = 
    ids match {
    case "none" => Nil
    case x => findAll.filter(e => e.parentId.toString equals x).map(e => 
     	node(e))
    }
    
  def addNew(item: ModelType, noticeMsg: String)(xhtml:NodeSeq):NodeSeq = 
    bind("item", xhtml, 
	       	"addNew" -> {SHtml.a({ ()=> 
	       	  SetHtml("item_create", edit(item))},
              Text(MenuTitle_Add) 
	       	  )}
	         )
  
  def treeActions(xhtml:NodeSeq):NodeSeq = {
    
    def deleteMe = {
      Log.info("Entering DeleteMe...")
    }
            
	 bind("treeItem", xhtml,
	         "actions" -> {link("", ()=>deleteMe, Text("Delete"))++Text(" | ")++
	             link("", ()=>true, Text("Save"))}
	         )
      
      
  }
  
  def edit(item:ModelType) =
    <form>{item.toForm(Full("Save"), { _.save })}</form>
    
  def delete(item:ModelType) = {
         item.delete_!
       }
    
  def list(ns: NodeSeq): NodeSeq =    
      
     User.currentUser.map({user => 
       findAll.flatMap({e =>           
            bind("item", chooseTemplate("item", "entry", ns),
                 "name" -> Text(e.itemName.is),
                 "parent" -> Text(e.parentName),
                 "actions" ->{ link("", () => delete(e), Text("Delete")) ++ Text(" ") ++
                                SHtml.a( {() =>
                                 SetHtml("item_create", edit(e))}, Text("Edit")) }
                )
          })
			}) openOr Text("You're not logged in") 
  
  
  
  def listItems = Template({ () =>
  <lift:surround with="default" at="content">
   <h3>{dbTableName + "s"}</h3>
    <div id="entryform"> 
      <lift:Menu.item name={MenuName_TreeView}/>   
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
      <div id="item_create"/>      
      
</lift:surround>
  })
  
  def treeItems = Template({ () =>
	  <lift:surround with="default" at="content">
	   <lift:items.buildTree/>       
	    <h3>{dbTableName + "s"}</h3>        
       <div id="entryform"> 
          <div class="column span-5"> 
             <lift:Menu.item name={MenuName_ListView}/>
             <ul id="tree"/>
          </div>
          <div class="column span-5">
            <div id="item_edit"/>
          </div>
       </div>
      <hr />
      <lift:items.addNew>
         <item:addNew/>                   
      </lift:items.addNew>         
      <div id="item_create"/>
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
  
  def parentId:Long =
    parent.obj.map(_.id.is) openOr -1
  
  def parents = owner.findAll(By(owner.parent, owner.id)) 
  
  
  def byItemName (name : String) = 
    owner.findAll(By(owner.itemName, name)) match {
      case o :: rest => o
      // create a treeItem for the given name if it doesn't exist... 
      case Nil => owner.create.itemName(name).saveMe
    } 
  
  def saveItem = save
  
}




