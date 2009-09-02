package net.liftweb.usermon.snippet

import _root_.net.liftweb.mapper._
import view._
import _root_.net.liftweb.util._
import net.liftweb.democritus.model._
import _root_.net.liftweb.http._
import js.JsCmds._
import SHtml._
import S._
import _root_.scala.xml._
import Helpers._


class ManageRoles {
    
  object role extends RequestVar[Role](new Role)
      
  /**
   * The list snippet
   */
  def list(ns: NodeSeq): NodeSeq =  User.currentUser.map({user =>
              
       def delete(role:Role) = {
         UserRole.findByRole(role).map(_.delete_!)
         role.delete_!
       }
      
       
       Role.findAll.flatMap({e => 
            bind("role", chooseTemplate("role", "entry", ns),
                 "name" -> Text(e.name.is),
                 "parent" -> e.parentName,
                 "actions" ->{ link("listRoles", () => delete(e), Text("Delete")) ++ Text(" ") ++
                               SHtml.a( {() => 
                                 role(e)                                      
                                 SetHtml("role-save", edit)}, Text("Edit")) }
                 
              )
          })
			}) openOr Text("You're not logged in") 
      
  def edit = <form>
  		{role.is.toForm(Full("Save"), { _.save })}
      </form>
  
  
  def addNew(xhtml:NodeSeq):NodeSeq =  
	    bind("role", xhtml, 
	       	"addNew" -> {SHtml.a({ ()=> 
	       	  SetHtml("role-save", edit)},
              Text("Create Role") 
	       	  )}
	         ) 
      
  }


