package net.liftweb.democritus.snippet

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
import _root_.scala.collection.mutable._


class UserView(entity:User, snippet:ManageUsers) extends ModelView[User](entity, snippet){
  override lazy val editAction = TheBindParam("edit", snippet.link("edit", ()=>load, Text(S?("Edit Roles"))))   
  
}


class ManageUsers extends ModelSnippet[User]{ 
  
   val view =  new UserView(new User, this)
  
  /**
   * The list snippet
   */
  def list(ns: NodeSeq): NodeSeq =  User.currentUser.map({user => 
      
        User.findAll.flatMap({u =>         	
            val v = new UserView(u, this)
            v.load
            val e = v.entity 
            
            bind("user",  chooseTemplate("user", "entry", ns),
                 "firstname" ->  Text(u.firstName.is),
                 "lastname" -> Text(u.lastName.is),
                 "email" -> Text(u.email.is),
                 "roles" -> Text(u.roles.all.map(r=>r.itemName.is).mkString(", ")),
                 v.editAction
                 
                 )
          })
			}) openOr Text("You're not logged in")
  
   
   
      
  /**
   * The edit snippet
   */
  def edit(ns: NodeSeq): NodeSeq =  { 
               
	  val theUser = view.entity
        
      val possible = theUser.validSelectValues.open_!
      val current = theUser.roles.all.map(c => c.id.toString)
      val updated = Set.empty[Role]
            
      def loadRole(ids: List[String]) = { 
        Log.info("Loading roles...")        
        for(x <- ids){
          
          Log.info("Loading role " + x)
          updated += Role.findAll(By(Role.id, x.toInt)).first 
         }
      } 
      
      def saveRoles = {
          Log.info("Saving roles...")
          theUser.roles.clear
    	  for(r <- updated) {
    	    Log.info("Saving role " + r.id.is)
    	    theUser.roles += r
          }
          theUser.save
          redirectTo("/list")
      }            

      bind("user", ns,
              "name" -> Text(theUser.firstName.is + " " + theUser.lastName.is),
              "roles" -> SHtml.multiSelect(possible, current, loadRole(_), ("title","Click to Select a Role")),
              "submit" -> SHtml.submit(S?("Save"), ()=> saveRoles)
          )
              
 
   
    } 
  
}
