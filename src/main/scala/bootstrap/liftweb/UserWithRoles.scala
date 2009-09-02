package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.mapper.view._
import _root_.java.sql.{Connection, DriverManager}
import _root_.net.liftweb.democritus.model._

object UserWithRoles {
	
  def init = {
    LiftRules.addToPackages("net.liftweb.usermon")
    Schemifier.schemify(true, Log.infoF _, Role, UserRole)
    
    val sitemap = LiftRules.siteMap match {
      case Full(sm) => LiftRules.setSiteMap(SiteMap(sm.menus ::: Role.menus:_* )) 
      case _ => LiftRules.setSiteMap(SiteMap(Role.menus:_*))
    }
    
    if(UserRole.findUserRoles("admin").isEmpty){
            
    		val role:Role = Role.findAll(By(Role.name, "admin")) match {
    		  case Nil => {
    			  val r = Role.create.name("admin")
                  r.save
    		  	  r
                }
    		  case x => x.head
            }
            
            Log.info("New role, " + role.name.is + ", created")
            val user = User.create.firstName("admin").lastName("admin") 
		    .locale("en_US").timezone("American/Los Angeles").email("admin@test.com").password("admin")
            user.save
            user.validated(true)
            user.roles += role
		    user.save	    	    
		 }
         else Log.info("Admin role already exits...")
    
    Log.info("UserWithRoles initialized")
  }
 }
