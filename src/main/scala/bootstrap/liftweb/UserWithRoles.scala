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
    
    //Check if user with admin role exists. Create if none exists
    if(UserRole.findUserRoles("admin").isEmpty ){    
      
	    //Add admin role if none exists  
	    val role:Role =	Role.findAll(By(Role.name, "admin")) match {
	    	 case Nil => {
	    		  val r = Role.create.name("admin")
	              r.save
	              Log.info("Admin role created")
	    	  	  r
	          }
	    	  case x => {
	    	    Log.info("Admin role already exists")
	    	    x.head
	    	  }
	       }
	    
	      
    
          //Create Admin user if none exists
	      val user:User = User.findAll(By(User.email, "admin@test.com")) match {
	          case Nil => {
	        	  val u = User.create.firstName("admin").lastName("admin") 
	        	  	.locale("en_US").timezone("American/Los Angeles").email("admin@test.com").password("admin")
	        	  	u.save
	        	  	u.validated(true)
	                Log.info("New admin user created")
	                u
	             }
	        	 case u => {
	        	   Log.info("Admin user already exists")
	        	   u.head
                 }
	      }
	           
	      //Assign admin role to admin user      
	      user.roles += role
	      user.save
	      Log.info("Admin role added to admin user")
	            
	}
         
    
    Log.info("UserWithRoles initialized")
  }
 }
