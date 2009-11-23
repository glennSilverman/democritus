package net.liftweb.democritus.auth

import _root_.net.liftweb.http.auth.{AuthRole}
import _root_.net.liftweb.common.{Box, Full, Empty} 
import net.liftweb.democritus.model._


object Authorizer{  
  
  def createAuthRole: Box[_root_.net.liftweb.http.auth.Role] = 
  
      User.isa_?("admin") match {
      case true => Full(new _root_.net.liftweb.http.auth.Role{
        def name = "admin"})
      case false => Empty
    }
  
  
  
}
