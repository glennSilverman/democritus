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


object Role extends Role with MetaMegaTreeItem[Role] {
 
  def isAuthorized():Boolean = User.isa_?("admin") 
  override def dbTableName = "Role"
 
}

class Role extends MegaTreeItem[Role]{
  def getSingleton = Role
  def owner = Role
  
}


