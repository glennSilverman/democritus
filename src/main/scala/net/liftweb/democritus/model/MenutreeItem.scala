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


object MenutreeItem extends MenutreeItem with MetaMegaTreeItem[MenutreeItem] {
 
  def isAuthorized():Boolean = User.isa_?("admin") 
  override def dbTableName = "Nav_Item"
 
}

class MenutreeItem extends MegaTreeItem[MenutreeItem]{
  def getSingleton = MenutreeItem
  def owner = MenutreeItem
  
}
