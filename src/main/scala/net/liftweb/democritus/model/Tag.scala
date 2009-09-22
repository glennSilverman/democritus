package net.liftweb.democritus.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http._
import java.util.Date
import _root_.scala.xml._
import Helpers._


object Tag extends Tag with MetaMegaTreeItem[Tag] {
 
  def isAuthorized():Boolean = true 
 
}

class Tag extends MegaTreeItem[Tag]{
  def getSingleton = Tag
  def owner = Tag
  
  def toAtom = {
    
    val id = "http://www.apgarbullard.com/model/tag/" + this.id
    
    <entry xmlns="http://www.w3.org/2005/Atom">
      <tag>
      	<id>{id}</id>
        <name>{itemName.is}</name>
      </tag>
    </entry>
   }
        
}

