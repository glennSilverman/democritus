package net.liftweb.democritus.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.util.Helpers
import Helpers._
import _root_.net.liftweb.widgets.menu._

class HelloWorld {
  def howdy(in: NodeSeq): NodeSeq =
    Helpers.bind("b", in, "time" -> (new _root_.java.util.Date).toString)
  
  def renderMenu(xhtml:NodeSeq):NodeSeq = {    
   MenuWidget("admin" :: Nil)    
  }
  
  def renderAdminMenu(xhtml:NodeSeq):NodeSeq = { 
      MenuWidget("administration" :: Nil)    
  }
}

