package net.liftweb.democritus.snippet

import _root_.scala.xml._
import net.liftweb._
import http._

import _root_.scala.xml.{NodeSeq, Node, Elem, PCData, Text, Unparsed}
import _root_.net.liftweb.http.{LiftRules, S}
import SHtml._
import S._

import js._
import JsCmds._
import JE._
import net.liftweb.http.js.jquery._
import JsCmds._
import JqJE._

import mapper._

import util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text}


import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js.JsExp


object TreeTableView {

  def apply(id: String, jsObj: JsObj) = new TreeTableView().onLoad(id, jsObj)
  
  
  /**
   * Call this function typically in boot
   */
  def init() {
    import _root_.net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "treeTable" :: _ => true
    })

  }
}

class TreeTableView {
	
  /**
   * Makes a static tree out of the <ul><li> lists. The tree is buid when page loads.
   *
   * @param id - the id of the empty <ul> element that will be populated with the tree
   * @param jsObj - the JSON object passed to the treeview function
   *
   */
  def onLoad(id: String, jsObj: JsObj) : NodeSeq = {
    <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/treeTable/jquery.treeTable.css"} type="text/css"/>
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/treeTable/jquery.treeTable.js"}/>
       <script type="text/javascript" charset="utf-8">{
         OnLoad(JqId(id) >> new JsExp with JQueryRight {
           def toJsCmd = "treeTable(" + jsObj.toJsCmd + ")"
         }) toJsCmd
       }
       </script>
    </head>

  }
  
  def onLoadTable() : NodeSeq = {
    <head>
      <script type="text/javascript" charset="utf-8">{
        Unparsed("""
         jQuery(document).ready(function() {
           $("tr.parent")
            .css("cursor","pointer")
            .attr("title","Click to expand/collapse")
            .click(function(){
			$(this).siblings('.child-'+this.id).toggle();
		});
           $('tr[@class^=child-]').hide().children('td');
         });""")
        
      }</script>
    </head>
  }
  
  
}


