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

trait DocumentReady {
  
  def head(script:JsCmd) = <head>{Script(script)}</head>
      
  def loadWidget(widget:String, id:String, jsObj: JsObj): JsCmd = 
         OnLoad(JqId(id) >> new JsExp with JQueryRight {  
		        def toJsCmd = widget + "(" + jsObj + ")" 
		      })
}

object JQueryUI extends JQueryUI{
  
  def apply(ui:String, id:String, jsObj:JsObj) = JQueryUI.renderUI(ui, id, jsObj, Empty)
  def apply(ui:String, id:String, jsObj:JsObj, in:JsExp) = JQueryUI.renderUI(ui, id, jsObj, Full(in))
  
  def init() {
    import _root_.net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "superfish" :: _ => true      
      case "ui" :: _ => true 
      case "wymeditor" :: _ => true
    })     
  }   
  
}

object JqContainer {
  def apply(in:String):JsExp with JQueryRight with JQueryLeft =
    new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "addClass('" + in + "')"
    } 
}

object JqAccordion {  
    def apply(in: JsExp): JsExp with JQueryRight with JQueryLeft =  
      new JsExp with JQueryRight with JQueryLeft {  
        def toJsCmd = "accordion("+in.toJsCmd+")"  
      }  
    
   def apply(): JsExp with JQueryRight with JQueryLeft =  
      apply(JsRaw("")) 
   
   def activate(in: String):JsExp with JQueryRight with JQueryLeft =
     new JsExp with JQueryRight with JQueryLeft {
       def toJsCmd = "accordion('activate'," +  in + ")"
     }
  } 
 
class JQueryUI extends DocumentReady {
  
  
  def renderUI(ui:String, id:String, jsObj:JsObj, in:Box[JsExp]):NodeSeq = 
    ui match {
      case "tabs" => renderTabs(id, jsObj, in)
      case "accordion" => renderAccordion(id, jsObj, in)      
      case _ => Nil
    }
      
  def renderTabs(id:String, jsObj:JsObj, in:Box[JsExp]):NodeSeq = {
     val tab_items = "t" + Helpers.nextFuncName
     val tabs_func = "tf" + Helpers.nextFuncName
        
     def renderLi = Jx(<li><a href={JsVar("it", "href")}><span>{JsVar("it", "text")}</span></a></li>)     
	 def items = JsCrVar(tab_items, jsObj)  
     def func = JsCrVar(tabs_func, Jx(<ul>
         {
           JxMap(JsVar("it.tabs"), renderLi)
             
         }
       </ul>).toJs)     
     def renderUl =  JsFunc("append", Call(tabs_func, JsVar(tab_items)))
              
     head(items & func & 
             OnLoad(JqId(id)~> renderUl) & 
             OnLoad(JqId(id) >> JqTabs(in openOr JsRaw(""))))               	
       
  }
  
  def renderAccordion(id:String, jsObj:JsObj, in:Box[JsExp]):NodeSeq = {
    val accordion_items = "a" + Helpers.nextFuncName
    val accordion_func = "af" + Helpers.nextFuncName
    
    def renderDiv = Jx(<h3><a href={JsVar("it", "href")}>{JsVar("it", "text")}</a></h3><div></div>)   
    def items = JsCrVar(accordion_items, jsObj)
    def func = JsCrVar(accordion_func, Jx(
         {
           JxMap(JsVar("it.accordion"), renderDiv)
         }
        ).toJs)
    
    def renderUl =  JsFunc("append", Call(accordion_func, JsVar(accordion_items)))
    
    head(items & func & 
           OnLoad(JqId(id) ~> renderUl) &
    	   OnLoad(JqId(id) >> JqAccordion(in openOr JsRaw(""))))
  
  }
  
  def tabs(xhtml:NodeSeq):NodeSeq = {
    val id = S.attr("id") openOr ""
    val in = S.attr("in") openOr "" 
    head(OnLoad(JqId(id) >> JqTabs(JsRaw(in))))
  }
  
  def accordion(xhtml:NodeSeq):NodeSeq = {
    val id = S.attr("id") openOr ""
    val in = S.attr("in") openOr "" 
    head(OnLoad(JqId(id) >> JqAccordion(JsRaw(in))))
  }
  
   /* The markup for a ui container
     <div class="ui-widget">
    	<div class="ui-widget-header">This is header text</div>
    	<div class="ui-widget-content">This is content text</div>
    </div> 
 */   
  def renderContainer(xhtml:NodeSeq):NodeSeq = {
    val id = S.attr("id") openOr ""
    head(OnLoad(JqId(id) >> JqContainer("ui-widget")) &
    		OnLoad(Jq("#" + id + " > div:first") >> JqContainer("ui-widget-header")) &
    		OnLoad(OnLoad(Jq("#" + id + " > div:last") >> JqContainer("ui-widget-content"))))

 } 
  
 def accordionActivate(xhtml:NodeSeq):NodeSeq = {
   val index = S.attr("index") openOr "-1"
   val id = S.attr("id") openOr ""
   head(OnLoad(JqId(id) >> JqAccordion.activate(index)))
 } 
 
 
  def onInit(xhtml: NodeSeq):NodeSeq =
     <head> 
       <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/ui/css/smoothness/jquery-ui-1.7.1.custom.css"} type="text/css"/>       
       <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/ui/js/jquery-ui-1.7.1.custom.min.js"}/>
       <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/wymeditor/jquery.wymeditor.pack.js"}></script>
       <script type="text/javascript" charset="utf-8">{
        Unparsed("""
         jQuery(document).ready(function() {
            jQuery('.wymeditor').wymeditor();
          })
         """)
       }
      </script>    
   </head>      
  
}
