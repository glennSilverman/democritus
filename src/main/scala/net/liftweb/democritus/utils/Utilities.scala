package net.liftweb.democritus.utils

import java.util.Date
import java.text.SimpleDateFormat
import java.util.Locale;
import java.util.TimeZone;
import java.io._

 
import _root_.scala.xml._
import net.liftweb._
import http._
import util._
import S._
import SHtml._
import Helpers._
import _root_.scala.collection.mutable._

object Utilities {

  val slashDate = new SimpleDateFormat("MM/dd/yyyy")
  
  val pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'"
  val GMT_TIME_ZONE = TimeZone.getTimeZone("GMT")
  
  val rfcDate = new SimpleDateFormat(pattern, Locale.US)
  
  type DateConverter = String => Date
  
  type DateFormatter = Date => String
  
  def parseDate(value:String, converter:DateConverter):Box[Date] = {
     
    try{
       Full(converter(value))        
      
    }catch {
      
      case e => {
        
        Empty
      }
    }
  }
  
  def formatDate(value:Date, formatter:DateFormatter):Box[String] ={
    try{
      Full(formatter(value))
      
    }catch {
      case e => Empty
    }
  }
  
  def getDateParam(name:String, converter:DateConverter):Box[Date] = {
    S.param(name).map(parseDate(_, converter)) openOr Empty
  }
  
  def addElemClass(in: Node, name:String, value:String): Box[Elem] = in match {      
      	case e: Elem => Full(e % new UnprefixedAttribute(name, Text(value), Null))
        case _ => Empty
      }
  
  def dataDir = S.hostName match {
    case "localhost" => S.?("data_dev")
    case _ => S.?("data")
  }
  
  def loadHtml(link:String) = link match {
      case "" => ""
      case _ => loadFile(dataDir + link + ".html").toList.mkString(",")
      
     }

  private def loadFile(filePath:String) = {
    
    val file = new File(filePath);
    var src = new ListBuffer[String]()
    
    try {
      val fis = new FileInputStream(file)

      // Here BufferedInputStream is added for fast reading.
      val bis = new BufferedInputStream(fis);
      val dis = new DataInputStream(bis)

      // dis.available() returns 0 if the file does not have more lines.
      while (dis.available() != 0) {
        val ln = dis.readLine()
        println("Read line in html = " + ln)
        if(ln.length > 0)
        	src += ln
      }

      // dispose all the resources after using them.
      fis.close();
      bis.close();
      dis.close();      

    } catch {
      case e => 
    }
    
    src
  
  }
  
  def writeHtml(link:String, contents:String) = link match {
      case "" => ""
      case _ => writeFile(new File(dataDir + link + ".html"), contents)
     }
  
  private def writeFile(aFile:File, contents:String) = {
    //use buffering
    var output = new BufferedWriter(new FileWriter(aFile));
    try {
      //FileWriter always assumes default encoding is OK!
      output.write( contents );
    }
    finally {
      output.close();
    }
  }
  
  def isValidElem(in:String):Boolean = {
    Log.info("Validating: " + in)
    var temp = in
    if(temp equals ""){ 
      temp = "<span></span>"
      true
    }
    
    try {
    	  XML.loadString(temp)
          true
        }catch {
          case e => e.getMessage
          false
        }
    }	
  

}
