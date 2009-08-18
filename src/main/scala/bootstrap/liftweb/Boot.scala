package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import _root_.java.sql.{Connection, DriverManager}
import _root_.net.liftweb.democritus.model._
import _root_.javax.servlet.http.{HttpServletRequest}
import net.liftweb.democritus.snippet._
import net.liftweb.democritus.api._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)

    // where to search snippet
    LiftRules.addToPackages("net.liftweb.democritus")
    Schemifier.schemify(true, Log.infoF _, User, Content, ContentTag, Tag, ContentAdmin)

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) :: User.sitemap ::: Content.menus
    LiftRules.setSiteMap(SiteMap(entries:_*))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)
    
    LiftRules.dispatch.prepend(RestAPI.dispatch)
    
    LiftRules.rewrite.append(rewriteContent)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)
    
    LiftRules.resourceNames = "democritus" :: Nil
    
    JQueryUI.init

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
 private def makeUtf8(req: HTTPRequest): Unit =
{req.setCharacterEncoding("UTF-8")} 
 
 def rewriteContent:LiftRules.RewritePF = {
   case RewriteRequest(
    		ParsePath(List("contents",_),_,_,_),_,_) =>
    			RewriteResponse("content" :: Nil)
 }

}

/**
* Database connection calculation
*/
object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4

  private def createOne: Box[Connection] = try {
     val driverName: String = Props.get("db.driver") openOr
    "com.mysql.jdbc.Driver"

    val dbUrl: String = Props.get("db.url") openOr
    "jdbc:mysql://localhost:3306/lift_democritus?user=<username>&password=<password>"
    

    Class.forName(driverName)

    val dm = (Props.get("db.user"), Props.get("db.password")) match {
      case (Full(user), Full(pwd)) =>
	DriverManager.getConnection(dbUrl, user, pwd)

      case _ => DriverManager.getConnection(dbUrl)
    }

    Full(dm)
  } catch {
    case e: Exception => e.printStackTrace; Empty
  }

  def newConnection(name: ConnectionIdentifier): Box[Connection] =
    synchronized {
      pool match {
	case Nil if poolSize < maxPoolSize =>
	  val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

	case Nil => wait(1000L); newConnection(name)
	case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
            pool = xs
            poolSize = poolSize - 1
            x.close
            newConnection(name)
          } catch {
            case e => newConnection(name)
          }
        }
      }
    }

  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}


