
import akka.Done
import com.glassbeam.model.{Logger, Opsdb}

object ContextMain extends Logger {
  import com.glassbeam.context.ContextSupervisor._

  private final val logger = Logging(this)

  def main(args: Array[String]): Unit = {
    val odb = Opsdb
    odb.init()
    val contextSupervisor = system.actorOf(props,name)
    contextSupervisor ! Done
  }


}