

import scala.swing._
import scala.swing.Button
import scala.swing.event._

class Bloque(id: Int,tab:Tablero) extends Button {
      listenTo(this)
        var nClicks = 0
        reactions += {
          case ButtonClicked(b) =>
            tab.buffer=b.asInstanceOf[Bloque].getId()::tab.buffer
        }  
  
  def getId() = id
  
}