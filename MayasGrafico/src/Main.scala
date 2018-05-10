


  import scala.swing._
import javax.swing.JOptionPane
import java.io.FileWriter
import java.util.Date
import java.text.DateFormat
import java.text.DateFormat._
import java.text.SimpleDateFormat

object Main{
def main(args:Array[String]) {
    do {
    
      val juego = obtenerTablero()    
      val label = juego.texto
       val alerta = juego.alerta
       val gridBotones= new GridPanel(12,12){ contents ++= juego.bloques }//(25,15)
       val gridTextos= new GridPanel(2,1){
         contents += alerta
         contents += label
         
       }
       
      val panel = new GridPanel(2,1){ 
      contents += gridBotones 
      contents += gridTextos      
    } 
   //
    val main = new MainFrame{
      title = "Bienvenido al Juego de los Bloques Mayas"
      contents = panel
      size = new Dimension(1000,1000)
      centerOnScreen
   }
     main.visible = true      
     val puntuacion = juego.iniciarJuego
      main.close
      
      //Se muestra información de la puntuación y se da la oportuinidad de almacenar la puntuación en un fichero sí así lo desea el usuario.
      
      JOptionPane.showMessageDialog(null, "¡PARTIDA FINALIZADA! La puntuación obtenida es de " + puntuacion, "Partida finalizada", JOptionPane.INFORMATION_MESSAGE)
      if (preguntarSiNo("¿Desea almacenarla la puntuación que ha obtenido? ")){
        val nombre = JOptionPane.showInputDialog("Escribe tu nombre: ",JOptionPane.QUESTION_MESSAGE); //Se obtiene una cadena.
      
        guardarPartida(puntuacion,juego.getDif,nombre) //Guarda los datos de la partida.
      }
    } while (preguntarSiNo("¿Desea seguir jugando?")) //Permite que se siga con el juego, siempre y cuando se desee.
      JOptionPane.showMessageDialog(null, "¡Muchas gracias por jugar!", "Partida finalizada", JOptionPane.INFORMATION_MESSAGE)
  }

  def guardarPartida(puntuacion: Int, dificultad: Int, nombre: String) {
  //Recibe la puntuación, la dificultad y el nombre de la persona que ha hecho la partida.
    
    //Abre un fichero (o lo crea en caso de no existir) con la modalidad de escribir al final de dicho fichero.
    val fichero = new FileWriter("puntos-dificultad-" + dificultad + ".txt", true)
    try {
      val ahora = new SimpleDateFormat("dd/MM/yyyy 'a las' HH:mm:ss").format(new Date) //PErmite obtener la fecha actual.
      fichero.write(puntuacion + " puntos de " + nombre + " obtenidos el " + ahora + "\r") //Efectúa la escritura en el fichero abierto.
      JOptionPane.showMessageDialog(null, "Tu partida ha sido guardada", "Partida guardada", JOptionPane.INFORMATION_MESSAGE)
    } catch {
      case _: Throwable => JOptionPane.showMessageDialog(null, "Error al guardar la puntuación.", "Partida no guardada", JOptionPane.ERROR_MESSAGE)
       //Si fallara la apertura del fichero, no se guarda.
    } finally { fichero.close() } //Pase lo que pase, se debe cerrar el fichero abierto (excepciones de IO por ejemplo, podría dar al traste la escritura).

  }
  def preguntarSiNo(msg: String): Boolean = {
    //Este método hace una pregunta a la que se debe responder con sí o no. Devolverá true si sí o false si se responde no. Se recibe el mensaje de petición.
    val guardar =  JOptionPane.showOptionDialog(null,msg, "Selector de opciones",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE,null,Array[Object]("Sí","No"),null) //Se responde
    guardar match { //Se analiza la respuesta.
      case 0 => true
      case 1 => false
      case _ => { //En caso de que no se haya respondido sí o no, la respuesta es fallida y se vuelve a hacer la petición.
        JOptionPane.showMessageDialog(null, "Error", "Error al introducir el valor", JOptionPane.ERROR_MESSAGE)
        preguntarSiNo(msg)
      }
    }
  }


  def obtenerTablero():Tablero = {
    val dificultad = JOptionPane.showOptionDialog(null,"Seleccione una nivel de dificultad", "Niveles",JOptionPane.YES_NO_CANCEL_OPTION,JOptionPane.QUESTION_MESSAGE,null,Array[Object]("BAJA","MEDIA","ALTA"),null)
     dificultad match{
        case 0 =>new Tablero(0,2,8,3)
        case 1 =>new Tablero(0,3,10,5)
        case 2 =>new Tablero(0,5,15,7)
        case _ => obtenerTablero()
      }
    
  }
}
