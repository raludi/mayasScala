
import scala.swing._
import java.awt.Color
import scala.util.Random
class Tablero(puntos:Int,bombas:Int,vidas:Int,dificultad:Int) {
  var buffer: List[Int] = List()
  val bloques = crearBloques(0)
  val texto = new Label
  val alerta = new Label
 def getDif: Int=dificultad
 val aleatorio = new Random
    //crea bloques que serán representados por botones 
 //Obtiene un entero que almacenara la puntuación y será pasada al main para ser mostrada por pantalla
 def iniciarJuego: Int = {
    val tablero = crearTablero(144) //Se genera el tablero con144 bloques.
    val pts = jugarPartida(vidas,puntos,12,12,tablero) //Se procede a empezar la jugada.
    pts
  }                

//Geeramos un tablero con colores aleatorios.  Dicho tablero incluirá un número de bombas dependiendo del nivel
  
def crearTablero(cantidadElem: Int): List[Color] = {
    insertarNBombas(144,bombas,rellenarColores(cantidadElem))
  }

 def rellenarColores(restantes: Int): List[Color] = restantes match {
    //Crea una lista con "restantes" números aleatorios que van entre 0 y dificultad-1.
    case 0 => Nil
    case _ => convertirAColor(aleatorio.nextInt(dificultad)+1) :: rellenarColores(restantes - 1)
  }
 def crearBloques(posicion: Int): List[Bloque] = {
    posicion match {
      //Imprime las posiciones del tablero.
      case 143          => new Bloque(143, this)::Nil
      case x if x > 144 => crearBloques(posicion % 144 + 1) //si es el último elemento de la línea, se salta a la siguiente
      case _           => new Bloque(posicion, this) :: crearBloques(posicion + 12); //En caso contrario, imprimer el elemento recibido y por recursion pintaría el siguiente, de 16 posiciones a la derecha.
    }
  }
  //def dificultadJuego(puntos:Int,vidas:Int,dificultad:String)={

 def puntuacionJuego(lista:List[Int]):Int= lista.length match{
    case 0=> 0
    case 1=> -1
    case 2=> -1
    case _=> 10*lista.length
  }
  def vidas(lista:List[Int]):Int= lista.length match{
    case 1=>  1
    case 2=>  2
    case _=>  0
  }
  /*Funciones de creacion del tablero.*/
  
  //Función auxiliar, recibe un entero y le asigna un color.
  def convertirAColor(numero:Int):Color= numero match{
    
    case 1 => Color.BLUE
    case 2 => Color.RED
    case 3 => Color.ORANGE
    case 4 => Color.GREEN
    case 5 => Color.PINK //En vez de plateado meto rosa
    case 6 => new Color(138,0,138)//morado
    case 7 => Color.GRAY
    case 8 => Color.BLACK //Reservo el negro para las bombas
    case _ => Color.WHITE //Dejando libre el blanco para los espacios en blanco.
  }
   
  //Introduce las bombas
    //AÑADE BOMBA A LA POSICION INDICADA
   def ponerUnaBomba(puntero:Int, posicion: Int,lista:List[Color]):List[Color]={
      if (lista.isEmpty) {return Nil}
          else if (puntero==posicion){ val nuevaLista = Color.BLACK::lista.tail
          return nuevaLista}
          else lista.head::(ponerUnaBomba(puntero+1,posicion,lista.tail))
   }
   
   // SIRVE PARA LLAMAR RECURSIVAMENTE E INSERTAR EL NUMERO DE BOMBAS DEPENDIENDO DEL NIVEL
    def insertarNBombas(Pos:Int,Nivel:Int,tablero:List[Color]):List[Color]={
          if(Nivel==0){return tablero}
          else {
          val posicion=new Random
          val nueva=(ponerUnaBomba(0,posicion.nextInt(Pos),tablero))
          insertarNBombas(Pos,Nivel-1,nueva)
          }
    }

//recibe una lista de botones y una lista de colores. imprime por pantalla botones de colores.
  def Imprimir(posicion: Int,tablero: List[Color],botones:List[Bloque]) {
    posicion match {
      //Imprime las posiciones del tablero.
      case 155        =>
      case x if x > 143 => Imprimir(posicion % 144+1,tablero,botones) //si es el último elemento de la línea, se salta a la siguiente
      case _ => {
        botones.head.background=tablero(posicion)
        Imprimir(posicion+12,tablero,botones.tail) //En caso contrario, imprimer el elemento recibido y por recursion pintaría el siguiente, de 8 posiciones a la derecha.
      }
    }
  }
def Imprimir2(Ncol:Int,l:List[Color],botones:List[Bloque]){
           if(l.isEmpty==false){
             if ((l.size-1)%Ncol==0)  {
               
               botones.head.background=l.head}
               else 
              
                botones.head.background=l.head
                 Imprimir(Ncol,l.tail,botones.tail)
           }
}
def imprimePantalla(tablero: List[Color], vidas: Int, puntuacion: Int) {
    //Imprime el tablero así como información de las jugadas y de la puntuación. La lista de borrables no se utiliza en la implementación sin GUI, pero servirá para la que tiene GUI.
  Imprimir(0,tablero,bloques)
    texto.text = "Puntuación: " + puntuacion + " - Vidas restantes: " + vidas
    Thread sleep 800
  }
   //Pos es buffer(0) que sería el boton que hemos elegido
   def listaBomba(Nfil:Int,Ncol:Int,pos:Int,lista:List[Color],listaVecinos:List[Int]):List[Int]={
   if ((pos>=0) && (pos<=Ncol*Nfil) && (!listaVecinos.contains(pos))){
     if(lista(pos)==Color.BLACK){
       comprobarBomba(Nfil,Ncol,pos,lista,pos::listaVecinos)
     }else  return pos::listaVecinos
   }else return listaVecinos
}
def comprobarBomba(Nfil:Int,Ncol:Int,pos:Int,lista:List[Color],listaVecinos:List[Int]):List[Int]={
 if (pos < 143){
  val actual= listaBomba(Nfil,Ncol,pos,lista,listaVecinos)
 val arriba= listaBomba(Nfil,Ncol,pos-1,lista,actual)
 val abajo= listaBomba(Nfil,Ncol,pos+1,lista,arriba)
 val derecha= listaBomba(Nfil,Ncol,pos+Ncol,lista,abajo)
 val izquierda= listaBomba(Nfil,Ncol,pos-Ncol,lista,derecha)
 val arribaDer= listaBomba(Nfil,Ncol,pos+(Ncol-1),lista,izquierda)
 val arribaIzq= listaBomba(Nfil,Ncol,pos-(Ncol+1),lista,arribaDer)
 val abajoDer= listaBomba(Nfil,Ncol,pos+(Ncol+1),lista,arribaIzq)
 val abajoIzq= listaBomba(Nfil,Ncol,pos-(Ncol-1),lista,abajoDer)
 return abajoIzq}
 else
 {
 val actual= listaBomba(Nfil,Ncol,pos,lista,listaVecinos)
 val arriba= listaBomba(Nfil,Ncol,pos-1,lista,actual)
 val izquierda= listaBomba(Nfil,Ncol,pos-Ncol,lista,arriba)
 val arribaIzq= listaBomba(Nfil,Ncol,pos-(Ncol+1),lista,izquierda)
 return arribaIzq

 }
}
  //LISTA CON LOS VECINOS IGUALES
//aqui valor seria esto(buffer(0) 
//elemento color

def comprobarVecinos(Nfil:Int,Ncol:Int,Valor:Int,elemento:Color,lista:List[Color],listaVecinos:List[Int]):List[Int]={
 //(Valor < (Nfil*Ncol-12) para evitar que de error. Falla un limite
 if((Valor>=0) && (elemento.equals(lista(Valor)))&&(Valor<Nfil*Ncol)&&(!listaVecinos.contains(Valor))){
   if ((Valor>=132)&&(Valor<143)) {val posicionActual= comprobarVecinos(Nfil,Ncol,Valor,elemento,lista,Valor::listaVecinos)
 val listaArriba= comprobarVecinos(Nfil,Ncol,Valor-1,elemento,lista,posicionActual)
 val listaAbajo= comprobarVecinos(Nfil,Ncol,Valor+1,elemento,lista,listaArriba)
 val listaIzquierda= comprobarVecinos(Nfil,Ncol,Valor-Ncol,elemento,lista,listaAbajo)
 return listaIzquierda }
   else if (Valor==143) {  
 val posicionActual= comprobarVecinos(Nfil,Ncol,Valor,elemento,lista,Valor::listaVecinos)    
 val listaArriba=comprobarVecinos(Nfil,Ncol,Valor-1,elemento,lista,posicionActual)
 val listaIzquierda= comprobarVecinos(Nfil,Ncol,Valor-Ncol,elemento,lista,listaArriba)
 return listaIzquierda}
   else {
 val posicionActual= comprobarVecinos(Nfil,Ncol,Valor,elemento,lista,Valor::listaVecinos)
 val listaArriba= comprobarVecinos(Nfil,Ncol,Valor-1,elemento,lista,posicionActual)
 val listaAbajo= comprobarVecinos(Nfil,Ncol,Valor+1,elemento,lista,listaArriba)
 val listaDerecha= comprobarVecinos(Nfil,Ncol,Valor+Ncol,elemento,lista,listaAbajo)
 val listaIzquierda= comprobarVecinos(Nfil,Ncol,Valor-Ncol,elemento,lista,listaDerecha)
 return listaIzquierda
 }
 }
 else return listaVecinos
}
//Cuando borramos vecinos, dejamos un espacio en blanco para indicar que ya no hay bloques alli  
def borrarVecinos(posicion:Int,tablero:List[Color],listaVecinos:List[Int]):List[Color]={
   if(listaVecinos.isEmpty){
     return tablero
   }else if(listaVecinos.contains(posicion)){
   Color.WHITE::borrarVecinos(posicion+1,tablero.tail,listaVecinos.tail)
   }else tablero.head::borrarVecinos(posicion+1,tablero.tail,listaVecinos)
 }
//fUNCIONES PARA DESPLAZAMIENTO HORIZONTAL (0 ARRIBA)
 //INSERTA UN ELEMENTO EN UNA POSICION
 def sustituir(puntero:Int,pos:Int,l:List[Color],valor:Color):List[Color]={
   if(l.isEmpty) {return Nil }
       else if(puntero==pos) {
     val aux = valor::l.tail
     return  aux 
   }
   else  l.head :: sustituir(puntero+1,pos,l.tail,valor)
  }
   
    def cambiarBloques(Nfil:Int, Ncol:Int,primer:Int,segundo:Int,lista:List[Color]):List[Color]={
               val uno = lista(primer) //Valor 0
               val dos = lista(segundo)//Cualquier otro valor
               val aux1 = sustituir(0,segundo,lista,uno) //la posicion f-1*Col+col pasara a tener un 0
               val aux2 = sustituir(0,primer,aux1,dos)//La posicion f*Col+col pasara a tener un valor
              aux2          
  }
 

 def sonIguales(lista1:List[Color],lista2:List[Color]):Boolean={
 if ((lista1.isEmpty)&&(lista2.isEmpty)) {return true}
 else{
  if ((lista1.head) != (lista2.head)) {
    return false
  }
  else {
  sonIguales(lista1.tail,lista2.tail)
       }
     } 
  }
 
 
 //SUBIR LOS 0 HACIA ARRIBA
  def bajarBloques(Nfil:Int,Ncol:Int,fila:Int,col:Int,tablero:List[Color]):List[Color]={
    if (col==Ncol) {return tablero}
    else if (fila==0){  bajarBloques(Nfil,Ncol,Nfil-1,col+1,tablero)}
        else if ( tablero(fila*Ncol+col)==Color.WHITE){
        val cambiada= cambiarBloques(Nfil,Ncol,(fila-1)*Ncol+col,fila*Ncol+col,tablero)
         bajarBloques(Nfil,Ncol,fila-1,col,cambiada)
            }else  bajarBloques(Nfil,Ncol,fila-1,col,tablero)
    }
 
 def comprobar(NFil:Int,Ncol:Int,lista:List[Color],lista2:List[Color]):List[Color]={
    if(sonIguales(lista,lista2)){
      return lista2
    }else{
      val op= bajarBloques(NFil,Ncol,NFil-1,0,lista2)
    comprobar(NFil,Ncol,lista2,op)}
  }
 
   def comprobarC(NFil:Int,Ncol:Int,lista:List[Color],lista2:List[Color]):List[Color]={
    if(sonIguales(lista,lista2)){
      return lista2
    }else{
      val op2=desplazarColumnas(NFil-1,0,NFil,Ncol,0,lista2) 

    comprobarC(NFil,Ncol,lista2,op2)}
  }
  //DESPLAZAR MATRIZ SI LA COLUMNA ES ENTERA DE 0

    def desplazarColumnas(fila:Int,col:Int,Nfila:Int,Ncol:Int,contador:Int,tablero:List[Color]):List[Color]={

  if  (col+contador == (Ncol)){    
    return tablero
  }
  else if (fila <0){   
    desplazarColumnas((Nfila-1),col+1,Nfila,Ncol,1,tablero)
  }
  else if ((tablero(fila*Ncol+col)) == Color.WHITE){ 
    if ((tablero(fila*Ncol+(col+contador)) != Color.WHITE) && (col+contador<=Ncol-1)){
      val cambio= cambiarBloques(Nfila,Ncol,fila*Ncol+col,fila*Ncol+(col+1),tablero)
      desplazarColumnas(fila-1,col,Nfila,Ncol,contador,cambio)
    }
    else if((col+contador>Ncol-1)){
      
      return tablero
    }
    else {
   
      desplazarColumnas((Nfila-1),col,Nfila,Ncol,contador+1,tablero)
    }
  }
  else{
    desplazarColumnas((Nfila-1),col+1,Nfila,Ncol,1,tablero)
  }
}
    
  def buscarPosicion(puntero:Int, posicion: Int,lista:List[Int]):Int={
        if (puntero==posicion){ return lista.head}
        else buscarPosicion(puntero+1,posicion,lista.tail)
    }
   //VER SI ACABA LA PARTIDA
    
    def terminar(tablero:List[Color]):Boolean={
     if(tablero.isEmpty){return true}
     else{
      if(tablero.head==Color.WHITE){
        terminar(tablero.tail)
      
        }else{return false}
    }
    }
   //Cambiar la lista de vecinos, no es color, es entera.
    //    val borrados= borrarVecinos(0,tableroJuego,bombas.sortWith(_<_)) 
       //          
   def jugarPartida(vida:Int,pts:Int,FilasTotales:Int,ColTotales:Int,tableroJuego:List[Color]):Int =vida match{
     case -1 => {imprimePantalla(tableroJuego,vida,pts) 
               pts}
     
     case 0=> {imprimePantalla(tableroJuego,vida,pts) 
               pts}
              
     case _ =>{
        imprimePantalla(tableroJuego,vida,pts)
             
           alerta.text="Elige bloque que deseas eliminar"
             buffer=List();
              while (buffer.length != 1) { Thread sleep 1000 }
                 println(buffer(0))
             
                       if (tableroJuego(buffer(0))==Color.BLACK){
                   val pos = tableroJuego(buffer(0))
                   println("el valor tablero juego buffer(0)",pos)
                   val bombas= comprobarBomba(FilasTotales,ColTotales,buffer(0),tableroJuego,List())  
                   val puntos=puntuacionJuego(bombas)
                   val jrestantes= vidas(bombas)
                   val borrados= borrarVecinos(0,tableroJuego,bombas.sortWith(_<_))
                   imprimePantalla(borrados,jrestantes,puntos)
                   val actualizada= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,borrados)
                   val actualizada2= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,actualizada)
                   val repeticion= comprobar(FilasTotales,ColTotales,actualizada,actualizada2)
                   val op1=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,repeticion) 
                   val op2=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,op1) 
                   val repeticionC= comprobarC(FilasTotales,ColTotales,op1,op2)
          
                   if (terminar(repeticionC)){
                     alerta.text="!!!!!HA GANADO LA PARTIDA!!"
                                          imprimePantalla(repeticionC,vida,puntos)
                                         puntos              
                   }else{
                   imprimePantalla(repeticionC,vida,puntos)
                   
                   if (puntos==(-1)){jugarPartida((vida-jrestantes),0,FilasTotales,ColTotales,repeticionC)}
                   else{jugarPartida(vida-jrestantes,pts+puntos,FilasTotales,ColTotales,repeticionC)}
                   }
                }else {
                  println("zona de colores normales")
                  val elemento = tableroJuego(buffer(0))
                  val vecinos=comprobarVecinos(FilasTotales,ColTotales,buffer(0),elemento,tableroJuego,List()) 
                  val puntos=puntuacionJuego(vecinos)
                  val jrestantes= vidas(vecinos)
                    val borrados= borrarVecinos(0,tableroJuego,vecinos.sortWith(_<_))
                   //imprimePantalla(borrados,jrestantes,puntos)
                   val actualizada= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,borrados)
                   val actualizada2= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,actualizada)
                   val repeticion= comprobar(FilasTotales,ColTotales,actualizada,actualizada2)
                   imprimePantalla(repeticion,puntos,jrestantes) 
                   val op1=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,repeticion) 
                   val op2=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,op1) 
                   val repeticionC= comprobarC(FilasTotales,ColTotales,op1,op2)
                 
                  if (terminar(repeticionC)){
                    alerta.text= "!!!!!HA GANADO LA PARTIDA!!!!¡"
                     imprimePantalla(repeticionC,jrestantes,puntos)
                    return puntos                 
                   }else {
                     imprimePantalla(repeticionC,jrestantes,puntos)               
                   if (puntos==(-1)){jugarPartida((vida-jrestantes),0,FilasTotales,ColTotales,repeticionC)}
                   else{jugarPartida(vida-jrestantes,pts+puntos,FilasTotales,ColTotales,repeticionC)}
               
                   }
                 
                }
    
     }
   
   }            
   
}