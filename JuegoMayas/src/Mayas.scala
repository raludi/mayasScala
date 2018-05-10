import scala.util.Random
import java.lang.Integer
import scala.MatchError
object Mayas extends App {
 
     //CREAMOS UNA LISTA EN FUNCION A LA DIFICULTAD Y EL NUMERO FIL,COL
    def crearLista(dif:Int,pos: Int):List[Int]= pos match{
    case 0 => List()
    case _ => {val r= new Random;r.nextInt(dif)+1::crearLista(dif,pos-1)}
    }
                                                  
    //DEVUELVE EL VALOR DE LA POSICION
    def buscarPosicion(puntero:Int, posicion: Int,lista:List[Int]):Int={
        if (puntero==posicion){ return lista.head}
        else buscarPosicion(puntero+1,posicion,lista.tail)
    }
    
   def convertirAColor(numero:Int):Char= numero match{
    
    case 1 => 'A'
    case 2 => 'R'
    case 3 => 'N'
    case 4 => 'V'
    case 5 => 'P'
    case 6 => 'M'
    case 7 => 'G'
    case 8 => 'B'
    case _ => '-'  
  }
   
   
  
def Imprimir(Ncol:Int,l:List[Int]){
           if(l.isEmpty==false){
             if ((l.size-1)%Ncol==0)  {
               
               println(convertirAColor(l.head)+" ")}
               else 
              
                 print((convertirAColor(l.head))+" ")
                 Imprimir(Ncol,l.tail)
           }
} 
     //AÑADE BOMBA A LA POSICION INDICADA
   def ponerUnaBomba(puntero:Int, posicion: Int,lista:List[Int]):List[Int]={

          if (lista.isEmpty) {return Nil}

          else if (puntero==posicion){ val nuevaLista = 8::lista.tail
          return nuevaLista}
          else lista.head::(ponerUnaBomba(puntero+1,posicion,lista.tail))
   }
   
   // SIRVE PARA LLAMAR RECURSIVAMENTE E INSERTAR EL NUMERO DE BOMBAS DEPENDIENDO DEL NIVEL
    def insertarNBombas(Pos:Int,Nivel:Int,tablero:List[Int]):List[Int]={

          if(Nivel==0){return tablero}

          else {
          val posicion=new Random
          val nueva=(ponerUnaBomba(0,posicion.nextInt(Pos),tablero))
          insertarNBombas(Pos,Nivel-1,nueva)

          }

    }
    //BOMBAS
def listaBomba(Nfil:Int,Ncol:Int,F:Int,C:Int,lista:List[Int],listaVecinos:List[Int]):List[Int]={
   if ((F>=0) && (C>=0) && (C<Ncol) && (F<Nfil) && (!listaVecinos.contains(F*Ncol+C))){
     if(lista(F*Ncol+C)==8){
       comprobarBomba(Nfil,Ncol,F,C,lista,F*Ncol+C::listaVecinos)
     }else  return F*Ncol+C::listaVecinos
   }else return listaVecinos
}
def comprobarBomba(Nfil:Int,Ncol:Int,F:Int,C:Int,lista:List[Int],listaVecinos:List[Int]):List[Int]={
 val actual= listaBomba(Nfil,Ncol,F,C,lista,listaVecinos)
 val arriba= listaBomba(Nfil,Ncol,F-1,C,lista,actual)
 val abajo= listaBomba(Nfil,Ncol,F+1,C,lista,arriba)
 val derecha= listaBomba(Nfil,Ncol,F,C+1,lista,abajo)
 val izquierda= listaBomba(Nfil,Ncol,F,C-1,lista,derecha)
 val arribaDer= listaBomba(Nfil,Ncol,F-1,C+1,lista,izquierda)
 val arribaIzq= listaBomba(Nfil,Ncol,F-1,C-1,lista,arribaDer)
 val abajoDer= listaBomba(Nfil,Ncol,F+1,C+1,lista,arribaIzq)
 val abajoIzq= listaBomba(Nfil,Ncol,F+1,C-1,lista,abajoDer)
 return abajoIzq
}
  //LISTA CON LOS VECINOS IGUALES
 def comprobarVecinos(Nfil:Int,Ncol:Int,F:Int,C:Int,Valor:Int,lista:List[Int],listaVecinos:List[Int]):List[Int]={
 
 if((C>=0) &&  (F<Nfil) && (F>=0) && (C<Ncol) && (Valor.equals(buscarPosicion(0,(F*Ncol+C),lista)) && (buscarPosicion(0,(F*Ncol+C),lista))!=0) && (!listaVecinos.contains(F*Ncol+C))){
  
 val posicionActual= comprobarVecinos(Nfil,Ncol,F,C,Valor,lista,(F*Ncol+C)::listaVecinos)
 val listaArriba= comprobarVecinos(Nfil,Ncol,F-1,C,Valor,lista,posicionActual)
 val listaAbajo= comprobarVecinos(Nfil,Ncol,F+1,C,Valor,lista,listaArriba)
 val listaDerecha= comprobarVecinos(Nfil,Ncol,F,C+1,Valor,lista,listaAbajo)
 val listaIzquierda= comprobarVecinos(Nfil,Ncol,F,C-1,Valor,lista,listaDerecha)
 return listaIzquierda
 }
 else return listaVecinos
}
 
   //ELEGIR LA LISTA MAS LARGA
 /*def LmayorqL2(comparar:List[Int],vecinos:List[Int]):List[Int]={
   if(comparar.length<vecinos.length){
     val lista= vecinos
     return lista
   }else{
     val lista= comparar
      return lista
       }
 }*/
 
 //BORRA LAS POSICIONES IGUALES A LA ELEGIDA
 def borrarVecinos(posicion:Int,tablero:List[Int],listaVecinos:List[Int]):List[Int]={

   if(listaVecinos.isEmpty){
     return tablero
   }else if(listaVecinos.contains(posicion)){
   0::borrarVecinos(posicion+1,tablero.tail,listaVecinos.tail)
   }else tablero.head::borrarVecinos(posicion+1,tablero.tail,listaVecinos)
 
 }

 //INSERTA UN ELEMENTO EN UNA POSICION
 def sustituir(puntero:Int,pos:Int,l:List[Int],valor:Int):List[Int]={
   if(l.isEmpty) {return Nil }
       else if(puntero==pos) {
     val aux = valor::l.tail
     return  aux 
   }
   else  l.head :: sustituir(puntero+1,pos,l.tail,valor)
  }
 
//CAMBIA LA POSICION DE DOS BLOQUES
 def cambiarBloques(Nfil:Int, Ncol:Int,primer:Int,segundo:Int,lista:List[Int]):List[Int]={
               val uno = buscarPosicion(0,primer,lista) //Valor 0
               val dos = buscarPosicion(0,segundo,lista)//Cualquier otro valor
               val aux1 = sustituir(0,segundo,lista,uno) //la posicion f-1*Col+col pasara a tener un 0
               val aux2 = sustituir(0,primer,aux1,dos)//La posicion f*Col+col pasara a tener un valor
              aux2          
  }
 
 
 //VER SI DOS LISTAS SON IGUALES O NO
 
 def sonIguales(lista1:List[Int],lista2:List[Int]):Boolean={
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
  def bajarBloques(Nfil:Int,Ncol:Int,fila:Int,col:Int,tablero:List[Int]):List[Int]={
    
    
    if (col==Ncol) {return tablero}
    
    else if (fila==0){  bajarBloques(Nfil,Ncol,Nfil-1,col+1,tablero)}
    
    else if ( tablero(fila*Ncol+col)==0){
        val cambiada= cambiarBloques(Nfil,Ncol,(fila-1)*Ncol+col,fila*Ncol+col,tablero)
         bajarBloques(Nfil,Ncol,fila-1,col,cambiada)
         
    
    }else  bajarBloques(Nfil,Ncol,fila-1,col,tablero)
    
  }
  //DEVUELVE LA PUNTUACION DE LA JUGADA EN CASO QUE SEA LONGITUD 1 O 2 NOS PONE LA PUNTUACIÓN A 0
  def puntuacion(lista:List[Int]):Int= lista.length match{
    case 0=> 0
    case 1=> -1
    case 2=> -1
    case _=> 10*lista.length
  }
  //RESTA VIDA EN CASO DE QUE SE BORRE UNA POSICIÓN
  def vidas(lista:List[Int]):Int= lista.length match{
    case 1=>  1
    case 2=>  2
    case _=>  0
  }
 //PARA ACTUALIZAR LA MATRIZ SUBIENDO 0 ARRIBA
  def comprobar(NFil:Int,Ncol:Int,lista:List[Int],lista2:List[Int]):List[Int]={
    if(sonIguales(lista,lista2)){
      return lista2
    }else{
      val op= bajarBloques(NFil,Ncol,NFil-1,0,lista2)
 

    comprobar(NFil,Ncol,lista2,op)}
  }
  //PARA ACTUALIZAR MATRIZ DESPLAZANDO LAS COLUMNAS DE 0 A LA DERECHA
    def comprobarC(NFil:Int,Ncol:Int,lista:List[Int],lista2:List[Int]):List[Int]={
    if(sonIguales(lista,lista2)){
      return lista2
    }else{
      val op2=desplazarColumnas(NFil-1,0,NFil,Ncol,0,lista2) 

    comprobarC(NFil,Ncol,lista2,op2)}
  }
  //DESPLAZAR MATRIZ SI LA COLUMNA ES ENTERA DE 0

    def desplazarColumnas(fila:Int,col:Int,Nfila:Int,Ncol:Int,contador:Int,tablero:List[Int]):List[Int]={

  if  (col+contador == (Ncol)){    
    return tablero
  }
  else if (fila <0){   
    desplazarColumnas((Nfila-1),col+1,Nfila,Ncol,1,tablero)
  }
  else if ((tablero(fila*Ncol+col)) == 0){ 
    if (((buscarPosicion(0,fila*Ncol+(col+contador),tablero)) != 0) && (col+contador<=Ncol-1)){
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
   //VER SI ACABA LA PARTIDA
    
    def terminar(tablero:List[Int]):Boolean={
     if(tablero.isEmpty){return true}
     else{
      if(tablero.head==0){
        terminar(tablero.tail)
      
        }else{return false}
    }
    }

  //JUGAR A LA PARTIDA
  def jugarPartida(creados:Int,vida:Int,pts:Int,FilasTotales:Int,ColTotales:Int,tableroJuego:List[Int],dif:String){
              
              print("\n TIENE "+vida+" VIDAS RESTANTES\n")
             print("\n PUNTOS ACUMULADOS: "+pts+"\n")
              if (terminar(tableroJuego)){
                     print("\n!!!!!HA GANADO LA PARTIDA¡¡¡¡¡\n")
                     dificultad(creados+1,pts,vida,dif)
                   }
              else if (vida==0){
                print("\n HA PERDIDO LA PARTIDA\n") 
                print("\n Ha jugado "+creados+" tableros")}
              else{
                  print("\nINTRODUCE LA FILA A ELIMINAR: ")
                 val X=Integer.parseInt(String.valueOf(Console.in.readLine()))
                 print("\nINTRODUCE LA COLUMNA A ELEMINAR: ")
                 val Y=Integer.parseInt(String.valueOf(Console.in.readLine()))
                if (buscarPosicion(0,(X*ColTotales+Y),tableroJuego)==8){
                   val bombas= comprobarBomba(FilasTotales,ColTotales,X,Y,tableroJuego,List())  
                   val puntos=puntuacion(bombas)
                   val jrestantes= vidas(bombas)
                   print("\n MATRIZ ACTUALIZADA\n")
                   val borrados= borrarVecinos(0,tableroJuego,bombas.sortWith(_<_))
                   Imprimir(ColTotales,borrados)
                   print("\n MATRIZ DESPLAZADA\n")
                   val actualizada= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,borrados)
                   val actualizada2= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,actualizada)
                   val repeticion= comprobar(FilasTotales,ColTotales,actualizada,actualizada2)
                   
                   val op1=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,repeticion) 
                   val op2=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,op1) 
                   val repeticionC= comprobarC(FilasTotales,ColTotales,op1,op2)
                   print("\nVIDAS RESTANTES: \n")
                     print(vida)
                  
                   Imprimir(ColTotales,repeticionC)
                   if (puntos==(-1)){jugarPartida(creados,vida-jrestantes,0,FilasTotales,ColTotales,repeticionC,dif)}
                   else{jugarPartida(creados,vida-jrestantes,pts+puntos,FilasTotales,ColTotales,repeticionC,dif)}
                   
                   
                }else{
                  val vecinos=comprobarVecinos(FilasTotales,ColTotales,X,Y,tableroJuego(X*ColTotales+Y),tableroJuego,List()) 
                  val puntos=puntuacion(vecinos)
                  val jrestantes= vidas(vecinos)
                  Imprimir(ColTotales,vecinos)
                  print("\n MATRIZ ACTUALIZADA\n")
                   val borrados= borrarVecinos(0,tableroJuego,vecinos.sortWith(_<_))
                   Imprimir(ColTotales,borrados)
                   print("\n MATRIZ DESPLAZADA\n")
                   val actualizada= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,borrados)
                   val actualizada2= bajarBloques(FilasTotales,ColTotales,FilasTotales-1,0,actualizada)
                   val repeticion= comprobar(FilasTotales,ColTotales,actualizada,actualizada2)
                   
                   val op1=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,repeticion) 
                   val op2=desplazarColumnas(FilasTotales-1,0,FilasTotales,ColTotales,0,op1) 
                   val repeticionC= comprobarC(FilasTotales,ColTotales,op1,op2)
                   
                 
                   Imprimir(ColTotales,repeticionC)
                   
                   if (puntos==(-1)){jugarPartida(creados,vida-jrestantes,0,FilasTotales,ColTotales,repeticionC,dif)}
                   else{jugarPartida(creados,vida-jrestantes,pts+puntos,FilasTotales,ColTotales,repeticionC,dif)}
                   }
                   
                  
                }
                 
  }
  
  //DIFICULTADES DEL JUEGO
  def dificultad(creados:Int,puntos:Int,vidas:Int,dif:String)={
  
  val tjuego = dif match
  {
  case "BAJA" => val tableroInicial = insertarNBombas(99,2,crearLista(3,99))
                 print("\n *******NUEVO NIVEL******* \n")
                 Imprimir(11,tableroInicial)
                 jugarPartida(creados,8,puntos,9,11,tableroInicial,"BAJA")
                  
               
  
  case "MEDIA" => val tableroInicial = insertarNBombas(192,3,crearLista(5,192))
                  print("\n *******NUEVO NIVEL******* \n")
                 Imprimir(16,tableroInicial)
                    jugarPartida(creados,10,puntos,12,16,tableroInicial,"MEDIA")
                 
  case "ALTA" => val tableroInicial = insertarNBombas(375,5,crearLista(7,375))
  
                  print("\n *******NUEVO NIVEL******* \n")
                 Imprimir(15,tableroInicial)
                 jugarPartida(creados,15,puntos,25,15,tableroInicial,"ALTA")
  }
  
  }
 
  print("ESCRIBA DIFICULTAD BAJA,MEDIA,ALTA ")
  val nivel = Console.in.readLine().toUpperCase
 
  dificultad(1,0,0,nivel)
 
}

