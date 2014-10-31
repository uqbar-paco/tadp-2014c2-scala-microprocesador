package ar.edu.utn.tadp

import scala.language.implicitConversions

package object microprocesador {
  implicit def SeqToList[T](s: Seq[T]) = s.toList

  trait ResultadoDeEjecucion{
    def micro: Microprocesador
    def map(f:(Microprocesador=>Microprocesador)): ResultadoDeEjecucion
    def filter(f:(Microprocesador=>Boolean)): ResultadoDeEjecucion
    def flatMap(f:(Microprocesador=>ResultadoDeEjecucion)): ResultadoDeEjecucion
  }
  
  case class Ejecutando(val micro: Microprocesador) extends ResultadoDeEjecucion{
    def map(f:(Microprocesador=>Microprocesador))= Ejecutando(f(micro))
    def filter(f:(Microprocesador=>Boolean))= if(f(micro)) this else Error(micro,"Fallo el filtrado")
    def flatMap(f:(Microprocesador=>ResultadoDeEjecucion))= f(micro)
  }
  
  case class Halt(val micro: Microprocesador) extends ResultadoDeEjecucion{
    def map(f:(Microprocesador=>Microprocesador))= this
    def filter(f:(Microprocesador=>Boolean))= this 
    def flatMap(f:(Microprocesador=>ResultadoDeEjecucion))= this
  }
  
  case class Error(val micro: Microprocesador, descripcion: String) extends ResultadoDeEjecucion{
    def map(f:(Microprocesador=>Microprocesador))= this
    def filter(f:(Microprocesador=>Boolean))= this 
    def flatMap(f:(Microprocesador=>ResultadoDeEjecucion))= this
  }

  // ****************************************************************
  // ** EJECUTAR
  // ****************************************************************

  def ejecutar(micro: Microprocesador, programa: Instruccion*): ResultadoDeEjecucion =
    programa.foldLeft(Ejecutando(micro): ResultadoDeEjecucion) {
    (resultado, instruccion) => 
//      case (Ejecutando(m), instruccion) =>
        val resultadoSiguiente = resultado.map { _ pc_+= instruccion.bytes }

      instruccion match {
        case HALT => Halt(resultado.micro)

        case inst @ IFNZ(instruccionesInternas @ _*) => for {
          micro <- resultadoSiguiente
          microPostInternas <- 
          ejecutar(micro, instruccionesInternas: _*)
        } yield if (micro.a == 0) 
          micro pc_+= inst.bytesCuerpo 
          else microPostInternas pc_+= 1

        case inst => for (micro <- resultadoSiguiente) yield inst match {
          case NOP => micro
          case ADD => micro.guardar(micro.a + micro.b)
          case MUL => micro.guardar(micro.a * micro.b)
          case SWAP => micro.copy(a = micro.b, b = micro.a)
          case LODV(valor) => micro.copy(a = valor)
          case LOD(direccion) => micro.copy(a = micro.memoriaDeDatos(direccion))
          case STR(direccion) => micro.copy(memoriaDeDatos = micro.memoriaDeDatos.updated(direccion, micro.a))
        }
      }
//      case (otro, _) => otro

    }

  // ****************************************************************
  // ** SIMPLIFICAR
  // ****************************************************************

  def simplificar(programa: Instruccion*): Seq[Instruccion] = programa.toList match {
    case Nil => Nil
    case NOP :: restantes => simplificar(restantes: _*)
    case SWAP :: SWAP :: restantes => simplificar(restantes: _*)
    case LODV(_) :: LODV(y) :: restantes => simplificar(LODV(y) :: restantes: _*)
    case LOD(_) :: LOD(y) :: restantes => simplificar(LOD(y) :: restantes: _*)
    case STR(_) :: STR(y) :: restantes => simplificar(STR(y) :: restantes: _*)
    case IFNZ() :: restantes => simplificar(restantes: _*)
    case IFNZ(instrucciones @ _*) :: restantes =>
      val simplificacion = simplificar(instrucciones: _*)
      if (simplificacion.isEmpty) simplificar(restantes: _*)
      else IFNZ(simplificacion: _*) :: simplificar(restantes: _*)
    case siguiente :: restantes => siguiente :: simplificar(restantes: _*)
  }

}
