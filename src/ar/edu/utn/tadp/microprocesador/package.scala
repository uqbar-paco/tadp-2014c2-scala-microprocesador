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
  programa.foldLeft(Ejecutando(micro): ResultadoDeEjecucion) { (resultado, instruccion) =>
    
      instruccion match {

      case HALT => Halt(resultado.micro pc_+= HALT.bytes)
      
      case other => for (m <- resultado) yield other match {
        case NOP => m
        case ADD => m.guardar(m.a + m.b)
        case MUL => m.guardar(m.a * m.b)
        case SWAP => m.copy(a = m.b, b = m.a)
        case LODV(valor) => m.copy(a = valor)
        case LOD(direccion) => m.copy(a = m.memoriaDeDatos(direccion))
        case STR(direccion) => m.copy(memoriaDeDatos = m.memoriaDeDatos.updated(direccion, m.a))
       
        case IFNZ(instrucciones @ _*) =>
          if (m.a == 0) m pc_+= instrucciones.map(_.bytes).sum + 1
          else (for (im <- ejecutar(m, instrucciones: _*)) yield im pc_+= 1).micro
      }

      }  
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
