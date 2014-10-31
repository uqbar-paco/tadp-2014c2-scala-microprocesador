package ar.edu.utn.tadp.microprocesador

abstract class Instruccion(val bytes: Int = 1)

trait InstruccionCompuesta {
  def instrucciones: Seq[Instruccion]
  def bytesCuerpo = instrucciones.map(_.bytes).sum + END.bytes
}


case object NOP extends Instruccion
case object ADD extends Instruccion
case object MUL extends Instruccion
case object SWAP extends Instruccion
case object HALT extends Instruccion

case class LODV(valor: Short) extends Instruccion(2)
case class LOD(direccion: Int) extends Instruccion(3)
case class STR(direccion: Int) extends Instruccion(3)
case class IFNZ(instrucciones: Instruccion*) extends Instruccion with InstruccionCompuesta

object END {
  val bytes = 1
}