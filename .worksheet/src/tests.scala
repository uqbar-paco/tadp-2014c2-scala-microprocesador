import ar.edu.utn.tadp.microprocesador.Microprocesador

object tests {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(96); 
val m = Microprocesador();System.out.println("""m  : ar.edu.utn.tadp.microprocesador.Microprocesador = """ + $show(m ));$skip(33); val res$0 = 
m.memoriaDeDatos.updated(0, 454);System.out.println("""res0: List[AnyVal] = """ + $show(res$0))}
}
