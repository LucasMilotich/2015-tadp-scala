package dbz

class ResultadoPelea (guerrero1: Guerrero, guerrero2: Option[Guerrero] = None) {
  
  def ganador: Option[Guerrero] = {
    if (guerrero2.isEmpty) Some(guerrero1)
    else None
  }
}

class PeleaTerminadaException (val ganador: Guerrero) extends Exception