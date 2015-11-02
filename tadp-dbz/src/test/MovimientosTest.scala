package test

import org.junit.Assert._
import org.junit.Test
import dbz._

class MovimientosTest {
  
  @Test
  def `Humano carga ki` = {
    var krilin = new Guerrero("krilin",Humano,350,1500)
    assertEquals(CargarKi(krilin).ki,450)
  }
  
}