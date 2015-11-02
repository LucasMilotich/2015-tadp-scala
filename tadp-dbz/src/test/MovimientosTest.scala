package test

import org.junit.Assert._
import org.junit.Test
import dbz._

class MovimientosTest {
  
  val krilin = new Guerrero("krilin",Humano,350,1500)
  val goku = new Guerrero("goku",Saiyajin,1500,3000)
  val a18 = new Guerrero("a18",Androide,0,0)
  
  
  @Test
  def `Guerrero_descansa_y_no_pasa_nada` = {
    assertEquals(DejarseFajar(krilin),krilin)
  }
  
  @Test
  def `Humano_carga_ki` = {
    assertEquals(CargarKi(krilin).ki,450)
  }
  
  @Test
  def `Androide_carga_ki` = {
    assertEquals(CargarKi(a18).ki,0)
  }
  
  @Test
  def `Saiyajin_carga_ki` = {
    assertEquals(CargarKi(goku).ki,1600)
  }
  
  @Test
  def `Fusion_Humano_Saiyajin_funciona` = {
    val nuevoGuerrero = FusionarCon(krilin)(goku)
    assertEquals(nuevoGuerrero.ki,1850)
    assertEquals(nuevoGuerrero.maximoKi,4500)
  }
  
  @Test
  //no me acuerdo como se testeaba el error
  def `Fusion_Saiyajin_Androide_lanza_error` = {
      FusionarCon(a18)(goku)
      fail()
  }
}