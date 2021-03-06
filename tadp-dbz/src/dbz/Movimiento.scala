package dbz

trait Movimiento extends Function2[Guerrero, Guerrero, (Guerrero, Guerrero)] {
  def apply(guerrero: Guerrero, oponente: Guerrero): (Guerrero, Guerrero)

  def cuantificadoSegun(g1: Guerrero, g2: Guerrero)(criterio: Criterio) = {
    val (atacante, defensor) = apply(g1, g2)
    criterio.analizar(atacante, defensor)
  }
}

case object DejarseFajar extends Movimiento {
  override def apply(guerrero: Guerrero, oponente: Guerrero) = {
    if (guerrero.podesHacerMovimiento) {
      println("pegame y decime " + guerrero.nombre)
      (guerrero, oponente)
    } else {
      (guerrero, oponente)

    }

  }
}

case object CargarKi extends Movimiento {
  override def apply(guerrero: Guerrero, oponente: Guerrero) = {
    if (guerrero.podesHacerMovimiento) {
      (guerrero.cargarKi, oponente)
    } else {
      (guerrero, oponente)
    }
  }

}

case class FusionarCon(compa: Guerrero) extends Movimiento {
  override def apply(guerrero: Guerrero, oponente: Guerrero) = {
    if (guerrero.podesHacerMovimiento) {
      if (guerrero.tipo.puedeFusionarse && compa.tipo.puedeFusionarse) {
        (guerrero.fusionar(compa), oponente)
      } else { throw new RuntimeException("No se puede fusionar una de esas especies") }

    } else { (guerrero, oponente) }

  }
}

//que valga para mono y para ssj al mismo tiempo
case class Convertirse(formaNueva: FormaSaiyajin) extends Movimiento {
  def apply(guerrero: Guerrero, oponente: Guerrero) = {
    if (guerrero.podesHacerMovimiento) {
      (formaNueva match {
        case SuperSaiyajin(_) if (guerrero.ki >= guerrero.maximoKi * 0.5) => Saiyajin().subirNivelSS(guerrero)
        case MonoSaiyajin if (guerrero.tieneCola) => guerrero.multiplicarMaximoKi(3).copy(tipo = Saiyajin(MonoSaiyajin))
        case _ => guerrero
      }, oponente)
    } else {
      (guerrero, oponente)
    }
  }
}

case object comerOponente extends Movimiento {
  def apply(guerrero: Guerrero, oponente: Guerrero) = {
    guerrero.formaDeDigerir match {
      case SoloUltimoGuerrero => (guerrero.limpiarMovimientosRobados.aprenderMovimientosDe(oponente), oponente.morir)
      case SoloAndroides if (oponente.tipo == Androide) => (guerrero.aprenderMovimientosDe(oponente), oponente.morir)
      case DigestionDefault => (guerrero.aprenderMovimientosDe(oponente), oponente.morir)
      case _ => (guerrero, oponente)
    }
  }
}

case class hacerMagia(estado: Estado, guerreroOpcional: Option[Guerrero]) extends Movimiento {
  def apply(guerrero: Guerrero, oponente: Guerrero) = {

    if (guerrero.podesHacerMovimiento) {
      guerrero.tipo match {
        case Namekusein | Monstruo(_) if (guerrero.esferasCompletas && guerreroOpcional.isEmpty) =>
          (guerrero.cambiarEstado(estado).quitarEsferas, oponente.cambiarEstado(estado))
        case Namekusein | Monstruo(_) if (guerrero.esferasCompletas && !guerreroOpcional.isEmpty) =>
          if (oponente.equals(guerreroOpcional.get)) {
            (guerrero.quitarEsferas, oponente.cambiarEstado(estado))

          } else
            (guerrero.cambiarEstado(estado).quitarEsferas, oponente)
        case _ => (guerrero, oponente)
      }
    } else { (guerrero, oponente) }

  }

}

