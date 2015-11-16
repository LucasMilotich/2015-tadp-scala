package dbz

class Criterio (funcion: (Guerrero,Guerrero)=>Float) {
  def analizar(atacante: Guerrero, defensor: Guerrero) = {
    funcion(atacante,defensor)
  }
}