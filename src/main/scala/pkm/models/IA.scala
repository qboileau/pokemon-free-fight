package pkm.models

import scala.util.Random

abstract class IA {
  def sortPokemons(pokemons: Seq[Pokemon]): Seq[Pokemon]
  def choiceAction(dresseur: Dresseur): Action

}

class RandomIA extends IA {

  override def sortPokemons(pokemons: Seq[Pokemon]): Seq[Pokemon] = {
    pokemons
  }

  override def choiceAction(dresseur: Dresseur): Action = {
    dresseur.firstAlivePokemon() match {
      case Some(pkm) => {
        Random.nextInt(3) match {
          case 0 => AtkDresseur(rdmAttack(dresseur, pkm))
          case 1 => AtkPokemon(rdmAttack(dresseur, pkm))
          case 2 => Defense()
        }
      }
      case None => Nothing()
  }

  def rdmAttack(dresseur: Dresseur, pokemon: Pokemon): Attack = {
      if (dresseur.rage >= pokemon.powerAttack.cost) {
        Random.nextInt(2) match {
          case 0 => pokemon.powerAttack
          case 1 => pokemon.basicAttack
        }
      } else pokemon.basicAttack
    }
  }

}

sealed trait Action
case class AtkDresseur(attack: Attack) extends Action
case class AtkPokemon(attack: Attack) extends Action
case class Defense() extends Action
case class Nothing() extends Action
