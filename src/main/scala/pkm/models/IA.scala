package pkm.models

import scala.util.Random

abstract class IA {
  def sortPokemons(pokemons: Seq[Pokemon]): Seq[Pokemon]
  def choiceAction(trainer: Trainer, arena: Arena): Action

}

class RandomIA extends IA {

  override def sortPokemons(pokemons: Seq[Pokemon]): Seq[Pokemon] = {
    pokemons
  }

  override def choiceAction(trainer: Trainer, arena: Arena): Action = {
    trainer.firstAlivePokemon() match {
      case Some(pkm) => {
        Random.nextInt(3) match {
          case 0 => AtkTrainer(rdmAttack(trainer, pkm))
          case 1 => AtkPokemon(rdmAttack(trainer, pkm))
          case 2 => Defense
        }
      }
      case None => Nothing
  }

  def rdmAttack(trainer: Trainer, pokemon: Pokemon): Attack = {
      if (trainer.rage >= pokemon.powerAttack.cost) {
        Random.nextInt(2) match {
          case 0 => pokemon.powerAttack
          case 1 => pokemon.basicAttack
        }
      } else pokemon.basicAttack
    }
  }

}

sealed trait Action
case class AtkTrainer(attack: Attack) extends Action
case class AtkPokemon(attack: Attack) extends Action
case object Defense extends Action
case object Nothing extends Action
