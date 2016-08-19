package pkm.models

/**
 * Created by quentin on 12/08/16.
 */
case class Arena(trainerA: Trainer, trainerB: Trainer) {

  private def withDefence(action: Action) = {
    action match {
      case Defense => true
      case  _ => false
    }
  }

  private def computeDmg(baseDamage: Int, withDefence: Boolean, decreasePercent: Int): Int = {
    if (withDefence) baseDamage - (baseDamage * decreasePercent / 100)
    else baseDamage
  }

  private def trainerAttacked(victim: Trainer, dmg: Int): Trainer = {
    victim.copy(life = victim.life - dmg)
  }

  private def pokemonAttacked(victim: Trainer, victimPokemon: Option[Pokemon], dmg: Int): Trainer = {
    def updatePokemon(pokemon: Pokemon, p: Pokemon): Pokemon = {
      if (p.number == pokemon.number) p.copy(stats = p.stats.copy(hp = Math.max(0, p.stats.hp - dmg))) else p
    }
    victimPokemon match {
      case Some(attackedPokemon) =>
        val updatePokemons = victim.pokemons.map(p => updatePokemon(attackedPokemon, p))
        victim.copy(pokemons = updatePokemons)
    }
  }


  def update(): Arena = {

    val pokemonA = trainerA.firstAlivePokemon()
    val pokemonB = trainerB.firstAlivePokemon()

    val actionA = trainerA.action(this)
    val actionB = trainerB.action(this)

    val newTrainerA = actionB match {
      case AtkTrainer(atk) => trainerAttacked(trainerA, computeDmg(atk.damage, withDefence(actionA), 25))
      case AtkPokemon(atk) => pokemonAttacked(trainerA, pokemonA, computeDmg(atk.damage, withDefence(actionA), 25))
      case Defense => trainerA
    }

    val newTrainerB = actionA match {
      case AtkTrainer(atk) => trainerAttacked(trainerB, computeDmg(atk.damage, withDefence(actionB), 25))
      case AtkPokemon(atk) => pokemonAttacked(trainerB, pokemonB, computeDmg(atk.damage, withDefence(actionB), 25))
      case Defense => trainerB
    }

    // TODO update victim rage
    this.copy(trainerA = newTrainerA, trainerB = newTrainerB)
  }

}
