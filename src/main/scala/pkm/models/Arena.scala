package pkm.models

/**
 * Created by quentin on 12/08/16.
 */
case class Arena(trainerA: Trainer, trainerB: Trainer, defenceRate: Int = 25, counterAtkRate: Int = 50, healRate: Int = 10) {

  private def withDefence(action: Action) = {
    action match {
      case Defense => true
      case  _ => false
    }
  }

  private def computeDmg(baseDamage: Int, withDefence: Boolean, decreasePercent: Int): Int = {
    if (withDefence) baseDamage * decreasePercent / 100
    else baseDamage
  }

  private def trainerAttacked(victim: Trainer, dmg: Int): Trainer = {
    victim.copy(life = victim.life - dmg)
  }

  private def updateTrainerPokemons(trainer: Trainer, update: Pokemon => Pokemon): Trainer =  {
    trainer.firstAlivePokemon() match {
      case Some(firstPokemon) =>
        trainer.copy(pokemons = trainer.pokemons.map(p => if (p.number == firstPokemon.number) update(firstPokemon) else p))
    }
  }

  private def pokemonAttacked(victim: Trainer, dmg: Int): Trainer = {
    def attak(pokemon: Pokemon): Pokemon = {
      pokemon.copy(stats = pokemon.stats.copy(hp = Math.max(0, pokemon.stats.hp - dmg)))
    }
    updateTrainerPokemons(victim, attak)
  }

  /* regen 10% hp */
  def heal(trainer: Trainer): Trainer = {
    def heal(pokemon: Pokemon): Pokemon = {
      pokemon.copy(stats = pokemon.stats.copy(hp = Math.max(0, pokemon.stats.hp + (pokemon.stats.hp * healRate / 100))))
    }

    updateTrainerPokemons(trainer, heal)
  }

  def countered(victim: Trainer, damage: Int): Trainer = {
    victim.copy(life = victim.life -  (damage * counterAtkRate / 100))
  }

  def update(): Arena = {

    val actionA = trainerA.action(this)
    val actionB = trainerB.action(this)

    (actionA, actionB) match {
      case (AtkTrainer(atkA), AtkTrainer(atkB)) =>
        this.copy(
          trainerA = trainerAttacked(trainerA, computeDmg(atkB.damage, false, defenceRate)),
          trainerB = trainerAttacked(trainerB, computeDmg(atkA.damage, false, defenceRate)))

      case (AtkPokemon(atkA), AtkPokemon(atkB)) => this.copy(
        trainerA = pokemonAttacked(trainerA, computeDmg(atkB.damage, false, defenceRate)),
        trainerB = pokemonAttacked(trainerB, computeDmg(atkA.damage, false, defenceRate)))

      case (AtkTrainer(atkA), AtkPokemon(atkB)) => this.copy(
        trainerA = pokemonAttacked(trainerA, computeDmg(atkB.damage, false, defenceRate)),
        trainerB = trainerAttacked(trainerB, computeDmg(atkA.damage, false, defenceRate)))

      case (AtkPokemon(atkA), AtkTrainer(atkB)) => this.copy(
        trainerA = trainerAttacked(trainerA, computeDmg(atkB.damage, false, defenceRate)),
        trainerB = pokemonAttacked(trainerB, computeDmg(atkA.damage, false, defenceRate)))

      case (AtkPokemon(atkA), Defense) => this.copy(
        trainerB = pokemonAttacked(trainerB, computeDmg(atkA.damage, true, defenceRate)))

      case (Defense, AtkPokemon(atkB)) => this.copy(
        trainerA = pokemonAttacked(trainerA, computeDmg(atkB.damage, true, defenceRate)))

      case (AtkTrainer(atkA), Defense) =>  this.copy(
        trainerA = countered(trainerA, atkA.damage),
        trainerB = trainerAttacked(trainerB, computeDmg(atkA.damage, false, defenceRate)))

      case (Defense, AtkTrainer(atkB)) => this.copy(
        trainerA = trainerAttacked(trainerA, computeDmg(atkB.damage, false, defenceRate)),
        trainerB = countered(trainerB, atkB.damage))

      case (Defense, Defense) => this.copy(trainerA = heal(trainerA), trainerB = heal(trainerB))
    }

  }

}
