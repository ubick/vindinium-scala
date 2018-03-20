- [x] Remove mine from path when traveling to Tavern
- [x] Alter path if it contains hero with HP more than hero
- [x] Compute real paths to mines / taverns
- [x] Change destination when no path exists to current target
- [x] if path to enemy with lower health is less than 4, then move towards enemy
- [x] Fix flee by moving towards path with maximum distance from enemy
- [x] Clarify reason message per valid condition
- [x] Add high path weight for enemy hero tile only if their life is higher
- [x] Remove 1 tile area around enemy from path
- [x] Add tile weight to neighbors of neighbors of hero
- [x] Calculate tile weight at start of game
- [~] Refactor tile-weight calculation starting from enemy hero positions and add another level of neighbor scores
- [~~] Attack hero with 50% of mines
- [ ] Review double bot / tavern infinite loop cycle (http://159.65.169.154:9000/3892z7oa, http://159.65.169.154:9000/d50u8o54?speed=max)
- [ ] Moving up/down in a loop outside tavern (http://159.65.169.154:9000/67yxnty6)
<!-- - [ ] If enemy with higher health in path, then seek next closest objective -->

                b.avoidBarBrawlsWithAllies, // Don't get stuck in a loop hurting allies
                b.doTelefrag,               // Telefrag someone if we can
                b.ratherDie,                // Suicide rather than give our mines to an opponent
                b.grabTavern,               // Interpose between enemy and tavern (short distance)
                b.goHunt,                   // Try to kill a nearby enemy
                b.snatchMine,               // Conquer a mine, but only if no enemy is near
                b.interceptEnemy,           // Try to intercept an enemy on his way to the tavern
                b.drinkIfEnemyIsNear,       // Drink if enemy is near
                b.assuredVictory,           // If we are likely to win, hang out at the tavern (thanks petbot!)
                b.doDrink,                  // Drink if we need to
                b.standAndFight,            // Deal as much damage as possible if we're in a fight we can't win
                b.goAfterTheRichKid,        // If someone has 50% of the mines, go attack him
                b.goMining,                 // Capture the nearest mine
                b.goToTavern,               // Go to the nearest tavern
                b.maybeSuicide,             // Can't do anything. Suicide if we don't have too many mines.
                b.bumRush,                  // Can't even suicide. Bum Rush the closest enemy.

