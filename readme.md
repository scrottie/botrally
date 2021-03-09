
notes.txt in this repo has more info.

rules:

https://www.ultraboardgames.com/robo-rally/game-rules.php

screenshots of each round are posted to, respectively:

http://45.79.14.228:11111/jpg/phase_1.png
http://45.79.14.228:11111/jpg/phase_2.png
http://45.79.14.228:11111/jpg/phase_3.png
http://45.79.14.228:11111/jpg/phase_4.png
http://45.79.14.228:11111/jpg/phase_5.png

manual parts of game play:

* placing your robot (including rotating it) 
* putting it back on the board after it falls off.
* refreshing the upgrade shop.
* buying anything from the upgrade shop (move energy to the bank, take the card and place it on your upgrades dock)
* voluntarily rebooting your robot is manually done but there isn't a reboot token yet. 
* various upgrades from the upgrade score require you to manually draw/discard/etc to do the thing it says.  see below.

automatic:

* triggered by dropping the dog on a pad:  dealing 9 cards (10 if you have the memory stick)
* playing through 5 registers then discarding your hand and the register cards
* conveyers, laser fire, movement, effects of many upgrades

release notes/rule nits:

* many upgrades are missing.
* some things happen automatically but many don't, which is sure to be confusing.
* conveyer belts:  double-speed don't go first. if completely full of objects and robots, or very full, it can deadlock and not move anyone.
* not applying $shoot_through_walls, $double_barrel_laser to rear facing lasers (rules not immediately clear to me/can be changed)
* tractor beam engages every other laser fired (eg, register 2, 4, 1, 3, 5, 2, 4...) instead of when desired
* presser beam engages every third laser fired
* teleporter doesn't yet correctly handle swapping places with another robot if you'd land on it
* deflector shield automatically engages and spends energy even when it may not optimal to do so
* when someone drops energy on the energy bank, there's a notification, but you can't see or interact with the energy (short of Firebug'ing it).  in general, once hidden, objects don't yet ever unhide.  todo.
* you don't actually get checkpoint tokens yet.

upgrade card with manual operations:

* defrag gizmo is a manual operation.  after dealing yourself cards (manually or using the dog), move a spam to the perm discard and draw another card, re-shuffling your discard (using the dog) to replenish it if exhausted.
* upgrade-recharge.png
* upgrade-spamblocker.png
* upgrade-recompile.png

upgrade cards that work automatically:

* presser beam:  you may push any robot you shoot one space in the direction you are shooting; costs 3 energy; fires automatically every third register [XXX would be nice if players could put markers on their register cards to indicate when they want to use presser/tractor beams]
* tractor beam:  pull any robot you shoot one space towards you (will not push you); fires automatically every other register
* double barrel laser:  deal one additional spam damage card to any robot you shoot; 2
* rail gun:  shoot through any number of walls and robots; costs 2
* firewall:  take no spam damage when rebooting; costs 3 
* memory stick:  draw one additional programming card at the start of each round; 3
* teleporter:  costs 1 energy to ignore an obstacle; costs 3   [XXX tho swapping places with another robot if you'd land on it isn't implemented]
* rear laser: your robot may shoot backward as well as forward; 2.  [robot does always shoot backwards as well as forward]
* ramming gear:  deal one spam when you push a robot; costs 2     [could do a version of this where you deal spam damage when you get pushed XXX]
* deflector shield:  spend one energy to negate robot attacks made on you for this register; 2 [automatically engages any time you would get hit, if you have energy, which may not be ideal]

still missing:

* life tokens -- four (lots of players) or three life tokens
* damage tokens -- for each damage token a robot has, they get one fewer than 9 cards
* powerdown token -- before the round starts a player may announce they will powerdown at the end of the round by placing the powerdown token on the sheet.  at the beginning of the next round, they discard all damage tokens.  confusing.
* drains, repulser fields, oil slicks, trap doors, water, flamers (only active on some phases)
* 1 fewer program cards per damage token
* 5 damage tokens or more and registers start to lock one by one starting at the right
* robot destroyed on 10th damage point, where it discards a life token
* explosions -- deal half damage for square away from the center
* deciding when extra/replacement weapons fire -- putting a counter on the expansion card isn't nearly as powerful as players deciding during the middle of activation whether they want to use various options but it's something

still missing, but can't implement or lower priority:

* repair sites
* ledges, ramps
* radioactive wast:  addition to causing damage gives the robot an option card when they end on it
* repair sites -- robot places their archive marker there (restore point); I guess the repair is queued for the end of the turn which is dumb; robot should repair at the end of the phase chop shop -- various decisions
* can chose to discard an option card to avoid taking a damage point -- player would have to make the decision before hand and maybe drop a counter on the option card
* flat devices, flying devices, launchers
* flying
