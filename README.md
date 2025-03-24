# Cornellopoly
### *An elegant and functional implementation of the classic board game*

created by James Sy (jcs547), Allison Hui (alh322), Nathan Tai (nct36)

## Installation

For detailed installation steps from scratch, refer to INSTALL.md

## Gameplay and Premise

*Cornellopoly* is a terminal-based adaptation of Monopoly, and players strive
to achieve many of the same goals as the original. The ultimate object of the
game is to bankrupt the other players by buying properties, building houses and
hotels, and trading properties and cash across the board.

- Gameplay revolves around the main menu, where each player has the choice to
  **ROLL**, **BUILD** or **DESTROY** houses or hotels, **VIEW BOARD**, or
  **TRADE** with other players.

- Players start with 1500 to their name at the beginning of the game, and can
  earn money through rent payments, Chance cards and the Community Chest, passing
  *GO*, or electing to **DESTROY** houses or hotels.

- After a player rolls, they may land on a property that can be purchased if the
  player has enough money. A player may either **BUY** or **PASS** at this point.

- Chance cards and the Community Chest are available to all players and spread
  across squares on the board -- these can be positive, such as gaining money,
  or negative, with the possibility of the player paying out to one or more
  players.

- The **TRADE** menu allows a player to select properties or cash to send to
  another player; when it is the recipient's turn, they will send over their
  agreed-upon cash/properties on their side of the trade.

- **BUILD** and **DESTROY** allow players to construct houses and hotels on
  owned properties once they have a *monopoly* (ownership of all properties
  within a set) -- these raise rent that an opposing player must pay upon
  landing on an owned property.

- The *GO* square resides at position 0 on the board, and grants a passing
  player 200 in cash.

- The *JAIL* square traps a player, and they may only escape by rolling doubles,
  bailing out by paying, or by posessing a Get Out of Jail Free card which is
  automatically consumed. A player may be sent to *JAIL* by landing on the
  *GO TO JAIL* square or by pulling the unfortunate Chance card.

## Usage and Changes

The squares and players of *Cornellopoly* can be easily adapted to any style
of the classic 40-square board. *Gameboard.json* contains all the squares and
players for *Cornellopoly*, but all their attributes (including property and 
player names, property values, starting cash, etc.) can be modified with no
necessary change to the code.
