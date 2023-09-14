(* Square interface file *)

type color =
  | Brown
  | Cyan
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | Blue
  | Library
  | Grey
  | NonBuyable
  | Jail
  | Chance  (** The abstract type of values representing color. *)

type chance = {
  text : string;
  index : int;
}
(** The type of values representing chances. *)

type player = {
  name : string;
  position : int;
  money : int;
  jail : bool;
  turn_order : int;
  properties : string list;
  has_gooj : bool;
}
(** The type of values representing players. *)

type square = {
  property : string;
  value : int;
  houses : int;
  set : color;
  position : int;
  owner : string;
  base_rent : int;
}
(** The type of values representing squares. *)

type board = {
  squares : square list;
  players : player list;
  chance : chance list;
  turn : int;
}
(** The type of values representing the board. *)

val populate_square : Yojson.Basic.t -> square
(* [populate_square j] is the square that [j] represents. Parses a JSON
   representation into property, value, buildings, set, position, owner, and
   base rent to initialize square **)

val populate_player : Yojson.Basic.t -> player
(* [populate_player j] is the player that [j] represents. Parses a JSON
   representation into name, money, and turn order to initialize player. Assigns
   initial position to 0 and whether they are in jail to false.**)

val populate_chance : Yojson.Basic.t -> chance
(* [populate_chance j] is the chance that [j] represents.**)

val populate_board : Yojson.Basic.t -> board
(* [populate_board j] is the board that [j] represents.**)

val squares : board -> square list
(* [squares a] is an array of squares of board [a]. **)

val players : board -> player list
(* [players a] is an array of players of board [a]. **)

val get_turn : board -> int
(* [get_turn a] is the current turn of board [a], which corresponds to a player
   who is next to act.**)

val rand_int : int -> int
(* [rand_int max] returns a random integer in the range from 0 to [max].**)

val players_list_pay : player list -> player -> int -> player list
(* [players_list_pay pls pl amt] finds the player [pl] in [pls] and reduces the
   amount of money owned by [pl] by [amt].**)

val players_list_recieve : player list -> player -> int -> player list
(* [players_list_receive pls pl amt] finds the player [pl] in [pls] and
   increases the amount of money owned by [pl] by [amt].**)

val board_move_free : board -> player -> int -> int -> board
(* [board_move_free b pl t num] moves a free (not jailed) player [pl] on board
   [b] according to the turn [t] and number [num]**)

val board_move_jailed : square list -> board -> player -> int -> int -> board
(* [board_move_jailed lst b pl t num] moves a jailed player [pl] on board [b] by
   changing the squares list [lst] according to turn [t] and number [num].**)

val board_buy : board -> square -> player -> board
(* [board_buy b sq pl] updates the board [b] after player [pl] buys something on
   square [sq].**)

val board_pay : board -> player -> player -> board
(* [board_buy b pl1 pl2] updates the board [b] after one player [pl1] buys
   something from another player [pl2].**)

val board_build : board -> square -> player -> board
(* [board_build b sq pl] updates the board [b] after player [pl] builds a house
   on square [sq] if the maximum number of houses on [sq] has not been reached
   yet.**)

val board_destroy : board -> square -> player -> board
(* [board_destroy b sq pl] updates the board [b] after player [pl] decides to
   give up a house on square [sq].**)

val board_bail : board -> player -> board
(* [board_bail b pl] updates the board [b] after player [pl] is bailed out of
   jail.**)

val board_chance : board -> player -> board
(* [board_chance b pl] updates the board [b] after player [pl] is given a chance
   card.**)

val find_square_by_player : square list -> player -> square
(* [find_square_by_player lst pl] returns the square of square list [lst] hat
   player [pl] is on.**)

val find_player_by_turn : player list -> int -> player
(* [find_square_by_player plst tn] returns the player in player list [plist]
   with the turn [tn].**)

val find_square_by_name : square list -> string -> square
(* [find_square_by_name sqlst name] returns the square in square list [sqlist]
   with the identifier [name].**)

val find_player_by_name : player list -> string -> player
(* [find_player_by_name plst name] returns the player in player list [plist]
   with the identifier [name].**)

val string_of_set : square -> string
(* [string_of_set sq] returns a string representation of the set attribute of
   [sq].**)

val valid_build : board -> string list -> square -> bool
(* [valid_build strlst name] searches through string list [strlst] and returns
   true if it is possible to build.**)

val valid_player : player list -> player -> bool
(* [valid_player pls pl] searches through [pls] and returns true if the name of
   [pl] matches any of the names in [pls].**)

val valid_trade : square list -> string -> bool
(* [valid_player sqlst name] searches through [sqlst] and returns true if [name]
   matches any of the property attributes of elements in [sqlst].**)

val transfer_ownership_board : board -> string -> player -> player -> board
(* [transfer_ownership_board b sqstr pl] changes the owner attribute of the
   property that has name [sqstr] to the new owner, [pl].**)

val buyable : square list -> player -> bool
(* [buyable sqlst pl] searches through square list [sqlst] and returns true if
   it the player [pl] is able to buy that square, considering it is not a
   NonBuyable, or Chance, and [pl] is not jailed.**)

val must_pay : square list -> player -> bool
(* [must_pay sqlst pl] finds the square of [sqlist] that player [pl] is on and
   returns true if it [pl] must pay for landing on that square.**)

val must_tax : square list -> player -> bool
(* [must_pay sqlst pl] finds the square of [sqlist] that player [pl] is on and
   returns true if it [pl] must be taxed for landing on that square.**)

val tax_board : board -> player -> board
(* [board_chance b pl] updates the board [b] after player [pl] is taxed.**)

val is_chance : square list -> player -> bool
(* [is_chance sqlst pl] finds the square of [sqlist] that player [pl] is on and
   returns true if it is a Chance. **)

val welcome_logo : unit -> string
(* [welcome_logo ()] returns the ASCII image to be shown at the start of the
   game.**)

val cornellopoly_board : unit -> string
(* [cornellopoly_board ()] returns the list-represented game board.**)
