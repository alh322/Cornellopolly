(** Test Plan: In order to test the functions involved in updating the board, we
    opted for a more hands-on approach by playing the game ourselves. We found
    that this method allowed us to easily track changes in player money,
    position, and other game elements. While we did use OUnit testing to test
    functions from the Square module, we employed both black and glass box
    testing strategies. For example, the function [is_string_set] was glass box
    tested because we had to know that sets without colors returned as the
    string "Not used." Other things were black box tested, such as "get_turn,"
    which simply returns the turn of the board. This testing approach
    demonstrates the correctness of the system because helpers were tested
    during OUnit tests and proved to pass when we manually tested by playing the
    game. Other functionality not in OUnit testing also pass while we played the
    game many times to ensure correctness. *)

open OUnit2
open Cornellopoly.Square
open Yojson.Basic

let data_dir_prefix = "data" ^ Filename.dir_sep

let cornellopoly =
  Yojson.Basic.from_file (data_dir_prefix ^ "gameboard.json") |> populate_board

let generic_player =
  {
    name = "fred beans";
    position = 0;
    money = 0;
    jail = false;
    turn_order = 1;
    properties = [];
    has_gooj = false;
  }

let prisoner =
  {
    name = "osama bin laden";
    position = 1;
    money = 1000;
    jail = false;
    turn_order = 0;
    properties = [];
    has_gooj = false;
  }

let prisoner2 =
  {
    name = "osama bin laden2";
    position = 3;
    money = 1000;
    jail = false;
    turn_order = 0;
    properties = [];
    has_gooj = false;
  }

let ally =
  {
    name = "ally";
    position = 22;
    money = 1000;
    jail = false;
    turn_order = 0;
    properties = [];
    has_gooj = false;
  }

let square1 =
  {
    property = "Mallott Hall";
    value = 60;
    houses = 0;
    set = Brown;
    position = 1;
    owner = "1";
    base_rent = 2;
  }

let square2 =
  {
    property = "Okenshield's Dining";
    value = 60;
    houses = 0;
    set = Brown;
    position = 3;
    owner = "2s";
    base_rent = 4;
  }

let square3 =
  {
    property = "Chance 2";
    value = 200;
    houses = 0;
    set = Chance;
    position = 22;
    owner = "Ally";
    base_rent = 0;
  }

let jailsq =
  {
    property = "Jail";
    value = 0;
    houses = 0;
    set = Jail;
    position = 10;
    owner = "";
    base_rent = 0;
  }

let real_prisoner =
  {
    name = "Baddie";
    position = 10;
    money = 1000;
    jail = true;
    turn_order = 0;
    properties = [];
    has_gooj = false;
  }

let c =
  {
    index = 14;
    text = "Your friend brings their car to campus. Advance 5 steps";
  }

let clist = [ c ]
let lst1 = [ generic_player; prisoner; prisoner2; real_prisoner ]
let lst2 = [ generic_player; prisoner; prisoner2; ally ]
let sqlst = [ square1; square2; square3; jailsq ]
let sqlst2 = [ square1; square2; square3; jailsq ]
let b1 = { squares = sqlst; players = lst1; chance = clist; turn = 0 }
let b2 = { squares = sqlst2; players = lst2; chance = clist; turn = 20 }

let cornellopoly =
  Yojson.Basic.from_file (data_dir_prefix ^ "gameboard.json") |> populate_board

let find_player_by_turn_test (name : string) (plist : player list) (t : int)
    (pl : player) =
  name >:: fun _ -> assert_equal pl (find_player_by_turn plist t)

let find_player_by_name_test (name : string) (plist : player list) (n : string)
    (pl : player) =
  name >:: fun _ -> assert_equal pl (find_player_by_name plist n)

let valid_player_test (name : string) (plist : player list) (pl : player)
    (b : bool) =
  name >:: fun _ -> assert_equal b (valid_player plist pl)

let find_square_by_name_test (name : string) (plist : square list) (t : string)
    (pl : square) =
  name >:: fun _ -> assert_equal pl (find_square_by_name plist t)

let find_square_by_player_test (name : string) (plist : square list)
    (pl1 : player) (pl : square) =
  name >:: fun _ -> assert_equal pl (find_square_by_player plist pl1)

let buyable_test (name : string) (plist : square list) (pl1 : player)
    (pl : bool) =
  name >:: fun _ -> assert_equal pl (buyable plist pl1)

let must_pay_test (name : string) (plist : square list) (pl1 : player)
    (pl : bool) =
  name >:: fun _ -> assert_equal pl (must_pay plist pl1)

let must_tax_test (name : string) (plist : square list) (pl1 : player)
    (pl : bool) =
  name >:: fun _ -> assert_equal pl (must_tax plist pl1)

let string_of_set_test (name : string) (sq : square) (s : string) =
  name >:: fun _ -> assert_equal s (string_of_set sq)

let is_chance_test (name : string) (plist : square list) (pl1 : player)
    (pl : bool) =
  name >:: fun _ -> assert_equal pl (is_chance plist pl1)

let get_turn_test (name : string) (b : board) (pl : int) =
  name >:: fun _ -> assert_equal pl (get_turn b)

let squares_test (name : string) (b : board) (pl : square list) =
  name >:: fun _ -> assert_equal pl (squares b)

let players_test (name : string) (b : board) (pl : player list) =
  name >:: fun _ -> assert_equal pl (players b)

let valid_trade_test (name : string) (plist : square list) (pl : string)
    (b : bool) =
  name >:: fun _ -> assert_equal b (valid_trade plist pl)

let player_tests =
  [
    find_player_by_turn_test "Player turn Test 1" lst1 0 prisoner;
    find_player_by_turn_test "Player turn test 2" lst1 1 generic_player;
    find_player_by_name_test "Player name test" lst1 "fred beans" generic_player;
    find_player_by_name_test "Player name test 2" lst1 "osama bin laden"
      prisoner;
    find_player_by_name_test "Player name test 2" lst1 "osama bin laden2"
      prisoner2;
    valid_player_test "Valid player 1" lst1 prisoner2 true;
    valid_player_test "Valid player 2" lst1 prisoner true;
    valid_player_test "Valid player 3" lst1 generic_player true;
    valid_player_test "Valid player 4" lst1 ally false;
    valid_player_test "Valid player 5" lst1 real_prisoner true;
    find_square_by_name_test "square by name1" sqlst "Mallott Hall" square1;
    find_square_by_name_test "square by name2" sqlst "Okenshield's Dining"
      square2;
    find_square_by_name_test "square by name3" sqlst "Chance 2" square3;
    find_square_by_name_test "square by name4" sqlst "Jail" jailsq;
    find_square_by_player_test "squarebyplayer1" sqlst prisoner square1;
    find_square_by_player_test "squarebyplayer2" sqlst prisoner2 square2;
    find_square_by_player_test "squarebyplayer3" sqlst ally square3;
    find_square_by_player_test "squarebyplayer4" sqlst real_prisoner jailsq;
    buyable_test "buy 1" sqlst prisoner false;
    buyable_test "buy 2" sqlst prisoner2 false;
    buyable_test "buy 3" sqlst generic_player false;
    buyable_test "buy 4" sqlst ally false;
    buyable_test "buy 5" sqlst real_prisoner false;
    must_pay_test "pay 1" sqlst prisoner true;
    must_pay_test "pay 2" sqlst prisoner2 true;
    must_pay_test "pay 3" sqlst prisoner true;
    must_pay_test "pay 4" sqlst2 ally false;
    must_pay_test "Ally must pay test" sqlst ally false;
    must_pay_test "pay 4" sqlst generic_player false;
    must_tax_test "tax1" sqlst prisoner false;
    must_tax_test "tax2" sqlst prisoner2 false;
    must_tax_test "tax3" sqlst ally false;
    must_tax_test "tax4" sqlst real_prisoner false;
    string_of_set_test "string1" square1 "Brown property";
    string_of_set_test "string2" square2 "Brown property";
    string_of_set_test "string3" square3 "Not used";
    string_of_set_test "string4" jailsq "Not used";
    is_chance_test "chance1" sqlst prisoner false;
    is_chance_test "chance2" sqlst prisoner2 false;
    is_chance_test "chance3" sqlst generic_player false;
    is_chance_test "chance4" sqlst ally true;
    is_chance_test "chance5" sqlst real_prisoner false;
    get_turn_test "Turn test1" b1 0;
    get_turn_test "Turn test2" b2 20;
    squares_test "Squares" b1 sqlst;
    players_test "Players" b1 lst1;
    squares_test "Squares 2" b2 sqlst2;
    players_test "Players 2" b2 lst2;
    valid_trade_test "Valid 1" sqlst "Mallott Hall" true;
    valid_trade_test "Valid 2" sqlst "Ives Hall" false;
  ]

let suite = "Cornellopoly test suite" >::: List.flatten [ player_tests ]
let _ = OUnit2.run_test_tt_main suite
