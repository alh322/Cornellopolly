(* Square, building and player implementation *)

open Yojson.Basic.Util

let pass_go_money = 400

type player = {
  name : string;
  position : int;
  money : int;
  jail : bool;
  turn_order : int;
  properties : string list;
  has_gooj : bool;
}

type chance = {
  text : string;
  index : int;
}

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
  | Chance

type square = {
  property : string;
  value : int;
  houses : int;
  set : color;
  position : int;
  owner : string;
  base_rent : int;
}

type board = {
  squares : square list;
  players : player list;
  chance : chance list;
  turn : int;
}

let populate_square json =
  {
    property = json |> member "property name" |> to_string;
    value = json |> member "value" |> to_int;
    houses = 0;
    set =
      (match json |> member "set" |> to_string with
      | "Brown" -> Brown
      | "Cyan" -> Cyan
      | "Pink" -> Pink
      | "Orange" -> Orange
      | "Red" -> Red
      | "Yellow" -> Yellow
      | "Green" -> Green
      | "Blue" -> Blue
      | "Library" -> Library
      | "Utility" -> Grey
      | "Grey" -> NonBuyable
      | "Chance" -> Chance
      | _ -> Jail);
    position = json |> member "pos" |> to_int;
    owner = "";
    base_rent = json |> member "base rent" |> to_int;
  }

let populate_player json =
  {
    name = json |> member "player name" |> to_string;
    money = json |> member "cash" |> to_int;
    position = 0;
    jail = false;
    turn_order = json |> member "turn order" |> to_int;
    properties = [];
    has_gooj = false;
  }

let populate_chance json =
  {
    index = json |> member "index" |> to_int;
    text = json |> member "text" |> to_string;
  }

let populate_board json =
  {
    squares =
      json |> member "squares" |> to_list |> List.map populate_square;
    players =
      json |> member "players" |> to_list |> List.map populate_player;
    turn = json |> member "first move" |> to_int;
    chance =
      json |> member "chance cards" |> to_list |> List.map populate_chance;
  }
let squares brd = brd.squares
let players brd = brd.players
let get_turn brd = brd.turn
let num_pls = 4
let house_cost = 50
let bail_cost = 50
let destory_house_bonus = 30
let max_houses = 5
let broke_ass_mf =
  {
    name = "";
    position = 0;
    money = min_int;
    jail = false;
    turn_order = -1;
    properties = [];
    has_gooj = false;
  }
let rich_ass_mf =
  {
    name = "";
    position = 0;
    money = max_int;
    jail = false;
    turn_order = -1;
    properties = [];
    has_gooj = false;
  }
let nowhere = 
  {
    property = "";
    value = 0;
    houses = -1;
    set = NonBuyable;
    position = 41;
    owner = "";
    base_rent = 0;
  }

let rand_int max = 
  Random.self_init ();
  Random.int max

let rec find_square_by_player list (player : player) = 
  match list with 
  | h :: t -> if h.position = player.position 
    then h 
    else find_square_by_player t player
  | _ -> nowhere

let rec find_player_by_turn list turn = 
  match list with 
  | h :: t -> if h.turn_order = turn mod num_pls
    then h 
    else find_player_by_turn t turn
  | _ -> broke_ass_mf

let rec find_square_by_num list number = 
  match list with 
  | h :: t -> if h.position = number mod 40
    then h
    else find_square_by_num t number
  | _ -> nowhere

let rec find_square_by_name (list : square list) string = 
  match list with 
  | h :: t -> if h.property = string 
    then h 
    else find_square_by_name t string
  | _ -> nowhere

let rec find_player_by_name (list : player list) string = 
  match list with
  | h :: t -> if h.name = string 
    then h
    else  find_player_by_name t string
  | _ -> broke_ass_mf
  
let string_of_set (sq : square) = match sq.set with
| Brown -> "Brown property"
| Cyan -> "Cyan property"
| Pink -> "Pink property"
| Orange -> "Orange property"
| Red -> "Red property"
| Yellow -> "Yellow property"
| Green -> "Green property"
| Blue -> "Blue property"
| Library -> "library"
| Grey -> "utility"
| _ -> "Not used"

let p_move_free (player : player) (number : int) : player =
  let new_loc = player.position + number in
  if new_loc = 30 then
    let () = ANSITerminal.print_string [ ANSITerminal.red] 
      "\n You got sent to jail. Your new position is 10\n"; in
    {
      player with 
      position = 10;
      jail = true;
    }
  else if new_loc > 39 then
    let () = ANSITerminal.print_string [ ANSITerminal.blue] (
      "\nYou have passed go and got paid!
      Your new money is " ^ 
      string_of_int (player.money + pass_go_money) ^ " dollars.\n"); in
    {
      player with
      position = new_loc mod 40;
      money = player.money + pass_go_money;
    }
  else { player with position = new_loc }

let rec p_list_move_free list player turn number = 
  match list with 
  | h :: t -> 
    if h.turn_order = turn mod num_pls
    then (p_move_free player number) :: t
    else h :: p_list_move_free t player turn number
  | _ -> []

let board_move_free board (player : player) turn number = 
  let dest = find_square_by_num board.squares (player.position + number) in
  let () = ANSITerminal.print_string [ ANSITerminal.blue] (
    "\nYou rolled a " ^ string_of_int number ^
    "\nYour new position is " ^ string_of_int dest.position ^
    "\nThis is " ^ dest.property ^ ".\n"); in
  { board with players = p_list_move_free board.players player turn number}

let p_move_jailed (squares : square list) (player : player) (number : int) : player = 
  (**PLEASE REWRITE THE CRITERA FOR ESCAPING JAIL SO THAT THIS BOOL
      IS TRUE ONLY WHEN THEY HAVE ESCAPED*)
  if player.has_gooj = true
  then let () = ANSITerminal.print_string [ANSITerminal.yellow] "\nYou used your Get Out of Jail Free card and are free!\n" in
    {
      player with
      position = 10;
      jail = false;
      has_gooj = false;
    }
  else let escaped = false in
      if escaped then let new_loc = 10 + number in 
      let dest = find_square_by_num squares new_loc in
      let () = ANSITerminal.print_string [ANSITerminal.yellow]
      ("\nYou have escaped jail!\nYou rolled a " ^ string_of_int number ^
      "\nYour new position is " ^ string_of_int new_loc ^
      "\nThis is " ^ dest.property ^ "\n"); in
      {
        player with
        position = new_loc;
        jail = false;
      }
      else let () = ANSITerminal.print_string [ANSITerminal.yellow]
      "\nYou are still in jail.\n"; in player

let rec p_list_move_jailed squares list player turn number = 
  match list with 
  | h :: t -> 
    if h.turn_order = turn mod num_pls
    then (p_move_jailed squares player number) :: t
    else h :: p_list_move_jailed squares t player turn number
  | _ -> []

let board_move_jailed squares board (player : player) turn number = 
  { board with 
  players = p_list_move_jailed squares board.players player turn number}

let rec properties_list_buy list square player = 
  match list with 
  | h :: t -> if h = square then { square with owner = player.name } :: t 
    else h :: (properties_list_buy t square player)
  | _ -> []

let rec players_list_buy list square player = 
  match list with 
  | h :: t -> if h = player then {
    player with 
    properties = square.property :: h.properties;
    money = h.money - square.value } :: t
    else h :: players_list_buy t square player
  | _ -> []

let rec properties_list_build list square = 
  match list with
  | h :: t -> if h = square then { 
    h with houses = h.houses + 1} :: t
    else h :: properties_list_build t square
  | _ -> []

let rec properties_list_destroy list square = 
  match list with 
  | h :: t -> if h = square then {
    h with houses = h.houses - 1} :: t
    else h :: properties_list_destroy t square
  | _ -> []

let rec players_list_build list square player = 
  let amt = match square.houses + 1 with
    | 0 -> square.base_rent
    | 1 -> square.base_rent * 5
    | 2 -> square.base_rent * 15 
    | 3 -> square.base_rent * 30
    | 4 -> square.base_rent * 45
    | _ -> square.base_rent * 60 in
  let cost = match square.set with 
    | Brown
    | Cyan -> house_cost
    | Pink
    | Orange -> house_cost * 2
    | Red
    | Yellow -> house_cost * 3
    | _  -> house_cost * 4 in
  match list with
  | h :: t -> if h = player then 
    let () = ANSITerminal.print_string [ ANSITerminal.magenta] (
    "\n" ^ 
    player.name ^ " has built a house on " ^
    square.property ^ " which now has " ^ 
    string_of_int (square.houses + 1) ^ " houses, bringing rent up to " ^
    string_of_int (amt) ^ " dollars. " ^
    player.name ^ " now has " ^
    string_of_int (player.money - cost) ^ " dollars.\n"); in
    { h with money = h.money - cost; } :: t
    else h :: players_list_build t square player
  | _ -> []

  let rec players_list_destroy list square player = 
    let amt = match square.houses - 1 with
      | 0 -> square.base_rent
      | 1 -> square.base_rent * 5
      | 2 -> square.base_rent * 15 
      | 3 -> square.base_rent * 30
      | 4 -> square.base_rent * 45
      | _ -> square.base_rent * 60 in
    let cost = match square.set with 
      | Brown
      | Cyan -> destory_house_bonus
      | Pink
      | Orange -> destory_house_bonus * 2
      | Red
      | Yellow -> destory_house_bonus * 3
      | _ -> destory_house_bonus * 4 in
    match list with
    | h :: t -> if h = player then 
      let () = ANSITerminal.print_string [ ANSITerminal.magenta] (
      "\n" ^ 
      player.name ^ " has destroyed a house on " ^
      square.property ^ " which now has " ^ 
      string_of_int (square.houses - 1) ^ " houses, bringing rent down to " ^
      string_of_int (amt) ^ " dollars. " ^
      player.name ^ " now has " ^
      string_of_int (player.money + cost) ^ " dollars.\n"); in
      { h with money = h.money + cost; } :: t
      else h :: players_list_build t square player
    | _ -> []

let rec players_list_pay list sender amt = 
  match list with 
  | h :: t -> 
    if h = sender then { h with money = h.money - amt} :: t
    else h :: players_list_pay t sender amt
  | _ -> []

let rec players_list_recieve list reciever amt = 
  match list with 
  | h :: t ->
    if h = reciever then { h with money = h.money + amt} :: t
    else h :: players_list_recieve t reciever amt
  | _ -> []

let rec players_list_bail list prisoner = 
  match list with 
  | h :: t ->
    if h = prisoner then let () = 
    ANSITerminal.print_string
    [ANSITerminal.yellow]
    (
      h.name ^ " has bailed out of prison for 50 dollars and now has " ^
      string_of_int (h.money - bail_cost) ^ " dollars.\n"
    ) in
    { h with 
      money = h.money - bail_cost;
      jail = false
    } :: t
    else h :: players_list_bail t prisoner
  | _ -> []

let board_bail board player = 
  {board with players = players_list_bail board.players player}

let board_buy board square player = 
  if square.value > player.money then 
  let () = ANSITerminal.print_string [ ANSITerminal.magenta ]
    "You can't afford that property!" in board else
  let () = ANSITerminal.print_string [ ANSITerminal.magenta ] (
    "\n" ^
    player.name ^ " has bought " ^ 
    square.property ^ " for " ^ 
    string_of_int square.value ^ " and now has " ^ 
    (string_of_int (player.money - square.value)) ^ " left.\n"); in
  { 
    board with players = players_list_buy board.players square player;
    squares = properties_list_buy board.squares square player
  }

let board_build board square player = match square.houses with
  | 5 -> let () = ANSITerminal.print_string [ANSITerminal.on_yellow]
  "Cannot build on this property, maximum number of houses reached"; in board
  | _ -> {
    board with 
    players = players_list_build board.players square player;
    squares = properties_list_build board.squares square;
  }

let board_destroy board square player = match square.houses with
  | 0 -> let () = ANSITerminal.print_string [ANSITerminal.on_yellow]
  "Cannot build on this property, maximum number of houses reached"; in board
  | _ -> {
    board with 
    players = players_list_destroy board.players square player;
    squares = properties_list_destroy board.squares square;
  }

let board_pay board (reciever : player) (sender : player) = 
  let square = find_square_by_num board.squares sender.position in
  let amt = match square.houses with
    | 0 -> square.base_rent
    | 1 -> square.base_rent * 5
    | 2 -> square.base_rent * 15 
    | 3 -> square.base_rent * 30
    | 4 -> square.base_rent * 45
    | _ -> square.base_rent * 60 in
  let () = ANSITerminal.print_string [ ANSITerminal.cyan] (
    "\nThis belongs to " ^
    square.owner ^ " and has " ^ 
    string_of_int square.houses ^ " houses, \nso " ^ 
    sender.name ^ " must pay him or her " ^
    string_of_int square.base_rent ^ " dollars. \n" ^
    sender.name ^ " now has " ^
    string_of_int (sender.money - amt) ^ " dollars. \n"); in
  let payed_list = players_list_pay board.players sender amt in
  {
    board with 
    players = players_list_recieve payed_list reciever amt
  }

let buyable squares player = 
  let square = find_square_by_player squares player in
  match square.set with 
  | Jail -> false
  | NonBuyable -> false
  | Chance -> false
  | _ -> square.owner = ""

let must_pay squares player = 
  let square = find_square_by_player squares player in
  match square.set with 
  | Jail -> false
  | NonBuyable -> false
  | Chance -> false
  | _ -> if square.owner = player.name
    then let () = ANSITerminal.print_string [ANSITerminal.blue]
  "\nYou own this, no need to pay\n"; in 
    (square.owner <> "" && square.owner <> player.name)
    else (square.owner <> "" && square.owner <> player.name)

let must_tax squares player =
  let square = find_square_by_player squares player in
  match square.position with
  | 4 -> true
  | 38 -> true
  | _ -> false

let tax_board board player = 
  let pl_in_board = find_player_by_name board.players player.name in
  let tax_square = find_square_by_num board.squares player.position in
  let () = ANSITerminal.print_string [ ANSITerminal.blue] (
    "\nYou have landed on " ^ (tax_square.property) ^
    " and have been taxed " ^ string_of_int (tax_square.value) ^ ".\n"); in
  match player.position with
  | 4 -> { board with players = players_list_pay board.players pl_in_board 200}
  | 38 -> { board with players = players_list_pay board.players pl_in_board 100}
  | _ -> board

let is_chance squares player = 
  let square = find_square_by_player squares player in
  match square.set with 
  | Chance -> true
  | _ -> false

let rec find_chance_by_number (list : chance list) (int : int) =
  match list with
  | h :: t -> if h.index = int then h
    else find_chance_by_number t int
  | _ -> failwith "error in find chance by number"

let rec all_player_chance list player amt =
  match list with 
  | h :: t -> if h <> player then {
    h with money = 
    h.money - amt} :: all_player_chance t player amt
    else {
    h with money =
    amt * (num_pls - 1) + h.money} :: all_player_chance t player amt
  | [] -> []

let rec one_player_chance_cash list player amt = 
  match list with 
  | h :: t -> if h = player then 
    let () = ANSITerminal.print_string
    [ANSITerminal.red] ( "\n" ^
    player.name ^ " now has " ^
    string_of_int (player.money + amt) ^ " dollars.\n"
    ) in {
    h with money = h.money + amt;} :: t
    else h :: one_player_chance_cash t player amt
  | [] -> []

let richer p1 p2 = if p1.money > p2.money then p1 else p2
let poorer p1 p2 = if p1.money < p2.money then p1 else p2
let rec richest_player list = 
  List.fold_right richer list broke_ass_mf

let rec poorest_player list =
  List.fold_right poorer list rich_ass_mf

let chance_rob_rich list player = 
  let rich = if richest_player list <> player 
    then 
      richest_player list
    else
      let list_without_player = 
      List.filter (fun x -> player.name <> x.name) list in 
      richest_player list_without_player in
  let amt = rich.money / 4 in
  let af_pay = players_list_pay list rich amt in
  let () = ANSITerminal.print_string
  [ANSITerminal.red] ( "\n" ^
  rich.name ^ " has been robbed by " ^
  player.name ^ " for " ^
  string_of_int amt ^ " dollars.\n" ^
  rich.name ^ " now has " ^
  string_of_int (rich.money - amt) ^ " dollars.\n" ^
  player.name ^ " now has " ^
  string_of_int (player.money + amt) ^ " dollars.\n") in
  players_list_recieve af_pay player amt

let chance_rob_poor list player = 
  let poor = if poorest_player list <> player
    then 
      poorest_player list
    else 
      let list_without_player = 
      List.filter (fun x -> player.name <> x.name) list in
      poorest_player list_without_player in 
  let amt = player.money / 4 in 
  let af_pay = players_list_pay list player amt in
  let () = ANSITerminal.print_string
  [ANSITerminal.red] ( "\n" ^
  player.name ^ " has been robbed by " ^
  poor.name ^ " for " ^
  string_of_int amt ^ " dollars.\n" ^
  player.name ^ " now has " ^
  string_of_int (player.money - amt) ^ " dollars.\n" ^
  poor.name ^ " now has " ^
  string_of_int (poor.money + amt) ^ " dollars.\n") in
  players_list_recieve af_pay poor amt

let rec goojify list player =
  match list with
  | h :: t -> if h = player then 
    let () = ANSITerminal.print_string
    [ANSITerminal.green] ("\n" ^ 
    player.name ^ " now has a Get Out of Jail Free Card!\n"
    ) in {
    h with has_gooj = true;} :: t
    else h :: goojify t player
  | [] -> []

let rec chance_move list (player : player) move =
  match list with 
  | h :: t -> if h = player then let p = p_move_free player move in
    p :: t else h :: chance_move t player move
  | [] -> []

let chance_move_library list (player : player) =
  let move = 10 - (player.position mod 5) in
  chance_move list player move

let chance_move_field list (player : player) = 
  let move = if player.position < 29 
    then 29 - player.position 
    else 69 - player.position in
  chance_move list player move

let rec chance_to_jail list (player : player) : player list= 
  match list with
  | h :: t -> if h = player then 
    {h with position = 10;
    jail = true;} :: t
    else h :: chance_to_jail t player
  | [] -> []

let board_chance board player = 
  let debug = false in
  let num = if debug then 1 else rand_int 16 in
  let chance = find_chance_by_number board.chance num in
  let () = ANSITerminal.print_string 
  [ANSITerminal.red] (
  "\nThe card reads: " ^
  chance.text ^ 
  "\n") in
  match chance.index with 
  | 0 -> {board with players = 
    all_player_chance board.players player 50}
  | 1 -> {board with players = 
    all_player_chance board.players player (-10)}
  | 2 -> {board with players = 
    one_player_chance_cash board.players player (-200)}
  | 3 -> {board with players = 
    one_player_chance_cash board.players player (-125)}
  | 4 -> {board with players = 
    one_player_chance_cash board.players player (-75)}
  | 5 -> {board with players = 
    one_player_chance_cash board.players player (-25)}
  | 6 -> {board with players = 
    one_player_chance_cash board.players player 250}
  | 7 -> {board with players = 
    one_player_chance_cash board.players player 175}
  | 8 -> {board with players = 
    one_player_chance_cash board.players player 100}
  | 9 -> {board with players = 
    one_player_chance_cash board.players player 25}
  | 10 -> {board with players = 
    chance_rob_rich board.players player}
  | 11 -> {board with players = 
    chance_rob_poor board.players player}
  | 12 -> {board with players = 
    chance_move_library board.players player}
  | 13 -> {board with players = 
    chance_move_field board.players player}
  | 14 -> {board with players = 
    chance_move board.players player 5}
  | 15 -> {board with players = 
    chance_move board.players player (-3)}
  | 17 -> {board with players = 
    goojify board.players player}
  | _ -> {board with players = 
    chance_to_jail board.players player}

let valid_build board strlist square = 
  let func = find_square_by_name board.squares in
  let list = List.map func strlist in
  let color = square.set in
  let critical_count = match color with
  | Brown
  | Blue -> 2
  | Cyan
  | Pink
  | Orange
  | Red
  | Yellow
  | Green -> 3
  | _ -> 5 in
  let func sq = (sq.set = color) in
  let filt = List.filter func list in
  let num = List.length filt in
  num = critical_count

(* -------------------------------------------------------------------------- *)

let welcome_logo () = 

  {|
                                                                                                          /|\    
  ██████╗ ██████╗ ██████╗ ███╗   ██╗███████╗██╗     ██╗      ██████╗ ██████╗  ██████╗ ██╗  ██╗   ██╗     // \\
  ██╔════╝██╔═══██╗██╔══██╗████╗  ██║██╔════╝██║     ██║     ██╔═══██╗██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝   //___\\
  ██║     ██║   ██║██████╔╝██╔██╗ ██║█████╗  ██║     ██║     ██║   ██║██████╔╝██║   ██║██║   ╚████╔╝   || III ||
  ██║     ██║   ██║██╔══██╗██║╚██╗██║██╔══╝  ██║     ██║     ██║   ██║██╔═══╝ ██║   ██║██║    ╚██╔╝    ||  0  ||
  ╚██████╗╚██████╔╝██║  ██║██║ ╚████║███████╗███████╗███████╗╚██████╔╝██║     ╚██████╔╝███████╗██║     ||     ||
    ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═══╝╚══════╝╚══════╝╚══════╝ ╚═════╝ ╚═╝      ╚═════╝ ╚══════╝╚═╝    ||     ||
  |}

let cornellopoly_board () =

  {|
  
  0: PASS GO -- Collect 200
  1: Mallott Hall -- Brown (60)
  2: COMMUNITY CHEST
  3: Okenshield's Dining -- Brown (60)
  4: HOUSING PAYMENT -- Pay 200
  5: URIS LIBRARY (200)
  6: Uris Hall -- Cyan (100)
  7: ??? CHANCE ???
  8: Plant Sciences Building -- Cyan (100)
  9: McGraw Hall -- Cyan (120)
  
  10: --------- JAIL ----------


  11: Goldwin-Smith Hall -- Pink (140)
  12: Academic Advising -- Utility (150)
  13: Baker Laboratory -- Pink (140)
  14: Rockefeller Hall -- Pink (140)
  15: OLIN LIBRARY (200)
  16: Cornell Health -- Orange (180)
  17: COMMUNITY CHEST
  18: Annabel Taylor Hall -- Orange (180)
  19: Barton Hall -- Orange (200)

  20: ------- FREE PARKING -------

  21: Warren Hall -- Red (220)
  22: ??? CHANCE ???
  23: Kennedy Hall -- Red (220)
  24: Physical Sciences Building (240)
  25: MANN LIBRARY (200)
  26: Dairy Bar -- Yellow (260)
  27: Art Museum -- Yellow (260)
  28: Career Services -- Utility (150)
  29: Schoelkopf Field -- Yellow (280)

  30: -------- GO TO JAIL --------

  31: Upson Hall -- Green (300)
  32: Klarman Hall -- Green (300)
  33: Community Chest
  34: Statler Hotel -- Green (320)
  35: MUI HO FINE ARTS LIBRARY (200)
  36: ??? CHANCE ???
  37: Gates Hall -- Blue (350)
  38: MEAL PLAN -- Pay 100
  39: Clock Tower -- Blue (350)

  - EXIT
  |} ^ "\n>> "

(* ------------------------------------------------------------------------- *)


let rec valid_player list pl = 
  match list with 
  | h :: t -> if h.name = pl.name then true else valid_player t pl
  | [] -> false

let rec valid_trade list str = 
  match list with
  | h :: t -> if h.property = str then true else valid_trade t str
  | [] -> false

let rec transfer_ownership_sq list sqstr reciever = 
  match list with
  | h :: t -> if h.property = sqstr then 
    {h with owner = reciever.name} :: t
    else h :: transfer_ownership_sq t sqstr reciever
  | [] -> []

let rec obtain_ownership_pl list reciever str =
  match list with 
  | h :: t -> if h = reciever then {
    h with 
    properties = str :: h.properties
    } :: t
    else h :: obtain_ownership_pl t reciever str
  | [] -> []

let rec lose_ownership_pl list loser str = 
  match list with 
  | h :: t -> if h = loser then {
    loser with properties = 
    List.filter (fun x -> x <> str) loser.properties
    } :: t
    else h :: lose_ownership_pl t loser str
  | [] -> []

  let transfer_ownership_board board str sender reciever = 
    let newsqs = transfer_ownership_sq board.squares str reciever in
    let tained_list = obtain_ownership_pl board.players reciever str in
    let lost_list = lose_ownership_pl tained_list sender str in
    {
      board with 
      squares = newsqs;
      players = lost_list;
    }
