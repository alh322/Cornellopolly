(* Immutable implementation of main loop*)
open Cornellopoly.Square
open Yojson.Basic
open Stdlib

let data_dir_prefix = "data" ^ Filename.dir_sep

let gameboard =
  populate_board (Yojson.Basic.from_file (data_dir_prefix ^ "gameboard.json"))

let sqrs = squares gameboard
let plrs = players gameboard
let turn = get_turn gameboard

let std_message player =
  let jail_opt = if player.jail then "\n- BAIL" else "" in
  let jail_msg = if player.jail then "\nYou are currently in jail.\n" else "" in
  "-----------------------------------------------------------------------------------------------------------------\n"
  ^ "-----------------------------------------------------------------------------------------------------------------"
  ^ "\nIt is currently " ^ player.name ^ "'s turn." ^ jail_msg
  ^ "\nYou current position is "
  ^ string_of_int player.position
  ^ "\nYou have " ^ string_of_int player.money
  ^ " dollars\n  \nPlease make a move.\nYou can choose to:\n" ^ jail_opt
  ^ "\n- ROLL\n- BUILD\n- DESTROY\n- VIEW BOARD\n- TRADE\n- QUIT\n>> "

let buy_message squares player =
  let square = find_square_by_player squares player in
  "\n" ^ player.name ^ " has landed on " ^ square.property ^ ", a "
  ^ string_of_set square ^ " owned by no one and worth "
  ^ string_of_int square.value ^ ". You can choose to:\n- BUY\n- PASS\n>> "

let pass_message squares player =
  let square = find_square_by_player squares player in
  "\n" ^ player.name ^ " has decided not to buy " ^ square.property ^ ".\n"

let rec list_to_string (list : string list) =
  match list with
  | h :: t -> "\n - " ^ h ^ list_to_string t
  | [] -> ""

let build_message board player =
  let func str =
    let sq = find_square_by_name board.squares str in
    valid_build board player.properties sq
  in
  "\n\
   Please type in the name of the property you would like to build on. \n\
  \    You can also type \"EXIT\" to exit the build menu.\n\
  \    Make sure the name of the property you would like to build is \n\
  \    exactly as how it appears below. Here is the list of properties you \n\
  \    can build on: "
  ^ list_to_string (List.filter func player.properties)
  ^ "\n>> "

let destroy_message board player =
  let func str =
    let sq = find_square_by_name board.squares str in
    valid_build board player.properties sq
  in
  "\n\
   Please type in the name of the property you would like to destroy on. \n\
  \    You can also type \"EXIT\" to exit the destroy menu.\n\
  \    Make sure the name of the property you would like to destroy is \n\
  \    exactly as how it appears below. Here is the list of properties you \n\
  \    can destroy on: "
  ^ list_to_string (List.filter func player.properties)
  ^ "\n>> "

let trade_message board player =
  let list_without_player =
    List.filter (fun x -> player.name <> x.name) board.players
  in
  "\n\
   Please type in the name of the player you would like to trade with. \n\
  \    You can also type \"EXIT\" to exit the trade menu.\n\
  \    Make sure the name of the player you would like to trade with is \n\
  \    exactly as how it appears below. Here is the list of players you \n\
  \    can trade with: "
  ^ list_to_string (List.map (fun x -> x.name) list_without_player)
  ^ "\n>> "

let trade_command_msg =
  "\n\
   Please type in what you would like to trade. \n\
  \    You can also type \"EXIT\" to exit the trade menu.\n\
  \    Make sure what you would like to trade is \n\
  \    exactly as how it appears below. Here is the list of items you \n\
  \    can trade: \n\
  \   - MONEY\n\
  \   - PROPERTIES" ^ "\n>> "

let money_trade_msg player =
  "\n\
   Please type in how much money you would like to trade. \n\
  \    You can also type \"EXIT\" to exit the trade menu.\n\
  \    Make sure you type in exactly the amount you would like to trade. \n\
  \    Here is your current balance:  " ^ string_of_int player.money ^ "\n "
  ^ "\n>> "

let prop_trade_msg player =
  "\n\
   Please type in the name of the property you would like to trade. \n\
  \    You can also type \"EXIT\" to exit the trade menu.\n\
  \    Make sure the name of the property you would like to trade is \n\
  \    exactly as how it appears below. Here is the list of properties you \n\
  \    can trade: "
  ^ list_to_string player.properties
  ^ "\n>> "

let rec process_trade_prop board sender reciever =
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.on_magenta; ANSITerminal.black ]
      (prop_trade_msg sender)
  in
  match read_line () with
  | "EXIT" -> board
  | str ->
      if valid_trade board.squares str then
        let () =
          ANSITerminal.print_string
            [ ANSITerminal.on_magenta; ANSITerminal.black ]
            (sender.name ^ " has traded with " ^ reciever.name
           ^ " and given them ownership of " ^ str)
        in
        transfer_ownership_board board str sender reciever
      else
        let () =
          ANSITerminal.print_string [ ANSITerminal.white ]
            "\nYou can't trade that property!"
        in
        process_trade_prop board sender reciever

let rec process_trade_money board sender reciever =
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.on_cyan; ANSITerminal.black ]
      (money_trade_msg sender)
  in
  match read_line () with
  | "EXIT" -> board
  | intstr -> (
      let opt = Stdlib.int_of_string_opt intstr in
      match opt with
      | Some amt ->
          if amt > sender.money then
            let () =
              ANSITerminal.print_string []
                "That is not a valid amount! Please enter a valid integer."
            in
            process_trade_money board sender reciever
          else
            let payed_list = players_list_pay board.players sender amt in
            let recieved_list = players_list_recieve payed_list reciever amt in
            let () =
              ANSITerminal.print_string
                [ ANSITerminal.on_magenta ]
                (sender.name ^ " has traded with " ^ reciever.name
               ^ " and given them " ^ string_of_int amt ^ " dollars.\n")
            in
            { board with players = recieved_list }
      | None ->
          let () =
            ANSITerminal.print_string []
              "That is not a valid amount! Please enter a valid integer."
          in
          process_trade_money board sender reciever)

let rec process_trade_cmd board player str =
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.on_magenta; ANSITerminal.black ]
      trade_command_msg
  in
  match read_line () with
  | "MONEY" -> process_trade_money board player str
  | "PROPERTIES" -> process_trade_prop board player str
  | "EXIT" -> board
  | _ ->
      let () =
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n Trouble understanding command. Try again.\n"
      in
      process_trade_cmd board player str

let rec process_trade board player =
  let () =
    ANSITerminal.print_string
      [ ANSITerminal.black; ANSITerminal.on_magenta ]
      (trade_message board player)
  in
  match read_line () with
  | "EXIT" -> board
  | str ->
      let target = find_player_by_name board.players str in
      if valid_player board.players target && player <> target then
        process_trade_cmd board player target
      else
        let () =
          ANSITerminal.print_string [ ANSITerminal.white ]
            "\nYou can't trade with that player!"
        in
        process_trade board player

let rec process_buy board square player =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    (buy_message board.squares player);
  match read_line () with
  | "BUY" -> board_buy board square player
  | "PASS" ->
      let () =
        ANSITerminal.print_string [ ANSITerminal.magenta ]
          (pass_message board.squares player)
      in
      board
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n Trouble understanding command. Try again.\n";
      process_buy board square player

let rec process_build board player =
  ANSITerminal.print_string [ ANSITerminal.green ] (build_message board player);
  match read_line () with
  | "EXIT" -> board
  | propstr ->
      let sq = find_square_by_name board.squares propstr in
      if valid_build board player.properties sq then board_build board sq player
      else
        let () =
          ANSITerminal.print_string [ ANSITerminal.white ]
            "\nYou can't build on that property!"
        in
        process_build board player

let rec process_destroy board player =
  ANSITerminal.print_string [ ANSITerminal.green ]
    (destroy_message board player);
  match read_line () with
  | "EXIT" -> board
  | propstr ->
      let sq = find_square_by_name board.squares propstr in
      if valid_build board player.properties sq then
        board_destroy board sq player
      else
        let () =
          ANSITerminal.print_string [ ANSITerminal.white ]
            "\nYou can't destroy on that property!"
        in
        process_destroy board player

let show_board (board : board) =
  ANSITerminal.print_string [ ANSITerminal.green ] (cornellopoly_board ());
  match read_line () with
  | "EXIT" -> board
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.on_red ]
        "\n Trouble understanding command. Try again.\n";
      board

let rec play board =
  let debug = true in
  let die1 = rand_int 5 in
  let die2 = rand_int 5 in
  let num_move = if debug then 1 else die1 + die2 + 2 in
  let curr_pl = find_player_by_turn board.players board.turn in
  ANSITerminal.print_string [ ANSITerminal.green ] (std_message curr_pl);
  match read_line () with
  | exception End_of_file ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "Thanks for playing!"
  | msg -> begin
      match msg with
      | "ROLL" ->
          let dub =
            if die1 = die2 then
              let () =
                ANSITerminal.print_string
                  [ ANSITerminal.on_white; ANSITerminal.black ]
                  "\n\
                   Congrats! You have rolled doubles! You will be able to roll \
                   again!\n"
              in
              0
            else 1
          in
          let af_roll =
            if curr_pl.jail then
              board_move_jailed board.squares board curr_pl board.turn num_move
            else board_move_free board curr_pl board.turn num_move
          in
          let af_pl = find_player_by_turn af_roll.players af_roll.turn in
          let af_sq = find_square_by_player af_roll.squares af_pl in
          let owner = find_player_by_name af_roll.players af_sq.owner in
          if buyable af_roll.squares af_pl then
            let af_buy = process_buy af_roll af_sq af_pl in
            play { af_buy with turn = af_buy.turn + dub }
          else if must_pay af_roll.squares af_pl then
            let pay_board = board_pay af_roll owner af_pl in
            play { pay_board with turn = pay_board.turn + dub }
          else if is_chance af_roll.squares af_pl then
            let chance_board = board_chance af_roll af_pl in
            play { chance_board with turn = chance_board.turn + dub }
          else if must_tax af_roll.squares af_pl then
            let taxed_player_board = tax_board board af_pl in
            play
              { taxed_player_board with turn = taxed_player_board.turn + dub }
          else play { af_roll with turn = af_roll.turn + dub }
      | "BUILD" -> play (process_build board curr_pl)
      | "DESTROY" -> play (process_destroy board curr_pl)
      | "BAIL" -> begin
          match curr_pl.jail with
          | true -> play (board_bail board curr_pl)
          | false ->
              ANSITerminal.print_string [ ANSITerminal.on_red ]
                "\n Trouble understanding command. Try again.\n";
              play board
        end
      | "TRADE" ->
          let af_trade = process_trade board curr_pl in
          play { af_trade with turn = af_trade.turn }
      | "VIEW BOARD" ->
          let af_view = show_board board in
          play { af_view with turn = af_view.turn }
      | "QUIT" ->
          ANSITerminal.print_string [ ANSITerminal.magenta ]
            "Thanks for playing!"
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.on_red ]
            "\n Trouble understanding command. Try again.\n";
          play board
    end

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    (welcome_logo () ^ "\n\nWelcome to Cornopoly.\n");
  play gameboard

let () = main ()
