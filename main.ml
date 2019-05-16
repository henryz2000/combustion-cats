(** [done_next_p] reads the user's command "done" and raises an exception if
    anything besides "done" is used. *)
let rec done_next_p () = 
  match read_line () with 
  | exception End_of_file -> ()
  | str when str = "done" -> () 
  | _ -> 
    ANSITerminal.(print_string [red] "Please enter ");
    ANSITerminal.(print_string [green] "\"done\"");
    ANSITerminal.(print_string [red] " to start your turn.\n");
    print_string "> ";
    done_next_p ()

(** [dead_switch is_dead i] checks whether a player is dead by drawing an
    exploding kitten. *)
let dead_switch (is_dead : bool) (i : int) = 
  if is_dead then 
    ( print_string "RIP Player ";
      print_int i;
      print_string "\n";
      ANSITerminal.(print_string [red] "5..4..3..2..1...BOOM!! 
You drew an Exploding Kitten :(\n");
      print_string "\n")

(** [next_player next_p states] updates the next player while taking into 
    account if the next player is dead. *)
let rec next_player (next_p : int) (states : State.player_states) : int =
  let num_players = List.length states in 
  match List.nth states next_p with 
  | Dead -> next_player ((next_p+1) mod num_players) states 
  | Alive -> next_p
  | _ -> failwith "never"

(** [switch_player next_p now_p num_player is_dead] prompts the next player
    to have a turn. *)
let switch_player (next_p : int) (now_p : int) (num_player : int) 
    (is_dead : bool) = 
  ANSITerminal.erase Screen;  
  dead_switch is_dead now_p;
  print_string "Now it's player ";
  print_int next_p;
  print_string "'s turn, please pass the computer to player ";
  print_int next_p;
  print_string "\n";
  print_string "Type ";
  ANSITerminal.(print_string [green] "\"done\"");
  print_endline " when you're ready to start your turn.";
  ANSITerminal.(print_string [yellow] "Please don't scroll up\n\n");
  print_string "> ";
  done_next_p ();
  ANSITerminal.erase Screen

(** [get_i] reads the user's input of an
    integer and checks to see if it's valid. *)
let rec get_i () =
  match read_int () with
  | exception End_of_file ->
    ANSITerminal.(print_string [red] "\n What you entered is invalid.\n");
    get_i ()
  | exception Failure _ -> 
    ANSITerminal.(print_string [red] "\n What you entered is invalid.\n");
    get_i ()
  | n  -> n

(** [favored i j setup] applies a player's command to
    play Nope or give a card to another player. *)
let rec favored (i : int) (j : int) (setup : Cats.hands*Cats.deck) 
    (states : State.player_states) (discard: Cats.name list) :
    ((Cats.hands*State.player_states)*Cats.name list) =
  ANSITerminal.(print_string [red]
  ("\nYou are being asked for a favor for Player " 
                                ^ (string_of_int i) ^ "\nYou can only enter "));
  ANSITerminal.(print_string [green] "\"give [cardname]\"");
  ANSITerminal.(print_string [red] " or ");
  ANSITerminal.(print_string [green] "\"play Nope\"\n");
  print_string "> ";
  let hands = fst setup in 
  let j_hand = List.nth hands j in 
  match Command.parse (read_line ()) j_hand with 
  | exception Command.Malformed -> favored i j setup states discard
  | exception Command.Empty -> favored i j setup states discard
  | Command.Give obj_phrase -> let obj = String.concat " " obj_phrase in
    ((Cats.favor_f i j obj hands, states), discard)
  | Command.Play ["Nope"] ->
    let new_setup = Cats.play hands (snd setup) "Nope" j in
    let new_states = State.change_state i (State.Nope j) states in 
    ((fst new_setup, new_states), "Nope"::discard)
  | _ -> favored i j setup states discard

(** [check i states] checks to see what the previous player did and changes the 
    next player's state accordingly. *)
let check (i : int) (states : State.player_states) =
  match List.nth states i with
  | State.Attack _ -> ANSITerminal.(print_string [red] 
                            "You have been attacked, and you must take 2 turns.
                            Try to Nope it if you can!\n")
  | State.Pre_Skipped n when n <> i -> ANSITerminal.(print_string [red] 
                                "Previous player just skipped their turn.
                                This is your chance to Nope if you have one!\n")
  | State.Nope n when n <> i -> ANSITerminal.(print_string [red] 
                                  "Previous player just noped your move.
                                  Get them back with a Nope if you have one!\n")
  | _ -> print_string "You must take 1 turn.\n"

(** [five_names acc count] prompts the user to enter five card names. *)
let rec five_names (acc :Cats.name list) (count : int)
    (hand : Cats.hand) : Cats.name list= 
  begin
    match read_line (), count with
    | exception End_of_file -> five_names acc count hand 
    | str, 1 ->
      let lst = String.split_on_char ' ' str in 
      let clean_lst = Command.cut_space lst [] in 
      let card_request =  String.concat " " clean_lst in
      card_request :: acc
    | str, n -> 
      let lst = String.split_on_char ' ' str in 
      let clean_lst = Command.cut_space lst [] in 
      let card_request =  String.concat " " clean_lst in
      if card_request = "quit" then []
      else if not (Cats.contains hand card_request) then 
        (ANSITerminal.(print_string [red] ("Your hand does not contain "
                                           ^card_request
                                           ^". Enter a card you have: \n"));
         print_string "> ";
         five_names acc count hand)
      else if List.mem card_request acc then
        (ANSITerminal.(print_string [red]
        ("The card's name has already been entered."
                                           ^". Enter a new card you have: \n"));
         print_string "> ";
         five_names acc count hand)
      else 
        (print_string
              ("You have "^ (string_of_int (count-1))^ " name(s) to enter.\n");
         print_string "> ";
         five_names (card_request::acc) (count-1) hand)
  end

(** [lose_cards i setup lst] removes player [i]'s card from their hand. *)
let rec lose_cards (i : int) (setup : (Cats.hands*Cats.deck))
    (lst : Cats.name list) : (Cats.hands*Cats.deck)= 
  match lst with 
  | [] -> setup
  | h::t -> let new_setup = Cats.play (fst setup) (snd setup) h i in 
    lose_cards i new_setup t 

(** [set_like lst acc] removes duplicate elements from a list. *)
let rec set_like (lst : 'a list) (acc : 'a list) : 'a list = 
  match lst with 
  | [] -> List.rev acc
  | h::t -> if List.mem h acc then set_like t acc else set_like t (h::acc)

(** [draw_discard set_discard] takes a card out of the discard deck. *)
let rec draw_discard (set_discard : Cats.name list) : Cats.name = 
  match read_line () with 
  | exception End_of_file -> draw_discard set_discard
  | str -> 
    let lst = String.split_on_char ' ' str in 
    let clean_lst = Command.cut_space lst [] in 
    let card_request =  String.concat " " clean_lst in
    if not (List.mem card_request set_discard) then 
      (ANSITerminal.(print_string [red] ("\nDiscard pile does not contain "
                                         ^card_request
                                         ^". Enter again: \n"));
       print_string "> ";
       draw_discard set_discard)
    else card_request

(** [play_game n] starts the game of combustion cat. *)
let rec play_game (start_p : int) (end_p : int) (setup : (Cats.hands*Cats.deck)) 
    (i : int) (states : State.player_states) (discard : Cats.name list) = 
  let num_players = end_p+1 in 
  let next_p = (i+1) mod num_players in 
  let is_attack = 
    match List.nth states i with 
    | State.Attack _ -> true 
    | _ -> false in 
  let next_alive_p = if is_attack then i else next_player next_p states in 
  (** print_int next_alive_p;
      print_endline; *)
  if i = next_alive_p && not is_attack then 
    (print_string "Player ";
     print_int i;
     print_endline ", you win! Conguatulations!";
     exit 0;)
  else
    (print_string "Player ";
     print_int i;
     print_endline "'s hand: ";
     let hand = List.nth (fst setup) i in 
     List.iter print_endline (Cats.cards_names hand);
     print_string "\n";
     print_string "Possible commands:
  | ";
     ANSITerminal.(print_string [magenta] "play [card name]");
     print_string ": play any cards besides favor. (e.g. ";
     ANSITerminal.(print_string [green] "\"play Skip\"");
     print_string ")";
     print_string "
  | ";
     ANSITerminal.(print_string [magenta] "five");
     print_string ": a prompt will appear to enter five different cards'
    names to draw a card from the discard pile.
  | ";
     ANSITerminal.(print_string [magenta] "draw");
     print_string ": draw a card from the top of the deck and ends your turn
  | ";
     ANSITerminal.(print_string [magenta]
     "use [card name] with [player number]");
     print_string ": can only be used with favor. 
    (e.g. ";
     ANSITerminal.(print_string [green] "\"use Favor with 2\"");
     print_string ")";
     print_string "
  | ";
     ANSITerminal.(print_string [magenta]
     "three [card name] with [player number]");
     print_string ": use three cards with the
    same title to name a card that you want from any other player. If the
    player doesn't have the card you want, you just wasted all three cards.
    (e.g. ";
     ANSITerminal.(print_string [green] "\"three Potato Cat with 1\"");
     print_string ")";
     print_string "
  | ";
     ANSITerminal.(print_string [magenta]
     "pair [card name] with [player number]");
     print_string ": pair two cards with the
    same name to pick a card from any other player.
    (e.g. ";
     ANSITerminal.(print_string [green] "\"pair Beard Cat with 1\"");
     print_string ")";
     print_string "
  | ";
     ANSITerminal.(print_string [magenta] "quit");
     print_string ": ends the game.
  | ";
     ANSITerminal.(print_string [magenta] "description [card name]");
     print_string ": shows the description of [card name]. 
    [card name] is case-sensitive.
    (e.g. ";
     ANSITerminal.(print_string [green] "\"description Defuse\"");
     print_endline ")\n";
     print_endline "If you misplay a card, you will lose it.\n";
     check i states;  
     print_string "> ";
     match Command.parse (read_line ()) hand with
     | exception Command.Malformed ->
       ANSITerminal.(print_string [red] 
                       "Error: the command cannot be understood. 
                        Please enter another command:\n");
       play_game start_p end_p setup i states discard
     | exception Command.Empty -> 
       ANSITerminal.(print_string [red] 
                       "Error: the command cannot be understood. 
                        Please enter another command:\n");
       play_game start_p end_p setup i states discard
     | Command.Quit -> 
       print_endline "Bye-bye!"; exit 0
     | Command.Description obj_phrase -> 
       let obj = String.concat " " obj_phrase in
       print_string ( "\n\n" ^ (Cats.description obj) ^ "\n\n");
       play_game start_p end_p setup i states discard
     | Command.Pass -> 
       let new_states = State.change_state i State.Alive states in 
       let final_states = State.change_state next_alive_p
       (State.Pre_Skipped i) new_states in 
       switch_player next_alive_p i num_players false;
       let aft_skip = Cats.play (fst setup) (snd setup) "Skip" i in 
       let new_setup = aft_skip in
       play_game start_p end_p new_setup next_alive_p final_states
       ("Skip" :: discard)
     | Command.Draw ->
       let new_states = State.change_state i State.Alive states in 
       let aft_draw = Cats.draw (fst setup) (snd setup) i in 
       let new_setup = fst aft_draw in 
       let is_dead = snd aft_draw in 
       let final_states = State.boom_i i is_dead new_states in 
       if is_dead && (Cats.contains hand "Defuse") then
         (ANSITerminal.(print_string [red] "You drew an Exploding Kitten :(\n");
          ANSITerminal.(print_string [red]
              "But you can revive back to life by playing your Defuse card.\n");
          ANSITerminal.(print_string [red] "CAUTION: you must enter ");
          ANSITerminal.(print_string [green] "\"play Defuse\"");
          ANSITerminal.(print_string [red] " to stay alive.\n"); 
          ANSITerminal.(print_string [red]
            "All other commands, even invalid ones, will lead to your death\n");
          print_string "> ";
          match Command.parse (read_line ()) hand with
          | exception Command.Malformed -> 
            switch_player next_alive_p i num_players is_dead;
            play_game start_p end_p new_setup next_alive_p final_states discard
          | exception Command.Empty ->
            switch_player next_alive_p i num_players is_dead;
            play_game start_p end_p new_setup next_alive_p final_states discard
          | Command.Play ["Defuse"] -> 
            let aft_defuse = Cats.play (fst setup) (snd setup) "Defuse" i in
            print_string
            "Enter where you want to insert Exploding Kitten back.\n";
            print_endline "If you insert it out of bounds,
            the card will be placed at the nearest index.\n";
            print_string "> ";
            let j = get_i () in 
            let new_deck = Cats.insert_back
            (List.tl (snd setup))(List.hd (snd setup)) j [] in 
            switch_player next_alive_p i num_players false;
            play_game start_p end_p (fst aft_defuse, new_deck)
            next_alive_p new_states ("Defuse" :: discard)
          | _  -> 
            switch_player next_alive_p i num_players is_dead;
            play_game start_p end_p new_setup next_alive_p final_states discard)
       else 
         (switch_player next_alive_p i num_players is_dead;
          play_game start_p end_p new_setup next_alive_p final_states discard)
     | Command.Play obj_phrase ->
       let obj = String.concat " " obj_phrase in
       if obj = "Attack" then 
         let aft_attack_p = if is_attack
         then next_player next_p states else next_alive_p in 
         let new_setup = Cats.play (fst setup) (snd setup) "Attack" i in
         let new_states = State.change_state aft_attack_p
         (State.Attack i) states in 
         switch_player aft_attack_p i num_players false;
         play_game start_p end_p new_setup aft_attack_p
         new_states ("Attack" :: discard)
       else if obj = "Defuse" then 
         let aft_defuse = Cats.play (fst setup) (snd setup) "Defuse" i in 
         play_game start_p end_p aft_defuse i states ("Defuse" :: discard)
       else if obj = "Nope" then 
         let new_states = State.change_state i State.Alive states in 
         let new_setup = Cats.play (fst setup) (snd setup) "Nope" i in 
         match List.nth states i with
         | State.Nope n | State.Pre_Skipped n | State.Attack n -> 
           let final_states = State.change_state n (State.Nope i) new_states in 
           switch_player n i num_players false;
           play_game start_p end_p new_setup n final_states ("Nope" :: discard)
         | _ -> play_game start_p end_p new_setup i states ("Nope" :: discard)
       else if obj = "See the Future" then 
         (Cats.see_the_future_f (snd setup);
          let new_setup = Cats.play
          (fst setup) (snd setup) "See the Future" i in 
          play_game start_p end_p new_setup i states
          ("See the Future" :: discard))
       else if obj = "Shuffle" then 
         let new_deck = Cats.shuffle_f (snd setup) in
         let new_hands = Cats.play (fst setup) (snd setup) "Shuffle" i in 
         let new_setup = (fst new_hands,new_deck) in  
         print_endline "You just shuffled the deck.";
         play_game start_p end_p new_setup i states ("Shuffle" :: discard)
       else play_game start_p end_p setup i states (obj :: discard)
     | Command.Use (_,player) -> 
       if player > end_p || player < start_p then
         (ANSITerminal.(print_string [red] 
                          "Error: the player index is out of bounds. 
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if player = i then
         (ANSITerminal.(print_string [red] 
                          "Error: you chose yourself.
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if List.length (List.nth (fst setup) player) = 0 then
         (ANSITerminal.(print_string [red] 
                          "Error: the player you chose has no cards. 
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if List.nth states player = State.Dead then
         (ANSITerminal.(print_string [red] 
                          "Error: that player you chose is currently dead.
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else 
         switch_player player i num_players false;
       print_string "Player ";
       print_int player;
       print_endline "'s hand: ";
       let player_hand = List.nth (fst setup) player in 
       List.iter print_endline (Cats.cards_names player_hand);
       print_string "\n";
       let new_hands_states_and_disc = favored i player setup states discard in 
       let new_hands_and_states = fst new_hands_states_and_disc in 
       let new_discard = snd new_hands_states_and_disc in
       let new_setup = Cats.play
       (fst new_hands_and_states) (snd setup) "Favor" i in
       switch_player i player num_players false;
       play_game start_p end_p new_setup i
       (snd new_hands_and_states) ("Favor"::new_discard) 
     | Command.Give _ ->        
       ANSITerminal.(print_string [red] 
                       "Error: the command cannot be understood. 
                        Please enter another command:\n");
       play_game start_p end_p setup i states discard
     | Command.Pair (card,player) -> 
       if player > end_p || player < start_p then
         (ANSITerminal.(print_string [red] 
                          "Error: the player index is out of bounds. 
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if player = i then
         (ANSITerminal.(print_string [red] 
                          "Error: you chose yourself.
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if List.length (List.nth (fst setup) player) = 0 then
         (ANSITerminal.(print_string [red] 
                          "Error: the player you chose has no cards. 
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if List.nth states player = State.Dead then
         (ANSITerminal.(print_string [red] 
                          "Error: that player you chose is currently dead.
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else 
         let setup_fst = Cats.play (fst setup) (snd setup) card i in 
         let setup_then = Cats.play (fst setup_fst) (snd setup_fst) card i in 
         print_string ("You can take a card from Player "
                      ^(string_of_int player)^"'s hand.\n\n");
         print_string "Enter which index you would like to take.\n";
         print_endline "If the index you chose is out of bounds,
         the card will be drawn at the nearest index.\n";
         print_string "> ";  
         let index = get_i () in 
         let the_card_drawn = Cats.find_name (fst setup) player index in 
         let final_hands = Cats.favor_f
         i player the_card_drawn (fst setup_then) in 
         switch_player player i num_players false;
         ANSITerminal.(print_string [red] 
                         ("Player "^(string_of_int i)^
                          " used a pair combo to take your "^the_card_drawn^
                          " . Enter anything to acknowledge this.\n"));
         print_string "> ";
         begin
           match read_line() with 
           | _ -> 
             switch_player i player num_players false;
             play_game start_p end_p (final_hands,snd setup_then)
             i states (card :: discard)
         end
     |Command.Three (card, player) -> 
       if player > end_p || player < start_p then
         (ANSITerminal.(print_string [red] 
                          "Error: the player index is out of bounds. 
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if player = i then
         (ANSITerminal.(print_string [red] 
                          "Error: you chose yourself.
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if List.length (List.nth (fst setup) player) = 0 then
         (ANSITerminal.(print_string [red] 
                          "Error: the player you chose has no cards. 
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else if List.nth states player = State.Dead then
         (ANSITerminal.(print_string [red] 
                          "Error: that player you chose is currently dead.
                        Please enter another command:\n");                      
          play_game start_p end_p setup i states discard)
       else
         let setup_fst = Cats.play (fst setup) (snd setup) card i in 
         let setup_then = Cats.play (fst setup_fst) (snd setup_fst) card i in 
         let setup_final = Cats.play (fst setup_then) (snd setup_then) card i in 
         print_string ("You can pick a card from Player "
                      ^(string_of_int player)^"'s hand:\n\n");
         print_string "Enter the name of the card you want to take.\n";
         print_endline ("\nIf the player doesn't have the card you requested, "^
         "you will lose your combo cards but will not get anything in return.");
         print_string "> "; 
         begin 
           match read_line () with 
           | exception End_of_file ->
              play_game start_p end_p setup i states (card::discard)
           | str ->
             let lst = String.split_on_char ' ' str in 
             let clean_lst = Command.cut_space lst [] in 
             let card_request =  String.concat " " clean_lst in 
             if not(Cats.contains
             (List.nth (fst setup_final) player) card_request) then
               (ANSITerminal.(print_string [red] 
                                ("The player you chose doesn't "^
                                 "have the card you requested. "^
                                 "You lost your three "^card^" cards.\n\n"));
                ANSITerminal.(print_string [red]
                                "Enter anything to start your next command.\n");                             
                print_string "> ";
                begin
                  match read_line() with 
                  | _ -> 
                  play_game start_p end_p setup_final i states (card :: discard)
                end
               )
             else 
               (let final_hands = Cats.favor_f
               i player card_request (fst setup_final) in 
                ANSITerminal.(print_string [green] 
                            ("That player you chose has the card you requested!"
                              ^" You successfully took "^card_request^"!\n\n"));
                print_string
               "Press anything to let the player know his/her card is drawn.\n";
                print_string "> ";
                begin
                  match read_line () with 
                  | _ ->
                    switch_player player i num_players false;
                    ANSITerminal.(print_string [red] 
                      ("Player "^(string_of_int i)^
                      " used three same cards combo to take your "^card_request^
                      " card. Enter anything to acknowledge this.\n"));
                    print_string "> ";
                    begin
                      match read_line() with 
                      | _ -> 
                        switch_player i player num_players false;
                        play_game start_p end_p (final_hands,snd setup_final)
                        i states (card::discard)
                    end
                end

               )
         end
     | Command.Five -> 
       if List.length discard = 0 then 
         (ANSITerminal.(print_string [red]
                            "\nThere's no card in discard pile to draw.\n\n");
          play_game start_p end_p setup i states discard)
       else
         (print_string ("Enter names of five different cards consecutively, "
                        ^"or enter ");
          ANSITerminal.(print_string [green] "\"quit\"");
          print_string " to stop your five command.\n";
          print_string "> ";
          let name_lst = five_names [] 5 (List.nth (fst setup) i) in 
          if name_lst = [] then play_game start_p end_p setup i states discard
          else
            let new_setup = lose_cards i setup name_lst in 
            let set_discard = set_like discard [] in  
            print_string
            "Now you can take any single card from discard pile.\n";
            print_string "Discard pile: \n";
            List.iter print_endline set_discard;
            print_string "\nEnter the card you want.\n\n";
            print_string "> ";
            let card_from_discard = draw_discard set_discard in 
            ANSITerminal.(print_string [green] 
                            ("You successfully drew "^card_from_discard^"!\n"));
            print_string "Press anything to start your next command.\n";
            print_string "> ";
            begin
              match read_line () with 
              | _ -> let aft_discard = List.filter
                (fun x -> x <> card_from_discard) set_discard in
                let final_setup = Cats.draw_discard (fst new_setup)
                (snd new_setup) card_from_discard i in 
                play_game start_p end_p final_setup i states
                (aft_discard @ name_lst)
            end
         )
    )

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to Combustion Cats.\n");
  print_endline "Please enter the number of players and fullscreen the game.\n";
  print_string  "> ";
  begin
    match read_int () with
    | exception End_of_file -> ()
    | exception Failure _ -> 
      ANSITerminal.(print_string [red]
                      "\n What you entered is invalid. 
                            The number of players must be between 2 and 5.\n");
      main()
    | n when (n < 6 && n > 1) -> 
      print_endline "The game starts yayyy!\n";
      ANSITerminal.erase Screen;  
      let setup = Cats.setup n in 
      play_game 0 (n-1) setup 0 (State.init_states n []) []
    | _ ->  ANSITerminal.(print_string [red]
                            "\n What you entered is invalid.
                            The number of players must be between 2 and 5.\n");
      main()
  end

(* Execute the game engine. *)
let () = main ()