open Yojson.Basic.Util

let exploding_kittens_json = Yojson.Basic.from_file "exploding_kittens.json"
let defuse_json = Yojson.Basic.from_file "defuse.json"
let attack_json  = Yojson.Basic.from_file "attack.json"
let skip_json  = Yojson.Basic.from_file "skip.json"
let favor_json  = Yojson.Basic.from_file "favor.json"
let shuffle_json  = Yojson.Basic.from_file "shuffle.json"
let see_the_future_json  = Yojson.Basic.from_file "see_the_future.json"
let nope_json  = Yojson.Basic.from_file "nope.json"
let taco_cat_json  = Yojson.Basic.from_file "taco_cat.json"
let watermelon_cat_json  = Yojson.Basic.from_file "watermelon_cat.json"
let potato_cat_json  = Yojson.Basic.from_file "potato_cat.json"
let beard_cat_json  = Yojson.Basic.from_file "beard_cat.json"
let rainbow_cat_json  = Yojson.Basic.from_file "rainbow_cat.json"

type name = string
type description = string

type card = {
  name : name;
  description : description;
}

type deck = card list
type hand = card list
type hands = hand list

let shuffle_f (lst : 'a list) : 'a list =
  QCheck.Gen.(generate1 (shuffle_l lst))

(** [card_of_json json] returns the name and description of a card [json] *)
let card_of_json json : card = {
  name = json |> member "name" |> to_string;
  description = json |> member "description" |> to_string;
}

type exploding_kittens = {
  lst : card list;
}

(** [exploding_kittens_list json] returns a list of exploding kitten cards. *)
let exploding_kittens_list json = {
  lst = json |> member "exploding kittens" |> to_list |> List.map card_of_json;
}

type defuse = {
  lst : card list;
}

(** [defuse_list json] returns a list of defuse cards. *)
let defuse_list json = {
  lst = json |> member "defuse" |> to_list |> List.map card_of_json;
}

type attack = {
  lst : card list;
}

(** [attack_list json] returns a list of attack cards. *)
let attack_list json = {
  lst = json |> member "attack" |> to_list |> List.map card_of_json;
}

type skip = {
  lst : card list;
}

(** [skip_list json] returns a list of skip cards. *)
let skip_list json = {
  lst = json |> member "skip" |> to_list |> List.map card_of_json;
}

type favor = {
  lst : card list;
}

(** [favor_list json] returns a list of favor cards. *)
let favor_list json = {
  lst = json |> member "favor" |> to_list |> List.map card_of_json;
}

type shuffle = {
  lst : card list;
}

(** [shuffle_list json] returns a list of shuffle cards. *)
let shuffle_list json = {
  lst = json |> member "shuffle" |> to_list |> List.map card_of_json;
}

type see_the_future = {
  lst : card list;
}

(** [see_the_future_list json] returns a list of see the future cards. *)
let see_the_future_list json = {
  lst = json |> member "see the future" |> to_list |> List.map card_of_json;
}

type nope = {
  lst : card list;
}

(** [nope_list json] returns a list of nope cards. *)
let nope_list json = {
  lst = json |> member "nope" |> to_list |> List.map card_of_json;
}

type taco_cat = {
  lst : card list;
}

(** [taco_cat_list json] returns a list of taco cat cards. *)
let taco_cat_list json = {
  lst = json |> member "taco cat" |> to_list |> List.map card_of_json;
}

type watermelon_cat = {
  lst : card list;
}

(** [watermelon_cat_list json] returns a list of watermelon cat cards. *)
let watermelon_cat_list json = {
  lst = json |> member "watermelon cat" |> to_list |> List.map card_of_json;
}

type potato_cat = {
  lst : card list;
}

(** [potato_cat_list json] returns a list of potato cat cards. *)
let potato_cat_list json = {
  lst = json |> member "potato cat" |> to_list |> List.map card_of_json;
}

type beard_cat = {
  lst : card list;
}

(** [beard_cat_list json] returns a list of beard cat cards. *)
let beard_cat_list json = {
  lst = json |> member "beard cat" |> to_list |> List.map card_of_json;
}

type rainbow_cat = {
  lst : card list;
}

(** [rainbow_cat_list json] returns a list of rainbow cat cards. *)
let rainbow_cat_list json = {
  lst = json |> member "rainbow cat" |> to_list |> List.map card_of_json;
}

(** [starting_deck] is a deck of all possible cards in random order.*)
let starting_deck : deck = 
  ((attack_list attack_json).lst @ 
   (skip_list skip_json).lst @
   (favor_list favor_json).lst @
   (shuffle_list shuffle_json).lst @
   (see_the_future_list see_the_future_json).lst @
   (nope_list nope_json).lst @
   (taco_cat_list taco_cat_json).lst @
   (watermelon_cat_list watermelon_cat_json).lst @
   (potato_cat_list potato_cat_json).lst @
   (beard_cat_list beard_cat_json).lst @
   (rainbow_cat_list rainbow_cat_json).lst @ []) |> shuffle_f

(** [leftover_defuse num_players] returns the number of left over defuse cards 
    based on the number of players. *)
let leftover_defuse (num_players : int) : card list =
  let defuse_card = List.hd (defuse_list defuse_json).lst in
  match num_players with
  | 5 -> defuse_card :: []
  | 4 -> defuse_card :: defuse_card :: []
  | 3 -> defuse_card :: defuse_card :: defuse_card :: []
  | 2 -> defuse_card :: defuse_card :: defuse_card :: defuse_card :: []
  | _ -> failwith "impossible number of players"

(** [starting_hands_empty num_players acc] adds n-1 defuse cards to the list of
    defuse cards for n players.*)
let rec starting_hands_empty (num_players : int) (acc : hands) : hands =
  let defuse_card = List.hd (defuse_list defuse_json).lst in
  match num_players with
  | 0 -> acc
  | x -> starting_hands_empty (x-1) ([defuse_card] :: acc)

(** [populate_starting_hands deck hands acc] adds 8 cards 
    to each player's hand. *)
let rec populate_starting_hands (deck : deck) (hands : hands)
    (acc : hands) : (hands * deck) = 
  match hands with 
  | [] -> ((List.rev acc), deck)
  | h::t -> 
    if List.length h <> 8 
    then populate_starting_hands (List.tl deck) (((List.hd deck)::h) :: t) acc
    else populate_starting_hands deck t ((shuffle_f h)::acc)

(** [starting_deck_with_kittens deck num_players] adds the exploding kitten 
    cards to the starting deck in a random order. *)
let starting_deck_with_kittens (deck : deck) (num_players : int) : deck = 
  (exploding_kittens_list exploding_kittens_json).lst @
  leftover_defuse num_players @ deck |> shuffle_f

let setup (num_players : int) : (hands * deck) = 
  let she = starting_hands_empty num_players [] in 
  let initial_hands = populate_starting_hands starting_deck she [] in 
  let initial_deck = 
    starting_deck_with_kittens (snd initial_hands) num_players in 
  (fst initial_hands, initial_deck)

let cards_names (cards : card list) : name list =
  List.map (fun card -> card.name) cards

(** [previous_hands hands i acc] is a list of each player's hands before a 
    card is played. *)
let rec previous_hands (hands : hands) (i : int) (acc : hands) : hands =
  match i with
  | -1 -> List.rev acc
  | _ -> previous_hands hands (i-1) ((List.nth hands i) :: acc)

let draw (hands : hands) (deck : deck) (i : int) : ((hands * deck) * bool) =
  let first_card_in_deck = List.hd deck in
  let flag = first_card_in_deck.name = "Exploding Kitten" in 
  let previous = previous_hands hands (i-1) [] in
  let current = first_card_in_deck :: (List.nth hands i) in
  let after = 
    previous_hands (List.rev hands) ((List.length hands) - (i+2)) [] in
  (((List.rev previous) @ (current :: after), List.tl deck),flag)

let play (hands : hands) (deck : deck) (card_name : name) 
    (i : int) : (hands * deck) = 
  let previous = previous_hands hands (i-1) [] in
  let current_without = List.filter (fun x -> x.name <> card_name)
                                                    (List.nth hands i) in 
  let current_with = List.tl (List.filter (fun x -> x.name = card_name)
                                                    (List.nth hands i)) in 
  let current = current_with @ current_without in 
  let after = 
    previous_hands (List.rev hands) ((List.length hands) - (i+2)) [] in
  ((List.rev previous) @ (current :: after), deck)

let description (card_name : name) : description = 
  let all_cards = (exploding_kittens_list exploding_kittens_json).lst @
                  (defuse_list defuse_json).lst @ starting_deck in 
  match List.find (fun x -> x.name = card_name) all_cards with
  | exception Not_found -> "**INVALID CARD**"
  | x -> x.description

(** [peek_draw_pile deck init acc] creates a list of names
    of the first [init] cards of a deck. *)
let rec peek_draw_pile (deck : deck) (init : int) (acc: name list): name list = 
  match deck,init with 
  | [],_ -> acc
  | h::t, 0 -> acc
  | h::t, n -> peek_draw_pile t (n-1) (acc @ [h.name])

let see_the_future_f (deck : deck) : unit = 
  let top_cards = peek_draw_pile deck 3 [] in 
  let cards_str =  String.concat ", " top_cards in 
  print_string "\nThe cards on the top of the draw pile are ";
  ANSITerminal.(print_string [green] cards_str);
  print_string " (from the top to the bottom)\n\n"

let contains (hand : hand) (card_name : name) : bool = 
  let card_name_lst = cards_names hand in 
  List.mem card_name card_name_lst 

let rec insert_back (deck : deck) (card : card) (i : int) (acc : deck) : deck = 
  match deck,i with
  | [],_ -> acc @ [card]
  | h::t, 0 -> acc @ [card] @ deck
  | h::t, n -> if n > 0 then insert_back t card (n-1) (acc @ [h])
                        else acc @ [card] @ deck

let favor_f (i : int) (j : int) (card_name : name) (hands : hands) : hands = 
  let j_hand = List.nth hands j in 
  let card = List.filter (fun x -> x.name = card_name) j_hand in 
  let previous = previous_hands hands (i-1) [] in
  let current = (List.hd card) :: (List.nth hands i) in
  let after = 
    previous_hands (List.rev hands) ((List.length hands) - (i+2)) [] in
  let new_hands_i =  (List.rev previous) @ (current :: after) in 
  let previous_new = previous_hands new_hands_i (j-1) [] in
  let current_without = List.filter (fun x -> x.name <> card_name) j_hand in 
  let current_with = List.tl
                          (List.filter (fun x -> x.name = card_name) j_hand) in 
  let current_new = current_with @ current_without in 
  let after_new = 
    previous_hands (List.rev new_hands_i)
    ((List.length new_hands_i) - (j+2)) [] in
  (List.rev previous_new) @ (current_new :: after_new)

let find_name (hands : hands) (player: int) (index : int) : name = 
  let player_hand = List.nth hands player in 
  let card = List.nth player_hand index in
  card.name

let draw_discard (hands : hands) (deck : deck) (card_name : name) 
    (i : int) : (hands * deck) = 
  let json = 
    match card_name with 
    | "Defuse" -> defuse_json
    | "Attack" -> attack_json 
    | "Skip" ->  skip_json  
    | "Favor" -> favor_json 
    | "Shuffle" -> shuffle_json  
    | "See the Future" -> see_the_future_json  
    | "Nope" -> nope_json  
    | "Taco Cat" -> taco_cat_json 
    | "Watermelon Cat" -> watermelon_cat_json  
    | "Potato Cat" -> potato_cat_json 
    | "Beard Cat" -> beard_cat_json  
    | "Rainbow Cat" -> rainbow_cat_json 
    | _ -> failwith "never" in 
  let previous = previous_hands hands (i-1) [] in
  let card_lst = json |> member (String.lowercase_ascii card_name) 
                 |> to_list |> List.map card_of_json in
  let card = List.hd card_lst in
  let current = card :: (List.nth hands i) in 
  let after = 
    previous_hands (List.rev hands) ((List.length hands) - (i+2)) [] in
  ((List.rev previous) @ (current :: after), deck)