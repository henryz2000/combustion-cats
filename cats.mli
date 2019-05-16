type name = string
type description = string

type card = {
  name : name;
  description : description;
}

type deck = card list
type hand = card list
type hands = hand list

(** [shuffle_f lst] is a random permutation of [lst]. *)
val shuffle_f : 'a list -> 'a list

(** [setup num_players] initializes the players' hands and 
    the starting deck based on the number of players. *)
val setup : int -> (hands * deck)

(** [cards_names cards] returns a list of the names of [cards]. *)
val cards_names : card list -> name list

(** [draw hands deck i] returns a new list of players' hands and updates the 
    deck after a card is drawn.*)
val draw : hands -> deck -> int -> ((hands * deck) * bool)

(** [play hands deck i] returns a new list of players' hands and updates the 
    deck after a card is played. *)
val play : hands -> deck -> name -> int -> (hands * deck) 

(** [description card_name] returns the description of [card_name]. *)
val description : name -> description

(** [see_the_future_f deck] returns the first 3 cards from the top of [deck]. *)
val see_the_future_f : deck -> unit

(** [contains hand card_name] checks to see if [hand] contains [card_name].*)
val contains : hand -> name -> bool

(** [insert_back deck card i acc] places
    [card] back in a certain spot in [deck]. *)
val insert_back : deck -> card -> int -> deck -> deck

(** [favor_f i j card_name hands] gives player
    [j] [card_name] from player [i]. *)
val favor_f : int -> int -> name -> hands -> hands

(** [find_name hands player index] finds
    the name of a card in [player]'s hand. *)
val find_name : hands -> int -> int -> name

(** [draw_discard hands deck card_name] adds the card with [card_name] to
    a player's hand from the discard pile. *)
val draw_discard : hands -> deck -> name -> int -> hands * deck 
