type state = Dead | Alive | Attack of int | Pre_Skipped of int | Nope of int 

type player_states = state list

(** [init_states num_players acc] initializes the state of the game
    and the initial number of players. *)
val init_states : int -> player_states -> player_states

(** [boom_i i flag states] updates the state of the game when a player dies. *)
val boom_i : int -> bool -> player_states -> player_states

(** [change_state i state states] changes player [i]'s state to [state]. *)
val change_state : int -> state -> player_states -> player_states