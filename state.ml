type state = Dead | Alive | Attack of int | Pre_Skipped of int | Nope of int 

type player_states = state list

let rec init_states (num_players : int) (acc : player_states) : player_states =
  match num_players with
  | 0 -> acc
  | x -> init_states (x-1) (Alive :: acc)

(** [previous_states states i acc] is the state of the game
    before it is updated.*)
let rec previous_states (states : player_states) (i : int) (acc : player_states)
  : player_states =
  match i with
  | -1 -> List.rev acc
  | _ -> previous_states states (i-1) ((List.nth states i) :: acc)

let boom_i (i : int) (flag : bool) (states : player_states) : player_states = 
  let previous = previous_states states (i-1) [] in
  let current = (if flag then Dead else Alive) in
  let after = previous_states (List.rev states)
      ((List.length states) - (i+2)) [] in
  (List.rev previous) @ (current :: after)

let change_state (i : int) (state: state)
                 (states : player_states) : player_states =
  let previous = previous_states states (i-1) [] in
  let current = state in
  let after = previous_states (List.rev states)
      ((List.length states) - (i+2)) [] in
  (List.rev previous) @ (current :: after)