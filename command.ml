type object_phrase = string list

type command = 
  | Pass
  | Play of object_phrase
  | Draw
  | Use of string*int
  | Quit
  | Description of object_phrase
  | Give of object_phrase
  | Pair of string*int
  | Three of string*int
  | Five 

exception Empty
exception Malformed

let rec cut_space (str_lst : string list) (new_lst : string list) 
  : string list = 
  match str_lst with
  | [] -> List.rev new_lst
  | h::t -> 
    if h = "" then cut_space t new_lst
    else cut_space t (h::new_lst)

(** [parse_with obj_lst] looks for the string "with" in [obj_lst] and returns 
    the object phrase after "with". *)
let rec parse_with (obj_lst : object_phrase) : object_phrase =
  match obj_lst with
  | [] -> raise Malformed
  | h::t -> 
    if h = "with" then t
    else parse_with t

(** [parse_use obj_lst] parses the user's command of "use" into the object 
    phrase before the "with" and the object phrase after the "with" and uses 
    them as inputs to the Use constructor. *)
let parse_use (obj_lst : object_phrase) : command =
  let item_lst = List.rev (parse_with (List.rev obj_lst)) in
  let player_lst = parse_with obj_lst in
  let item = String.concat " " item_lst in 
  let player = String.concat " " player_lst in 
  match int_of_string player with
  | exception Failure _ -> raise Malformed
  | i -> Use (item,i)

(** [parse_pair obj_lst] parses the user's command of "pair" into the object 
    phrase before the "with" and the object phrase after the "with" and
    uses them as inputs to the Pair constructor. *)
let parse_pair (obj_lst : object_phrase) : command =
  let item_lst = List.rev (parse_with (List.rev obj_lst)) in
  let player_lst = parse_with obj_lst in
  let item = String.concat " " item_lst in 
  let player = String.concat " " player_lst in 
  match int_of_string player with
  | exception Failure _ -> raise Malformed
  | i -> Pair (item,i)

(** [parse_three obj_lst] parses the user's command of "three" into the object 
    phrase before the "with" and the object phrase after the "with" and
    uses them as inputs to the Three constructor. *)
let parse_three (obj_lst : object_phrase) : command =
  let item_lst = List.rev (parse_with (List.rev obj_lst)) in
  let player_lst = parse_with obj_lst in
  let item = String.concat " " item_lst in 
  let player = String.concat " " player_lst in 
  match int_of_string player with
  | exception Failure _ -> raise Malformed
  | i -> Three (item,i)

let parse (str : string) (hand : Cats.hand) : command =
  if String.trim str = "" then raise Empty
  else 
    let lst = String.split_on_char ' ' str in 
    let clean_lst = cut_space lst [] in 
    let head = List.hd clean_lst in
    if List.length clean_lst = 1 then 
      if head = "quit" then Quit
      else if head = "pass" then Pass
      else if head = "draw" then Draw
      else if head = "five" then Five
      else raise Malformed
    else if head = "give" then 
      let tl = List.tl clean_lst in 
      let card = String.concat " " tl in 
      if not(Cats.contains hand card) then raise Malformed
      else Give tl
    else if head = "play" then 
      let tl = List.tl clean_lst in 
      let card = String.concat " " tl in 
      if not(Cats.contains hand card) then raise Malformed
      else
        (if List.hd tl = "Favor" then raise Malformed 
         else if List.hd tl = "Skip" && List.length tl = 1 then Pass
         else Play tl)
    else if head = "use" then 
      let use = parse_use (List.tl clean_lst) in
      match use with 
      | Use ("Favor", _) -> use
      | _ -> raise Malformed
    else if head = "pair" then
      let pair = parse_pair (List.tl clean_lst) in 
      match pair with
      | Pair (str,_) -> if List.length
                          (List.find_all (fun x -> x.Cats.name = str) hand) >= 2
                          then pair else raise Malformed 
      | _ -> raise Malformed
    else if head = "three" then 
      let three = parse_three (List.tl clean_lst) in 
      match three with
      | Three (str,_) -> if List.length
                          (List.find_all (fun x -> x.Cats.name = str) hand) >= 3
                          then three else raise Malformed 
      | _ -> raise Malformed
    else if head = "description" then 
      let tl = List.tl clean_lst in 
      Description tl
    else raise Malformed
