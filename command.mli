(**
   Parsing of player commands.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["go clock tower"], then the object phrase is 
      [["clock"; "tower"]].
    - If the player command is ["go clock     tower"], then the object phrase is
      again [["clock"; "tower"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
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

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str hand] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    go   clock   tower   "] is [Go ["clock"; "tower"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val parse : string -> Cats.hand -> command

(** [cut_space str_lst new_lst] creates a [new_lst] from [str_lst] that
    takes out the leading and trailing spaces on each element. *)
val cut_space : string list -> string list -> string list