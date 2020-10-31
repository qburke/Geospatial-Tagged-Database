(**
   Parsing of interface commands
*)

(** The type [object_phrase] represents an argument to a command *)
type object_phrase = string list

(** The type [serach_parameter] represents a list of tags to be searched
      upon. *)
type search_parameter = string list

(** The type [command]  represents an interface command that is 
    decomposed into a verb and possibly an object phrase *)
type command =
  | Help
  | Initialize
  | Load of object_phrase
  | Query of object_phrase
  | Add
  | Delete
  | Write of object_phrase
  | Quit

(** Raised when an empty command is parsed *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses an interface input into a [command] as follows.
    The first word becomes the verb. The rest of the  words, if any, 
    become the object phrase 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. 
*)
  
val parse : string -> command
