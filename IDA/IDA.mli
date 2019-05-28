(** Main module for interacting with IDA. *)

(** {6 Functions} *)

(** Returns the address at which the user's cursor is currently located. *)
val get_screen_ea     : unit -> int32

(** Sets a comment at an address; the boolean specifies whether the comment 
    repeats. *)
val set_cmt           : int32 -> string -> bool -> unit

(** Sets an anterior or posterior comment at an address; the boolean specifies 
    whether the comment repeats. *)
val set_xterior_cmt   : int32 -> string -> bool -> unit

(** Sets an anterior comment at an address; the boolean specifies whether the 
    comment repeats. *)
val set_anterior_cmt  : int32 -> string -> unit

(** Sets a posterior comment at an address; the boolean specifies whether the 
    comment repeats. *)
val set_posterior_cmt : int32 -> string -> unit

(** Sets a comment at the beginning of the function, whose repeatability is 
    determined by the third argument *)
val set_func_cmt      : int32 -> string -> bool -> unit

(** Prints a formatted message into IDA's status window. *)
val msg               : ('a, unit, string, unit) format4 -> 'a

(** Displays a pop-up warning message box from a format string. *)
val warning           : ('a, unit, string, unit) format4 -> 'a

(** Returns the beginning of the function given by the address, if the function
    is properly declared as such inside of IDA. *)
val find_func_begin   : int32 -> int32 option

(** Return a byte from a location.  Have an option here? *)
val get_byte          : int32 -> int32
val get_word          : int32 -> int32
val get_dword         : int32 -> int32

(** Put a byte at a location. *)
val put_byte         : int32 -> int32 -> bool


(** Returns a list of a number of bytes from an address. *)
val get_many_bytes    : int32 -> int32 -> int32 list

(** Jumps to the given address in the current disassembly pane. *)
val jumpto            : int32 -> bool

(** Retrieves a byte from memory during debugging.  Should be part of another 
    module. *)
val get_mem_byte      : int32 -> int32

(** Adds a hotkey to the internal hot key manager.  Should be part of another 
    module. *)
val add_ocaml_hotkey  : string -> int -> bool

(** Removes a hotkey from the internal hot key manager.  Should be part of 
    another module. *)
val del_ocaml_hotkey  : string -> bool

(** Issues a single-step command.  Should be part of another module. *)
val step_into         : unit -> bool