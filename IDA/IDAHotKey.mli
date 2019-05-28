(** Module for mapping and unmapping hotkeys dynamically. *)

(** {6 Functions} *)

(** Register a hotkey that, when pressed, invokes the function provided by the
    second argument.  Returns bool on success of installation, which fails if
    the hotkey is already bound (say, due to idagui.cfg). *)
val register   : string -> (unit -> unit) -> bool

(** Unregister a previously-registered hotkey.  Has no effect on hotkeys 
    registered via plugins or in IDA's user interface configuration files. *)
val unregister : string -> unit
