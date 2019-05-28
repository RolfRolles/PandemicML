(** Interacting with IDA's basic GUI *)

(** {6 Types} *)

type 'a chooser =
{ columns: (int * string) array;
  describe: ('a -> string list);
  elements: 'a array;
  del:     ('a chooser -> int -> int32)  option;
  ins:     ('a chooser -> unit) option;
  edit:    ('a chooser -> int -> unit)   option;
  enter:   ('a chooser -> int -> unit)   option;
  destroy: ('a chooser -> unit) option;
}


(** {6 Functions} *)

(** Asks the user to specify a filename.  The bool dictates whether the file is
    for saving; if true, the file doesn't have to exist.  The string becomes the
    title of the dialog.  The return value might be [None] if the user cancelled. *)
val askfile_c : bool -> string -> string option

val choose2 : 'a chooser -> string -> bool -> int(*'*)