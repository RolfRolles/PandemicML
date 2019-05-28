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

external askfile_c : bool -> string -> string option = "IDAOCaml_askfile_c"
external choose2 : 'a chooser -> string -> bool -> int= "OCamlShowChoose2" (*'*)
