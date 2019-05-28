class data_consumer :
  in_channel ->
  object
    val mutable file : in_channel
    val mutable streampos : int
    method get_byte : unit -> int32
    method get_byte_list : int -> int32 list
    method get_dword : unit -> int32
    method get_many_bytes_exn : int -> string
    method get_streampos : unit -> int
    method get_word : unit -> int32
    method set_file : in_channel -> unit
    method set_streampos : int -> unit
  end

val consume_mz_header : data_consumer -> PE32.mz_header
val int32_of_machine : PE32.machine -> int32
val machine_of_int32 : int32 -> PE32.machine
val int32_of_characteristic : PE32.characteristics -> int32
val characteristics_of_int32 : int32 -> PE32.characteristics
val optional_magic_of_int32 : int32 -> PE32.optional_magic
val int32_of_optional_magic : PE32.optional_magic -> int32
val int32_of_subsystem : PE32.subsystem -> int32
val subsystem_of_int32 : int32 -> PE32.subsystem
val dll_characteristics_of_int32 : int32 -> PE32.dll_characteristics
val int32_of_dll_characteristics : PE32.dll_characteristics -> int32
val decompose_into_bits : ?low:int -> int -> int32 -> int32 list
val characteristics_of_field : int32 -> PE32.characteristics list
val dll_characteristics_of_field : int32 -> PE32.dll_characteristics list
val consume_file_header : data_consumer -> PE32.file_header
val consume_data_directory : data_consumer -> PE32.data_directory
val consume_optional_header : data_consumer -> PE32.optional_header
val int_of_image_directory : PE32.image_directory -> int
val consume_nt_headers : data_consumer -> PE32.nt_headers
val section_has_characteristic :
  int32 -> PE32.image_section_characteristic -> bool
val int32_of_section_characteristic :
  int32 -> PE32.image_section_characteristic
val decompose_section_characteristics :
  int32 -> PE32.image_section_characteristic list
val consume_section_header : data_consumer -> PE32.section_header
val consume_all_headers : data_consumer -> PE32.all_headers
val make_sections_list :
  PE32.all_headers -> (int32 * int32 * int32 * int option) list
val rva_to_physical :
  (int32 * int32 * int32 * int option) list -> int32 -> PE32.rva2phys
val int32_of_base_relocation : PE32.base_relocation -> int32
val base_relocation_of_int32 : int32 -> PE32.base_relocation
val consume_image_import_descriptor :
  data_consumer -> PE32.image_import_descriptor
val consume_image_import_descriptors :
  data_consumer -> PE32.image_import_descriptor list
val consume_null_terminated_dword_array : data_consumer -> int32 list
val consume_null_terminated_string : data_consumer -> string
val point_to_address : PE32.all_headers -> int32 -> data_consumer -> unit
val point_to_directory :
  PE32.image_directory -> PE32.all_headers -> data_consumer -> unit
type import_entry = Ordinal of int32 | Symbol of string
val consume_import_descriptors :
  PE32.all_headers ->
  data_consumer ->
  PE32.image_import_descriptor list * (string * import_entry list) list
val decode_base_relocation_entry :
  int32 -> int32 -> PE32.base_relocation * int32
val consume_base_relocation_block :
  (PE32.base_relocation * int32) list ->
  data_consumer -> (PE32.base_relocation * int32) list * int32
val consume_base_relocations :
  PE32.all_headers -> data_consumer -> (PE32.base_relocation * int32) list
val make_relocation_amt_list : int32 -> int32 -> int * int * int * int
module Int32Comparator :
  sig type t = int32 val compare : Int32.t -> Int32.t -> int end
module Int32Set :
  sig
    type elt = Int32Comparator.t
    type t = Set.Make(Int32Comparator).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val setify_base_relocations :
  'a ->
  data_consumer -> (PE32.base_relocation * Int32Set.elt) list -> Int32Set.t
val consume_and_setify_base_relocations :
  PE32.all_headers -> data_consumer -> Int32Set.t
val clopen_interval_base_relocations :
  Int32Set.t -> Int32Set.elt -> Int32Set.elt -> Int32Set.elt list
val string_fold_left : string -> ('a -> char -> 'a) -> 'a -> 'a
val extract_executable_sections :
  ?baseaddr:int32 option ->
  string -> int32 * ((int * Int32Set.elt * int * string) list)
