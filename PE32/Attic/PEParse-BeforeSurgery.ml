(* I think I would like this to operate in two modes.
   #1, online  mode:  consume it as it comes in.
   #2, offline mode:  treat the file as random-access.
   
   This poses something of a challenge.  In particular, if I were simply coding
   it in online mode, I would have no need for a concept such as a cursor into
   the stream.  And if I were simply coding it in offline mode, I would 
   probably code it in such a way where I would pass the index in every time.
   Although even with an offline mode, the operating system usually provides a
   primitive wherein seek support maintains a position, which gets updated 
   during each seek operation.
   
   So how about this for a compromise.  The functionality that provides raw data
   is called "consume_dword_from" and takes a position in addition to any other
   arguments.  Internally the cursor object maintains a position, and if it the
   designated position matches the location from which we are requesting data,
   then we do not perform any seek operation.  Otherwise, we seek.
   
   That sounds good.
     *)
     
exception Unimplemented of string

type mz_header = 
{
  (* +00 "MZ" or "ZM" *) 
  e_magic: int32;

  (* +02 Count of Bytes in Last 512-byte Page (CBLP) *)
  e_cblp: int32;

  (* +04 Count of 512-byte Pages (CP) *)
  e_cp: int32;

  (* +06 Count of ReLoCation entries (CRLC) *)
  e_crlc: int32;

  (* +08 Count of PARagraphs in HeaDeR (CPARHDR) *)
  e_cparhdr: int32;

  (* +0A MINimum paragraphs ALLOCated beyond code size (MINALLOC) *)
  e_minalloc: int32;

  (* +0C MINimum paragraphs ALLOCated beyond code size (MAXALLOC) *)
  e_maxalloc: int32;

  (* +0E Initial SS (SS) *)
  e_ss: int32;

  (* +10 Initial SP (SP) *)
  e_sp: int32;

  (* +12 CheckSUM (CSUM) *)
  e_csum: int32;

  (* +14 Initial IP (IP) *)
  e_ip: int32;

  (* +16 Initial CS (CS) *)
  e_cs: int32;

  (* +18 Location (FAR) of ReLoCation table (LFARLC) *)
  e_lfarlc: int32;

  (* +1A OVerlay Number (OVNO) *)
  e_ovno: int32;

  (* +1C Format-specific REPRESENTED AS AN ARRAY OF WORDS *)
  e_res: int32 array;  
  
  (* +3C Location (Far) of NEW executable header (e_lfanew) *)
  e_lfanew: int32;
}

(* Need to look up OCaml's virtual method syntax *)
class data_consumer f =
  object (self)
    (* Specifically for an on-disk file *)
    val mutable file          = f
    method set_file         f = file <- f

    val mutable streampos     = 0
    method get_streampos   () = streampos
    method set_streampos    p = streampos <- p; seek_in file p
    method get_byte        () = Int32.of_int (input_byte file)
    method get_word        () = let b0 = self#get_byte () in Int32.logor (Int32.shift_left (self#get_byte ())  8) b0
    method get_dword       () = let w0 = self#get_word () in Int32.logor (Int32.shift_left (self#get_word ()) 16) w0
    method get_byte_list  n   = 
      let rec aux i list =
        if i = n
        then List.rev list
        else aux (i+1) ((self#get_byte ())::list)
      in aux 0 []

    (* Returns as a *string* *)
    method get_many_bytes_exn n   =
      let s = String.create n in
      really_input file s 0 n;
      s

  end;;

let get_word_array dc n = 
  let rec aux i list =
    if i = n
    then List.rev list
    else aux (i+1) ((dc#get_word ())::list)
  in Array.of_list (aux 0 [])

let consume_mz_header (dc:data_consumer) =
  (* Shitty, but putting the consumption directly in the structure instance 
     results in the data being consumed backwards. *)
  let e_magic    = dc#get_word ()       in
  let e_cblp     = dc#get_word ()       in
  let e_cp       = dc#get_word ()       in
  let e_crlc     = dc#get_word ()       in
  let e_cparhdr  = dc#get_word ()       in
  let e_minalloc = dc#get_word ()       in
  let e_maxalloc = dc#get_word ()       in
  let e_ss       = dc#get_word ()       in
  let e_sp       = dc#get_word ()       in
  let e_csum     = dc#get_word ()       in
  let e_ip       = dc#get_word ()       in
  let e_cs       = dc#get_word ()       in
  let e_lfarlc   = dc#get_word ()       in
  let e_ovno     = dc#get_word ()       in
  let e_res      = get_word_array dc 16 in
  let e_lfanew   = dc#get_dword ()      in
  {
    e_magic    = e_magic;
    e_cblp     = e_cblp;
    e_cp       = e_cp;
    e_crlc     = e_crlc;
    e_cparhdr  = e_cparhdr;
    e_minalloc = e_minalloc;
    e_maxalloc = e_maxalloc;
    e_ss       = e_ss;
    e_sp       = e_sp;
    e_csum     = e_csum;
    e_ip       = e_ip;
    e_cs       = e_cs;
    e_lfarlc   = e_lfarlc;
    e_ovno     = e_ovno;
    e_res      = e_res;
    e_lfanew   = e_lfanew;
  }

type file_header =
{
  (* +00 *)
  machine: int32;

  (* +02 *)
  number_of_sections: int32;

  (* +04 *)
  time_date_stamp: int32;

  (* +08 *)
  pointer_to_symbol_table: int32;

  (* +0C *)
  number_of_symbols: int32;

  (* +10 *)
  size_of_optional_header: int32;

  (* +12 *)
  characteristics: int32;
}

type data_directory =
{
  (* +00 *)
  virtual_address: int32;
  
  (* +04 *)
  size: int32;
}

type optional_header =
{
  (* +00 *)
  magic: int32;
  
  (* +02 *)
  major_linker_version: int32;
  
  (* +03 *)
  minor_linker_version: int32;
  
  (* +04 *)
  size_of_code: int32;
  
  (* +08 *)
  size_of_initialized_data: int32;

  (* +0C *)
  size_of_uninitialized_data: int32;

  (* +10 *)
  address_of_entrypoint: int32;
  
  (* +14 *)
  base_of_code: int32;

  (* +18 *)
  base_of_data: int32;

  (* +1C *)
  imagebase: int32;

  (* +20 *)
  section_alignment: int32;

  (* +24 *)
  file_alignment: int32;

  (* +28 *)
  major_os_version: int32;

  (* +2A *)
  minor_os_version: int32;

  (* +2C *)
  major_image_version: int32;

  (* +2E *)
  minor_image_version: int32;

  (* +30 *)
  major_subsystem_version: int32;

  (* +32 *)
  minor_subsystem_version: int32;

  (* +34 *)
  win32_version_value: int32;

  (* +38 *)
  size_of_image: int32;

  (* +3C *)
  size_of_headers: int32;

  (* +40 *)
  checksum: int32;

  (* +44 *)
  subsystem: int32;

  (* +46 *)
  dll_characteristics: int32;

  (* +48 *)
  size_of_stack_reserve: int32;

  (* +4C *)
  size_of_stack_commit: int32;

  (* +50 *)
  size_of_heap_reserve: int32;

  (* +54 *)
  size_of_heap_commit: int32;

  (* +58 *)
  loader_flags: int32;

  (* +5C *)
  number_of_rvas_and_sizes: int32;
  
  (* +60 *)
  data_directories: data_directory array;
}

type machine = 
| IMAGE_FILE_MACHINE_UNKNOWN  
| IMAGE_FILE_MACHINE_I386     
| IMAGE_FILE_MACHINE_R3000    
| IMAGE_FILE_MACHINE_R4000    
| IMAGE_FILE_MACHINE_R10000   
| IMAGE_FILE_MACHINE_WCEMIPSV2
| IMAGE_FILE_MACHINE_ALPHA    
| IMAGE_FILE_MACHINE_SH3      
| IMAGE_FILE_MACHINE_SH3DSP   
| IMAGE_FILE_MACHINE_SH3E     
| IMAGE_FILE_MACHINE_SH4      
| IMAGE_FILE_MACHINE_SH5      
| IMAGE_FILE_MACHINE_ARM      
| IMAGE_FILE_MACHINE_THUMB    
| IMAGE_FILE_MACHINE_AM33     
| IMAGE_FILE_MACHINE_POWERPC  
| IMAGE_FILE_MACHINE_POWERPCFP
| IMAGE_FILE_MACHINE_IA64     
| IMAGE_FILE_MACHINE_MIPS16   
| IMAGE_FILE_MACHINE_ALPHA64  
| IMAGE_FILE_MACHINE_MIPSFPU  
| IMAGE_FILE_MACHINE_MIPSFPU16
| IMAGE_FILE_MACHINE_UNDEFINED of int32

let int32_of_machine = function
| IMAGE_FILE_MACHINE_UNKNOWN      -> 0l
| IMAGE_FILE_MACHINE_I386         -> 332l
| IMAGE_FILE_MACHINE_R3000        -> 354l
| IMAGE_FILE_MACHINE_R4000        -> 358l
| IMAGE_FILE_MACHINE_R10000       -> 360l
| IMAGE_FILE_MACHINE_WCEMIPSV2    -> 361l
| IMAGE_FILE_MACHINE_ALPHA        -> 388l
| IMAGE_FILE_MACHINE_SH3          -> 418l
| IMAGE_FILE_MACHINE_SH3DSP       -> 419l
| IMAGE_FILE_MACHINE_SH3E         -> 420l
| IMAGE_FILE_MACHINE_SH4          -> 422l
| IMAGE_FILE_MACHINE_SH5          -> 424l
| IMAGE_FILE_MACHINE_ARM          -> 448l
| IMAGE_FILE_MACHINE_THUMB        -> 450l
| IMAGE_FILE_MACHINE_AM33         -> 467l
| IMAGE_FILE_MACHINE_POWERPC      -> 496l
| IMAGE_FILE_MACHINE_POWERPCFP    -> 497l
| IMAGE_FILE_MACHINE_IA64         -> 512l
| IMAGE_FILE_MACHINE_MIPS16       -> 614l
| IMAGE_FILE_MACHINE_ALPHA64      -> 644l
| IMAGE_FILE_MACHINE_MIPSFPU      -> 870l
| IMAGE_FILE_MACHINE_MIPSFPU16    -> 1126l
| IMAGE_FILE_MACHINE_UNDEFINED(i) -> i
  
let machine_of_int32 = function
| 0l    -> IMAGE_FILE_MACHINE_UNKNOWN  
| 332l  -> IMAGE_FILE_MACHINE_I386     
| 354l  -> IMAGE_FILE_MACHINE_R3000    
| 358l  -> IMAGE_FILE_MACHINE_R4000    
| 360l  -> IMAGE_FILE_MACHINE_R10000   
| 361l  -> IMAGE_FILE_MACHINE_WCEMIPSV2
| 388l  -> IMAGE_FILE_MACHINE_ALPHA    
| 418l  -> IMAGE_FILE_MACHINE_SH3      
| 419l  -> IMAGE_FILE_MACHINE_SH3DSP   
| 420l  -> IMAGE_FILE_MACHINE_SH3E     
| 422l  -> IMAGE_FILE_MACHINE_SH4      
| 424l  -> IMAGE_FILE_MACHINE_SH5      
| 448l  -> IMAGE_FILE_MACHINE_ARM      
| 450l  -> IMAGE_FILE_MACHINE_THUMB    
| 467l  -> IMAGE_FILE_MACHINE_AM33     
| 496l  -> IMAGE_FILE_MACHINE_POWERPC  
| 497l  -> IMAGE_FILE_MACHINE_POWERPCFP
| 512l  -> IMAGE_FILE_MACHINE_IA64     
| 614l  -> IMAGE_FILE_MACHINE_MIPS16   
| 644l  -> IMAGE_FILE_MACHINE_ALPHA64  
| 870l  -> IMAGE_FILE_MACHINE_MIPSFPU  
| 1126l -> IMAGE_FILE_MACHINE_MIPSFPU16
| x     -> IMAGE_FILE_MACHINE_UNDEFINED(x)

type characteristics = 
| IMAGE_FILE_RELOCS_STRIPPED
| IMAGE_FILE_EXECUTABLE_IMAGE
| IMAGE_FILE_LINE_NUMS_STRIPPED
| IMAGE_FILE_LOCAL_SYMS_STRIPPED
| IMAGE_FILE_AGGRESSIVE_WS_TRIM
| IMAGE_FILE_LARGE_ADDRESS_AWARE
| IMAGE_FILE_RESERVED
| IMAGE_FILE_BYTES_REVERSED_LO
| IMAGE_FILE_32BIT_MACHINE
| IMAGE_FILE_DEBUG_STRIPPED
| IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
| IMAGE_FILE_NET_RUN_FROM_SWAP
| IMAGE_FILE_SYSTEM
| IMAGE_FILE_DLL
| IMAGE_FILE_UP_SYSTEM_ONLY
| IMAGE_FILE_BYTES_REVERSED_HI

let int32_of_characteristic = function
| IMAGE_FILE_RELOCS_STRIPPED         -> 0x0001l
| IMAGE_FILE_EXECUTABLE_IMAGE        -> 0x0002l
| IMAGE_FILE_LINE_NUMS_STRIPPED      -> 0x0004l
| IMAGE_FILE_LOCAL_SYMS_STRIPPED     -> 0x0008l
| IMAGE_FILE_AGGRESSIVE_WS_TRIM      -> 0x0010l
| IMAGE_FILE_LARGE_ADDRESS_AWARE     -> 0x0020l
| IMAGE_FILE_RESERVED                -> 0x0040l
| IMAGE_FILE_BYTES_REVERSED_LO       -> 0x0080l
| IMAGE_FILE_32BIT_MACHINE           -> 0x0100l
| IMAGE_FILE_DEBUG_STRIPPED          -> 0x0200l
| IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP -> 0x0400l
| IMAGE_FILE_NET_RUN_FROM_SWAP       -> 0x0800l
| IMAGE_FILE_SYSTEM                  -> 0x1000l
| IMAGE_FILE_DLL                     -> 0x2000l
| IMAGE_FILE_UP_SYSTEM_ONLY          -> 0x4000l
| IMAGE_FILE_BYTES_REVERSED_HI       -> 0x8000l

let characteristics_of_int32 = function
| 0x0001l -> IMAGE_FILE_RELOCS_STRIPPED
| 0x0002l -> IMAGE_FILE_EXECUTABLE_IMAGE
| 0x0004l -> IMAGE_FILE_LINE_NUMS_STRIPPED
| 0x0008l -> IMAGE_FILE_LOCAL_SYMS_STRIPPED
| 0x0010l -> IMAGE_FILE_AGGRESSIVE_WS_TRIM
| 0x0020l -> IMAGE_FILE_LARGE_ADDRESS_AWARE
| 0x0040l -> IMAGE_FILE_RESERVED
| 0x0080l -> IMAGE_FILE_BYTES_REVERSED_LO
| 0x0100l -> IMAGE_FILE_32BIT_MACHINE
| 0x0200l -> IMAGE_FILE_DEBUG_STRIPPED
| 0x0400l -> IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
| 0x0800l -> IMAGE_FILE_NET_RUN_FROM_SWAP
| 0x1000l -> IMAGE_FILE_SYSTEM
| 0x2000l -> IMAGE_FILE_DLL
| 0x4000l -> IMAGE_FILE_UP_SYSTEM_ONLY
| 0x8000l -> IMAGE_FILE_BYTES_REVERSED_HI
| _ -> invalid_arg "characteristic_of_int32"

type optional_magic =
| PE32
| PE32Plus

let optional_magic_of_int32 = function
| 0x10bl -> PE32
| 0x20bl -> PE32Plus
| _ -> invalid_arg "optional_magic_of_int32"

let int32_of_optional_magic = function
| PE32     -> 0x10bl
| PE32Plus -> 0x20bl

type subsystem = 
| IMAGE_SUBSYSTEM_UNKNOWN
| IMAGE_SUBSYSTEM_NATIVE
| IMAGE_SUBSYSTEM_WINDOWS_GUI
| IMAGE_SUBSYSTEM_WINDOWS_CUI
| IMAGE_SUBSYSTEM_POSIX_CUI
| IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
| IMAGE_SUBSYSTEM_EFI_APPLICATION
| IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
| IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
| IMAGE_SUBSYSTEM_EFI_ROM
| IMAGE_SUBSYSTEM_XBOX

let int32_of_subsystem = function
| IMAGE_SUBSYSTEM_UNKNOWN                 -> 0l
| IMAGE_SUBSYSTEM_NATIVE                  -> 1l
| IMAGE_SUBSYSTEM_WINDOWS_GUI             -> 2l
| IMAGE_SUBSYSTEM_WINDOWS_CUI             -> 3l
| IMAGE_SUBSYSTEM_POSIX_CUI               -> 7l
| IMAGE_SUBSYSTEM_WINDOWS_CE_GUI          -> 9l
| IMAGE_SUBSYSTEM_EFI_APPLICATION         -> 10l
| IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER -> 11l
| IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER      -> 12l
| IMAGE_SUBSYSTEM_EFI_ROM                 -> 13l
| IMAGE_SUBSYSTEM_XBOX                    -> 14l

let subsystem_of_int32 = function
| 0l  -> IMAGE_SUBSYSTEM_UNKNOWN
| 1l  -> IMAGE_SUBSYSTEM_NATIVE
| 2l  -> IMAGE_SUBSYSTEM_WINDOWS_GUI
| 3l  -> IMAGE_SUBSYSTEM_WINDOWS_CUI
| 7l  -> IMAGE_SUBSYSTEM_POSIX_CUI
| 9l  -> IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
| 10l -> IMAGE_SUBSYSTEM_EFI_APPLICATION
| 11l -> IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
| 12l -> IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
| 13l -> IMAGE_SUBSYSTEM_EFI_ROM
| 14l -> IMAGE_SUBSYSTEM_XBOX
| _ -> invalid_arg "subsystem_of_int32"

type dll_characteristics = 
| IMAGE_DLL_CHARACTERISTICS_RESERVED1
| IMAGE_DLL_CHARACTERISTICS_RESERVED2
| IMAGE_DLL_CHARACTERISTICS_RESERVED3
| IMAGE_DLL_CHARACTERISTICS_RESERVED4
| IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE
| IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY
| IMAGE_DLL_CHARACTERISTICS_NX_COMPAT
| IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION
| IMAGE_DLL_CHARACTERISTICS_NO_SEH
| IMAGE_DLL_CHARACTERISTICS_NO_BIND
| IMAGE_DLL_CHARACTERISTICS_RESERVED5
| IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER
| IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE

let dll_characteristics_of_int32 = function
| 0x0001l -> IMAGE_DLL_CHARACTERISTICS_RESERVED1
| 0x0002l -> IMAGE_DLL_CHARACTERISTICS_RESERVED2
| 0x0004l -> IMAGE_DLL_CHARACTERISTICS_RESERVED3
| 0x0008l -> IMAGE_DLL_CHARACTERISTICS_RESERVED4
| 0x0040l -> IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE
| 0x0080l -> IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY
| 0x0100l -> IMAGE_DLL_CHARACTERISTICS_NX_COMPAT
| 0x0200l -> IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION
| 0x0400l -> IMAGE_DLL_CHARACTERISTICS_NO_SEH
| 0x0800l -> IMAGE_DLL_CHARACTERISTICS_NO_BIND
| 0x1000l -> IMAGE_DLL_CHARACTERISTICS_RESERVED5
| 0x2000l -> IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER
| 0x8000l -> IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE
| _ -> invalid_arg "dll_characteristics_of_int32"

let int32_of_dll_characteristics = function
| IMAGE_DLL_CHARACTERISTICS_RESERVED1             -> 0x0001l
| IMAGE_DLL_CHARACTERISTICS_RESERVED2             -> 0x0002l
| IMAGE_DLL_CHARACTERISTICS_RESERVED3             -> 0x0004l
| IMAGE_DLL_CHARACTERISTICS_RESERVED4             -> 0x0008l
| IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE          -> 0x0040l
| IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY       -> 0x0080l
| IMAGE_DLL_CHARACTERISTICS_NX_COMPAT             -> 0x0100l
| IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION          -> 0x0200l
| IMAGE_DLL_CHARACTERISTICS_NO_SEH                -> 0x0400l
| IMAGE_DLL_CHARACTERISTICS_NO_BIND               -> 0x0800l
| IMAGE_DLL_CHARACTERISTICS_RESERVED5             -> 0x1000l
| IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER            -> 0x2000l
| IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE -> 0x8000l

(* Useful for dealing with bitfields.  Decompose an element into a list of each
   bit that is set.  Deals with the bits in range [low,low+n). *)
let decompose_into_bits ?(low=0) n i32 = 
  let rec aux list i mask =
    if i = n
    then List.rev list
    else 
      let list =
        let v = Int32.logand mask i32 in
        if v <> 0l
        then (v::list)
        else list
      in
      aux list (i+1) (Int32.shift_left mask 1)
  in
  aux [] low (Int32.shift_left 1l low)

let     characteristics_of_field v = List.map     characteristics_of_int32 (decompose_into_bits 16 v)
let dll_characteristics_of_field v = List.map dll_characteristics_of_int32 (decompose_into_bits 16 v)

let consume_file_header (dc:data_consumer) =
  (* Shitty, but putting the consumption directly in the structure instance 
     results in the data being consumed backwards. *)
  let machine                 = dc#get_word  () in
  let number_of_sections      = dc#get_word  () in
  let time_date_stamp         = dc#get_dword () in
  let pointer_to_symbol_table = dc#get_dword () in
  let number_of_symbols       = dc#get_dword () in
  let size_of_optional_header = dc#get_word  () in
  let characteristics         = dc#get_word  () in
  {
    machine = machine;
    number_of_sections = number_of_sections;
    time_date_stamp = time_date_stamp;
    pointer_to_symbol_table = pointer_to_symbol_table;
    number_of_symbols = number_of_symbols;
    size_of_optional_header = size_of_optional_header;
    characteristics = characteristics;
  }

let consume_data_directory (dc:data_consumer) =
  let virtual_address = dc#get_dword () in
  let size            = dc#get_dword () in
  {
    virtual_address = virtual_address;
    size = size;
  }

let consume_optional_header (dc:data_consumer) =
  let magic                      = dc#get_word  () in 
  let major_linker_version       = dc#get_byte  () in
  let minor_linker_version       = dc#get_byte  () in
  let size_of_code               = dc#get_dword () in 
  let size_of_initialized_data   = dc#get_dword () in 
  let size_of_uninitialized_data = dc#get_dword () in 
  let address_of_entrypoint      = dc#get_dword () in 
  let base_of_code               = dc#get_dword () in 
  let base_of_data               = dc#get_dword () in 
  let imagebase                  = dc#get_dword () in 
  let section_alignment          = dc#get_dword () in 
  let file_alignment             = dc#get_dword () in 
  let major_os_version           = dc#get_word  () in 
  let minor_os_version           = dc#get_word  () in 
  let major_image_version        = dc#get_word  () in 
  let minor_image_version        = dc#get_word  () in 
  let major_subsystem_version    = dc#get_word  () in 
  let minor_subsystem_version    = dc#get_word  () in 
  let win32_version_value        = dc#get_dword () in 
  let size_of_image              = dc#get_dword () in 
  let size_of_headers            = dc#get_dword () in 
  let checksum                   = dc#get_dword () in 
  let subsystem                  = dc#get_word  () in 
  let dll_characteristics        = dc#get_word  () in 
  let size_of_stack_reserve      = dc#get_dword () in 
  let size_of_stack_commit       = dc#get_dword () in 
  let size_of_heap_reserve       = dc#get_dword () in 
  let size_of_heap_commit        = dc#get_dword () in 
  let loader_flags               = dc#get_dword () in 
  let number_of_rvas_and_sizes   = dc#get_dword () in 
  let data_directories = 
    let rec aux i list = 
      if i = 16
      then Array.of_list (List.rev list)
      else aux (i+1) ((consume_data_directory dc)::list)
    in aux 0 []
  in
  {
    magic                      = magic;
    major_linker_version       = major_linker_version; 
    minor_linker_version       = minor_linker_version; 
    size_of_code               = size_of_code; 
    size_of_initialized_data   = size_of_initialized_data; 
    size_of_uninitialized_data = size_of_uninitialized_data; 
    address_of_entrypoint      = address_of_entrypoint; 
    base_of_code               = base_of_code; 
    base_of_data               = base_of_data; 
    imagebase                  = imagebase; 
    section_alignment          = section_alignment; 
    file_alignment             = file_alignment; 
    major_os_version           = major_os_version; 
    minor_os_version           = minor_os_version; 
    major_image_version        = major_image_version; 
    minor_image_version        = minor_image_version; 
    major_subsystem_version    = major_subsystem_version; 
    minor_subsystem_version    = minor_subsystem_version; 
    win32_version_value        = win32_version_value; 
    size_of_image              = size_of_image; 
    size_of_headers            = size_of_headers; 
    checksum                   = checksum; 
    subsystem                  = subsystem; 
    dll_characteristics        = dll_characteristics; 
    size_of_stack_reserve      = size_of_stack_reserve; 
    size_of_stack_commit       = size_of_stack_commit; 
    size_of_heap_reserve       = size_of_heap_reserve; 
    size_of_heap_commit        = size_of_heap_commit; 
    loader_flags               = loader_flags; 
    number_of_rvas_and_sizes   = number_of_rvas_and_sizes; 
    data_directories           = data_directories; 
  }

type image_directory =
| IMAGE_DIRECTORY_ENTRY_EXPORT
| IMAGE_DIRECTORY_ENTRY_IMPORT
| IMAGE_DIRECTORY_ENTRY_RESOURCE
| IMAGE_DIRECTORY_ENTRY_EXCEPTION
| IMAGE_DIRECTORY_ENTRY_SECURITY
| IMAGE_DIRECTORY_ENTRY_BASERELOC
| IMAGE_DIRECTORY_ENTRY_DEBUG
| IMAGE_DIRECTORY_ENTRY_ARCHITECTURE
| IMAGE_DIRECTORY_ENTRY_GLOBALPTR
| IMAGE_DIRECTORY_ENTRY_TLS
| IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG
| IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT
| IMAGE_DIRECTORY_ENTRY_IAT
| IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT
| IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR

let int_of_image_directory = function
| IMAGE_DIRECTORY_ENTRY_EXPORT         -> 0
| IMAGE_DIRECTORY_ENTRY_IMPORT         -> 1
| IMAGE_DIRECTORY_ENTRY_RESOURCE       -> 2
| IMAGE_DIRECTORY_ENTRY_EXCEPTION      -> 3
| IMAGE_DIRECTORY_ENTRY_SECURITY       -> 4
| IMAGE_DIRECTORY_ENTRY_BASERELOC      -> 5
| IMAGE_DIRECTORY_ENTRY_DEBUG          -> 6
| IMAGE_DIRECTORY_ENTRY_ARCHITECTURE   -> 7
| IMAGE_DIRECTORY_ENTRY_GLOBALPTR      -> 8
| IMAGE_DIRECTORY_ENTRY_TLS            -> 9
| IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    -> 10
| IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   -> 11
| IMAGE_DIRECTORY_ENTRY_IAT            -> 12
| IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   -> 13
| IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR -> 14

type nt_headers =
{
  (* +00 Signature (0x00004550) *)
  signature: int32;
  
  (* +04 FileHeader *)
  file_header: file_header;
  
  (* +18 OptionalHeader *)
  optional_header: optional_header;
}

let consume_nt_headers (dc:data_consumer) =
  let signature = dc#get_dword () in
  let file_header = consume_file_header dc in
  let optional_header = consume_optional_header dc in
  {
    signature = signature;
    file_header = file_header;
    optional_header = optional_header;
  }

type section_header =
{
  (* +00 Name *)
  name: string;

  (* +08 VirtualSize *)
  virtual_size: int32;

  (* +0C VirtualAddress *)
  section_virtual_address: int32;

  (* +10 SizeOfRawData*)
  size_of_raw_data: int32;

  (* +14 PointerToRawData *)
  pointer_to_raw_data: int32;

  (* +18 PointerToRelocations *)
  pointer_to_relocations: int32;

  (* +1C PointerToLineNumbers *)
  pointer_to_line_numbers: int32;

  (* +20 NumberOfRelocations *)
  number_of_relocations: int32;

  (* +22 NumberOfLineNumbers *)
  number_of_line_numbers: int32;

  (* +24 Characteristics *)
  section_characteristics: int32;
}

type image_section_characteristic = 
| IMAGE_SCN_RESERVED1
| IMAGE_SCN_RESERVED2
| IMAGE_SCN_RESERVED3
| IMAGE_SCN_TYPE_NO_PAD
| IMAGE_SCN_RESERVED4
| IMAGE_SCN_CNT_CODE
| IMAGE_SCN_CNT_INITIALIZED_DATA
| IMAGE_SCN_CNT_UNINITIALIZED_DATA
| IMAGE_SCN_LNK_OTHER
| IMAGE_SCN_LNK_INFO
| IMAGE_SCN_RESERVED5
| IMAGE_SCN_LNK_REMOVE
| IMAGE_SCN_LNK_COMDAT
| IMAGE_SCN_RESERVED6
| IMAGE_SCN_RESERVED7
| IMAGE_SCN_GPREL
| IMAGE_SCN_RESERVED8
| IMAGE_SCN_MEM_PURGEABLE
| IMAGE_SCN_MEM_LOCKED
| IMAGE_SCN_MEM_PRELOAD
| IMAGE_SCN_ALIGN_1BYTES
| IMAGE_SCN_ALIGN_2BYTES
| IMAGE_SCN_ALIGN_4BYTES
| IMAGE_SCN_ALIGN_8BYTES
| IMAGE_SCN_ALIGN_16BYTES
| IMAGE_SCN_ALIGN_32BYTES
| IMAGE_SCN_ALIGN_64BYTES
| IMAGE_SCN_ALIGN_128BYTES
| IMAGE_SCN_ALIGN_256BYTES
| IMAGE_SCN_ALIGN_512BYTES
| IMAGE_SCN_ALIGN_1024BYTES
| IMAGE_SCN_ALIGN_2048BYTES
| IMAGE_SCN_ALIGN_4096BYTES
| IMAGE_SCN_ALIGN_8192BYTES
| IMAGE_SCN_LNK_NRELOC_OVFL
| IMAGE_SCN_MEM_DISCARDABLE
| IMAGE_SCN_MEM_NOT_CACHED
| IMAGE_SCN_MEM_NOT_PAGED
| IMAGE_SCN_MEM_SHARED
| IMAGE_SCN_MEM_EXECUTE
| IMAGE_SCN_MEM_READ
| IMAGE_SCN_MEM_WRITE

let int32_of_section_characteristic = function
| IMAGE_SCN_RESERVED1              -> 0x00000001l
| IMAGE_SCN_RESERVED2              -> 0x00000002l
| IMAGE_SCN_RESERVED3              -> 0x00000004l
| IMAGE_SCN_TYPE_NO_PAD            -> 0x00000008l
| IMAGE_SCN_RESERVED4              -> 0x00000010l
| IMAGE_SCN_CNT_CODE               -> 0x00000020l
| IMAGE_SCN_CNT_INITIALIZED_DATA   -> 0x00000040l
| IMAGE_SCN_CNT_UNINITIALIZED_DATA -> 0x00000080l
| IMAGE_SCN_LNK_OTHER              -> 0x00000100l
| IMAGE_SCN_LNK_INFO               -> 0x00000200l
| IMAGE_SCN_RESERVED5              -> 0x00000400l
| IMAGE_SCN_LNK_REMOVE             -> 0x00000800l
| IMAGE_SCN_LNK_COMDAT             -> 0x00001000l
| IMAGE_SCN_RESERVED6              -> 0x00002000l
| IMAGE_SCN_RESERVED7              -> 0x00004000l
| IMAGE_SCN_GPREL                  -> 0x00008000l
| IMAGE_SCN_RESERVED8              -> 0x00010000l
| IMAGE_SCN_MEM_PURGEABLE          -> 0x00020000l
| IMAGE_SCN_MEM_LOCKED             -> 0x00040000l
| IMAGE_SCN_MEM_PRELOAD            -> 0x00080000l
| IMAGE_SCN_ALIGN_1BYTES           -> 0x00100000l
| IMAGE_SCN_ALIGN_2BYTES           -> 0x00200000l
| IMAGE_SCN_ALIGN_4BYTES           -> 0x00300000l
| IMAGE_SCN_ALIGN_8BYTES           -> 0x00400000l
| IMAGE_SCN_ALIGN_16BYTES          -> 0x00500000l
| IMAGE_SCN_ALIGN_32BYTES          -> 0x00600000l
| IMAGE_SCN_ALIGN_64BYTES          -> 0x00700000l
| IMAGE_SCN_ALIGN_128BYTES         -> 0x00800000l
| IMAGE_SCN_ALIGN_256BYTES         -> 0x00900000l
| IMAGE_SCN_ALIGN_512BYTES         -> 0x00A00000l
| IMAGE_SCN_ALIGN_1024BYTES        -> 0x00B00000l
| IMAGE_SCN_ALIGN_2048BYTES        -> 0x00C00000l
| IMAGE_SCN_ALIGN_4096BYTES        -> 0x00D00000l
| IMAGE_SCN_ALIGN_8192BYTES        -> 0x00E00000l
| IMAGE_SCN_LNK_NRELOC_OVFL        -> 0x01000000l
| IMAGE_SCN_MEM_DISCARDABLE        -> 0x02000000l
| IMAGE_SCN_MEM_NOT_CACHED         -> 0x04000000l
| IMAGE_SCN_MEM_NOT_PAGED          -> 0x08000000l
| IMAGE_SCN_MEM_SHARED             -> 0x10000000l
| IMAGE_SCN_MEM_EXECUTE            -> 0x20000000l
| IMAGE_SCN_MEM_READ               -> 0x40000000l
| IMAGE_SCN_MEM_WRITE              -> 0x80000000l

(* Don't pass IMAGE_SCN_ALIGN_xxxBYTES as these are not masks. *)
let section_has_characteristic c f =
  let m = int32_of_section_characteristic f in
  Int32.logand m c <> 0l

let int32_of_section_characteristic = function
| 0x00000001l -> IMAGE_SCN_RESERVED1
| 0x00000002l -> IMAGE_SCN_RESERVED2
| 0x00000004l -> IMAGE_SCN_RESERVED3
| 0x00000008l -> IMAGE_SCN_TYPE_NO_PAD
| 0x00000010l -> IMAGE_SCN_RESERVED4
| 0x00000020l -> IMAGE_SCN_CNT_CODE
| 0x00000040l -> IMAGE_SCN_CNT_INITIALIZED_DATA
| 0x00000080l -> IMAGE_SCN_CNT_UNINITIALIZED_DATA
| 0x00000100l -> IMAGE_SCN_LNK_OTHER
| 0x00000200l -> IMAGE_SCN_LNK_INFO
| 0x00000400l -> IMAGE_SCN_RESERVED5
| 0x00000800l -> IMAGE_SCN_LNK_REMOVE
| 0x00001000l -> IMAGE_SCN_LNK_COMDAT
| 0x00002000l -> IMAGE_SCN_RESERVED6
| 0x00004000l -> IMAGE_SCN_RESERVED7
| 0x00008000l -> IMAGE_SCN_GPREL
| 0x00010000l -> IMAGE_SCN_RESERVED8
| 0x00020000l -> IMAGE_SCN_MEM_PURGEABLE
| 0x00040000l -> IMAGE_SCN_MEM_LOCKED
| 0x00080000l -> IMAGE_SCN_MEM_PRELOAD
| 0x00100000l -> IMAGE_SCN_ALIGN_1BYTES
| 0x00200000l -> IMAGE_SCN_ALIGN_2BYTES
| 0x00300000l -> IMAGE_SCN_ALIGN_4BYTES
| 0x00400000l -> IMAGE_SCN_ALIGN_8BYTES
| 0x00500000l -> IMAGE_SCN_ALIGN_16BYTES
| 0x00600000l -> IMAGE_SCN_ALIGN_32BYTES
| 0x00700000l -> IMAGE_SCN_ALIGN_64BYTES
| 0x00800000l -> IMAGE_SCN_ALIGN_128BYTES
| 0x00900000l -> IMAGE_SCN_ALIGN_256BYTES
| 0x00A00000l -> IMAGE_SCN_ALIGN_512BYTES
| 0x00B00000l -> IMAGE_SCN_ALIGN_1024BYTES
| 0x00C00000l -> IMAGE_SCN_ALIGN_2048BYTES
| 0x00D00000l -> IMAGE_SCN_ALIGN_4096BYTES
| 0x00E00000l -> IMAGE_SCN_ALIGN_8192BYTES
| 0x01000000l -> IMAGE_SCN_LNK_NRELOC_OVFL
| 0x02000000l -> IMAGE_SCN_MEM_DISCARDABLE
| 0x04000000l -> IMAGE_SCN_MEM_NOT_CACHED
| 0x08000000l -> IMAGE_SCN_MEM_NOT_PAGED
| 0x10000000l -> IMAGE_SCN_MEM_SHARED
| 0x20000000l -> IMAGE_SCN_MEM_EXECUTE
| 0x40000000l -> IMAGE_SCN_MEM_READ
| 0x80000000l -> IMAGE_SCN_MEM_WRITE
| _ -> invalid_arg "int32_of_section_characteristic"

let decompose_section_characteristics c = 
 (List.map int32_of_section_characteristic (decompose_into_bits 20 c))@
 [int32_of_section_characteristic (Int32.logand 0x00F00000l c)]@
 (List.map int32_of_section_characteristic (decompose_into_bits ?low:(Some(24)) 32 c))

let consume_section_header (dc:data_consumer) =
  let name = String.make 9 (char_of_int 0) in
  let rec aux i =
    if i = 8
    then ()
    else 
      let _ = String.set name i (char_of_int (Int32.to_int (dc#get_byte ()))) in
      aux (i+1)
  in
  let _ = aux 0 in
  let virtual_size            = dc#get_dword () in
  let section_virtual_address = dc#get_dword () in
  let size_of_raw_data        = dc#get_dword () in
  let pointer_to_raw_data     = dc#get_dword () in
  let pointer_to_relocations  = dc#get_dword () in
  let pointer_to_line_numbers = dc#get_dword () in
  let number_of_relocations   = dc#get_word ()  in
  let number_of_line_numbers  = dc#get_word ()  in
  let characteristics         = dc#get_dword () in
  {
    name = name;
    virtual_size = virtual_size;
    section_virtual_address = section_virtual_address;
    size_of_raw_data = size_of_raw_data;
    pointer_to_raw_data = pointer_to_raw_data;
    pointer_to_relocations = pointer_to_relocations;
    pointer_to_line_numbers = pointer_to_line_numbers;
    number_of_relocations = number_of_relocations;
    number_of_line_numbers = number_of_line_numbers;
    section_characteristics = characteristics;
  }

type all_headers = 
{
  mz_header: mz_header;
  nt_headers: nt_headers;
  section_headers: section_header array;
}

(* Does not consume directories *)
let consume_all_headers (dc:data_consumer) = 
  let mz_header = consume_mz_header dc in
  dc#set_streampos (Int32.to_int mz_header.e_lfanew);
  let nt_headers = consume_nt_headers dc in
  let n = Int32.to_int nt_headers.file_header.number_of_sections in
  let rec aux i list = 
    if i = n
    then Array.of_list (List.rev list)
    else aux (i+1) ((consume_section_header dc)::list)
  in
  {
    mz_header = mz_header;
    nt_headers = nt_headers;
    section_headers = aux 0 [];
  }

(* Int32 = raw address, int = section number (None for header) *)
type rva2phys =
| PhysOffset of int32 * int option
| Padding of int32 * int option
| OutsideFile

(* Duplicated from ArrayUtil *)
let fold_lefti f acc arr =
  snd(Array.fold_left  (fun (i,acc) arrel -> (i+1,f i acc arrel)) (0,acc) arr)

let make_sections_list allh = 
  let list = 
   (*ArrayUtil.*)fold_lefti 
     (fun i l e -> 
       (e.section_virtual_address,e.pointer_to_raw_data,e.size_of_raw_data,Some(i))::l)
     [(0l,0l,allh.nt_headers.optional_header.size_of_headers,None)]
     allh.section_headers
  in 
  List.sort (fun (v1,_,_,_) (v2,_,_,_) -> Pervasives.compare v1 v2) list

let rva_to_physical list rva = 
  let rec aux = function
  | [] -> OutsideFile
  | (va,raw,rawsize,no)::xs 
    when 
      rva >= va && 
      rva <= (Int32.add va rawsize) 
    -> 
      PhysOffset(Int32.add raw (Int32.sub rva va),no)
  | (va1,raw1,rawsize1,no1)::
    (va2,raw2,rawsize2,no2)::xs 
    when
      rva >= va1 &&
      rva <  va2 &&
      rva >= (Int32.add va1 rawsize1) 
    ->
      Padding(Int32.add raw1 (Int32.sub rva va1),no1)
  | _::xs -> aux xs
  in aux list
  
type base_relocation =
| IMAGE_REL_BASED_ABSOLUTE
| IMAGE_REL_BASED_HIGH
| IMAGE_REL_BASED_LOW
| IMAGE_REL_BASED_HIGHLOW
| IMAGE_REL_BASED_HIGHADJ
| IMAGE_REL_BASED_ARM_MOV32A
| IMAGE_REL_BASED_ARM_MOV32T
| IMAGE_REL_BASED_MIPS_JMPADDR16
| IMAGE_REL_BASED_DIR64

let int32_of_base_relocation = function
| IMAGE_REL_BASED_ABSOLUTE       -> 0l 
| IMAGE_REL_BASED_HIGH           -> 1l 
| IMAGE_REL_BASED_LOW            -> 2l 
| IMAGE_REL_BASED_HIGHLOW        -> 3l 
| IMAGE_REL_BASED_HIGHADJ        -> 4l 
| IMAGE_REL_BASED_ARM_MOV32A     -> 5l 
| IMAGE_REL_BASED_ARM_MOV32T     -> 7l 
| IMAGE_REL_BASED_MIPS_JMPADDR16 -> 9l 
| IMAGE_REL_BASED_DIR64          -> 10l

let base_relocation_of_int32 = function
| 0l  -> IMAGE_REL_BASED_ABSOLUTE
| 1l  -> IMAGE_REL_BASED_HIGH
| 2l  -> IMAGE_REL_BASED_LOW
| 3l  -> IMAGE_REL_BASED_HIGHLOW
| 4l  -> IMAGE_REL_BASED_HIGHADJ
| 5l  -> IMAGE_REL_BASED_ARM_MOV32A
| 7l  -> IMAGE_REL_BASED_ARM_MOV32T
| 9l  -> IMAGE_REL_BASED_MIPS_JMPADDR16
| 10l -> IMAGE_REL_BASED_DIR64
| _   -> invalid_arg "base_relocation_of_int32"

type rva32 = int32

type image_export_directory =
{
  (* +00 Export Flags *)
  characteristics: int32;
  
  (* +04 Time/Date Stamp *)
  time_date_stamp: int32;
  
  (* +08 Major Version *)
  major_version: int32;
  
  (* +0A Minor Version *)
  minor_version: int32;

   (* +0C Name RVA*)
  name: rva32;

  (* +10 Ordinal Base *)
  base: int32;

  (* +14 Address Table Entries *)
  number_of_functions: int32;

  (* +18 Number of Name Pointers *)
  number_of_names: int32;

  (* +1C Export Address Table RVA *)
  address_of_functions: rva32;

  (* +20 Name Pointer RVA *)
  address_of_names: rva32;
  
  (* +24 Ordinal Table RVA *)
  address_of_name_ordinals: rva32;  
}

type image_import_descriptor =
{
  (* +00 Import Lookup Table RVA *)
  original_first_thunk: rva32;
  
  (* +04 Time/Date Stamp *)
  time_date_stamp: int32;
  
  (* +08 Forwarder Chain *)
  forwarder_chain: int32;
  
  (* +0C Name RVA *)
  name: rva32;
  
  (* +10 Import Address Table RVA *)
  first_thunk: rva32;  
}

let consume_image_import_descriptor (dc:data_consumer) =
  let original_first_thunk = dc#get_dword () in
  let time_date_stamp      = dc#get_dword () in
  let forwarder_chain      = dc#get_dword () in
  let name                 = dc#get_dword () in
  let first_thunk          = dc#get_dword () in
  {
    original_first_thunk = original_first_thunk;
    time_date_stamp = time_date_stamp;
    forwarder_chain = forwarder_chain;
    name = name;
    first_thunk = first_thunk;
  }
  
let consume_image_import_descriptors (dc:data_consumer) = 
  let rec aux list = 
    let iid = consume_image_import_descriptor dc in
    if iid.name = 0l 
    then List.rev list
    else aux (iid::list)
  in
  aux []


let consume_null_terminated_dword_array (dc:data_consumer) = 
  let rec aux list =
    let d = dc#get_dword () in
    if d = 0l then List.rev list else aux (d::list)
  in aux []
  
let consume_null_terminated_string (dc:data_consumer) = 
  let rec aux i list =
    let b = dc#get_byte() in
    if b = 0l then (i,List.rev list) else aux (i+1) (b::list)
  in
  let i,l = aux 0 [] in
  let s = String.create i in
  let _ = 
    List.fold_left 
     (fun i e -> let _ = String.set s i (char_of_int (Int32.to_int e)) in i+1)
      0
      l
  in
  s

let point_to_address all_headers physaddr (dc:data_consumer) =
  (* SHOULD STORE THIS IN ONE OF THE STRUCTURES *)
  let seclist = make_sections_list all_headers in 
  let rva_to_phys = rva_to_physical seclist in
  
  (* USE A CUSTOM EXCEPTION *)
  if physaddr = 0l then invalid_arg "point_to_address 1";
  
  (* USE CUSTOM EXCEPTIONS *)
  let physaddr2 = match rva_to_phys physaddr with
  | PhysOffset(o,_) -> o
  | Padding(_,_)    -> invalid_arg ("point_to_address 2: "^(Int32.to_string physaddr))
  | OutsideFile     -> invalid_arg "point_to_address 3"
  in
  dc#set_streampos (Int32.to_int physaddr2);
  ()

let point_to_directory whichdir all_headers (dc:data_consumer) = 
  let dirno = int_of_image_directory whichdir in
  let dirent = all_headers.nt_headers.optional_header.data_directories.(dirno) in
  let imprva = dirent.virtual_address in
  point_to_address all_headers imprva dc

type import_entry =
| Ordinal of int32
| Symbol of string

let consume_import_descriptors all_headers (dc:data_consumer) = 
  let _ = point_to_directory (IMAGE_DIRECTORY_ENTRY_IMPORT) all_headers dc in
  let iids = consume_image_import_descriptors dc in
  let imports = 
    List.map
     (fun iid ->
        let name = 
          point_to_address all_headers iid.name dc;
          let s = consume_null_terminated_string dc in
          Printf.printf "Name: %s\n" s;
          s
        in
        let arr = 
          point_to_address all_headers iid.first_thunk dc;
          consume_null_terminated_dword_array dc
        in
        let imports = 
          List.map 
           (fun el ->
              if Int32.logand 0x80000000l el <> 0l
              then Ordinal(Int32.logand 0xffffl el)
              else Symbol(
                let s = 
                  point_to_address all_headers (Int32.add el 2l) dc;
                  let s = consume_null_terminated_string dc in
                  Printf.printf "Import: %s\n" s;
                  s
                in s))
            arr
        in
        (name,imports))
      iids
  in
  (iids,imports)

type image_base_relocation = 
{
  (* +00 Page RVA *)
  virtual_address: rva32;
  
  (* +04 Block Size incl this structure's size (8)*)
  block_size: int32;
}

let decode_base_relocation_entry br baserva = 
  let typ    = Int32.shift_right_logical br 12 in
  let offset = Int32.logand br 0x0FFFl in
  if typ > 3l then raise (Unimplemented("Base relocation type >3: "^Int32.to_string typ));
  (base_relocation_of_int32 typ,Int32.add baserva offset)
  
let consume_base_relocation_block list (dc:data_consumer) = 
  let rva  = dc#get_dword () in
  let size = dc#get_dword () in
  let n = Int32.div (Int32.sub size 8l) 2l in
  let rec aux i list =
    if i = n
    then list
    else aux (Int32.succ i) ((decode_base_relocation_entry (dc#get_word ()) rva)::list)
  in (aux 0l list,size)

let consume_base_relocations all_headers (dc:data_consumer) =
  let dir = IMAGE_DIRECTORY_ENTRY_BASERELOC in
  let dirno = int_of_image_directory dir in
  let dirent = all_headers.nt_headers.optional_header.data_directories.(dirno) in
  let size = dirent.size in
  let _ = point_to_directory dir all_headers dc in
  let rec aux nbytes list = 
    if nbytes >= size
    then list
    else 
      let (list,sz) = consume_base_relocation_block list dc in
      aux (Int32.add sz nbytes) list
  in aux 0l []

let make_relocation_amt_list actual preferred =
  (* r0: relocation amount *)
  let r0 = Int32.sub actual preferred in
  Printf.printf "actual: %lx preferred: %lx r0: %lx\n" actual preferred r0;
  let b0,r1 = Int32.logand r0 0xffl,Int32.shift_right_logical r0 8 in
  let b1,r2 = Int32.logand r1 0xffl,Int32.shift_right_logical r1 8 in
  let b2,b3 = Int32.logand r2 0xffl,Int32.shift_right_logical r2 8 in
  let i = Int32.to_int in
  (i b0,i b1,i b2,i b3)
  
module Int32Comparator = struct
  type t = int32
  let compare i j = Int32.compare i j
end

module Int32Set = Set.Make(Int32Comparator)

let setify_base_relocations all_headers (dc:data_consumer) relocs =
  let add s = function
  | (IMAGE_REL_BASED_HIGHLOW,rva) -> Int32Set.add rva s
  | (IMAGE_REL_BASED_ABSOLUTE,_)  -> s
  | _ -> raise (Unimplemented("consume_and_setify_base_relocations"))
  in
  List.fold_left add Int32Set.empty relocs
  
let consume_and_setify_base_relocations all_headers (dc:data_consumer) =
  setify_base_relocations all_headers dc (consume_base_relocations all_headers dc)

let clopen_interval_base_relocations set low high =
  List.rev (Int32Set.fold (fun i l -> if low <= i && i < high then (i::l) else l) set [])
  
let string_fold_left string f acc = 
  let v = ref acc in
  String.iter (fun c -> let nv = f !v c in v := nv) string;
  !v

let extract_executable_sections ?(baseaddr=None) s_exefile =
  (* New data consumer object to parse headers, consume section contents. *)
  let dc = new data_consumer (open_in_bin s_exefile) in
  
  (* Consume the headers. *)
  let allh = consume_all_headers dc in

  (* Consume the base relocations.  Will behave gracefully if none exist. *)
  let brs = consume_and_setify_base_relocations allh dc in
  
  (* Store imagebase for further processing. *)
  let imagebase = allh.nt_headers.optional_header.imagebase in

  (* Gather up the executable sections from the section headers. *)
  let exec_secs = 
   (*ArrayUtil.*)fold_lefti 
     (fun i l e -> 
        (* Only keep the ones with the executable characteristic *)
        if section_has_characteristic e.section_characteristics (IMAGE_SCN_MEM_EXECUTE)
        then 
         (* Store the 0-indexed section number, va, pointer to raw data, and size *)
         (i,
          e.section_virtual_address,
          e.pointer_to_raw_data,
          Int32.to_int (min e.virtual_size e.size_of_raw_data))::l
        else l)
      []
      allh.section_headers
  in

  (* Get the relocation bytes if necessary *)
  let r0,r1,r2,r3 = 
    match baseaddr with 
    | None -> (0,0,0,0)
    | Some(x) -> make_relocation_amt_list x imagebase 
  in
  
  (* Relocate the executable sections if necessary. *)
  (* Should rewrite this to just explicitly bail if no relocations need application. *)
  let exec_secs = 
    List.rev_map
     (* This loop iterates over all of the executable sections *)
     (fun (num,va,raw,size) -> 
        (* Grab the bytes and the relocations for the section *)
        let _ = dc#set_streampos (Int32.to_int raw) in
        let b = dc#get_many_bytes_exn size in
        let r = clopen_interval_base_relocations brs va (Int32.add va (Int32.of_int size)) in
        let set_and_relocate_byte i c v = 
          let new_v = ((int_of_char c)+v)  in
          String.set b i (char_of_int (new_v land 0xff));
          if new_v >= 0x100 then 1 else 0
        in

        (* Relocate the section *)
        let _ = 
          string_fold_left b 
           (* Arguments:  
              va      -- the virtual address of the present byte
              valist  -- the ordered relocations for the section
              dvalist -- "deferred" relocations, since we only have
                         relocation information for 1/4 of each 
                         4-byte relocation entry.  See below.
              i       -- counterpart to va; byte number within the 
                         string
              c       -- the character at b.(i)
           *)
           (fun (va,valist,dvalist,i) c -> 
              let ip,vap = i+1,Int32.succ va in
              match dvalist with
              | v::w::vs -> let c = set_and_relocate_byte i c v in (vap,valist,(w+c)::vs,ip)
              | v::[]    -> let _ = set_and_relocate_byte i c v in (vap,valist,[],ip)
              | []       ->
               (match valist with
                | []                -> (0l,[],[],ip)
                | v::vs when va = v -> let c = set_and_relocate_byte i c r0 in (vap,vs,[r1+c;r2;r3],ip)
                | _                 -> (vap,valist,[],ip)))
            (va,r,[],0)
        in
        (num,va,size,b))
      exec_secs
  in
  exec_secs

(*let consume_all_headers (dc:data_consumer) = *)


(*
let dc = new data_consumer (open_in_bin "c:\\temp\\toyproject-patched.exe");;
let allh = consume_all_headers dc;;
let rva2physlist = make_sections_list allh;;
let rva_to_phys = rva_to_physical rva2physlist;;
rva_to_phys allh.nt_headers.optional_header.address_of_entrypoint;;
*)

(*
#use "c:\\paframework\\PE32\\PEParse.ml";;
let dc = new data_consumer (open_in_bin "c:\\temp\\depends.dll");;
let allh = consume_all_headers dc;;
let brs = consume_base_relocations allh dc;;

#use "c:\\paframework\\PE32\\PEParse.ml";;
let secs = extract_executable_sections ~baseaddr:(Some(0x10000000l)) "c:\\temp\\depends.dll";;
List.iter 
 (fun (num,va,size,b) -> output_string (open_out_bin (Printf.sprintf "C:\\temp\\dmine0x%lx.bin" va)) b)
  secs;;

let rva2physlist = make_sections_list allh;;
let rva_to_phys = rva_to_physical rva2physlist;;
rva_to_phys allh.nt_headers.optional_header.address_of_entrypoint;;
*)