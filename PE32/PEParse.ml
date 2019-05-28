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
     
open PE32

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

let get_dword_array dc n = 
  let rec aux i list =
    if i = n
    then List.rev list
    else aux (i+1) ((dc#get_dword ())::list)
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

let optional_magic_of_int32 = function
| 0x10bl -> PE32
| 0x20bl -> PE32Plus
| _ -> invalid_arg "optional_magic_of_int32"

let int32_of_optional_magic = function
| PE32     -> 0x10bl
| PE32Plus -> 0x20bl

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
    directory_virtual_address = virtual_address;
    directory_size = size;
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

let consume_nt_headers (dc:data_consumer) =
  let signature = dc#get_dword () in
  let file_header = consume_file_header dc in
  let optional_header = consume_optional_header dc in
  {
    signature = signature;
    file_header = file_header;
    optional_header = optional_header;
  }

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
    section_name = name;
    section_virtual_size = virtual_size;
    section_virtual_address = section_virtual_address;
    size_of_raw_data = size_of_raw_data;
    pointer_to_raw_data = pointer_to_raw_data;
    pointer_to_relocations = pointer_to_relocations;
    pointer_to_line_numbers = pointer_to_line_numbers;
    number_of_relocations = number_of_relocations;
    number_of_line_numbers = number_of_line_numbers;
    section_characteristics = characteristics;
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

let consume_image_import_descriptor (dc:data_consumer) =
  let original_first_thunk = dc#get_dword () in
  let time_date_stamp      = dc#get_dword () in
  let forwarder_chain      = dc#get_dword () in
  let name                 = dc#get_dword () in
  let first_thunk          = dc#get_dword () in
  {
    original_first_thunk = original_first_thunk;
    import_time_date_stamp = time_date_stamp;
    forwarder_chain = forwarder_chain;
    import_name = name;
    first_thunk = first_thunk;
  }
  
let consume_image_import_descriptors (dc:data_consumer) = 
  let rec aux list = 
    let iid = consume_image_import_descriptor dc in
    if iid.import_name = 0l 
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

let consume_unicode_string (dc:data_consumer) = 
  let l = Int32.to_int (dc#get_word ()) in
  let s = String.make l '0' in
  let rec aux i =
    if i = l
    then s
    else (let c = dc#get_word () in s.[i] <- (char_of_int (Int32.(to_int (logand 0xffl c)))); aux (i+1))
  in
  aux 0;
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
  let imprva = dirent.directory_virtual_address in
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
          point_to_address all_headers iid.import_name dc;
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

let consume_image_export_descriptor (dc:data_consumer) =
  let export_characteristics = dc#get_dword () in
  let time_date_stamp        = dc#get_dword () in
  let major_version          = dc#get_word () in
  let minor_version          = dc#get_word () in
  let name_rva               = dc#get_dword () in
  let ordinal_base           = dc#get_dword () in
  let table_entries          = dc#get_dword () in
  let name_pointers          = dc#get_dword () in
  let export_address_table   = dc#get_dword () in
  let name_pointer_rva       = dc#get_dword () in
  let ordinal_table_rva      = dc#get_dword () in
  {
    export_characteristics   = export_characteristics;
    export_time_date_stamp   = time_date_stamp;
    major_version            = major_version;
    minor_version            = minor_version;
    export_name              = name_rva;
    base                     = ordinal_base;
    number_of_functions      = table_entries;
    number_of_names          = name_pointers;
    address_of_functions     = export_address_table;
    address_of_names         = name_pointer_rva;
    address_of_name_ordinals = ordinal_table_rva;
  }

let consume_exports all_headers (dc:data_consumer) = 
  let _ = point_to_directory (IMAGE_DIRECTORY_ENTRY_EXPORT) all_headers dc in  
  let export = consume_image_export_descriptor dc in
  let _ = point_to_address all_headers export.export_name dc in
  let name = consume_null_terminated_string dc in
  let num = Int32.to_int export.number_of_functions in  
  let _ = point_to_address all_headers export.address_of_functions dc in
  let arr_funcs = get_dword_array dc (Int32.to_int export.number_of_functions) in
  let _ = point_to_address all_headers export.address_of_names dc in
  let arr_names = get_dword_array dc (Int32.to_int export.number_of_names) in
  let _ = point_to_address all_headers export.address_of_name_ordinals dc in
  let arr_nameords = get_dword_array dc (Int32.to_int export.number_of_names) in
 (export,arr_funcs,arr_names,arr_nameords)

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
  let size = dirent.directory_size in
  let _ = point_to_directory dir all_headers dc in
  let rec aux nbytes list = 
    if nbytes >= size
    then list
    else 
      let (list,sz) = consume_base_relocation_block list dc in
      aux (Int32.add sz nbytes) list
  in aux 0l []

let consume_image_resource_directory_entry (dc:data_consumer) =
  let name_id = dc#get_dword () in
  let data    = dc#get_dword () in
  {
    rsrc_name_id  = name_id;
    rsrc_dir_data = data;
  }

let consume_image_resource_data_entry (dc:data_consumer) =
  let data = dc#get_dword () in
  let size = dc#get_dword () in
  let code = dc#get_dword () in
  let resv = dc#get_dword () in
  {
    rsrc_entry_data = data;
    rsrc_entry_size = size;
    rsrc_entry_code_page = code;
    rsrc_entry_reserved  = resv;
  }

let consume_image_resource_descriptor (dc:data_consumer) =
  let characteristics = dc#get_dword () in
  let time_date_stamp = dc#get_dword () in
  let major_version   = dc#get_word ()  in
  let minor_version   = dc#get_word ()  in
  let named_entries   = dc#get_word ()  in
  let id_entries      = dc#get_word ()  in
  {
    rsrc_characteristics = characteristics;
    rsrc_time_date_stamp = time_date_stamp;
    rsrc_major_version   = major_version;
    rsrc_minor_version   = minor_version;
    rsrc_number_of_named_entries = named_entries;
    rsrc_number_of_id_entries = id_entries;
  }

(* image_resource_directory_entry:  
   NameId & 0x80000000 != 0 ? 
     Named(get_string (ResourceDirectoryBegin + (NameId&0x7FFFFFFF)) :
     Id(NameId & 0xFFFF); *)
type name_or_id =
| Named of string
| Id of resource_type

type resource_tree_inner = 
| Level1 of name_or_id * resource_tree_inner list
| Level2 of int32 * resource_tree_inner list
| Leaf   of image_resource_data_entry

type resource_tree = Root of resource_tree_inner list

(* 
   To get string IDs:
   (id-1)*16:  base string ID, increment by 1 for each string
*)
let parse_resources all_headers (dc:data_consumer) =
  let dir = IMAGE_DIRECTORY_ENTRY_RESOURCE in
  let dirno = int_of_image_directory dir in
  let dirent = all_headers.nt_headers.optional_header.data_directories.(dirno) in
  let rva  = dirent.directory_virtual_address in
  let size = dirent.directory_size in

  let reposition l = point_to_address all_headers l dc in 
  let rec consume_directory level = 
    let root = consume_image_resource_descriptor dc in
    let subentries n =
      let n = Int32.to_int n in
      let rec aux l i =
        if i = n
        then List.rev l
        else aux ((consume_image_resource_directory_entry dc)::l) (i+1)
      in aux [] 0
    in
    let entries = subentries Int32.(add root.rsrc_number_of_named_entries root.rsrc_number_of_id_entries) in
    let topbit n = Int32.(logand n 0x80000000l <> 0l,add rva (logand n 0x7FFFFFFFl)) in
    List.map (fun { rsrc_name_id = n; rsrc_dir_data = d; } -> 
      let b1,v1 = topbit n in
      let ent = if b1
        then Named(let _ = reposition v1 in consume_unicode_string dc)
        else Id(resource_type_of_int32 (Int32.logand 0xFFFFl n))
      in
      let b,v = topbit d in let _ = reposition v in
      if b
        then let d = consume_directory (level+1) in 
        if level=0
          then Level1(ent,d)
          else Level2(Int32.logand 0xFFFFl n,d)
        else Leaf(consume_image_resource_data_entry dc))
      entries
  in
  let _ = reposition rva in
  Root(consume_directory 0)
        
let print_resource_tree (Root(l)) =
  let _ = Printf.printf "Root:\n" in
  let rec aux header = function
  | Level1(x,l) -> 
   (match x with 
    | Named(s)  -> Printf.printf "%s%s:\n"  header s
    | Id(d)     -> Printf.printf "%s%s:\n"  header (string_of_resource_type d));
    List.iter (aux (header^"  ")) l
  | Level2(d,l) -> Printf.printf "%s%ld:\n" header d; 
    List.iter (aux (header^"  ")) l
  | Leaf({
      rsrc_entry_data = d; 
      rsrc_entry_size = s; 
      rsrc_entry_code_page = c; 
      rsrc_entry_reserved = r;}) ->
    let _ = Printf.printf "%sData: 0x%lx\n" header d in
    let _ = Printf.printf "%sSize: 0x%lx\n" header s in
    let _ = Printf.printf "%sCode page: 0x%lx\n" header c in
    Printf.printf "%sReserved: 0x%lx\n" header r
  in
  List.iter (aux "  ") l

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
          Int32.to_int (min e.section_virtual_size e.size_of_raw_data))::l
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
  (imagebase,exec_secs)

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

#use "c:\\paframework\\PE32\\PEStructures.mli";;
#use "c:\\paframework\\PE32\\PEParse.ml";;
let secs = extract_executable_sections ~baseaddr:(Some(0x10000000l)) "c:\\temp\\depends.dll";;
List.iter 
 (fun (num,va,size,b) -> output_string (open_out_bin (Printf.sprintf "C:\\temp\\dmine0x%lx.bin" va)) b)
  secs;;

let rva2physlist = make_sections_list allh;;
let rva_to_phys = rva_to_physical rva2physlist;;
rva_to_phys allh.nt_headers.optional_header.address_of_entrypoint;;
*)

let hash_name str =
  let str = str^"\x00" in
  let len = String.length str in
  let mk_byte  i = Int32.of_int (int_of_char (String.get str i)) in
  let mk_word  i = Int32.logor (Int32.shift_left (mk_byte (i+1))  8) (mk_byte i) in
  let mk_dword i = Int32.logor (Int32.shift_left (mk_word (i+2)) 16) (mk_word i) in
  let rec aux pos left hash = match left with
  | n when n < 0 -> failwith "impossible"
  | 0 -> hash
  | 1 -> Int32.logxor hash (mk_byte pos)
  | 2 -> Int32.logxor hash (mk_word pos)
  | 3 -> aux (pos+2) (left-2) (Int32.logxor hash (mk_word pos))
  | n -> aux (pos+4) (left-4) (Int32.logxor hash (mk_dword pos))
  in 
  aux 0 len 0l

let _ =
  let dllname = "shell32" in
  let dc = new data_consumer (open_in_bin ("c:\\Windows\\System32\\"^dllname^".dll")) in
  let allh = consume_all_headers dc in
  let (export,arr_func,arr_name,arr_nameord) = consume_exports allh dc in
  let names = Array.map (fun nrva -> let _ = point_to_address allh nrva dc in consume_null_terminated_string dc) arr_name in
  let names = Array.map (fun name -> (name,hash_name name)) names in
  Array.iter (fun (name,hash) -> Printf.printf "hash_%s_%s = 0x%lx,\n" dllname name hash) names
(*let tree = parse_resources allh dc in
  print_resource_tree tree*)
  
