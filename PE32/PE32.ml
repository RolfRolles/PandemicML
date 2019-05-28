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
  directory_virtual_address: int32;
  
  (* +04 *)
  directory_size: int32;
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

type optional_magic =
| PE32
| PE32Plus

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

type nt_headers =
{
  (* +00 Signature (0x00004550) *)
  signature: int32;
  
  (* +04 FileHeader *)
  file_header: file_header;
  
  (* +18 OptionalHeader *)
  optional_header: optional_header;
}

type section_header =
{
  (* +00 Name *)
  section_name: string;

  (* +08 VirtualSize *)
  section_virtual_size: int32;

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

type all_headers = 
{
  mz_header: mz_header;
  nt_headers: nt_headers;
  section_headers: section_header array;
}

(* Int32 = raw address, int = section number (None for header) *)
type rva2phys =
| PhysOffset of int32 * int option
| Padding of int32 * int option
| OutsideFile

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

type rva32 = int32

type image_export_directory =
{
  (* +00 Export Flags *)
  export_characteristics: int32;
  
  (* +04 Time/Date Stamp *)
  export_time_date_stamp: int32;
  
  (* +08 Major Version *)
  major_version: int32;
  
  (* +0A Minor Version *)
  minor_version: int32;

   (* +0C Name RVA*)
  export_name: rva32;

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
  import_time_date_stamp: int32;
  
  (* +08 Forwarder Chain *)
  forwarder_chain: int32;
  
  (* +0C Name RVA *)
  import_name: rva32;
  
  (* +10 Import Address Table RVA *)
  first_thunk: rva32;  
}

type image_base_relocation = 
{
  (* +00 Page RVA *)
  reloc_virtual_address: rva32;
  
  (* +04 Block Size incl this structure's size (8)*)
  block_size: int32;
}

type image_resource_directory =
{
  (* +00:  Characteristics *)
  rsrc_characteristics: int32;

  (* +04:  Time/date stamp *)
  rsrc_time_date_stamp: int32;
  
  (* +08:  Major version *)
  rsrc_major_version: int32;

  (* +0A:  Minor version *)
  rsrc_minor_version: int32;

  (* +0C:  Number of named entries *)
  rsrc_number_of_named_entries: int32;

  (* +0E:  Number of ID entries *)
  rsrc_number_of_id_entries: int32;
}

type image_resource_directory_entry =
{
  (* +00: NameID *)
  rsrc_name_id: int32;
  
  (* +04: Data. Relative to resource section start. *)
  rsrc_dir_data: rva32;
}

type image_resource_data_entry =
{
  (* +00: Data. Relative to resource section start. *)
  rsrc_entry_data: rva32;

  (* +04: Size *)
  rsrc_entry_size: int32;

  (* +08: Code page (for Unicode strings). *)
  rsrc_entry_code_page: int32;

  (* +0C: Reserved. *)
  rsrc_entry_reserved: int32;
}

type resource_type = 
| Unnamed0
| Cursor
| Bitmap
| Icon
| Menu
| Dialog
| String
| FontDirectory
| Font
| Accelerators
| RCData
| MessageTable
| GroupCursor
| Unnamed13
| GroupIcon
| Unnamed15
| Version
| DialogInclude
| Unnamed18
| PlugAndPlay
| VXD
| AnimatedCursor
| AnimatedIcon
| HTML
| Manifest
| UserSpecified of int32

let resource_type_of_int32 = function
| 0x00l -> Unnamed0
| 0x01l -> Cursor
| 0x02l -> Bitmap
| 0x03l -> Icon
| 0x04l -> Menu
| 0x05l -> Dialog
| 0x06l -> String
| 0x07l -> FontDirectory
| 0x08l -> Font
| 0x09l -> Accelerators
| 0x0Al -> RCData
| 0x0Bl -> MessageTable
| 0x0Cl -> GroupCursor
| 0x0Dl -> Unnamed13
| 0x0El -> GroupIcon
| 0x0Fl -> Unnamed15
| 0x10l -> Version
| 0x11l -> DialogInclude
| 0x12l -> Unnamed18
| 0x13l -> PlugAndPlay
| 0x14l -> VXD
| 0x15l -> AnimatedCursor
| 0x16l -> AnimatedIcon
| 0x17l -> HTML
| 0x18l -> Manifest
| x     -> UserSpecified(x)

let int32_of_resource_type = function
| Unnamed0         -> 0x00l
| Cursor           -> 0x01l
| Bitmap           -> 0x02l
| Icon             -> 0x03l
| Menu             -> 0x04l
| Dialog           -> 0x05l
| String           -> 0x06l
| FontDirectory    -> 0x07l
| Font             -> 0x08l
| Accelerators     -> 0x09l
| RCData           -> 0x0Al
| MessageTable     -> 0x0Bl
| GroupCursor      -> 0x0Cl
| Unnamed13        -> 0x0Dl
| GroupIcon        -> 0x0El
| Unnamed15        -> 0x0Fl
| Version          -> 0x10l
| DialogInclude    -> 0x11l
| Unnamed18        -> 0x12l
| PlugAndPlay      -> 0x13l
| VXD              -> 0x14l
| AnimatedCursor   -> 0x15l
| AnimatedIcon     -> 0x16l
| HTML             -> 0x17l
| Manifest         -> 0x18l
| UserSpecified(x) -> x    

let string_of_resource_type = function
| Unnamed0         -> "Unnamed0"
| Cursor           -> "Cursor"
| Bitmap           -> "Bitmap"
| Icon             -> "Icon"
| Menu             -> "Menu"
| Dialog           -> "Dialog"
| String           -> "String"
| FontDirectory    -> "FontDirectory"
| Font             -> "Font"
| Accelerators     -> "Accelerators"
| RCData           -> "RCData"
| MessageTable     -> "MessageTable"
| GroupCursor      -> "GroupCursor"
| Unnamed13        -> "Unnamed13"
| GroupIcon        -> "GroupIcon"
| Unnamed15        -> "Unnamed15"
| Version          -> "Version"
| DialogInclude    -> "DialogInclude"
| Unnamed18        -> "Unnamed18"
| PlugAndPlay      -> "PlugAndPlay"
| VXD              -> "VXD"
| AnimatedCursor   -> "AnimatedCursor"
| AnimatedIcon     -> "AnimatedIcon"
| HTML             -> "HTML"
| Manifest         -> "Manifest"
| UserSpecified(x) -> "UserSpecified("^Int32.to_string x^")"

