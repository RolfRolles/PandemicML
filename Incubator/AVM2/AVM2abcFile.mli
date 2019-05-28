open AVM2Pervasives

type t =
{ minor_version: u16;
  major_version: u16;
  constant_pool: AVM2cpool_info.t;
  method_count: u30;
  method_: AVM2method_info.t array;
  metadata_count: u30;
  metadata: AVM2metadata_info.t array;
  class_count: u30;
  instance: AVM2instance_info.t array;
  class_: AVM2class_info.t array;
  script_count: u30;
  script: AVM2script_info.t array;
  method_body_count: u30;
  method_body: AVM2method_body_info.t array;
}

val parse : prim_parser -> t