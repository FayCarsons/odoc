open Odoc_unit

val packages :
  dirs:dirs ->
  extra_paths:Voodoo.extra_paths ->
  remap:bool ->
  gen_indices:bool ->
  Packages.t list ->
  t list
