(** [patch_from_string s] parses the string [s] as a patch. *)
val patch_from_string : string -> AST.patch

(** [patch_from_file f] parses the contents of file [f] as a
    patch. *)
val patch_from_file : string -> AST.patch
