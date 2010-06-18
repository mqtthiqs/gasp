let try_finalize = fun body f_block ->
  try
    let result = body () in
      f_block (); result
  with
    | e -> f_block (); raise e

module type IO_Sig =
sig
  (** Open [filename] and return all its content.
   ** Raise [Sys_error] on I/O error.
   **)
  val input_file : filename:string -> string

  (** Write [data] to [filename].
   ** Raise [Sys_error] on I/O error.
   **)
  val output_file : filename:string -> string -> unit

  (** Same as [input_file] but takes a [in_channel] instead of a
   ** filename. Does not close given [in_channel].
   **)
  val fd_input_file : file:in_channel -> string

  (** Some as [output_file] but takes as [out_channel] instead of a
   ** filename. Does not close given [out_channel].
   **)
  val fd_output_file : file:out_channel -> string -> unit
end

module IO: IO_Sig  =
struct
  let io_buffer_size = 1024

  let fd_input_file = fun ~file ->
    let buffer = Buffer.create 1024 in
    let io_buffer = String.create io_buffer_size
    and io_rr = ref 0
    in
      while (io_rr := input file io_buffer 0 io_buffer_size; !io_rr > 0) do
        Buffer.add_substring buffer io_buffer 0 !io_rr;
      done;
      Buffer.contents buffer

  let fd_output_file = fun ~file data ->
    output file data 0 (String.length data)

  let input_file = fun ~filename ->
    let input = open_in filename in
      try_finalize
        (fun () -> fd_input_file input)
        (fun () -> close_in input)

  let output_file = fun ~filename data ->
    let output = open_out filename in
      try_finalize
        (fun () -> fd_output_file output data)
        (fun () -> close_out output)
end
