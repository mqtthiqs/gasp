open Ocamlbuild_plugin

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let alphaCaml = A"alphaCaml";;

rule "alphaCaml: mla -> ml & mli"
  ~prods:["%.ml"; "%.mli"]
  ~dep:"%.mla"
  begin fun env _build ->
    Cmd(S[alphaCaml; P(env "%.mla")])
  end;;

let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"

let find_syntaxes () = ["camlp4o"; "camlp4r"]

let ocamlfind x = S[A"ocamlfind"; x]

let _ = dispatch begin function
   | Before_options ->
       Options.ocamlc     := ocamlfind & A"ocamlc";
       Options.ocamlopt   := ocamlfind & A"ocamlopt";
       Options.ocamldep   := ocamlfind & A"ocamldep";
       Options.ocamldoc   := ocamlfind & A"ocamldoc";
       Options.ocamlmktop := ocamlfind & A"ocamlmktop"

   | After_rules ->
       flag ["ocaml"; "link"; "program"] & A"-linkpkg";
       List.iter begin fun pkg ->
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());
       List.iter begin fun s ->
         flag ["ocaml"; "compile";  "syntax_"^s] & S[A"-syntax"; A s];
         flag ["ocaml"; "ocamldep"; "syntax_"^s] & S[A"-syntax"; A s];
         flag ["ocaml"; "doc";      "syntax_"^s] & S[A"-syntax"; A s];
       end (find_syntaxes ());
       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
       flag ["ocaml"; "need_td"; "compile"] & S[A"-pp"; Quote(S[A"camlp5o"; P"tools/typdef.cmo"])];
       flag ["ocaml"; "need_td"; "ocamldep"] & S[A"-pp"; Quote(S[A"camlp5o"; P"tools/typdef.cmo"])];
       dep ["ocaml"; "need_td"; "compile"] & ["tools/typdef.cmo"];
       dep ["ocaml"; "need_td"; "ocamldep"] & ["tools/typdef.cmo"];
       flag ["ocaml"; "use_camlp5"] & S[A"-I"; A"+camlp5"];
       flag ["file:kernel/gasp.byte"] & S[A"-I"; A"+camlp5"; A"camlp5.cma"];
       flag ["file:front-end/spl/spltc.byte"] & S[A"-I"; A"+camlp5"; A"camlp5.cma"];
   | _ -> ()
end
