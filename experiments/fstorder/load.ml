Toploop.initialize_toplevel_env ();;

#load "dynlink.cma"
#load "camlp4o.cma"
#directory "_build"
#load "LF.cma"

#install_printer SLF.Printer.term
#install_printer SLF.Printer.sign

#install_printer LF.Printer.obj
#install_printer LF.Printer.fam
#install_printer LF.Printer.kind
#install_printer LF.Printer.entity
#install_printer LF.Printer.sign
#install_printer LF.Printer.env

#install_printer Repo.Printer.t_light
;;

open Util
