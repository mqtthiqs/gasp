#load "dynlink.cma"
#load "camlp4o.cma"
#directory "_build"
#load "LF.cma"

#install_printer SLF.Printer.term
#install_printer SLF.Printer.sign

#install_printer SLF.Printer.obj
#install_printer SLF.Printer.fam
#install_printer SLF.Printer.kind
#install_printer SLF.Printer.sign
#install_printer SLF.Printer.env
#install_printer SLF.Printer.context
#install_printer SLF.Printer.repo

#install_printer Names.Meta.print
;;
