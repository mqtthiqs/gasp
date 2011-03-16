tc_joinprog emptyenv (bindval (bind (_ x _*) tint))
(joinenv emptyenv (bindval (bind (_ x _*) tint)))
(joinenv (bindval (bind (_ y _*) tint))
 (joinenv (joinenv emptyenv (bindval (bind (_ x _*) tint)))
  (bindval (bind (_ y _*) tint))))
(joinprog (declprog (valdecl (bind (_ x _*) tint) (eint 42))) emptyprog)
(joinprog (declprog (valdecl (bind (_ y _*) tint) (eint 42))) emptyprog)
(tc_declprog emptyenv (valdecl (bind (_ x _*) tint) (eint 42))
 (bindval (bind (_ x _*) tint))
 (tc_valdecl emptyenv (_ x _*) tint (eint 42) (tc_eint emptyenv 42)))
(subenv_evidence (joinenv emptyenv (bindval (bind (_ x _*) tint)))
 (joinenv emptyenv (bindval (bind (_ x _*) tint))))
(tc_joinprog (joinenv emptyenv (bindval (bind (_ x _*) tint)))
 (bindval (bind (_ y _*) tint))
 (joinenv (joinenv emptyenv (bindval (bind (_ x _*) tint)))
  (bindval (bind (_ y _*) tint)))
 emptyenv
 (joinprog (declprog (valdecl (bind (_ y _*) tint) (eint 42))) emptyprog)
 emptyprog
 (tc_declprog (joinenv emptyenv (bindval (bind (_ x _*) tint)))
  (valdecl (bind (_ y _*) tint) (eint 42)) (bindval (bind (_ y _*) tint))
  (tc_valdecl (joinenv emptyenv (bindval (bind (_ x _*) tint))) (_ y _*) tint
   (eint 42) (tc_eint (joinenv emptyenv (bindval (bind (_ x _*) tint))) 42)))
 (subenv_evidence
  (joinenv (joinenv emptyenv (bindval (bind (_ x _*) tint)))
   (bindval (bind (_ y _*) tint)))
  (joinenv (joinenv emptyenv (bindval (bind (_ x _*) tint)))
   (bindval (bind (_ y _*) tint))))
 (tc_emptyprog
  (joinenv (joinenv emptyenv (bindval (bind (_ x _*) tint)))
   (bindval (bind (_ y _*) tint)))))

