commit (

%tc-joinprog emptyenv (bindval (bind (- x -*) tint))
%(joinenv emptyenv (bindval (bind (- x -*) tint)))
%(joinenv (bindval (bind (- y -*) tint))
% (joinenv (joinenv emptyenv (bindval (bind (- x -*) tint)))
%  (bindval (bind (- y -*) tint))))
%(joinprog (declprog (valdecl (bind (- x -*) tint) (eint 42))) emptyprog)
%(joinprog (declprog (valdecl (bind (- y -*) tint) (eint 42))) emptyprog)
%(tc-declprog emptyenv (valdecl (bind (- x -*) tint) (eint 42))
% (bindval (bind (- x -*) tint))
% (tc-valdecl emptyenv (- x -*) tint (eint 42) (tc-eint emptyenv 42)))
% (subenv-evidence (joinenv emptyenv (bindval (bind (- x -*) tint)))
%(joinenv emptyenv
 (bindval (bind (- x -*) tint) emptyenv)
%)
%)
% (tc-joinprog (joinenv emptyenv (bindval (bind (- x -*) tint)))
%  (bindval (bind (- y -*) tint))
%  (joinenv (joinenv emptyenv (bindval (bind (- x -*) tint)))
%   (bindval (bind (- y -*) tint)))
%  emptyenv
%  (joinprog (declprog (valdecl (bind (- y -*) tint) (eint 42))) emptyprog)
%  emptyprog
%  (tc-declprog (joinenv emptyenv (bindval (bind (- x -*) tint)))
%   (valdecl (bind (- y -*) tint) (eint 42)) (bindval (bind (- y -*) tint))
%   (tc-valdecl (joinenv emptyenv (bindval (bind (- x -*) tint))) (- y -*) tint
%    (eint 42) (tc-eint (joinenv emptyenv (bindval (bind (- x -*) tint))) 42)))
%  (subenv-evidence
%   (joinenv (joinenv emptyenv (bindval (bind (- x -*) tint)))
%    (bindval (bind (- y -*) tint)))
%   (joinenv (joinenv emptyenv (bindval (bind (- x -*) tint)))
%    (bindval (bind (- y -*) tint))))
%  (tc-emptyprog
%   (joinenv (joinenv emptyenv (bindval (bind (- x -*) tint)))
%    (bindval (bind (- y -*) tint)))))


)
