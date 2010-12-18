
type variable = string
type constant = string

type env = NLF_env.t

type kind = 
  | Kind of env

type fam = 
  | Fam of env * fhead

type obj =
  | Obj of env * ohead * fhead

type fhead =
  | FVar of variable * env
  | FConst of constant * env

type ohead =
  | OVar of variable * env
  | Oconst of constant * env

