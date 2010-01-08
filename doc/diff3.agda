module diff2 where

infixr 20 _#_

data Type : Set where
  _#_ : Type -> Type -> Type
  A : Type

infixl 20 _::_

data Env : Set where
  [] : Env
  _::_ : Type → Env → Env

data _⊢?_ : Env → Type → Set where
  o : {E : Env} {a : Type} → a :: E ⊢? a
  s : {E : Env} {a b : Type} → E ⊢? a → b :: E ⊢? a

infix 10 _⊢_
infixl 20 _`_
infixl 20 L_

data _⊢_ : Env → Type → Set where
  _`_ : {E : Env} {a b : Type} → 

    E ⊢ a # b →    E ⊢ a →
    --------------------
            E ⊢ b

  V : {E : Env} {a : Type} → 

    E ⊢? a → 
    --------
    E ⊢ a

  L_ : {E : Env} {a b : Type} → 

    a :: E ⊢ b → 
    ----------
    E ⊢ a # b 

church1 : (a : Type) → [] ⊢ (a # a) # a # a
church1 _ = L L (V (s o) ` V o)

data _==_ {A : Set} (x : A) : A → Set where
  refl : x == x

infixr 10 _,_

data Ctx : Set where
  [] : Ctx
  _,_ : {E : Env} {a : Type} → E ⊢ a → Ctx → Ctx

infix 8 _∈_

data _∈_ : ∀ {E a} → E ⊢ a → Ctx → Set where
  heq : ∀ {E a Δ} → {t : E ⊢ a} → 
    t ∈ t , Δ
  seq : ∀ {Δ E F a b} → {t : E ⊢ a} → {u : F ⊢ b} →
    t ∈ Δ → t ∈ u , Δ

_⊂_ : Ctx → Ctx → Set
E ⊂ F = ∀ {Γ a} → (t : Γ ⊢ a) → t ∈ E → t ∈ F

mutual
  infix 8 _⊧_⇒_ _⊧_

  data _⊧_⇒_ (Δ : Ctx) : ∀ {Γ₁ Γ₂ a b} → Γ₁ ⊢ a → Γ₂ ⊢ b → Set where

    id : ∀ {E a} → {t : E ⊢ a} → 

      ---------
      Δ ⊧ t ⇒ t

    unload : ∀ {E a F b} → {t : E ⊢ a} → {u : F ⊢ b} →

      t , Δ ⊧ u → 
      -------------------
      Δ ⊧ t ⇒ u

    app_e1 : ∀ {E a b F c} → {t : E ⊢ a # b} {u : E ⊢ a} {v : F ⊢ c} → 

      u , Δ ⊧ t ⇒ v →
      -------------
      Δ ⊧ t ` u ⇒ v

    app_i1 : ∀ {E a b F c} → {t : E ⊢ a # b} {u : E ⊢ a} {v : F ⊢ c} → 

      Δ ⊧ t ` u ⇒ v →      Δ ⊧ t → 
      --------------------------
               Δ ⊧ u ⇒ v

    app_i2 : ∀ {E a b F c} → {t : E ⊢ a # b} {u : E ⊢ a} {v : F ⊢ c} → 

      Δ ⊧ t ` u ⇒ v →       Δ ⊧ u →
      ---------------------------
               Δ ⊧ t ⇒ v


    lam_i : ∀ {E F a b c} → {t : a :: E ⊢ b} {u : F ⊢ c} →

      Δ ⊧ L t ⇒ u →
      -----------
      Δ ⊧ t ⇒ u

    lam_e : ∀ {E F a b c} → {t : a :: E ⊢ b} {u : F ⊢ c} →

      Δ ⊧ t ⇒ u →
      -----------
      Δ ⊧ L t ⇒ u

  data _⊧_ : (Δ : Ctx) → ∀ {E a} → E ⊢ a → Set  where
    
    load : ∀ {Δ E a F b} → {t : E ⊢ a} {u : F ⊢ b} →

      t ∈ Δ →    Δ ⊧ t ⇒ u →
      --------------------
              Δ ⊧ u

    var : ∀ {Δ E a} → {v : E ⊢? a} →

      -----------
      Δ ⊧ V v

    app : ∀ {Δ E a b} → {t : E ⊢ a # b} {u : E ⊢ a} →

      Δ ⊧ t →    Δ ⊧ u →
      ----------------
          Δ ⊧ t ` u

    lam : ∀ {Δ E a b} → {t : b :: E ⊢ a} →

      Δ ⊧ t →
      --------
      Δ ⊧ L t

eta : ∀ {Δ E a b} → {t : a :: E ⊢ a # b} →
  Δ ⊧ L (t ` V o) ⇒ t
eta = lam_e (app_e1 id)

-- TODO
postulate
  weak : ∀ {Δ₁ Δ₂ E F a b} → {t : E ⊢ a} → {u : F ⊢ b} →
    Δ₁ ⊧ t ⇒ u →      Δ₁ ⊂ Δ₂ →
    -------------------------
            Δ₂ ⊧ t ⇒ u

weak1 : ∀ {Δ E F a b} → {u : E ⊢ a} → (t : F ⊢ b) → 
  t ∈ Δ → 
  t ∈ u , Δ
weak1 _ heq = seq heq
weak1 _ (seq H) = seq (seq H)

weak2 : ∀ {Δ G c E F a b} → {u : E ⊢ a} → {v : F ⊢ b} → (t : G ⊢ c) → 
  t ∈ u , Δ → 
  t ∈ u , v , Δ
weak2 _ heq = heq
weak2 _ (seq H) = seq (seq H)

app_e2 : ∀ {Δ E a b F c} → {t : E ⊢ a # b} {u : E ⊢ a} {v : F ⊢ c} → 
  t , Δ ⊧ u ⇒ v →
  -------------
  Δ ⊧ t ` u ⇒ v
app_e2 H = app_e1 (unload (load (seq heq) (weak H weak2)))

mutual
  concat : ∀ {Δ G c E F a b} → {u : E ⊢ a} → {v : F ⊢ b} → {t : G ⊢ c} → 
    Δ ⊧ t ⇒ u →      Δ ⊧ u ⇒ v →
    --------------------------
    Δ ⊧ t ⇒ v
  concat id b = b
  concat (unload a) b = unload (load _ (weak b weak1))
  concat (app_i1 a u) b = app_i1 (concat a b) u
  concat (app_i2 a t) b = app_i2 (concat a b) t
  concat (app_e1 a) b = app_e1 (concat a (weak b weak1))
  concat (lam_i a) b = lam_i (concat a b)
  concat (lam_e a) b = lam_e (concat a b)


replace : ∀ {Δ E a} → (u : E ⊢ a) →
  Δ ⊧ u
replace (V v) = var
replace (t ` u) = app (replace t) (replace u)
replace (L t) = lam (replace t)

replace' : ∀ {Δ E F a b} → {u : E ⊢ a} → (v : F ⊢ b) →
  Δ  ⊧  u ⇒ v
replace' v = unload (replace v)

fab'fba : ∀ {Δ E a b} → {f : E ⊢ a # a # b} → {u v : E ⊢ a} →
  Δ  ⊧  f ` u ` v  ⇒  f ` v ` u
fab'fba = app_e1 (app_e1 (app_i2 (app_i2 id (load heq id)) (load (seq heq) id)))

K : ∀ {E a b} → E ⊢ a # b # a
K = L L V (s o)

S : ∀ {E a b c} → E ⊢ (a # b # c) # (a # b) # a # c
S = L L L (V (s (s o)) ` V o ` (V (s o) ` V o))

SKKt'St : ∀ {E a} → {t : E ⊢ a} →
  [] ⊧ S ` K ` K {b = a} ` t ⇒ t
SKKt'St = app_e2 id

