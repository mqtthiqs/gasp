
all-i                           % true (forall [x] imp (eq 0 x) (eq 0 x))
  ([x] eq O x)                  % i -> o
  [x]
    imp-i                       % true (imp (eq 0 x) (eq 0 x))
      (eq O x)                  % o
      (eq O x)                  % o
        [H]
          H                     % true (eq 0 x)
