commit
  (forall [x] imp (eq O x) (eq O x))
  (all-i                           % true (forall [x] imp (eq O x) (eq O x))
    ([x] eq O x)                  % i -> o
    [x]
      imp-i                       % true (imp (eq O x) (eq O x))
        (eq O x)                  % o
        (eq O x)                  % o
          [H]
            H                     % true (eq O x)
  )
