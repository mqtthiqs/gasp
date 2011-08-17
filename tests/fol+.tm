commit
  (forall [x : i] imp (eq O x) (eq O x))
  (all-i                           % true (forall [x] imp (eq O x) (eq O x))
    ([x : i] (imp (eq O x) (eq O x)))                  % i -> o
    [x : i]
      imp-i                       % true (imp (eq O x) (eq O x))
        (eq O x)                  % o
        (eq O x)                  % o
          ([H : true (eq O x)]
            H)                     % true (eq O x)
  )
