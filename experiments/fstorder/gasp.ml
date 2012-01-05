let sign = <:sign<
  tree : type.
  leaf : tree.
  node : tree -> tree -> tree.
>>

let repo = Slicer.commit (Repo.init Slicer.prelude)
  <:obj<
    node leaf leaf
  >>
