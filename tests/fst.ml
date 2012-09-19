#use "load.ml"
;;

let repo = Version.init
<:sign<

o : type.
a : o.
b : o.
c : o.
f : o -> o.
g : o -> o -> o.

commit : o -> Commit.

>>
;;

let repo = Tests.commit repo <<
commit (
  f (f (g a (f b)))
)
>>
;;

let repo = Tests.commit repo <<
commit (
  g ?aod02uirca a
)
>>
;;

let repo = Tests.commit repo <<
  commit (g ?iigw5x5yd6uw (g ?iigw5x5yd6uw ?cvh7m23nguzb))
>>
;;

42
;;
