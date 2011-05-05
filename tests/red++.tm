%([x: {a:o} o] x) ([y:o] y)
% is eta-expanded to:
% [a:o] ([x:{a:o} o] [a:o] x [a=a]) [x=[y:o] y] [a=a]


[a:o] ([x:{c:o} o] [b:o] x b) ([c:o] c) a
