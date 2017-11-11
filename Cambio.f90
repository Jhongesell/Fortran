Program hola
  implicit none
  INTEGER::a, b, c
  a=5
  b=6
  write(*, *) a, b
  c=b
  b=a
  a=c
  write(*, *) a, b
end Program hola
