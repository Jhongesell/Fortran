Program CPi
  implicit none
  REAL*16::pi, s
  INTEGER::i
  s=0.0
  do i=0, 1000000000
     s=(-1.0)**i/(2.0*i+1.0)+s
  end do
  pi=4.0*s
  write(*, *) pi
end Program CPi

