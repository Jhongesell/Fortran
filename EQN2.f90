Program EQN2
  implicit none
  REAL::A, B, C, X1, X2, X, D
  COMPLEX::Y1, Y2
  write(*, *) "Dame los coeficientes de la ecuación de la forma Ax^2+Bx+C=0"
  read(*, *) A, B, C
  D=B**2-4.0*A*C
  if (D<0) then
     Y1=COMPLEX(-B/(2.0*A), D/(2.0*A))
     Y2=COMPLEX(-B/(2.0*A), -D/(2.0*A))
     write(*, *) "Raíces complejas"
     write(*, *) "X1=", Y1
     write(*, *) "X2=", Y2     
  else if (D==0) then
     X=-B/(2.0*A)
     write(*, *) "Raíces repetidas"
     write(*, *) "X1=X2=", X
  else
     X1=(-B+D)/(2.0*A)
     X2=(-B-D)/(2.0*A)
     write(*, *) "Raíces reales diferentes"
     write(*, *) "X1=", X1
     write(*, *) "X2=", X2
  end if
end Program EQN2

