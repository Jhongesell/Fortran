!Autor: Claudio Iván Esparza Castañeda
!Título: Chicharronera
!Descripción: Programa que calcula las raíces de una ecuación cuadrárica por medio de la fórmula general
!Fecha: 13/07/2018

Program EQN2 !Inicio del programa
  implicit none !Desactivar variables implícitas
  REAL::A, B, C, X1, X2, X, D !Declaración de variables reales
  COMPLEX::Y1, Y2 !Declaración de variables complejas
  write(*, *) "Dame los coeficientes de la ecuación de la forma Ax^2+Bx+C=0" !Pedir coeficientes
  read(*, *) A, B, C !Ingresar coeficientes
  D=B**2-4.0*A*C !Discriminante
  if (D<0) then !Validación del discriminante si es menor que cero
     Y1=COMPLEX(-B/(2.0*A), D/(2.0*A)) !Primera raíz compleja
     Y2=COMPLEX(-B/(2.0*A), -D/(2.0*A)) !Segunda raíz compleja
     write(*, *) "Raíces complejas" 
     write(*, *) "X1=", Y1
     write(*, *) "X2=", Y2     
  else if (D==0) then !Validación del discrimiante si es igual que cero
     X=-B/(2.0*A) !Raíces iguales
     write(*, *) "Raíces repetidas"
     write(*, *) "X1=X2=", X
  else !Validación del discriminante si es mayor que cero
     X1=(-B+D)/(2.0*A) 
     X2=(-B-D)/(2.0*A)
     write(*, *) "Raíces reales diferentes"
     write(*, *) "X1=", X1
     write(*, *) "X2=", X2
  end if
end Program EQN2 !Final del programa


