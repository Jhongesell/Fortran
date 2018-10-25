!Autor: Claudio Iván Esparza Castañeda
!Título: Punto Fijo (Sistema)
!Descripción: Este programa permite encontrar las raíces de un sistema de ecuaciones no lineales por el método del punto fijo
!Fecha: 24/10/2018

Program bis
  implicit none
  REAL::e, er, x, y, x0, y0, xe, f, g
  INTEGER::i
  WRITE(*, *) "Valor inicial" !Preguntar valor inicial
  READ(*, *) x0 !Ingresar valor inicial
  WRITE(*, *) "Valor inicial" !Preguntar valor inicial
  READ(*, *) y0 !Ingresar valor inicial
  WRITE(*, *) "Error esperado" !Preguntar el error
  READ(*, *) er !Ingresar el error
  x=x0 !Inicializar x
  y=y0 !Inicializar y
  i=0 !Inicializar contador de ciclos
  do
     i=i+1 !Añadir uno al contador
     xe=x !Asignar x anterior
     x=f(x, y)
     y=g(x, y)
     if (x .NE. 0) then !Validación del intervalo
        e=abs((x-xe)/x)*100 !Cálculo del error
     end if
     if (e<er) then !Validar si el error es menor al esperado
        EXIT
     end if
  end do
  write(*, *) "       x              y                 e               i" !Cabecera del resultado
  write(*, *) x, y, e, i !Imprimir resultado
end Program bis

function f(x, y) !Función original
  implicit none
  REAL::f, x, y
  f=sqrt(10-x*y)
end function f

function g(x, y) !Función con despeje
  implicit none
  REAL::g, x, y
  g=sqrt((57-y)/(3*x))
end function g



