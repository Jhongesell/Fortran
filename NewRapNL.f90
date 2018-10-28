!Autor: Claudio Iván Esparza Castañeda
!Título: Newton-Raphson para sistema de ecuaciones
!Descripción: Programa que calcula las raíces de una sistema de ecuaciones no lineales
!Fecha: 27/10/2018
  

Program newrap
  implicit none
  REAL::e, er, x, y, xe, u, v, pux, puy, pvx, pvy, J
  INTEGER::i
  WRITE(*, *) "Valor inicial de x" !Preguntar final del intervalo
  READ(*, *) x !Ingresar final del intervalo
  WRITE(*, *) "Valor inicial de y" !Preguntar final del intervalo
  READ(*, *) y !Ingresar final del intervalo
  WRITE(*, *) "Error esperado" !Preguntar el error
  READ(*, *) er !Ingresar el error
  i=0 !Inicializar contador de ciclos
  do
     i=i+1 !Añadir uno al contador
     xe=x !Asignar x anterior
     J=pux(x, y)*pvy(x, y)-puy(x, y)*pvx(x, y)
     x=x-(u(x, y)*pvy(x, y)-v(x, y)*puy(x, y))/J
     y=y-(v(x, y)*pux(x, y)-u(x, y)*pvx(x, y))/J
     if (x .NE. 0) then !Validación del intervalo
        e=abs((x-xe)/x)*100 !Cálculo del error
     end if
     if (e<er) then !Validar si el error es menor al esperado
        EXIT
     end if
  end do
  write(*, *) "          i        x                y               e" !Cabecera del resultado
  write(*, *) i, x, y, e!Imprimir resultado
  close(3)
end Program newrap

function u(x, y) !Primera ecuación
  implicit none
  REAL::x, y, u
  u=x**2+x*y-10
end function u

function pux(x, y) !Derivada parcial de u con respecto a x
  implicit none
  REAL::x, y, pux
  pux=2*x+y
end function pux

function puy(x, y) !Derivada parcial de u con respecto a y
  implicit none
  REAL::x, y, puy
  puy=x
end function puy

function v(x, y) !Segunda ecuación
  implicit none
  REAL::x, y, v
  v=y+3*x*y**2-57
end function v

function pvx(x, y) !Derivada parcial de v con respecto a x
  implicit none
  REAL::x, y, pvx
  pvx=3*x**2
end function pvx

function pvy(x, y) !Derivada parcial de v con respecto a y
  implicit none
  REAL::x, y, pvy
  pvy=1+6*x*y
end function pvy




