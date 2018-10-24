!Autor: Claudio Iván Esparza Castañeda
!Título: Newton-Raphson simple
!Descripción: Programa que calcula las raíces de una función por medio del método de Newton-Raphson. Versión modificada para raíces múltiples
!Fecha: 23/10/2018
  

Program newrap
  implicit none
  REAL::e, er, x, y, f, fp, fpp, xe
  INTEGER::i
  CALL graf1()
  WRITE(*, *) "Valor inicial" !Preguntar final del intervalo
  READ(*, *) x !Ingresar final del intervalo
  WRITE(*, *) "Error esperado" !Preguntar el error
  READ(*, *) er !Ingresar el error
  i=0 !Inicializar contador de ciclos
  do
     i=i+1 !Añadir uno al contador
     xe=x !Asignar x anterior
     x=x-f(x)*fp(x)/(fp(x)**2-f(x)*fpp(x))
     if (x .NE. 0) then !Validación del intervalo
        e=abs((x-xe)/x)*100 !Cálculo del error
     end if
     if (e<er) then !Validar si el error es menor al esperado
        EXIT
     end if
  end do
  write(*, *) "       x                 f(x)            e               i" !Cabecera del resultado
  write(*, *) x, f(x), e, i !Imprimir resultado
  open(unit=3, file="Punto.dat", status="unknown") !Archivo para graficar el punto de intersección
  write(3, *) x, f(x) !Escribir en archivo
  close(3)
  CALL graf2()
end Program newrap

subroutine graf1() !subrutina que grafica
  implicit none !Quitar variables implícitas
  REAL::x, y, f, h, a, b !Variables de tipo real
  INTEGER::N, i !Variables de tipo entero
  open(unit=1, file="Datos.dat", status="unknown") !Abrir archivo para guardar puntos
  open(unit=2, file="Graf.plt", status="unknown") !Abrir archivo para guardar información de la gŕafica
  WRITE(*, *) "Inicio de la gráfica" !Preguntar inicio de gráfica
  READ(*, *) a !Ingresar inicio de la gráfica
  WRITE(*, *) "Final de la gráfica" !Preguntar final de la gráfica
  READ(*, *) b !Ingresar final de la gráfica
  WRITE(*, *) "¿Con cuántos puntos?" !Preguntar número de puntos
  READ(*, *) N !Ingresar número de puntos
  h=(b-a)/N !Tamaño de paso
  do i=0, N !Construir datos
     x=a+i*h 
     y=f(x)
     WRITE(1, *) x, y !Escribir datos en archivo
  end do
  close(1)
  !Establecer todos los parámetros de la gráfica
  WRITE(2, *) "set title 'y=sin(x)-cos(x)'"
  WRITE(2, *) "set xlabel 'x'"
  WRITE(2, *) "set ylabel 'y'"
  WRITE(2, *) "m='./Datos.dat'"
  WRITE(2, *) "set nokey"
  WRITE(2, *) "set grid"
  WRITE(2, *) "p m w l"
  WRITE(2, *) "replot"
  close(2)
  CALL SYSTEM("gnuplot -p Graf.plt") !Llamar a GNUPlot desde la terminal
end subroutine graf1

subroutine graf2() !subrutina que grafica
  implicit none !Quitar variables implícitas
  REAL::x, y, f, h, a, b !Variables de tipo real
  INTEGER::N, i !Variables de tipo entero
  open(unit=1, file="Datos.dat", status="unknown") !Abrir archivo para guardar puntos
  open(unit=2, file="Graf.plt", status="unknown") !Abrir archivo para guardar información de la gŕafica
  WRITE(*, *) "Inicio de la gráfica" !Preguntar inicio de gráfica
  READ(*, *) a !Ingresar inicio de la gráfica
  WRITE(*, *) "Final de la gráfica" !Preguntar final de la gráfica
  READ(*, *) b !Ingresar final de la gráfica
  WRITE(*, *) "¿Con cuántos puntos?" !Preguntar número de puntos
  READ(*, *) N !Ingresar número de puntos
  h=(b-a)/N !Tamaño de paso
  do i=0, N !Construir datos
     x=a+i*h 
     y=f(x)
     WRITE(1, *) x, y !Escribir datos en archivo
  end do
  close(1)
  !Establecer todos los parámetros de la gráfica
  WRITE(2, *) "set title 'y=sin(x)-cos(x)'"
  WRITE(2, *) "set xlabel 'x'"
  WRITE(2, *) "set ylabel 'y'"
  WRITE(2, *) "m='./Datos.dat'"
  WRITE(2, *) "n='./Punto.dat'"
  WRITE(2, *) "set nokey"
  WRITE(2, *) "set grid"
  WRITE(2, *) "p m w l, n"
  WRITE(2, *) "set term png"
  WRITE(2, *) "set out 'Graf.png'"
  WRITE(2, *) "replot"
  close(2)
  CALL SYSTEM("gnuplot -p Graf.plt") !Llamar a GNUPlot desde la terminal
end subroutine graf2

function f(x) !Función que genera los puntos
  implicit none
  REAL::f, x
  f=sin(x)-cos(x)
end function f

function fp(x) !Función derivada
  implicit none
  REAL::fp, x
  fp=cos(x)+sin(x)
end function fp

function fpp(x) !Función doble derivada
  implicit none
  REAL::fpp, x
  fpp=-sin(x)+cos(x)
end function fpp

