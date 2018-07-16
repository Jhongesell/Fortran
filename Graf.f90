!Autor: Claudio Iván Esparza Castañeda
!Título: Gráfica
!Descripción: Este programa permite graficar funciones del tipo y=f(x)
!Fecha: 15/07/2018
  
Program graf !Inicio del programa
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
  WRITE(2, *) "set term png"
  WRITE(2, *) "set out 'Graf.png'"
  WRITE(2, *) "replot"
  close(2)
  CALL SYSTEM("gnuplot -p Graf.plt") !Llamar a GNUPlot desde la terminal
end Program graf

function f(x) !Función que genera los puntos
  implicit none
  REAL::f, x
  f=sin(x)-cos(x)
end function f


