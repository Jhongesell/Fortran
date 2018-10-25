!Autor: Claudio Iván Esparza Castañeda
!Título: Punto fijo
!Descripción: Este programa permite encontrar las raíces de una función cerca de un punto y graficarlo funciones del tipo y=f(x)
!Fecha: 15/07/2018

Program fijo
  implicit none
  REAL::e, er, x, x0, xe, f, g
  INTEGER::i
  CALL graf1()
  WRITE(*, *) "Valor inicial" !Preguntar valor inicial
  READ(*, *) x0 !Ingresar valor inicial
  WRITE(*, *) "Error esperado" !Preguntar el error
  READ(*, *) er !Ingresar el error
  x=x0 !Inicializar x
  i=0 !Inicializar contador de ciclos
  do
     i=i+1 !Añadir uno al contador
     xe=x !Asignar x anterior
     x=g(x)
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
end Program fijo

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
  WRITE(2, *) "set title 'y=x²-5x+3'"
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
  WRITE(2, *) "set title 'y=x²-5x+3'"
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

function f(x) !Función original
  implicit none
  REAL::f, x
  f=x**2-5*x+3
end function f

function g(x) !Función con despeje
  implicit none
  REAL::g, x
  g=sqrt(5*x-3)
end function g



