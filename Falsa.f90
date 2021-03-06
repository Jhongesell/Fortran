!Autor: Claudio Iván Esparza Castañeda
!Título: Regla falsa
!Descripción: Este programa permite encontrar las raíces de una función cerca de un punto por medio del método de la regla falsa y graficarlo funciones del tipo y=f(x)
!Fecha: 16/07/2018

Program false
  implicit none
  REAL::e, er, a, b, x, y, f, xe
  INTEGER::i
  CALL graf1()
  WRITE(*, *) "Inicio del intervalo" !Preguntar inicio del intervalo
  READ(*, *) a !Ingresar inicio del intervalo
  WRITE(*, *) "Final del intervalo" !Preguntar final del intervalo
  READ(*, *) b !Ingresar final del intervalo
  WRITE(*, *) "Error esperado" !Preguntar el error
  READ(*, *) er !Ingresar el error
  x=0.0 !Inicializar x
  i=0 !Inicializar contador de ciclos
  do
     i=i+1 !Añadir uno al contador
     xe=x !Asignar x anterior
     x=b-f(b)*(a-b)/(f(a)-f(b)) !Partición del intervalo
     if (x .NE. 0) then !Validación del intervalo
        e=abs((x-xe)/x)*100 !Cálculo del error
     end if
     y=f(a)*f(x) !Evaluación de la función
     if (y<0) then !Verificar si cambió de positivo a negativo
        b=x
     elseif (y>0) then !Verificar si cambió de negativo a positivo
        a=x
     else
        e=0
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
end Program falsa

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


