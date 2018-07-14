!Autor: Claudio Iván Esparza Castañeda
!Título: Pi
!Descripción: Programa que calcula Pi por medio de la serie de Leibinz
!Fecha: 13/07/2018

Program CPi !Inicio del programa
  implicit none !Quitar variables implícitas
  REAL*16::pi, s !Declaración de variables reales de 16 bits
  INTEGER::i !Variable entera iterativa
  s=0.0 !inicializar memoria auxiliar
  do i=0, 1000000000 !Ciclo que contiene a la serie
     s=(-1.0)**i/(2.0*i+1.0)+s !Forma matemática de la serie
  end do 
  pi=4.0*s !Valor de pi igual a cuatro veces el valor de la serie
  write(*, *) pi !Imprimir pi
end Program CPi !Finalizar programa

