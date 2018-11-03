!Autor: Claudio Iván Esparza Castañeda
!Título: División sintética
!Descripción: Programa que se encarga de dividir un polinomio entre un binomio por el método de la división sintética
!Fecha: 31/10/2018

program DivSint !Inicio del programa
  implicit none !Sin variables implícitas
  REAL, allocatable, dimension(:)::a, b !Vector tipo REAL con dimensión dinámica
  REAL::p, d, x, r, s, t !Varibles tipo REAL
  INTEGER::i, n, m !Variables enteras
  d=0.0 !Inicializar valor de derivada
  p=0.0 !Inicializar valor de polinomio
  WRITE(*, *) "Grado del polinomio" !Preguntar por el grado del polinomio
  READ(*, *) n
  allocate(a(0:n), b(0:n-1)) !Asignar valor a variable dinámica
  WRITE(*, *) "Coeficientes del polinomio a0+a1x+a2x²+...anx^n" !Pregunta coeficientes del polinomio
  do i=0, n 
     READ(*, *) a(i)
  end do
  WRITE(*, *) "t de (x-t) para dividir el  polinomio" !Preguntar valor para evaluar polinomio
  READ(*, *) t
  r=a(n) !Salvar el n-ésimo término del polinomio
  a(n)=0 !Hacer 0 el n-ésimo término del polinomio
  do i=n-1, 0, -1 !Para cada término del polinomio de atrás para adelante
     s=a(i) !Salvar el i-ésimo término del polinomio
     a(i)=r  !Tomar el valor del término anterior
     r=s+r*t !Sumar el i-ésimo término al producto del término anterior y el divisor
     b(i)=r !Salvar en un vector resultado
  end do
  do i=n-1, 0, -1
     WRITE(*, *) b(i) !Valores del vector resultado
  end do
  deallocate(a, b) !Liberar memoria dinámica
end program DivSint
