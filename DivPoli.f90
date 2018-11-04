!Autor: Claudio Iván Esparza Castañeda
!Título: División de dos polinomios
!Descripción: Programa que se encarga de dividir un polinomio de grado mayor entre uno de grado menor
!Fecha: 03/11/2018

program DivPoli
  !Inicio del programa
  implicit none !Sin variables implícitas
  REAL, allocatable, dimension(:)::a, b, c, d, e, f !Vector tipo REAL con dimensión dinámica
  INTEGER::i, j, m, n !Variables enteras
  WRITE(*, *) "Grado del polinomio mayor" !Preguntar por el grado del polinomio
  READ(*, *) m
  WRITE(*, *) "Grado del polinomio menor" !Preguntar por el grado del polinomio
  READ(*, *) n
  allocate(a(0:m), b(0:m), c(0:m), d(0:m), e(0:m)) !Asignar valor a variables dinámica
  WRITE(*, *) "Coeficientes del polinomio mayor a0+a1x+a2x²+...amx^m" !Pregunta coeficientes del polinomio
  do i=0, m 
     READ(*, *) a(i)
  end do
  WRITE(*, *) "Coeficientes del polinomio menor b0+b1x+b2x²+...bnx^n, con n<m" !Pregunta coeficientes del polinomio
  do i=0, n 
     READ(*, *) b(i)
  end do
  do i=0, m !Respaldar los valores del vector mayor
     c(i)=a(i)
     d(i)=0
  end do
  do i=m-n, 0, -1 
     d(i+1)=c(n+i)/b(n) !Dividir los últimos elementos del polinomio mayor entre el último del polinomio menor
     do j=n+i-1, i, -1 
        c(j)=c(j)-d(i+1)*b(j-i) !A los demás términos restarles el producto de el mayor elemento del polinomio mayor y el i-ésimo elemento del polinomio menor
     end do
  end do
  do i=n, m !Hacer 0 los últimos términos del residuo
     c(i)=0
  end do
  do i=0, m-n !Vector solución
     e(i)=d(i+1)
  end do
  write(*, *) "División real [c0+c1x+c2x²+...c(m-n)x^(m-n)]" !Imprimir resultado
  do i=0, m-n
     write(*, 1) "C(", i, ")=", e(i)
  end do
  write(*, *) "Con residuo [r0+r1x+r2x²+...r(m-n-1)x^(m-n-1)]" !Imprimir residuo
  do i=0, m-n
     write(*, 1) "R(", i, ")=", c(i)
  end do
  1 format(' ', a2, i2, a2, f10.3) !Formato para imprimir el resultado
  deallocate(a, b, c, d, e) !Liberar memoria dinámica
end program DivPoli
