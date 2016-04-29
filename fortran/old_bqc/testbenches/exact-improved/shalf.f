#include "eigen.f"
#include "matrix.f"
      subroutine shalf (S, U, W, m)
      implicit double precision (a-h,o-z)
      double precision S(*), U(*), W(*)
      integer m
C
      data crit, one/1.0D-10,1.0D+00/
      call eigen(S,U,m)
C.....Transpose the eigenvalues of S for convenience
      do i = 1, m
         do j =1 , i
            ij=m*(j-1)+i ; ji=m*(i-1)+j ; d=U(ij)
            U(ij) = U(ji) ; U(ji) = d
         end do
      end do
C.....Get the inverse root of the eigenvalues
      do i = 1, m
         ii = (i-1)*m+i
         if (S(ii).LT.crit) then
             write(OUTPUT_UNIT, 200)
             STOP
         end if
         S(ii) = one/dsqrt(S(ii))
      end do
      call gtprd(U, S, W, m, m, m)
      call gmprd(W, U, S, m, m, m)
C
      return
 200  format(" Basis is linearly deoendent; S is singular! ")
      end
