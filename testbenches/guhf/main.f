#include "head.f"
#include "scfR.f"
#include "scfGR.f"
#include "matrix.f"
#include "eigen.f"
#include "spinor.f"
#include "getint.f"
#include "beigen.f"
C      subroutine LCAOMOSCF(H, V, HF, C, R, E)
      program LCAOMOSCF
      double precision H(4000), HF(4000), C(4000), V(4000), R(4000)
      double precision Cbar(4000), epsilon(4000), Rold(4000)
      double precision E, zero, Rsum, term, crit, half
      double precision lambda, turm
      integer m, n, i, nfile, icon, mm, kount, mby2
      integer interp
c      integer iblock(2)
      data zero/0.0D+00/, crit/1.0D-06/, half/0.5D+00/
C.....SCF interpolation options
      interp = 123
      lambda = 0.00D+00
C.....orthoginalizing matrix
      data V/
     &  1.02421, -0.14351, -0.01030,  0.00000,  0.00000,  0.02205,
     &  0.02205,
     & -0.14351,  1.24886,  0.11273,  0.00000,  0.00000, -0.29740, 
     & -0.29740,
     & -0.01030,  0.11273,  1.05600,  0.00000,  0.00000, -0.14822, 
     & -0.14822,
     &  0.00000,  0.00000,  0.00000,  1.11451,  0.00000, -0.23271,  
     &  0.23271,
     &  0.00000,  0.00000,  0.00000,  0.00000,  1.00000,  0.00000,  
     &  0.00000,
     &  0.02205, -0.29740, -0.14822, -0.23271,  0.00000,  1.21199, 
     & -0.09117,
     &  0.02205, -0.29740, -0.14822,  0.23271,  0.00000, -0.09117,  
     &  1.21199, 3951*0.0000/
C.....one-electron integrals matrix
      data H/
     &-32.72233, -7.61313, -0.01906,  0.00000,  0.00000, -1.75465,
     & -1.75465,
     & -7.61313, -9.33609, -0.22404,  0.00000,  0.00000, -3.74917,
     & -3.74917,
     & -0.01906, -0.22404, -7.55265,  0.00000,  0.00000, -1.64732,
     & -1.64732,
     &  0.00000,  0.00000,  0.00000, -7.61542,  0.00000, -2.03290,
     &  2.03290,
     &  0.00000,  0.00000,  0.00000,  0.00000, -7.45869,  0.00000,
     &  0.00000,
     & -1.75465, -3.74917, -1.64732, -2.03290,  0.00000, -5.08179,
     & -1.61434,
     & -1.75465, -3.74917, -1.64732,  2.03290,  0.00000, -1.61434,
     & -5.08179, 3951*0.0000/
C.....special data for H2O molecule
      n    =10
      mby2 = 7
C
      m = mby2 + mby2
      mm = m*m
c      iblock(1) = mby2
c      iblock(2) = mby2
C
      call spinor(H, mby2)
      call spinor(V, mby2)
C
C.....set initial R to zero, i.e. start from H not HF
      do i = 1, mm
         R(i) = zero; Rold(i) = zero
         C(i) = V(i)
      enddo
      kount = 0
      icon = 100
C
C.....assume that the first iteration will change R to be non-zero!
      do while ((icon.NE.0).AND.(kount.LT.MAX_ITERATIONS))
         kount = kount + 1
         E = zero; icon = 0
         do i = 1, mm
            HF(i) = H(i)
            E = E + R(i)*HF(i)
         enddo
         call scfGR(R, HF, m, nfile)
         do i = 1, mm
            E = E + R(i)*HF(i)
         enddo   
         E = E * half
C
         write(OUTPUT_UNIT, 200) E
C
         call gtprd(C, HF, R, m, m, m)
         call gmprd(R, C, HF, m, m, m)
         call eigen(HF, Cbar, m)
c         call beigen(HF, Cbar, m, 0, 2, iblock)
         do i = 1, m
            epsilon(i) = HF(m*(i-1)+i)
         enddo
         call gmprd(C, Cbar, V, m, m, m)
         call scfR(V, R, m, n)
         Rsum = zero
         do i = 1, mm
            turm = R(i) - Rold(i)
            term = dabs(turm)
            Rold(i) = R(i)
            C(i) = V(i)
            if (term.GT.crit) icon = icon + 1
            Rsum = Rsum + term
            if (kount.LT.interp) R(i) = R(i) - lambda*turm
         enddo
         write(OUTPUT_UNIT, 201) Rsum, icon
      enddo
C
      if ((kount.EQ.MAX_ITERATIONS).AND.(icon.NE.0)) then
          write(OUTPUT_UNIT, 204)
      else
         write(OUTPUT_UNIT, 202) kount
         write(OUTPUT_UNIT, 203) (epsilon(i), i=1, m)
      endif
C
 200  format(" Current Electronic Energy = ", f12.6)
 201  format(" Convergence in R = ", f12.5,i6, "  Changing")
 202  format(" SCF converged in", i4, " iterations")
 203  format(" Orbital Energies ", (7f10.5))
 204  format(" SCF did not converged... quitting")
C
      STOP
      end
