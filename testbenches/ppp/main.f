#include "head.f"
#include "scfR.f"
#include "scfGR.f"
#include "matrix.f"
#include "eigen.f"
      program LCAOMOSCF
      double precision HF(400), R(400)
      double precision Cbar(400), epsilon(7), Rold(400)
      double precision E, zero, Rsum, term, crit
      double precision pppone
C
      integer m, n, i, nfile, icon, mm, kount
      double precision xy(20,2)
      common /xydata/xy,m
      data zero/0.0D+00/, crit/1.0D-06/
C.....special data for H2O molecule
      OPEN(UNIT=INPUT_UNIT,FILE='butadiene.dat',ACCESS='SEQUENTIAL')
      READ(INPUT_UNIT,*) m, n
      READ(INPUT_UNIT,*) (xy(i,1), xy(i,2), i=1,m)
      mm = m*m
C
C.....set initial R to zero, i.e. start from H not HF
      do i = 1, mm
         R(i) = zero; Rold(i) = zero
      enddo
      kount = 0
      icon = 100
C
C.....assume that the first iteration will change R to be non-zero!
      do while ((icon.NE.0).AND.(kount.LT.MAX_ITERATIONS))
         kount = kount + 1
         E = zero; icon = 0
C
C........compute the one-electron Hamiltonian each time
         do i = 1, m
            do j = 1, i
               ij= m*(j-1) + i; ji = m*(i-1) + j
               val = pppone(xy, m, i, j)
               HF(ij) = val; HF(ji) = val
            end do
         end do
C
         do i = 1, mm
            E = E + R(i)*HF(i)
         enddo
         call scfGR(R, HF, m, nfile)
         do i = 1, mm
            E = E + R(i)*HF(i)
         enddo   
C
         write(OUTPUT_UNIT, 200) E
C
         call eigen(HF, Cbar, m)
         do i = 1, m
            epsilon(i) = HF(m*(i-1)+i)
         enddo
         call scfR(Cbar, R, m, n)
         Rsum = zero
         do i = 1, mm
            term = dabs(R(i) - Rold(i))
            Rold(i) = R(i)
            if (term.GT.crit) icon = icon + 1
            Rsum = Rsum + term
         enddo
         write(OUTPUT_UNIT, 201) Rsum, icon
      enddo
C
      if ((kount.EQ.MAX_ITERATIONS).AND.(icon.NE.0)) then
          write(OUTPUT_UNIT, 204)
      else
         write(OUTPUT_UNIT, 202) kount
         write(OUTPUT_UNIT, 203) (epsilon(i), i=1, n)
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
