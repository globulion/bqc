C      subroutine scfGR (R, G, m, nfile)
C      return
C      end

      subroutine scfR (C, R, m, nocc)
      double precision C(MATRIX_SIZE), R(MATRIX_SIZE)
      integer m, nocc
C
      double precision suma, zero
      integer i, j, k, ij, ji, kk, ik, jk
      data zero/0.0D+00/
C
      do i = 1, m
         do j = 1, i
            suma = zero
            do k = 1, nocc
               kk = m*(k-1)
               ik = kk + i
               jk = kk + j
               suma = suma + C(ik)*C(jk)
            enddo
            ij = m*(j-1) + i
            ji = m*(i-1) + j
            R(ij) = suma
            R(ji) = suma
         enddo
      enddo
C
      return
      end
