C subroutines here: 
C gtprd, gmprd
C
      subroutine gtprd (A, B, R, n, m, l)
      double precision A(MATRIX_SIZE), B(MATRIX_SIZE)
      double precision R(MATRIX_SIZE)
      integer n, m, l
C
      double precision zero
      integer k, ik, j, ir, ij, ib
      data zero/0.0D+00/
C.....stride counters initialization
      ir = 0; ik = -n
      do k = 1, l
         ij = 0
         ik = ik + m
         do j = 1, m
            ir = ir + 1; ib = ik
            R(ir) = zero
            do i = 1, n
               ij = ij + 1; ib = ib + 1
               R(ir) = R(ir) + A(ij)*B(ib)
            enddo
         enddo
      enddo
C
      return
      end

      subroutine gmprd (A, B, R, n, m, l)
C>\brief Some happy happy documentation
      double precision A(MATRIX_SIZE), B(MATRIX_SIZE)
      double precision R(MATRIX_SIZE)
      integer n, m, l
C
      double precision zero
      integer k, ik, j, ir, ji, ib
      data zero/0.0D+00/
C.....stride counters initialization
      ir = 0; ik = -m
      do k = 1, l
         ik = ik + m
         do j = 1, n
            ir = ir + 1; ji = j - n; ib = ik
            R(ir) = zero
            do i = 1, m
               ji = ji + n; ib = ib + 1
               R(ir) = R(ir) + A(ji)*B(ib)
            enddo
         enddo
      enddo
C
      return
      end

