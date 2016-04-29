#define INPUT_UNIT 11
      program reading
      double precision v(10000,3)
      integer m,n, i,j
      double precision u(3)
      OPEN(UNIT=INPUT_UNIT,FILE='inp',ACCESS='SEQUENTIAL')
      READ(INPUT_UNIT,*) m, n
      READ(INPUT_UNIT,*) (v(i,1), v(i,2), v(i,3),i=1,m)

      do i=1,m
      do j=1,3
         u(j) = v(i,j)
      end do
      write(*,200) (u(j),j=1,3)
      end do
 200  format((10F10.5))
      STOP 
      end
