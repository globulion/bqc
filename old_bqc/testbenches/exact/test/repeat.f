      program repetition
      integer i, j, k, happy
      double precision val
      i = 1
      do while (i<4)
         write(*,*) "Give i= "
         read *, i
      end do
      i = 1; j = 2; k = 3
      do while (happy(i,j,k,val).EQ.0)
         write(*,*) i,j,k,val
      enddo
      stop
      end

      integer function happy (i,j,k,val)
      save
      integer i,j,k
      double precision val
      i = k+j
      k = i+j+2
      j = i
      val = DCOS(DFLOAT(i+j+k))
      if (i.GT.100) then
          happy = 1
          return
      endif
      happy = 0
      return
      end
