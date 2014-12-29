      program c
      integer i, j, k, l 
      read(*,*) i
      select case (i)
         case (1)
           j = 10; k = 10; l = -10
         case (2)
           j= 12; k = 14; k = -1344
         case default
           j=i; k=i; l=i
      end select
      write(*,*) i,j,k,l
      STOP
      end
