#include "happy/head.f"
      program loop
      integer i,v(3)
      data (v(i), i=1,3) /1,2,3/
      do 10 i = 1, 10
         write(*,*) i
         go to 10
         write(*,*) 0
10    continue
      do i=1,3
         write(*,*) v(i), DUPCIA, END_OF_FILE
      end do
      STOp
      end
