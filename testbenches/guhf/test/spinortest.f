#include "../spinor.f"
      program spinortest
      double precision H(1000), g(4)
      data H/3.24,4.23,-2.34,3.24,996*0.0/
      call spinor(H,2)
      do i=1,4
         do j=1,4
            ij = 4*(j-1)+i
            g(j) = H(ij)
         end do
         write(*,*) g(1), g(2), g(3), g(4)
      end do
      STOP
      end
