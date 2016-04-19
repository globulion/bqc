      subroutine spinor(H,m)
      double precision H(*)
      integer m

      double precision zero
      integer i, j, ij, ji, ip, jp, ijp, ijd, nl, n
      data zero/0.0D+00/

      n = 2*m; nl = m+1
      
      do i = 1, m
         do j = 1, m
            ij=m*(j-1)+i;  ip=i+m;  jp=j+m
            ijp=n*(jp-1)+ip;  H(ijp) = H(ij)
         end do
      end do

      do i = 1, m
         do j = 1, m
            ip = i+m; jp = j+m; ijp=n*(jp-1)+ip
            ijd=n*(j-1)+i; H(ijd) = H(ijp)
         end do
      end do
 
      do i = 1, m
         do j = nl, n
            ij = n*(j-1)+i; ji = n*(i-1)+j
            H(ij) = zero
            H(ji) = zero
         end do
      end do

      return
      end

