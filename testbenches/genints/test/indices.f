      program indices 
      double precision alfa(100,4)
      integer m,i,j
      
      m = 3
      do i=1,m
         do j=1,4
            alfa(i,j) = i*j
         end do
      end do
      call print_a(alfa,m)
      call print_a_s(alfa,m)
      STOP
      end


      subroutine print_a_s(alpha,m)
      double precision alpha(100,4)
      integer m,i,j
      double precision v(4)
      do i=1,m
         do j=1,4
            v(j) = alpha(i,j)
         enddo
         write(*,10) (v(j),j=1,4)
      enddo
 10   format((10f10.4)) 
   
      return
      end

 
      subroutine print_a(alpha,m)
      double precision alpha(100,4)
      integer m,i,j
      double precision v(4)
      do i=1,m
         do j=1,4
            v(j) = alpha(i,j)
         enddo
         write(*,10) (v(j),j=1,4)
      enddo
 10   format((10f10.4)) 
   
      return
      end
