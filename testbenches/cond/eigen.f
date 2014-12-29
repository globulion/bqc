#define loch(i,j) (n*(j-1) + i)
      subroutine eigen (H, U, n)
      implicit double precision (a-h,o-z)
      double precision H(1), U(1)
      integer n      
C
      data zero, eps, one, two, four, big/0.0D+00,1.0D-20,1.0D+00,
     &                                    2.0D+00,4.0D+00,1.0D+20/
C
C.....Initialize U matrix to unity
      do i = 1, n
         ii = loch(i,i)
         do j = 1, n
            ij = loch(i,j)
            U(ij) = zero
         end do
         U(ii) = one
      end do
C
C.....start sweep through off-diagonal elements
      hmax = big
      do 90 while (hmax.GT.eps)
         hmax = zero
         do i = 2, n
            jtop = i-1
            do 10 j = 1, jtop
               ii = loch(i,i); jj = loch(j,j)
               ij = loch(i,j); ji = loch(j,i)
               hii= H(ii); hjj = H(jj); hij = H(ij)
               hsq= hij*hij
               if (hsq.GT.hmax) hmax = hsq
               if (hsq.LT.eps) go to 10
               del = hii - hjj; sign = one
               if (del.LT.zero) then
                   sign = -one
                   del  = -del
               end if 
               denom = del + dsqrt(del*del + four*hsq)
               tan   = two*sign*hij/denom
               c     = one/dsqrt(one + tan*tan)
               s     = c*tan
               do 20 k = 1, n
                  kj = loch(k,j); ki = loch(k,i)
                  jk = loch(j,k); ik = loch(i,k)
                  temp = c*U(kj) - s*U(ki)
                  U(ki)= s*U(kj) + c*U(ki); U(kj) = temp
                  if ((i.EQ.k).OR.(j.EQ.k)) go to 20
C.................update the parts of H matrix affected by a rotation
                  temp = c*H(kj) - s*H(ki)
                  H(ki)= s*H(kj) + c*H(ki)
                  H(kj)= temp; H(ik)= H(ki); H(jk)= H(kj)
 20            continue
C..............now transform the four elements explicitly targeted by theta
               H(ii) = c*c*hii + s*s*hjj + two*c*s*hij
               H(jj) = c*c*hjj + s*s*hii - two*c*s*hij
               H(ij) = zero; H(ji) = zero
 10         continue 
         end do
C
C.....Finish when largest off-diagonal is small enough
c      end do
 90    continue
C
C.....Now sort the eigenvectors into eigenvalue order
      iq = -n
      do i = 1, n
         iq = iq + n; ii = loch(i,i); jq = n*(i-2)
         do j = i, n
            jq = jq + n; jj = loch(j,j)
            if (H(ii).LT.H(jj)) go to 30
            temp = H(ii); H(ii) = H(jj); H(jj) = temp
            do k = 1, n
               ilr = iq + k; imr = jq + k
               temp = U(ilr); U(ilr) = U(imr); U(imr) = temp
            end do
 30         continue
         end do
      end do
      return
      end
