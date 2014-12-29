#define locGR(i,j) ((j-1)*m+i)
      subroutine scfGR(R, G, m, nfile)
      double precision R(*), G(*)
      integer m, nfile
C
      integer mby2
      double precision val
      integer i, j, k, l, is, js, ks, ls, ijs, kls, mu
      integer getint
      double precision zero, one, a, b
      integer pointer, spin, skip
      data one,zero/1.0D+00,0.0D+00/
C
      mby2 = m/2
c      rewind nfile
      pointer = 0

      do while (getint(nfile, is, js, ks, ls, mu, val, pointer).NE.
     &                                              END_OF_FILE)

         ijs = is*(is-1)/2+js 
         kls = ks*(ks-1)/2+ls

         do spin = 1, 4
            skip = NO

            select case (spin)                                
                case (1) 
                  i=is
                  j=js
                  k=ks
                  l=ls
                case (2)
                  i=is+mby2
                  j=js+mby2
                  k=ks+mby2
                  l=ls+mby2
                case (3)
                  i=is+mby2
                  j=js+mby2
                  k=ks
                  l=ls
                case (4)
                  if (ijs.EQ.kls) skip = YES
                  i=is
                  j=js
                  k=ks+mby2
                  l=ls+mby2
                  call order(i,j,k,l)
            end select
 
            if (skip.EQ.YES) cycle

            a = one; b = one
            if (spin.GE.3) b = zero
            
            call GofR(R,G,m,a,b,i,j,k,l,val)
         end do
      enddo
C
C.....symmetrize the G matrix
      do i = 1, m
         do j = 1, i-1
            ij = locGR(i,j); ji = locGR(j,i)
            G(ji) = G(ij)
         end do
      end do
C
      return
      end



      subroutine GofR(R,G,m,a,b,i,j,k,l,val)
      double precision R(*), G(*)
      double precision val, a, b
      integer i, j, k, l, m
      integer ij, kl, il, ik, jk, jl
      double precision coul1, coul2, coul3, exch

      ij = locGR(i,j); kl = locGR(k,l)
      il = locGR(i,l); ik = locGR(i,k)
      jk = locGR(j,k); jl = locGR(j,l)
      if (j.LT.k) jk = locGR(k,j)
      if (j.LT.l) jl = locGR(l,j)

      coul1 = a*R(ij)*val; coul2 = a*R(kl)*val; exch = b*val

      if (k.NE.l) then
          coul2 = coul2 + coul2
          G(ik) = G(ik) - R(jl)*exch
          if ((i.NE.j).AND.(j.GE.k)) G(jk) = G(jk) - R(il)*exch
      end if
      
      G(il) = G(il) - R(jk)*exch; G(ij) = G(ij) + coul2

      if ((i.NE.j).AND.(j.GE.l)) G(jl) = G(jl) - R(ik)*exch
      
      if (ij.NE.kl) then
          coul3 = coul1
          if (i.NE.j) coul3 = coul3 + coul1
          if (j.LE.k) then
              G(jk) = G(jk) - R(il)*exch
              if ((i.NE.j).AND.(i.LE.k)) G(ik) = G(ik) - R(jl)*exch
              if ((k.NE.l).AND.(j.LE.l)) G(jl) = G(jl) - R(ik)*exch
          end if
          G(kl) = G(kl) + coul3
      end if

      return 
      end

      subroutine order(i,j,k,l)
      integer i, j, k, l
      integer integ

      i = abs(i); j = abs(j); k = abs(k); l = abs(l)

      if (i.LT.j) then
          integ = i
          i = j
          j = integ
      end if
      
      if (k.LT.l) then
          integ = k
          k = l
          l = integ
      end if

      if ((i.LT.k).OR.((i.EQ.k).AND.(j.LT.l))) then
           integ = i
           i = k
           k = integ
           integ = j
           j = l
           l = integ
      end if

      return
      end
