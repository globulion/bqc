#include "getint.f"
      subroutine USE(i,j,k,l,val,R,G,m)
      double precision val
      double precision R(*), G(*)
      integer i, j, k, l, irs, itu, iru, its
      irs=m*(j-1)+i;itu=m*(l-1)+k
      iru=m*(l-1)+i;its=m*(j-1)+k
      G(irs)=G(irs)+2.0d00*R(itu)*val
      G(iru)=G(iru)-R(its)*val
      return
      end

      subroutine scfGR(R, G, m, nfile)
      double precision G(*), R(*)
      double precision val
      integer irs, itu, iru, its
      integer i, j, k, l, mu, ij, kl, STATE
      integer m, nfile, pointer
      integer getint
C
c      rewind nfile
      pointer = 0
      do while (getint(nfile, i, j, k, l, mu, val, pointer).NE.
     &                                              END_OF_FILE)
         call USE(i,j,k,l,val,R,G,m)
         if (i.NE.j) call USE(j,i,k,l,val,R,G,m)
         if (k.NE.l) then
             call USE(i,j,l,k,val,R,G,m)
             if (i.NE.j) call USE(j,i,l,k,val,R,G,m)
         end if
C
         ij = (i*(i-1))/2 + j
         kl = (k*(k-1))/2 + l
C
         if (ij.NE.kl) then
             call USE(k,l,i,j,val,R,G,m)
             if (i.NE.j) call USE(k,l,j,i,val,R,G,m)
             if (k.NE.l) then
                 call USE(l,k,i,j,val,R,G,m)
                 if (i.NE.j) call USE(l,k,j,i,val,R,G,m)
             end if
         end if
      enddo
C
      return
      end
