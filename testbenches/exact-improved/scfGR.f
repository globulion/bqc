#include "getint.f"
#define locGR(i,j) ((j-1)*m+i)
      subroutine scfGR(R, G, m, nfile)
      double precision G(*), R(*)
      integer m, nfile
C
      double precision val, two, coul1, coul2, coul3, exch
      integer i, j, k, l, mu, ij, kl, il, ik, jk, lj
      integer getint
      integer pointer
      data two/2.0D+00/
C
c      rewind nfile
      pointer = 0
      do while (getint(nfile, i, j, k, l, mu, val, pointer).NE.
     &                                              END_OF_FILE)
         ij = locGR(i,j); kl = locGR(k,l)
         il = locGR(i,l); ik = locGR(i,k)
         jk = locGR(j,k); jl = locGR(j,l)
         if (j.LT.k) jk = locGR(k,j)
         if (j.LT.l) jl = locGR(l,j)

         coul1 = two*R(ij)*val; coul2 = two*R(kl)*val; exch = val

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
      enddo
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
