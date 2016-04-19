#include "defs.f"
c#include "fmch.f"
C#include "genints.f"
#include "integral.f_old"
#include "gints.f"
C#include "utils.f"
      program gen_ints
C
C     TESTBENCH FOR WATER MOLECULE - GENERATING INTEGRALS
C
      double precision vlist(MAX_CENTRES,4)
      double precision eta(MAX_PRIMITIVES,5)
      integer nfirst(7)
      integer nlast(7)
      integer ntype(7)
      integer ncntr(7)
      integer nr(NO_OF_TYPES,3)
      integer nbfns, ngmx, noc, nfile, ncmx
C
      integer i, j
      double precision vlist1(4), vlist2(4), vlist3(4)
      double precision u(5)
      double precision hydrE(3), hydrC(3)
      double precision oxygE(15), oxygC(15)
      double precision S(10000),H(10000)
C
      nfile = 17
      data nr /
     &     0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,
     &     0,0,1,0,0,2,0,1,0,1,0,3,0,1,0,2,2,0,1,1,
     &     0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1/
C
C.....Coordinates of water in Bohr
      data noc/3/
      data vlist1/0.00000000,  0.00000000,  1.79523969, 8.0000/
      data vlist2/0.00000000,  0.00000000,  0.00000000, 1.0000/
      data vlist3/1.69257088,  0.00000000,  2.39364599, 1.0000/

      do i = 1, 4
         vlist(1,i) = vlist1(i)
         vlist(2,i) = vlist2(i)
         vlist(3,i) = vlist3(i)
      end do
      call print_vlist(vlist,vlist1)
C
C.....basis set
C Hydrogen 1s
      data hydrE/3.42525091,0.62391373,0.16885540/
      data hydrC/0.15432897,0.53532814,0.44463454/
C Oxygen 1S 2S 2PX 2PY 2PZ
      data oxygE/130.7093200,23.8088610,6.4436083,
     &           5.0331513,1.1695961,0.3803890,
     &           5.0331513,1.1695961,0.3803890, 
     &           5.0331513,1.1695961,0.3803890,
     &           5.0331513,1.1695961,0.3803890/
      data oxygC/0.15432897,0.53532814,0.44463454,
     &          -0.09996723,0.39951283,0.70011547,
     &           1.15591627,0.60768372,0.39195739,
     &           0.15591627,0.60768372,0.39195739,
     &           0.15591627,0.60768372,0.39195739/
C     Do the primitive GTOs in eta
      do i = 1, 3
c........oxygen 1
         do j = 1, 15
            eta(j,i) = vlist1(i)
            eta(j,4) = oxygE(j)
            eta(j,5) = oxygC(j)
         end do
c........hydrogen 2
         do j = 16, 18
            eta(j,i) = vlist2(i)
            eta(j,4) = hydrE(j-15)
            eta(j,5) = hydrC(j-15)
         end do
c........hydrogen 3
         do j = 19, 21
            eta(j,i) = vlist3(i)
            eta(j,4) = hydrE(j-18)
            eta(j,5) = hydrC(j-18)
         end do
      end do
      call print_eta(eta,u)
c.....specification of contraction 
      data nfirst/1,4,7,10,13,16,19/
      data nlast/3,6,9,12,15,18,21/
c.....types of basis functions: s,s,px,py,pz,s,s
      data ntype/1,1,2,3,4,1,1/
c.....nuclear center and basis function
      data ncntr/1,1,1,1,1,2,3/
      data ngmx/21/
      data nbfns/7/
      data ncmx/3/
      call genint(ngmx,nbfns,eta,ntype,ncntr,nfirst,nlast,
     &            vlist,ncmx,noc,S,H,nfile)
      STOP
      end

      subroutine print_eta(eta,u)
      double precision eta(MAX_PRIMITIVES,5), u(5)
      write(*,*) " PRIMITIVE GTOs"
      do i = 1,21
         do j = 1,5
            u(j) = eta(i,j)
         end do
         write(*,200) (u(j), j=1,5)
      end do
 200  FORMAT("", (10f10.5))
      return
      end

      subroutine print_vlist(vlist,vlist1)
      double precision vlist(MAX_CENTRES,4), vlist1(4)
      write(*,*) " ATOMIC COORDINATES AND NUCLEAR CHARGES"
      do i = 1,3
         do j = 1,4
            vlist1(j) = vlist(i,j)
         end do
         write(*,200) (vlist1(j), j=1,4)
      end do
 200  FORMAT("", (10f10.5))
      return
      end
     
