
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle dods.web"
C  RUN TIME:     "Sunday, December 7, 2014 at 0:55."
C  WEB FILE:     "dods.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 5 "dods.web"
      program calcDODS
      double precision vlist(MAX_CENTRES,4)
      double precision eta(MAX_PRIMITIVES,5)
      integer nfirst(7)
      integer nlast(7)
      integer ntype(7)
      integer ncntr(7)
      integer nr(NO_OF_TYPES,3)
      integer nbfns,ngmx,noc,nfile,ncmx
      integer i,j
      double precision vlist1(4),vlist2(4),vlist3(4)
      double precision u(5)
      double precision hydrE(3),hydrC(3)
      double precision oxygE(15),oxygC(15)
      double precision S(1000),H(1000),HF(1000),R(1000),Rold(1000)
      double precision C(1000),Cbar(1000),V(1000)
      double precision crit,damp,E
      double precision epsilon(100)
      integer scf
      integer nelec,nbasis,interp,irite
      data nr/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,0,0,1,0,0,2,0,1,0,
     &1,0,3,0,1,0,2,2,0,1,1,0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1/
      data noc/3/
      data vlist1/0.00000000,0.00000000,1.79523969,8.0000/
      data vlist2/0.00000000,0.00000000,0.00000000,1.0000/
      data vlist3/1.69257088,0.00000000,2.39364599,1.0000/
      do i=1,4
      vlist(1,i)=vlist1(i)
      vlist(2,i)=vlist2(i)
      vlist(3,i)=vlist3(i)
      end do
      data hydrE/3.42525091,0.62391373,0.16885540/
      data hydrC/0.15432897,0.53532814,0.44463454/
      data oxygE/130.7093200,23.8088610,6.4436083,5.0331513,1.1695961,0.
     &3803890,5.0331513,1.1695961,0.3803890,5.0331513,1.1695961,0.380389
     &0,5.0331513,1.1695961,0.3803890/
      data oxygC/0.15432897,0.53532814,0.44463454,-0.09996723,0.39951283
     &,0.70011547,1.15591627,0.60768372,0.39195739,0.15591627,0.60768372
     &,0.39195739,0.15591627,0.60768372,0.39195739/
      do i=1,3
      do j=1,15
      eta(j,i)=vlist1(i)
      eta(j,4)=oxygE(j)
      eta(j,5)=oxygC(j)
      end do
      do j=16,18
      eta(j,i)=vlist2(i)
      eta(j,4)=hydrE(j-15)
      eta(j,5)=hydrC(j-15)
      end do
      do j=19,21
      eta(j,i)=vlist3(i)
      eta(j,4)=hydrE(j-18)
      eta(j,5)=hydrC(j-18)
      end do
      end do
      data nfirst/1,4,7,10,13,16,19/
      data nlast/3,6,9,12,15,18,21/
      data ntype/1,1,2,3,4,1,1/
      data ncntr/1,1,1,1,1,2,3/
      data ngmx/21/
      data nbfns/7/
      data ncmx/3/
      nelec=10
      nbasis=7
      irite=12
      nfile=ERI_UNIT
      crit=1.00D-06
      damp=0.13D+00
      interp=5
      call genint(ngmx,nbasis,eta,ntype,ncntr,nfirst,nlast,vlist,ncmx,no
     &c,S,H,nfile)
      call shalf(S,R,Cbar,nbasis)
      
      STOP
      END
C* :1 * 
      
