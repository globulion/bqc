
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -TD main.web"
C  RUN TIME:     "Monday, December 8, 2014 at 0:36."
C  WEB FILE:     "main.web"
C  CHANGE FILE:  (none)
*line 49 "main.web"
      program calcDODS
      double precision vlist(50,4)
      double precision eta(1000,5)
      integer nfirst(7)
      integer nlast(7)
      integer ntype(7)
      integer ncntr(7)
      integer nr(20,3)
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
      write(*,*)" Provide the number of electrons: "
      read(*,*)nelec
      nbasis=7
      irite=12
      nfile=17
      crit=1.00D-06
      damp=0.13D+00
      interp=0
      call genint(ngmx,nbasis,eta,ntype,ncntr,nfirst,nlast,vlist,ncmx,no
     &c,S,H,nfile)
      call shalf(S,R,Cbar,nbasis)
      
      if(scf(H,S,nbasis,nelec,nfile,irite,damp,interp,E,HF,V,R,Rold,Cbar
     &,epsilon,crit).EQ.0)then
      write(*,*)" SUCCESS"
      else
      write(*,*)" You have to work it more ..."
      end if
      STOP
      END
C* :3 * 
C* 5: * 
*line 245 "main.web"
      
      integer function SCF(H,C,nbasis,nelec,nfile,irite,damp,interp,E,HF
     &,V,R,Rold,Cbar,epsilon,crit)
C* 6: * 
*line 273 "main.web"
      implicit double precision(a-h,o-z)
      integer nbasis,nelec,nfile,irite
      integer interp
      double precision H(1),C(1),HF(1),V(1),R(1)
      double precision Rold(1),Cbar(1)
      double precision epsilon(1)
      double precision E,damp,crit
C* :6 * 
*line 249 "main.web"
      
C* 7: * 
*line 283 "main.web"
      integer scftype,kount,maxit,nocc,m,mm,i
      double precision term,turm,Rsum
      double precision zero,half
      data zero,half/0.0D+00,0.5D+00/
C* :7 * 
*line 251 "main.web"
      
C* 8: * 
*line 290 "main.web"
      if(nelec.GT.zero)then
      scftype=21
      nocc=nelec/2
      m=nbasis
      WRITE(*,*)"RHF CALCULATION CHOSEN"
      else
      scftype=11
      nocc=abs(nelec)
      m=nbasis*2
      call spinor(H,nbasis)
      call spinor(C,nbasis)
      WRITE(*,*)"UHF CALCULATION CHOSEN"
      end if
C* :8 * 
*line 253 "main.web"
      
C* 9: * 
*line 306 "main.web"
      mm=m*m
      do i=1,mm
      R(i)=zero
      Rold(i)=zero
      end do
      SCF=0
      kount=0
      icon=100
C* :9 * 
*line 255 "main.web"
      
      do while((icon.NE.0).AND.(kount.LT.50))
C* 10: * 
*line 317 "main.web"
      kount=kount+1
      E=zero
      icon=0
      do i=1,mm
      HF(i)=H(i)
      E=E+R(i)*HF(i)
      enddo
      call scfGR(R,HF,nbasis,nfile,scftype)
      do i=1,mm
      E=E+R(i)*HF(i)
      enddo
      if(scftype.EQ.11)E=half*E
      write(61,200)E
      call gtprd(C,HF,R,m,m,m)
      call gmprd(R,C,HF,m,m,m)
      call eigen(HF,Cbar,m)
      do i=1,m
      epsilon(i)=HF(m*(i-1)+i)
      enddo
      call gmprd(C,Cbar,V,m,m,m)
      call scfR(V,R,m,nocc)
      Rsum=zero
      do i=1,mm
      turm=R(i)-Rold(i)
      term=dabs(turm)
      Rold(i)=R(i)
      C(i)=V(i)
      if(term.GT.crit)icon=icon+1
      Rsum=Rsum+term
      if(kount.LT.interp)R(i)=R(i)-damp*turm
      enddo
C* :10 * 
*line 259 "main.web"
      
      end do
C* 11: * 
*line 353 "main.web"
      write(61,201)Rsum,icon
      if((kount.EQ.50).AND.(icon.NE.0))then
      write(61,204)
      SCF=100
      else
      write(61,202)kount
      write(61,203)(epsilon(i),i=1,nocc)
      endif
C* :11 * 
*line 263 "main.web"
      
C* 12: * 
*line 365 "main.web"
200   format(" Current Electronic Energy = ",f12.6)
201   format(" Convergence in R = ",f12.5,i6,"  Changing")
202   format(" SCF converged in",i4," iterations")
203   format(" Orbital Energies ",(7f10.5))
204   format(" SCF did not converged... quitting")
C* :12 * 
*line 265 "main.web"
      
      return
      end
C* :5 * 
C* 14: * 
*line 376 "main.web"
      subroutine scfGR(R,G,n,nfile,ntype)
      double precision R(*),G(*)
      integer m,n,nfile,ntype
      double precision val
      integer i,j,k,l,is,js,ks,ls,ijs,kls,mu
      integer getint
      double precision zero,one,cJ,cK
      integer pointer,spin,skip
      data zero,one,two/0.0D+00,1.0D+00,2.0D+00/
      rewind nfile
      pointer=0
C* 15: * 
*line 434 "main.web"
      if(ntype.EQ.21)then
      m=n
      cJ=two
      cK=one
      else
      m=2*n
      cJ=one
      cK=one
      end if
C* :15 * 
*line 392 "main.web"
      
      do while(getint(nfile,is,js,ks,ls,mu,val,pointer).NE.-1)
      ijs=is*(is-1)/2+js
      kls=ks*(ks-1)/2+ls
      do spin=1,4
C* 16: * 
*line 448 "main.web"
      if((spin.GT.1).AND.(ntype.EQ.21))exit
C* :16 * 
*line 402 "main.web"
      
      skip=100
      select case(spin)
      case(1)
      i=is
      j=js
      k=ks
      l=ls
      case(2)
      i=is+n
      j=js+n
      k=ks+n
      l=ls+n
      case(3)
      i=is+n
      j=js+n
      k=ks
      l=ls
      case(4)
      if(ijs.EQ.kls)skip=0
      i=is
      j=js
      k=ks+n
      l=ls+n
      call order(i,j,k,l)
      end select
      if(skip.EQ.0)cycle
      cK=one
      if(spin.GE.3)cK=zero
      call GofR(R,G,m,cJ,cK,i,j,k,l,val)
      end do
      enddo
C* 17: * 
*line 452 "main.web"
      do i=1,m
      do j=1,i-1
      ij=(m*(j-1)+i)
      ji=(m*(i-1)+j)
      G(ji)=G(ij)
      end do
      end do
C* :17 * 
*line 427 "main.web"
      
      return
      end
C* :14 * 
C* 19: * 
*line 464 "main.web"
      subroutine GofR(R,G,m,a,b,i,j,k,l,val)
      double precision R(*),G(*)
      double precision val,a,b
      integer i,j,k,l,m
      integer ij,kl,il,ik,jk,jl
      double precision coul1,coul2,coul3,exch
      ij=(m*(j-1)+i)
      kl=(m*(l-1)+k)
      il=(m*(l-1)+i)
      ik=(m*(k-1)+i)
      jk=(m*(k-1)+j)
      jl=(m*(l-1)+j)
      if(j.LT.k)jk=(m*(j-1)+k)
      if(j.LT.l)jl=(m*(j-1)+l)
      coul1=a*R(ij)*val
      coul2=a*R(kl)*val
      exch=b*val
      if(k.NE.l)then
      coul2=coul2+coul2
      G(ik)=G(ik)-R(jl)*exch
      if((i.NE.j).AND.(j.GE.k))G(jk)=G(jk)-R(il)*exch
      end if
      G(il)=G(il)-R(jk)*exch
      G(ij)=G(ij)+coul2
      if((i.NE.j).AND.(j.GE.l))G(jl)=G(jl)-R(ik)*exch
      if(ij.NE.kl)then
      coul3=coul1
      if(i.NE.j)coul3=coul3+coul1
      if(j.LE.k)then
      G(jk)=G(jk)-R(il)*exch
      if((i.NE.j).AND.(i.LE.k))G(ik)=G(ik)-R(jl)*exch
      if((k.NE.l).AND.(j.LE.l))G(jl)=G(jl)-R(ik)*exch
      end if
      G(kl)=G(kl)+coul3
      end if
      return
      end
C* :19 * 
C* 21: * 
*line 507 "main.web"
      subroutine order(i,j,k,l)
      integer i,j,k,l
      integer integ
      i=abs(i)
      j=abs(j)
      k=abs(k)
      l=abs(l)
      if(i.LT.j)then
      integ=i
      i=j
      j=integ
      end if
      if(k.LT.l)then
      integ=k
      k=l
      l=integ
      end if
      if((i.LT.k).OR.((i.EQ.k).AND.(j.LT.l)))then
      integ=i
      i=k
      k=integ
      integ=j
      j=l
      l=integ
      end if
      return
      end
C* :21 * 
C* 23: * 
*line 541 "main.web"
      subroutine scfR(C,R,m,nocc)
      double precision C(1),R(1)
      integer m,nocc
      
      double precision suma,zero
      integer i,j,k,ij,ji,kk,ik,jk
      data zero/0.0D+00/
      
      do i=1,m
      do j=1,i
      suma=zero
      do k=1,nocc
      kk=m*(k-1)
      ik=kk+i
      jk=kk+j
      suma=suma+C(ik)*C(jk)
      enddo
      ij=m*(j-1)+i
      ji=m*(i-1)+j
      R(ij)=suma
      R(ji)=suma
      enddo
      enddo
      
      return
      end
C* :23 * 
C* 27: * 
*line 587 "main.web"
      double precision function genoei(i,j,eta,ngmx,nfirst,nlast,ntype,n
     &r,ntmx,vlist,noc,ncmx,ovltot,kintot)
      implicit double precision(a-h,o-z)
      integer i,j,ngmx,ncmx,noc,ntmx
      integer nfirst(*),nlast(*),ntype(*),nr(ntmx,3)
      double precision ovltot,kintot
      double precision eta(1000,5),vlist(50,4)
      
C* 28: * 
*line 690 "main.web"
      double precision Airu(10),Ajsv(10),Aktw(10)
      double precision p(3),sf(10,3),tf(20)
      double precision fact(20),g(50)
      double precision kin
      data zero,one,two,half,quart/0.0d00,1.0d00,2.0d00,0.5d00,0.25d00/
      data pi/3.141592653589d00/
C* :28 * 
*line 598 "main.web"
      
      
C* 39: * 
*line 1030 "main.web"
      data fact/1.0D00,1.0D00,2.0D00,6.0D00,24.0D00,120.0D00,720.0D00,50
     &40.0D00,40320.0D00,362880.0D00,3628800.0D00,39916800.0D00,47900160
     &0.0D00,6227020800.0D00,6*0.0D00/
C* :39 * 
*line 603 "main.web"
      
      
C* 29: * 
*line 702 "main.web"
      ityp=ntype(i)
      jtyp=ntype(j)
      l1=nr(ityp,1)
      m1=nr(ityp,2)
      n1=nr(ityp,3)
      l2=nr(jtyp,1)
      m2=nr(jtyp,2)
      n2=nr(jtyp,3)
      imax=l1+l2+1
      jmax=m1+m2+1
      kmax=n1+n2+1
      maxall=imax
      if(maxall.LT.jmax)maxall=jmax
      if(maxall.LT.kmax)maxall=kmax
      if(maxall.LT.2)maxall=2
      iss=nfirst(i)
      il=nlast(i)
      jss=nfirst(j)
      jl=nlast(j)
C* :29 * 
*line 608 "main.web"
      
      
      rAB=(eta(iss,1)-eta(jss,1))**2+(eta(iss,2)-eta(jss,2))**2+(eta(iss
     &,3)-eta(jss,3))**2
      
      genoei=zero
      totnai=zero
      kintot=zero
      ovltot=zero
      
      do irun=iss,il
      do jrun=jss,jl
C* 41: * 
*line 1070 "main.web"
      aexp=eta(irun,4)
      anorm=eta(irun,5)
      bexp=eta(jrun,4)
      bnorm=eta(jrun,5)
      
      t1=aexp+bexp
      deleft=one/t1
      p(1)=(aexp*eta(irun,1)+bexp*eta(jrun,1))*deleft
      p(2)=(aexp*eta(irun,2)+bexp*eta(jrun,2))*deleft
      p(3)=(aexp*eta(irun,3)+bexp*eta(jrun,3))*deleft
      pax=p(1)-eta(irun,1)
      pay=p(2)-eta(irun,2)
      paz=p(3)-eta(irun,3)
      pbx=p(1)-eta(jrun,1)
      pby=p(2)-eta(jrun,2)
      pbz=p(3)-eta(jrun,3)
      prefa=dexp(-aexp*bexp*rAB/t1)*pi*anorm*bnorm/t1
C* :41 * 
*line 632 "main.web"
      
C* 30: * 
*line 719 "main.web"
      prefa=two*prefa
      expab=dexp(-aexp*bexp*rAB/t1)
      s00=(pi/t1)**1.5*expab
      dum=one
      tf(1)=one
      del=half/t1
      do n=2,maxall
      tf(n)=tf(n-1)*dum*del
      dum=dum+two
      end do
      ox0=ovrlap(l1,l2,pax,pbx,tf)
      oy0=ovrlap(m1,m2,pay,pby,tf)
      oz0=ovrlap(n1,n2,paz,pbz,tf)
      ox2=ovrlap(l1,l2+2,pax,pbx,tf)
      oxm2=ovrlap(l1,l2-2,pax,pbx,tf)
      oy2=ovrlap(m1,m2+2,pay,pby,tf)
      oym2=ovrlap(m1,m2-2,pay,pby,tf)
      oz2=ovrlap(n1,n2+2,paz,pbz,tf)
      ozm2=ovrlap(n1,n2-2,paz,pbz,tf)
      ov0=ox0*oy0*oz0
      ovl=ov0*s00
      ov1=ox2*oy0*oz0
      ov4=oxm2*oy0*oz0
      ov2=ox0*oy2*oz0
      ov5=ox0*oym2*oz0
      ov3=ox0*oy0*oz2
      ov6=ox0*oy0*ozm2
C* :30 * 
*line 634 "main.web"
      
      ovltot=ovltot+anorm*bnorm*ovl
C* 32: * 
*line 786 "main.web"
      xl=dfloat(l2*(l2-1))
      xm=dfloat(m2*(m2-1))
      xn=dfloat(n2*(n2-1))
      xj=dfloat(2*(l2+m2+n2)+3)
      kin=s00*(bexp*(xj*ov0-two*bexp*(ov1+ov2+ov3))-half*(xl*ov4+xm*ov5+
     &xn*ov6))
C* :32 * 
*line 638 "main.web"
      
      kintot=kintot+anorm*bnorm*kin
      
      tnai=zero
C* 33: * 
*line 793 "main.web"
      m=imax+jmax+kmax-2
      do n=1,imax
      sf(n,1)=fj(l1,l2,n-1,pax,pbx)
      end do
      do n=1,jmax
      sf(n,2)=fj(m1,m2,n-1,pay,pby)
      end do
      do n=1,kmax
      sf(n,3)=fj(n1,n2,n-1,paz,pbz)
      end do
C* :33 * 
*line 646 "main.web"
      
      do n=1,noc
      pn=zero
      
C* 36: * 
*line 871 "main.web"
      cpx=p(1)-vlist(n,1)
      cpy=p(2)-vlist(n,2)
      cpz=p(3)-vlist(n,3)
      pcsq=cpx*cpx+cpy*cpy+cpz*cpz
C* :36 * 
*line 655 "main.web"
      
      t=t1*pcsq
      call auxg(m,t,g)
C* 34: * 
*line 811 "main.web"
      epsi=quart/t1
      do ii=1,10
      Airu(ii)=zero
      Ajsv(ii)=zero
      Aktw(ii)=zero
      end do
      call aform(imax,sf,fact,cpx,epsi,Airu,1)
      call aform(jmax,sf,fact,cpy,epsi,Ajsv,2)
      call aform(kmax,sf,fact,cpz,epsi,Aktw,3)
C* :34 * 
*line 661 "main.web"
      
      
      do ii=1,imax
      do jj=1,jmax
      do kk=1,kmax
      nu=ii+jj+kk-2
      pn=pn+Airu(ii)*Ajsv(jj)*Aktw(kk)*g(nu)
      end do
      end do
      end do
      tnai=tnai-pn*vlist(n,4)
      end do
      totnai=totnai+prefa*tnai
      end do
      end do
      genoei=totnai+kintot
      return
      end
C* :27 * 
C* 31: * 
*line 749 "main.web"
      double precision function ovrlap(l1,l2,pax,pbx,tf)
      implicit double precision(a-h,o-z)
      integer l1,l2
      double precision pax,pbx
      double precision tf(*)
      double precision zero,one,dum
      data zero,one/0.0d00,1.0d00/
      if((l1.LT.0).OR.(l2.LT.0))then
      ovrlap=zero
      return
      end if
      if((l1.EQ.0).AND.(l2.EQ.0))then
      ovrlap=one
      return
      end if
      dum=zero
      maxkk=(l1+l2)/2+1
      do kk=1,maxkk
      dum=dum+tf(kk)*fj(l1,l2,2*kk-2,pax,pbx)
      end do
      ovrlap=dum
      return
      end
C* :31 * 
C* 35: * 
*line 838 "main.web"
      subroutine aform(imax,sf,fact,cpx,epsi,Airu,xyorz)
      implicit double precision(a-h,o-z)
      integer imax,xyorz
      double precision Airu(*),fact(*),sf(10,*)
      double precision one
      data one/1.0d00/
      do i=1,imax
      ai=(-one)**(i-1)*sf(i,xyorz)*fact(i)
      irmax=(i-1)/2+1
      do ir=1,irmax
      irumax=irmax-ir+1
      do iru=1,irumax
      iq=ir+iru-2
      ip=i-2*iq-1
      at5=one
      if(ip.GT.0)at5=cpx**ip
      tiru=ai*(-one)**(iru-1)*at5*epsi**iq/(fact(ir)*fact(iru)*fact(ip+1
     &))
      nux=ip+iru
      Airu(nux)=Airu(nux)+tiru
      end do
      end do
      end do
      return
      end
C* :35 * 
C* 37: * 
*line 883 "main.web"
      double precision function generi(i,j,k,l,xyorz,eta,ngmx,nfirst,nla
     &st,ntype,nr,ntmx)
      implicit double precision(a-h,o-z)
      integer i,j,k,l,xyorz,ngmx,ntmx
      double precision eta(1000,5)
      integer nfirst(*),nlast(*),ntype(*),nr(ntmx,3)
      
C* 38: * 
*line 1019 "main.web"
      double precision p(3),q(3),ppx(20),ppy(20),ppz(20)
      double precision bbx(20),bby(20),bbz(20),sf(10,6)
      double precision xleft(5,10),yleft(5,10),zleft(5,10)
      double precision r(3),fact(20),g(50)
      data zero,one,two,half/0.0D00,1.0D00,2.0D00,0.5D00/
      data pi/3.141592653589D00/
C* :38 * 
*line 894 "main.web"
      
      
C* 39: * 
*line 1030 "main.web"
      data fact/1.0D00,1.0D00,2.0D00,6.0D00,24.0D00,120.0D00,720.0D00,50
     &40.0D00,40320.0D00,362880.0D00,3628800.0D00,39916800.0D00,47900160
     &0.0D00,6227020800.0D00,6*0.0D00/
C* :39 * 
*line 899 "main.web"
      
      
C* 40: * 
*line 1040 "main.web"
      ityp=ntype(i)
      jtyp=ntype(j)
      ktyp=ntype(k)
      ltyp=ntype(l)
      l1=nr(ityp,1)
      m1=nr(ityp,2)
      n1=nr(ityp,3)
      l2=nr(jtyp,1)
      m2=nr(jtyp,2)
      n2=nr(jtyp,3)
      l3=nr(ktyp,1)
      m3=nr(ktyp,2)
      n3=nr(ktyp,3)
      l4=nr(ltyp,1)
      m4=nr(ltyp,2)
      n4=nr(ltyp,3)
      is=nfirst(i)
      il=nlast(i)
      js=nfirst(j)
      jl=nlast(j)
      ks=nfirst(k)
      kl=nlast(k)
      ls=nfirst(l)
      ll=nlast(l)
C* :40 * 
*line 906 "main.web"
      
      
      rAB=(eta(is,1)-eta(js,1))**2+(eta(is,2)-eta(js,2))**2+(eta(is,3)-e
     &ta(js,3))**2
      rCD=(eta(ks,1)-eta(ls,1))**2+(eta(ks,2)-eta(ls,2))**2+(eta(ks,3)-e
     &ta(ls,3))**2
      
      generi=zero
      
      do irun=is,il
      do jrun=js,jl
      
C* 41: * 
*line 1070 "main.web"
      aexp=eta(irun,4)
      anorm=eta(irun,5)
      bexp=eta(jrun,4)
      bnorm=eta(jrun,5)
      
      t1=aexp+bexp
      deleft=one/t1
      p(1)=(aexp*eta(irun,1)+bexp*eta(jrun,1))*deleft
      p(2)=(aexp*eta(irun,2)+bexp*eta(jrun,2))*deleft
      p(3)=(aexp*eta(irun,3)+bexp*eta(jrun,3))*deleft
      pax=p(1)-eta(irun,1)
      pay=p(2)-eta(irun,2)
      paz=p(3)-eta(irun,3)
      pbx=p(1)-eta(jrun,1)
      pby=p(2)-eta(jrun,2)
      pbz=p(3)-eta(jrun,3)
      prefa=dexp(-aexp*bexp*rAB/t1)*pi*anorm*bnorm/t1
C* :41 * 
*line 936 "main.web"
      
      
C* 43: * 
*line 1138 "main.web"
      i1max=l1+l2+1
      j1max=m1+m2+1
      k1max=n1+n2+1
      mleft=i1max+j1max+k1max
      do n=1,i1max
      sf(n,1)=fj(l1,l2,n-1,pax,pbx)
      end do
      do n=1,j1max
      sf(n,2)=fj(m1,m2,n-1,pay,pby)
      end do
      do n=1,k1max
      sf(n,3)=fj(n1,n2,n-1,paz,pbz)
      end do
      call theta(i1max,sf,1,fact,t1,xleft)
      call theta(j1max,sf,2,fact,t1,yleft)
      call theta(k1max,sf,3,fact,t1,zleft)
C* :43 * 
*line 943 "main.web"
      
      do krun=ks,kl
      do lrun=ls,ll
      eribit=zero
      
C* 42: * 
*line 1100 "main.web"
      cexpp=eta(krun,4)
      cnorm=eta(krun,5)
      dexpp=eta(lrun,4)
      dnorm=eta(lrun,5)
      
      t2=cexpp+dexpp
      t2m1=one/t2
      fordel=t2m1+deleft
      q(1)=(cexpp*eta(krun,1)+dexpp*eta(lrun,1))*t2m1
      q(2)=(cexpp*eta(krun,2)+dexpp*eta(lrun,2))*t2m1
      q(3)=(cexpp*eta(krun,3)+dexpp*eta(lrun,3))*t2m1
      qcx=q(1)-eta(krun,1)
      qcy=q(2)-eta(krun,2)
      qcz=q(3)-eta(krun,3)
      qdx=q(1)-eta(lrun,1)
      qdy=q(2)-eta(lrun,2)
      qdz=q(3)-eta(lrun,3)
      r(1)=p(1)-q(1)
      r(2)=p(2)-q(2)
      r(3)=p(3)-q(3)
      t=(r(1)*r(1)+r(2)*r(2)+r(3)*r(3))/fordel
      prefc=exp(-cexpp*dexpp*rCD/t2)*pi*cnorm*dnorm/t2
C* :42 * 
*line 955 "main.web"
      
      w=pi/(t1+t2)
      
C* 44: * 
*line 1166 "main.web"
      i2max=l3+l4+1
      j2max=m3+m4+1
      k2max=n3+n4+1
      twodel=half*fordel
      delta=half*twodel
      do n=1,i2max
      sf(n,4)=fj(l3,l4,n-1,qcx,qdx)
      end do
      do n=1,j2max
      sf(n,5)=fj(m3,m4,n-1,qcy,qdy)
      end do
      do n=1,k2max
      sf(n,6)=fj(n3,n4,n-1,qcz,qdz)
      end do
      m=mleft+i2max+j2max+k2max+1
C* :44 * 
*line 964 "main.web"
      
      call auxg(m,t,g)
      
C* 45: * 
*line 1192 "main.web"
      ppx(1)=one
      bbx(1)=zero
      ppy(1)=one
      bby(1)=zero
      ppz(1)=one
      bbz(1)=zero
      jt1=i1max+i2max
      do n=2,jt1
      ppx(n)=-ppx(n-1)*r(1)
      bbx(n)=zero
      end do
      jt1=j1max+j2max
      do n=2,jt1
      ppy(n)=-ppy(n-1)*r(2)
      bby(n)=zero
      end do
      jt1=k1max+k2max
      do n=2,jt1
      ppz(n)=-ppz(n-1)*r(3)
      bbz(n)=zero
      end do
      call bform(i1max,i2max,sf,1,fact,xleft,t2,delta,ppx,bbx,xyorz)
      call bform(j1max,j2max,sf,2,fact,yleft,t2,delta,ppy,bby,xyorz)
      call bform(k1max,k2max,sf,3,fact,zleft,t2,delta,ppz,bbz,xyorz)
C* :45 * 
*line 972 "main.web"
      
      
      jt1=i1max+i2max-1
      jt2=j1max+j2max-1
      jt3=k1max+k2max-1
      do ii=1,jt1
      do jj=1,jt2
      do kk=1,jt3
      nu=ii+jj+kk-2
      if(xyorz.NE.0)nu=nu+1
      
      eribit=eribit+g(nu)*bbx(ii)*bby(jj)*bbz(kk)
      end do
      end do
      end do
      
      generi=generi+prefa*prefc*eribit*dsqrt(w)
      end do
      end do
      end do
      end do
      if(xyorz.EQ.0)generi=generi*two
      return
      end
C* :37 * 
C* 47: * 
*line 1235 "main.web"
      double precision function fj(l,m,j,a,b)
      implicit double precision(a-h,o-z)
      integer l,m,j
      double precision a,b
      double precision sum,term,aa,bb
      integer i,imax,imin
      double precision fact(20)
C* 39: * 
*line 1030 "main.web"
      data fact/1.0D00,1.0D00,2.0D00,6.0D00,24.0D00,120.0D00,720.0D00,50
     &40.0D00,40320.0D00,362880.0D00,3628800.0D00,39916800.0D00,47900160
     &0.0D00,6227020800.0D00,6*0.0D00/
C* :39 * 
*line 1244 "main.web"
      
      imax=min(j,l)
      imin=max(0,j-m)
      sum=0.0D00
      do i=imin,imax
      term=fact(l+1)*fact(m+1)/(fact(i+1)*fact(j-i+1))
      term=term/(fact(l-i+1)*fact(m-j+i+1))
      aa=1.0D00
      bb=1.0D00
      if((l-i).NE.0)aa=a**(l-i)
      if((m+i-j).NE.0)bb=b**(m+i-j)
      term=term*aa*bb
      sum=sum+term
      end do
      fj=sum
      return
      end
C* :47 * 
C* 49: * 
*line 1285 "main.web"
      subroutine theta(i1max,sf,isf,fact,t1,xleft)
      implicit double precision(a-h,o-z)
      integer i1max,isf
      double precision t1
      double precision sf(10,*),fact(*),xleft(5,*)
      integer i1,ir1,ir1max,jt2
      double precision zero,sfab,bbb
      data zero/0.0D00/
      do i1=1,10
      do ir1=1,5
      xleft(ir1,i1)=zero
      end do
      end do
      do 100 i1=1,i1max
      sfab=sf(i1,isf)
      if(sfab.EQ.zero)go to 100
      ir1max=(i1-1)/2+1
      bbb=sfab*fact(i1)/t1**(i1-1)
      do ir1=1,ir1max
      jt2=i1+2-ir1-ir1
      xleft(ir1,i1)=bbb*(t1**(ir1-1))/(fact(ir1)*fact(jt2))
      end do
100   continue
      return
      end
C* :49 * 
C* 51: * 
*line 1340 "main.web"
      subroutine bform(i1max,i2max,sf,isf,fact,xleft,t2,delta,ppx,bbx,xy
     &orz)
      implicit double precision(a-h,o-z)
      integer i1max,i2max,isf
      double precision fact(*),sf(10,*),xleft(5,*),bbx(*),ppx(20)
      double precision delta
      integer xyorz,itab
      double precision zero,one,two,twodel,fordel,sfab,sfcd
      double precision bbc,bbd,bbe,bbf,bbg,ppqq
      integer i1,i2,jt1,jt2,ir1max,ir2max
      data zero,one,two/0.0D00,1.0D00,2.0D00/
      itab=0
      if(xyorz.EQ.isf)itab=1
      twodel=two*delta
      fordel=two*twodel
      do 200 i1=1,i1max
      sfab=sf(i1,isf)
      if(sfab.EQ.zero)go to 200
      ir1max=(i1-1)/2+1
      do 210 i2=1,i2max
      sfcd=sf(i2,isf+3)
      if(sfcd.EQ.zero)go to 210
      jt1=i1+i2-2
      ir2max=(i2-1)/2+1
      bbc=((-one)**(i2-1))*sfcd*fact(i2)/(t2**(i2-1)*(fordel**jt1))
      do 220 ir1=1,ir1max
      jt2=i1+2-ir1-ir1
      bbd=bbc*xleft(ir1,i1)
      if(bbd.EQ.zero)go to 220
      do 230 ir2=1,ir2max
      jt3=i2+2-ir2-ir2
      jt4=jt2+jt3-2
      irumax=(jt4+itab)/2+1
      jt1=ir1+ir1+ir2+ir2-4
      bbe=bbd*(t2**(ir2-1))*(twodel**jt1)*fact(jt4+1)/(fact(ir2)*fact(jt
     &3))
      do 240 iru=1,irumax
      jt5=jt4-iru-iru+3
      ppqq=ppx(jt5)
      if(ppqq.EQ.zero)go to 240
      bbf=bbe*((-delta)**(iru-1))*ppqq/(fact(iru)*fact(jt5))
      bbg=one
      if(itab.EQ.1)then
      bbg=dfloat(jt4+1)*ppx(2)/(delta*dfloat(jt5))
      end if
      bbf=bbf*bbg
      nux=jt4-iru+2
      bbx(nux)=bbx(nux)+bbf
240   continue
230   continue
220   continue
210   continue
200   continue
      return
      end
C* :51 * 
C* 53: * 
*line 1430 "main.web"
      subroutine auxg(mmax,x,g)
      implicit double precision(a-h,o-z)
      integer mmax
      double precision x,g(*)
      double precision fmch
      double precision two,y
      integer mp1mx,mp1,md,mdm
      data two/2.0D00/
      y=dexp(-x)
      mp1mx=mmax+1
      g(mp1mx)=fmch(mmax,x,y)
      if(mmax.LT.1)go to 303
      
      do mp1=1,mmax
      md=mp1mx-mp1
      mdm=md-1
      g(md)=(two*x*g(md+1)+y)/dfloat(2*mdm+1)
      end do
303   return
      end
C* :53 * 
C* 55: * 
*line 1554 "main.web"
      double precision function fmch(nu,x,y)
C* 56: * 
*line 1570 "main.web"
      implicit double precision(a-h,o-z)
      double precision x,y
      integer nu
C* :56 * 
*line 1556 "main.web"
      
C* 57: * 
*line 1576 "main.web"
      double precision ten,half,one,zero,rootpi4,xd,crit
      double precision term,partialsum
      integer m,i,numberofterms,maxone,maxtwo
      data zero,half,one,rootpi4,ten/0.0D00,0.5D00,1.0D00,0.88622692D00,
     &10.0D00/
      data crit/1.0D-08/
      data maxone/50/,maxtwo/200/
C* :57 * 
*line 1557 "main.web"
      
      m=nu
      a=dfloat(m)
      if(x.LE.ten)then
C* 58: * 
*line 1589 "main.web"
      a=a+half
      term=one/a
      partialsum=term
      do i=2,maxone
      a=a+one
      term=term*x/a
      partialsum=partialsum+term
      if(term/partialsum.LT.crit)go to 111
      end do
111   continue
      if(i.EQ.maxone)then
      write(61,200)
200   format('i > 50 in fmch')
      STOP
      end if
      fmch=half*partialsum*y
      return
C* :58 * 
*line 1561 "main.web"
      
      else
C* 59: * 
*line 1609 "main.web"
      b=a+half
      a=a-half
      xd=one/x
      approx=rootpi4*dsqrt(xd)*xd**m
      if(m.GT.0)then
      do i=1,m
      b=b-one
      approx=approx*b
      end do
      end if
      fimult=half*y*xd
      partialsum=zero
      if(fimult.EQ.zero)then
      fmch=approx
      return
      end if
      fiprop=fimult/approx
      term=one
      partialsum=term
      numberofterms=maxtwo
      do i=2,numberofterms
      term=term*a*xd
      partialsum=partialsum+term
      if(dabs(term*fiprop/partialsum).LE.crit)then
      fmch=approx-fimult*partialsum
      return
      end if
      a=a-one
      end do
      write(61,201)
201   format(' numberofterms reached in fmch')
      STOP
C* :59 * 
*line 1563 "main.web"
      
      end if
      end
C* :55 * 
C* 63: * 
*line 1654 "main.web"
      integer function getint(file,i,j,k,l,mu,val,pointer)
      integer file,i,j,k,l,mu,pointer
      double precision val
      save
      integer max_pointer,id,iend
      double precision zero
      double precision value(20000)
      character*8 labels(20000)
      data max_pointer/0/,iend/-12/,zero/0.0D00/
      
      if(pointer.EQ.max_pointer)then
      if(iend.EQ.12)then
      val=zero
      i=0
      j=0
      k=0
      l=0
      max_pointer=0
      iend=-12
      getint=-1
      return
      end if
      read(file)max_pointer,iend,labels,value
      pointer=0
      end if
      pointer=pointer+1
      call unpack(labels(pointer),i,j,k,l,mu,id)
      val=value(pointer)
      getint=10
      return
      end
C* :63 * 
C* 65: * 
*line 1693 "main.web"
      subroutine putint(nfile,i,j,k,l,mu,val,pointer,last)
      implicit double precision(a-h,o-z)
      save
      integer nfile,i,j,k,l,mu,pointer,last
      double precision value(20000)
      character*8 labels(20000)
      double precision val
      data max_pointer/20000/,id/0/
      if(last.EQ.-10)go to 100
      iend=-12
      if(pointer.EQ.max_pointer)then
      write(nfile)pointer,iend,labels,value
      pointer=0
      end if
      pointer=pointer+1
      call pack(labels(pointer),i,j,k,l,mu,id)
      value(pointer)=val
      if(last.EQ.0)then
      iend=12
      last=-10
      write(nfile)pointer,iend,labels,value
      end if
100   return
      end
C* :65 * 
C* 67: * 
*line 1726 "main.web"
      subroutine genint(ngmx,nbfns,eta,ntype,ncntr,nfirst,nlast,vlist,nc
     &mx,noc,S,H,nfile)
      integer ngmx,nbfns,noc,ncmx
      double precision eta(1000,5),vlist(50,4)
      double precision S(1),H(1)
      integer ntype(1),nfirst(1),nlast(1),ncntr(1),nfile
      
      integer i,j,k,l,ltop,ij,ji,mu,m,n,jtyp,js,jf,ii,jj
      double precision generi,genoei
      integer pointer,last
      double precision ovltot,kintot
      double precision val,crit,alpha,t,t1,t2,t3,sum,pitern
      double precision SOO
      double precision gtoC(1000)
      double precision dfact(20)
      integer nr(20,3)
      data nr/0,1,0,0,2,0,0,1,1,0,3,0,0,2,2,1,0,1,0,1,0,0,1,0,0,2,0,1,0,
     &1,0,3,0,1,0,2,2,0,1,1,0,0,0,1,0,0,2,0,1,1,0,0,3,0,1,0,1,2,2,1/
      data crit,half,onep5,one,zero/1.0D-08,0.5D+00,1.5D+00,1.0D+00,0.0D
     &+00/
      data dfact/1.0,3.0,15.0,105.0,945.0,10395.0,135135.0,2027025.0,12*
     &0.0/
      data gtoC/1000*0.0D+00/
      mu=0
C* 68: * 
*line 1790 "main.web"
      do i=1,ngmx
      gtoC(i)=eta(i,5)
      end do
C* :68 * 
*line 1754 "main.web"
      
C* 69: * 
*line 1797 "main.web"
      pitern=5.568327997D+00
      do j=1,nbfns
      jtyp=ntype(j)
      js=nfirst(j)
      jf=nlast(j)
      l=nr(jtyp,1)
      m=nr(jtyp,2)
      n=nr(jtyp,3)
      do i=js,jf
      alpha=eta(i,4)
      SOO=pitern*(half/alpha)**1.5
      t1=dfact(l+1)/alpha**l
      t2=dfact(m+1)/alpha**m
      t3=dfact(n+1)/alpha**n
      eta(i,5)=one/dsqrt(SOO*t1*t2*t3)
      end do
      end do
      do j=1,nbfns
      jtyp=ntype(j)
      js=nfirst(j)
      jf=nlast(j)
      l=nr(jtyp,1)
      m=nr(jtyp,2)
      n=nr(jtyp,3)
      sum=zero
      do ii=js,jf
      do jj=js,jf
      t=one/(eta(ii,4)+eta(jj,4))
      SOO=pitern*(t**onep5)*eta(ii,5)*eta(jj,5)
      t=half*t
      t1=dfact(l+1)/t**l
      t2=dfact(m+1)/t**m
      t3=dfact(n+1)/t**n
      sum=sum+gtoC(ii)*gtoC(jj)*SOO*t1*t2*t3
      end do
      end do
      sum=one/sqrt(sum)
      do ii=js,jf
      gtoC(ii)=gtoC(ii)*sum
      end do
      end do
      do ii=1,ngmx
      eta(ii,5)=eta(ii,5)*gtoC(ii)
      end do
C* :69 * 
*line 1756 "main.web"
      
      
      DO i=1,nbfns
      DO j=1,i
      ij=(j-1)*nbfns+i
      ji=(i-1)*nbfns+j
      H(ij)=genoei(i,j,eta,ngmx,nfirst,nlast,ntype,nr,20,vlist,noc,ncmx,
     &ovltot,kintot)
      H(ji)=H(ij)
      S(ij)=ovltot
      S(ji)=ovltot
      END DO
      END DO
      write(*,*)" ONE ELECTRON INTEGRALS COMPUTED"
      
      rewind nfile
      pointer=0
      last=100
      i=1
      j=1
      k=1
      l=0
      DO 10 WHILE(next_label(i,j,k,l,nbfns).EQ.0)
      IF(l.EQ.nbfns)last=0
      val=generi(i,j,k,l,0,eta,ngmx,nfirst,nlast,ntype,nr,20)
      IF(dabs(val).LT.crit)go to 10
      CALL putint(nfile,i,j,k,l,mu,val,pointer,last)
10    CONTINUE
      
      return
      end
C* :67 * 
C* 73: * 
*line 1846 "main.web"
      
      subroutine gtprd(A,B,R,n,m,l)
      double precision A(1),B(1)
      double precision R(1)
      integer n,m,l
      double precision zero
      integer k,ik,j,ir,ij,ib
      data zero/0.0D+00/
      ir=0
      ik=-n
      do k=1,l
      ij=0
      ik=ik+m
      do j=1,m
      ir=ir+1
      ib=ik
      R(ir)=zero
      do i=1,n
      ij=ij+1
      ib=ib+1
      R(ir)=R(ir)+A(ij)*B(ib)
      enddo
      enddo
      enddo
      return
      end
C* :73 * 
C* 75: * 
*line 1877 "main.web"
      subroutine gmprd(A,B,R,n,m,l)
      double precision A(1),B(1)
      double precision R(1)
      integer n,m,l
      double precision zero
      integer k,ik,j,ir,ji,ib
      data zero/0.0D+00/
      ir=0
      ik=-m
      do k=1,l
      ik=ik+m
      do j=1,n
      ir=ir+1
      ji=j-n
      ib=ik
      R(ir)=zero
      do i=1,m
      ji=ji+n
      ib=ib+1
      R(ir)=R(ir)+A(ji)*B(ib)
      enddo
      enddo
      enddo
      return
      end
C* :75 * 
C* 77: * 
*line 1907 "main.web"
      subroutine eigen(H,U,n)
      implicit double precision(a-h,o-z)
      double precision H(1),U(1)
      integer n
      data zero,eps,one,two,four,big/0.0D+00,1.0D-20,1.0D+00,2.0D+00,4.0
     &D+00,1.0D+20/
      do i=1,n
      ii=(n*(i-1)+i)
      do j=1,n
      ij=(n*(j-1)+i)
      U(ij)=zero
      end do
      U(ii)=one
      end do
      hmax=big
      do 90 while(hmax.GT.eps)
      hmax=zero
      do i=2,n
      jtop=i-1
      do 10 j=1,jtop
      ii=(n*(i-1)+i)
      jj=(n*(j-1)+j)
      ij=(n*(j-1)+i)
      ji=(n*(i-1)+j)
      hii=H(ii)
      hjj=H(jj)
      hij=H(ij)
      hsq=hij*hij
      if(hsq.GT.hmax)hmax=hsq
      if(hsq.LT.eps)go to 10
      del=hii-hjj
      sign=one
      if(del.LT.zero)then
      sign=-one
      del=-del
      end if
      denom=del+dsqrt(del*del+four*hsq)
      tan=two*sign*hij/denom
      c=one/dsqrt(one+tan*tan)
      s=c*tan
      do 20 k=1,n
      kj=(n*(j-1)+k)
      ki=(n*(i-1)+k)
      jk=(n*(k-1)+j)
      ik=(n*(k-1)+i)
      temp=c*U(kj)-s*U(ki)
      U(ki)=s*U(kj)+c*U(ki)
      U(kj)=temp
      if((i.EQ.k).OR.(j.EQ.k))go to 20
      temp=c*H(kj)-s*H(ki)
      H(ki)=s*H(kj)+c*H(ki)
      H(kj)=temp
      H(ik)=H(ki)
      H(jk)=H(kj)
20    continue
      H(ii)=c*c*hii+s*s*hjj+two*c*s*hij
      H(jj)=c*c*hjj+s*s*hii-two*c*s*hij
      H(ij)=zero
      H(ji)=zero
10    continue
      end do
90    continue
      iq=-n
      do i=1,n
      iq=iq+n
      ii=(n*(i-1)+i)
      jq=n*(i-2)
      do j=i,n
      jq=jq+n
      jj=(n*(j-1)+j)
      if(H(ii).LT.H(jj))go to 30
      temp=H(ii)
      H(ii)=H(jj)
      H(jj)=temp
      do k=1,n
      ilr=iq+k
      imr=jq+k
      temp=U(ilr)
      U(ilr)=U(imr)
      U(imr)=temp
      end do
30    continue
      end do
      end do
      return
      end
C* :77 * 
C* 79: * 
*line 1985 "main.web"
      subroutine pack(a,i,j,k,l,m,n)
      character*8 a,b
      integer i,j,k,l,m,n
      data b/"        "/
      a=b
      a(1:1)=char(i)
      a(2:2)=char(j)
      a(3:3)=char(k)
      a(4:4)=char(l)
      a(5:5)=char(m)
      a(6:6)=char(n)
      return
      end
C* :79 * 
C* 81: * 
*line 2001 "main.web"
      subroutine unpack(a,i,j,k,l,m,n)
      character*8 a
      integer i,j,k,l,m,n
      i=ichar(a(1:1))
      j=ichar(a(2:2))
      k=ichar(a(3:3))
      l=ichar(a(4:4))
      m=ichar(a(5:5))
      n=ichar(a(6:6))
      return
      end
C* :81 * 
C* 83: * 
*line 2054 "main.web"
      integer function next_label(i,j,k,l,n)
      integer i,j,k,l,n
      
      integer ltop
      next_label=0
      ltop=k
      if(i.EQ.k)ltop=j
      if(l.LT.ltop)then
      l=l+1
      else
      l=1
      if(k.LT.i)then
      k=k+1
      else
      k=1
      if(j.LT.i)then
      j=j+1
      else
      j=1
      if(i.LT.n)then
      i=i+1
      else
      next_label=100
      end if
      end if
      end if
      end if
      return
      end
C* :83 * 
C* 85: * 
*line 2092 "main.web"
      subroutine shalf(S,U,W,m)
      implicit double precision(a-h,o-z)
      double precision S(*),U(*),W(*)
      integer m
      data crit,one/1.0D-10,1.0D+00/
      call eigen(S,U,m)
      do i=1,m
      do j=1,i
      ij=m*(j-1)+i
      ji=m*(i-1)+j
      d=U(ij)
      U(ij)=U(ji)
      U(ji)=d
      end do
      end do
      do i=1,m
      ii=(i-1)*m+i
      if(S(ii).LT.crit)then
      write(61,200)
      STOP
      end if
      S(ii)=one/dsqrt(S(ii))
      end do
      call gtprd(U,S,W,m,m,m)
      call gmprd(W,U,S,m,m,m)
      
      return
200   format(" Basis is linearly dependent; S is singular! ")
      end
C* :85 * 
C* 87: * 
*line 2128 "main.web"
      subroutine spinor(H,m)
      double precision H(*)
      integer m
      double precision zero
      integer i,j,ij,ji,ip,jp,ijp,ijd,nl,n
      data zero/0.0D+00/
      n=2*m
      nl=m+1
      do i=1,m
      do j=1,m
      ij=m*(j-1)+i
      ip=i+m
      jp=j+m
      ijp=n*(jp-1)+ip
      H(ijp)=H(ij)
      end do
      end do
      do i=1,m
      do j=1,m
      ip=i+m
      jp=j+m
      ijp=n*(jp-1)+ip
      ijd=n*(j-1)+i
      H(ijd)=H(ijp)
      end do
      end do
      do i=1,m
      do j=nl,n
      ij=n*(j-1)+i
      ji=n*(i-1)+j
      H(ij)=zero
      H(ji)=zero
      end do
      end do
      return
      end
C* :87 * 
      
