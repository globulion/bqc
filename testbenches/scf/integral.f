
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle integral.web"
C  RUN TIME:     "Sunday, December 7, 2014 at 0:37."
C  WEB FILE:     "integral.web"
C  CHANGE FILE:  (none)
      C* 3: * 
*line 21 "integral.web"
      
      
      double precision function genoei(i,j,eta,ngmx,nfirst,nlast,ntype,n
     &r,ntmx,vlist,noc,ncmx,ovltot,kintot)
      implicit double precision(a-h,o-z)
      integer i,j,ngmx,ncmx,noc,ntmx
      integer nfirst(*),nlast(*),ntype(*),nr(ntmx,3)
      double precision ovltot,kintot
      double precision eta(MAX_PRIMITIVES,5),vlist(MAX_CENTRES,4)
      
C* 4: * 
*line 126 "integral.web"
      double precision Airu(10),Ajsv(10),Aktw(10)
      double precision p(3),sf(10,3),tf(20)
      double precision fact(20),g(50)
      double precision kin
      data zero,one,two,half,quart/0.0d00,1.0d00,2.0d00,0.5d00,0.25d00/
      data pi/3.141592653589d00/
C* :4 * 
*line 34 "integral.web"
      
      
C* 15: * 
*line 469 "integral.web"
      data fact/1.0D00,1.0D00,2.0D00,6.0D00,24.0D00,120.0D00,720.0D00,50
     &40.0D00,40320.0D00,362880.0D00,3628800.0D00,39916800.0D00,47900160
     &0.0D00,6227020800.0D00,6*0.0D00/
C* :15 * 
*line 39 "integral.web"
      
      
C* 5: * 
*line 138 "integral.web"
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
C* :5 * 
*line 44 "integral.web"
      
      
      rAB=(eta(iss,1)-eta(jss,1))**2+(eta(iss,2)-eta(jss,2))**2+(eta(iss
     &,3)-eta(jss,3))**2
      
      genoei=zero
      totnai=zero
      kintot=zero
      ovltot=zero
      
      do irun=iss,il
      do jrun=jss,jl
C* 17: * 
*line 509 "integral.web"
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
C* :17 * 
*line 68 "integral.web"
      
C* 6: * 
*line 155 "integral.web"
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
C* :6 * 
*line 70 "integral.web"
      
      ovltot=ovltot+anorm*bnorm*ovl
C* 8: * 
*line 223 "integral.web"
      xl=dfloat(l2*(l2-1))
      xm=dfloat(m2*(m2-1))
      xn=dfloat(n2*(n2-1))
      xj=dfloat(2*(l2+m2+n2)+3)
      kin=s00*(bexp*(xj*ov0-two*bexp*(ov1+ov2+ov3))-half*(xl*ov4+xm*ov5+
     &xn*ov6))
C* :8 * 
*line 74 "integral.web"
      
      kintot=kintot+anorm*bnorm*kin
      
      tnai=zero
C* 9: * 
*line 230 "integral.web"
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
C* :9 * 
*line 82 "integral.web"
      
      do n=1,noc
      pn=zero
      
C* 12: * 
*line 309 "integral.web"
      cpx=p(1)-vlist(n,1)
      cpy=p(2)-vlist(n,2)
      cpz=p(3)-vlist(n,3)
      pcsq=cpx*cpx+cpy*cpy+cpz*cpz
C* :12 * 
*line 91 "integral.web"
      
      t=t1*pcsq
      call auxg(m,t,g)
C* 10: * 
*line 248 "integral.web"
      epsi=quart/t1
      do ii=1,10
      Airu(ii)=zero
      Ajsv(ii)=zero
      Aktw(ii)=zero
      end do
      call aform(imax,sf,fact,cpx,epsi,Airu,1)
      call aform(jmax,sf,fact,cpy,epsi,Ajsv,2)
      call aform(kmax,sf,fact,cpz,epsi,Aktw,3)
C* :10 * 
*line 97 "integral.web"
      
      
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
C* :3 * 
C* 7: * 
*line 185 "integral.web"
      
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
C* :7 * 
C* 11: * 
*line 275 "integral.web"
      
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
C* :11 * 
C* 13: * 
*line 321 "integral.web"
      
      double precision function generi(i,j,k,l,xyorz,eta,ngmx,nfirst,nla
     &st,ntype,nr,ntmx)
      implicit double precision(a-h,o-z)
      integer i,j,k,l,xyorz,ngmx,ntmx
      double precision eta(MAX_PRIMITIVES,5)
      integer nfirst(*),nlast(*),ntype(*),nr(ntmx,3)
      
C* 14: * 
*line 458 "integral.web"
      double precision p(3),q(3),ppx(20),ppy(20),ppz(20)
      double precision bbx(20),bby(20),bbz(20),sf(10,6)
      double precision xleft(5,10),yleft(5,10),zleft(5,10)
      double precision r(3),fact(20),g(50)
      data zero,one,two,half/0.0D00,1.0D00,2.0D00,0.5D00/
      data pi/3.141592653589D00/
C* :14 * 
*line 333 "integral.web"
      
      
C* 15: * 
*line 469 "integral.web"
      data fact/1.0D00,1.0D00,2.0D00,6.0D00,24.0D00,120.0D00,720.0D00,50
     &40.0D00,40320.0D00,362880.0D00,3628800.0D00,39916800.0D00,47900160
     &0.0D00,6227020800.0D00,6*0.0D00/
C* :15 * 
*line 338 "integral.web"
      
      
C* 16: * 
*line 479 "integral.web"
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
C* :16 * 
*line 345 "integral.web"
      
      
      rAB=(eta(is,1)-eta(js,1))**2+(eta(is,2)-eta(js,2))**2+(eta(is,3)-e
     &ta(js,3))**2
      rCD=(eta(ks,1)-eta(ls,1))**2+(eta(ks,2)-eta(ls,2))**2+(eta(ks,3)-e
     &ta(ls,3))**2
      
      generi=zero
      
      do irun=is,il
      do jrun=js,jl
      
C* 17: * 
*line 509 "integral.web"
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
C* :17 * 
*line 375 "integral.web"
      
      
C* 19: * 
*line 577 "integral.web"
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
C* :19 * 
*line 382 "integral.web"
      
      do krun=ks,kl
      do lrun=ls,ll
      eribit=zero
      
C* 18: * 
*line 539 "integral.web"
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
C* :18 * 
*line 394 "integral.web"
      
      w=pi/(t1+t2)
      
C* 20: * 
*line 605 "integral.web"
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
C* :20 * 
*line 403 "integral.web"
      
      call auxg(m,t,g)
      
C* 21: * 
*line 631 "integral.web"
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
C* :21 * 
*line 411 "integral.web"
      
      
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
C* :13 * 
C* 23: * 
*line 674 "integral.web"
      
      double precision function fj(l,m,j,a,b)
      implicit double precision(a-h,o-z)
      integer l,m,j
      double precision a,b
      double precision sum,term,aa,bb
      integer i,imax,imin
      double precision fact(20)
C* 15: * 
*line 469 "integral.web"
      data fact/1.0D00,1.0D00,2.0D00,6.0D00,24.0D00,120.0D00,720.0D00,50
     &40.0D00,40320.0D00,362880.0D00,3628800.0D00,39916800.0D00,47900160
     &0.0D00,6227020800.0D00,6*0.0D00/
C* :15 * 
*line 684 "integral.web"
      
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
C* :23 * 
C* 25: * 
*line 725 "integral.web"
      
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
C* :25 * 
C* 27: * 
*line 781 "integral.web"
      
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
C* :27 * 
C* 29: * 
*line 872 "integral.web"
      
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
C* :29 * 
C* 31: * 
*line 997 "integral.web"
      double precision function fmch(nu,x,y)
C* 32: * 
*line 1013 "integral.web"
      implicit double precision(a-h,o-z)
      double precision x,y
      integer nu
C* :32 * 
*line 999 "integral.web"
      
C* 33: * 
*line 1019 "integral.web"
      double precision ten,half,one,zero,rootpi4,xd,crit
      double precision term,partialsum
      integer m,i,numberofterms,maxone,maxtwo
      data zero,half,one,rootpi4,ten/0.0D00,0.5D00,1.0D00,0.88622692D00,
     &10.0D00/
      data crit/1.0D-08/
      data maxone/50/,maxtwo/200/
C* :33 * 
*line 1000 "integral.web"
      
      m=nu
      a=dfloat(m)
      if(x.LE.ten)then
C* 34: * 
*line 1032 "integral.web"
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
      write(ERROR_OUTPUT_UNIT,200)
200   format('i > 50 in fmch')
      STOP
      end if
      fmch=half*partialsum*y
      return
C* :34 * 
*line 1004 "integral.web"
      
      else
C* 35: * 
*line 1052 "integral.web"
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
      write(ERROR_OUTPUT_UNIT,201)
201   format(' numberofterms reached in fmch')
      STOP
C* :35 * 
*line 1006 "integral.web"
      
      end if
      end
C* :31 * 
      
