
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle scf.web"
C  RUN TIME:     "Sunday, December 7, 2014 at 0:38."
C  WEB FILE:     "scf.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 83 "scf.web"
      
      integer function SCF(H,C,nbasis,nelec,nfile,irite,damp,interp,E,HF
     &,V,R,Rold,Cbar,epsilon,crit)
C* 2: * 
*line 111 "scf.web"
      implicit double precision(a-h,o-z)
      integer nbasis,nelec,nfile,irite
      integer interp
      double precision H(ARB),C(ARB),HF(ARB),V(ARB),R(ARB)
      double precision Rold(ARB),Cbar(ARB)
      double precision epsilon(ARB)
      double precision E,damp,crit
C* :2 * 
*line 87 "scf.web"
      
C* 3: * 
*line 121 "scf.web"
      integer scftype,kount,maxit,nocc,m,mm,i
      double precision term,turm,Rsum
      double precision zero,half
      data zero,half/0.0D+00,0.5D+00/
C* :3 * 
*line 89 "scf.web"
      
C* 4: * 
*line 128 "scf.web"
      if(nelec.GT.zero)then
      scftype=CLOSED_SHELL_CALCULATION
      nocc=abs(nelec/2)
      m=nbasis
      else
      scftype=UHF_CALCULATION
      nocc=nelec
      m=nbasis*2
      call spinor(H,nbasis)
      call spinor(C,nbasis)
      end if
C* :4 * 
*line 91 "scf.web"
      
C* 5: * 
*line 142 "scf.web"
      m=m*m
      do i=1,mm
      R(i)=zero
      Rold(i)=zero
      end do
      SCF=YES
      kount=0
      icon=100
C* :5 * 
*line 93 "scf.web"
      
      do while((icon.NE.0).AND.(kount.LT.50))
C* 6: * 
*line 153 "scf.web"
      kount=kount+1
      E=zero
      icon=0
      do i=1,mm
      HF(i)=H(i)
      E=E+R(i)*HF(i)
      enddo
      call scfGR(R,HF,m,nfile)
      do i=1,mm
      E=E+R(i)*HF(i)
      enddo
      if(scftype.EQ.UHF_CALCULATION)E=half*E
      write(ERROR_OUTPUT_UNIT,200)E
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
C* :6 * 
*line 97 "scf.web"
      
      end do
C* 7: * 
*line 190 "scf.web"
      write(ERROR_OUTPUT_UNIT,201)Rsum,icon
      if((kount.EQ.50).AND.(icon.NE.0))then
      write(ERROR_OUTPUT_UNIT,204)
      else
      write(ERROR_OUTPUT_UNIT,202)kount
      write(ERROR_OUTPUT_UNIT,203)(epsilon(i),i=1,nocc)
      endif
C* :7 * 
*line 101 "scf.web"
      
C* 8: * 
*line 201 "scf.web"
200   format(" Current Electronic Energy = ",f12.6)
201   format(" Convergence in R = ",f12.5,i6,"  Changing")
202   format(" SCF converged in",i4," iterations")
203   format(" Orbital Energies ",(7f10.5))
204   format(" SCF did not converged... quitting")
C* :8 * 
*line 103 "scf.web"
      
      return
      end
C* :1 * 
C* 10: * 
*line 212 "scf.web"
      subroutine scfGR(R,G,m,nfile)
      double precision R(*),G(*)
      integer m,nfile
      
      integer mby2
      double precision val
      integer i,j,k,l,is,js,ks,ls,ijs,kls,mu
      integer getint
      double precision zero,one,a,b
      integer pointer,spin,skip
      data one,zero/1.0D+00,0.0D+00/
      
      mby2=m/2
      pointer=0
      do while(getint(nfile,is,js,ks,ls,mu,val,pointer).NE.END_OF_FILE)
      ijs=is*(is-1)/2+js
      kls=ks*(ks-1)/2+ls
      do spin=1,4
      skip=NO
      select case(spin)
      case(1)
      i=is
      j=js
      k=ks
      l=ls
      case(2)
      i=is+mby2
      j=js+mby2
      k=ks+mby2
      l=ls+mby2
      case(3)
      i=is+mby2
      j=js+mby2
      k=ks
      l=ls
      case(4)
      if(ijs.EQ.kls)skip=YES
      i=is
      j=js
      k=ks+mby2
      l=ls+mby2
      call order(i,j,k,l)
      end select
      if(skip.EQ.YES)cycle
      a=one
      b=one
      if(spin.GE.3)b=zero
      call GofR(R,G,m,a,b,i,j,k,l,val)
      end do
      enddo
      do i=1,m
      do j=1,i-1
      ij=(m*(j-1)+i)
      ji=(m*(i-1)+j)
      G(ji)=G(ij)
      end do
      end do
      return
      end
C* :10 * 
C* 12: * 
*line 286 "scf.web"
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
C* :12 * 
C* 14: * 
*line 329 "scf.web"
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
C* :14 * 
C* 16: * 
*line 363 "scf.web"
      subroutine scfR(C,R,m,nocc)
      double precision C(ARB),R(ARB)
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
C* :16 * 
      
