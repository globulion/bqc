
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle utilities.web"
C  RUN TIME:     "Sunday, December 7, 2014 at 0:38."
C  WEB FILE:     "utilities.web"
C  CHANGE FILE:  (none)
      C* 3: * 
*line 6 "matrix.web"
      
      subroutine gtprd(A,B,R,n,m,l)
      double precision A(ARB),B(ARB)
      double precision R(ARB)
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
C* :3 * 
C* 6: * 
*line 38 "matrix.web"
      subroutine gmprd(A,B,R,n,m,l)
      double precision A(ARB),B(ARB)
      double precision R(ARB)
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
C* :6 * 
C* 9: * 
*line 69 "matrix.web"
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
C* :9 * 
C* 11: * 
*line 10 "utilities.web"
      
*line 10 "utilities.web"
      subroutine pack(a,i,j,k,l,m,n)
      double precision a
      integer i,j,k,l,m,n
      double precision word
      integer id(6)
      character*1 chr1(8),chr2(24)
      equivalence(word,chr1(1)),(id(1),chr2(1))
      id(1)=i
      id(2)=j
      id(3)=k
      id(4)=l
      id(5)=m
      id(6)=n
      do ii=1,6
      chr1(ii)=chr2((ii-1)*BYTES_PER_INTEGER+LEAST_BYTE)
      end do
      a=word
      return
      end
C* :11 * 
C* 13: * 
*line 34 "utilities.web"
      
      subroutine unpack(a,i,j,k,l,m,n)
      double precision a
      integer i,j,k,l,m,n
      double precision word
      integer id(6)
      character*1 chr1(8),chr2(24)
      equivalence(word,chr1(1)),(id(1),chr2(1))
      do ii=1,6
      chr2((ii-1)*BYTES_PER_INTEGER+LEAST_BYTE)=chr1(ii)
      end do
      id(1)=i
      id(2)=j
      id(3)=k
      id(4)=l
      id(5)=m
      id(6)=n
      return
      end
C* :13 * 
C* 15: * 
*line 97 "utilities.web"
      
      integer function next_label(i,j,k,l,n)
      integer i,j,k,l,n
      
      integer ltop
      next_label=YES
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
      next_label=NO
      end if
      end if
      end if
      end if
      return
      end
C* :15 * 
C* 17: * 
*line 136 "utilities.web"
      
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
      write(ERROR_OUTPUT_UNIT,200)
      STOP
      end if
      S(ii)=one/dsqrt(S(ii))
      end do
      call gtprd(U,S,W,m,m,m)
      call gmprd(W,U,S,m,m,m)
      
      return
200   format(" Basis is linearly deoendent; S is singular! ")
      end
C* :17 * 
C* 19: * 
*line 173 "utilities.web"
      
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
C* :19 * 
      
