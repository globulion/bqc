
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -TD gints.web"
C  RUN TIME:     "Sunday, December 7, 2014 at 14:58."
C  WEB FILE:     "gints.web"
C  CHANGE FILE:  (none)
*line 12 "gints.web"
      
      
      
      integer function getint(file,i,j,k,l,mu,val,pointer)
      integer file,i,j,k,l,mu,pointer
      double precision val
      save
      integer max_pointer,id,iend
      double precision zero
      double precision labels(20),value(20)
      data max_pointer/0/,iend/0/,zero/0.0D00/
      
      if(pointer.EQ.max_pointer)then
      if(iend.EQ.1)then
      val=zero
      i=0
      j=0
      k=0
      l=0
      max_pointer=0
      iend=0
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
C* :1 * 
C* 3: * 
*line 54 "gints.web"
      
      subroutine putint(nfile,i,j,k,l,mu,val,pointer,last)
      implicit double precision(a-h,o-z)
      save
      integer nfile,i,j,k,l,mu,pointer,last
      double precision labels(20),value(20)
      double precision val
      data max_pointer/20/,id/0/
      
      
      if(last.EQ.-10)go to 100
      iend=0
      if(pointer.EQ.max_pointer)then
      write(nfile)pointer,iend,labels,value
      pointer=0
      end if
      pointer=pointer+1
      call pack(labels(pointer),i,j,k,l,mu,id)
      value(pointer)=val
      if(last.EQ.0)then
      iend=1
      last=-10
      write(nfile)pointer,iend,labels,value
      end if
      
100   return
      end
C* :3 * 
C* 5: * 
*line 89 "gints.web"
      
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
C* 6: * 
*line 153 "gints.web"
      
      do i=1,ngmx
      gtoC(i)=eta(i,5)
      end do
C* :6 * 
*line 117 "gints.web"
      
C* 7: * 
*line 162 "gints.web"
      
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
      SOO=pitern*(half/alpha)**1.5+D00
      t1=dfact(l+1)/alpha**l
      t2=dfact(m+1)/alpha**m
      t3=dfact(n+1)/alpha**n
      write(*,*)dfact(l+1),dfact(m+1),dfact(n+1),l,m,n
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
      sum=one/dsqrt(sum)
      do ii=js,jf
      gtoC(ii)=gtoC(ii)*sum
      end do
      end do
      do ii=1,ngmx
      eta(ii,5)=eta(ii,5)*gtoC(ii)
      end do
C* :7 * 
*line 119 "gints.web"
      
      
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
      last=1
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
C* :5 * 
C* 9: * 
*line 214 "gints.web"
      
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
      chr1(ii)=chr2((ii-1)*4+1)
      end do
      a=word
      return
      end
C* :9 * 
C* 11: * 
*line 238 "gints.web"
      
      subroutine unpack(a,i,j,k,l,m,n)
      double precision a
      integer i,j,k,l,m,n
      double precision word
      integer id(6)
      character*1 chr1(8),chr2(24)
      equivalence(word,chr1(1)),(id(1),chr2(1))
      do ii=1,6
      chr2((ii-1)*4+1)=chr1(ii)
      end do
      id(1)=i
      id(2)=j
      id(3)=k
      id(4)=l
      id(5)=m
      id(6)=n
      return
      end
C* :11 * 
C* 13: * 
*line 301 "gints.web"
      
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
      next_label=1
      end if
      end if
      end if
      end if
      return
      end
C* :13 * 
      
